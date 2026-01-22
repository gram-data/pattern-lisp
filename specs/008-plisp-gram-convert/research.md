# Research: Plisp–Gram Convert CLI

**Date**: 2025-01-30  
**Feature**: 008-plisp-gram-convert  
**Purpose**: Resolve open design explorations from the spec and technical unknowns

## 1. Gram→plisp when gram lacks pattern-lisp program structure

**Question** (from spec): Parse gram into Pattern Subject data, then determine if any of the data can be evaluated; resulting behavior (output plisp, partial plisp, or error) was left open.

**Decision**: For **this feature (Phase 1)**, only support gram that contains a valid pattern-lisp program: i.e. `gramToProgram` succeeds (file has `{ kind: "Pattern Lisp" }` and a sequence of value patterns that `patternSubjectToValue` accepts). If the gram parses as valid gram but does not meet that (e.g. missing `kind: "Pattern Lisp"`, or pattern(s) that cannot be converted to Value), the CLI **reports a clear error** and exits with non-zero; it does **not** output plisp. The “parse into data, then evaluate what can be evaluated” and “single global for raw data” design is **deferred** to a future iteration.

**Rationale**:
- Keeps scope bounded and matches existing `gramToProgram` behavior.
- Delivers the main user value: round-trip for pattern-lisp programs and `.plisp.gram`; clear failure for other gram.
- Aligns with FR-005/FR-008: we do not yet define behavior for gram-without-program; we only implement the “has program” path.

**Alternatives considered**:
- **Implement “parse→data→evaluate” now**: Would require defining how to turn arbitrary Pattern Subject into plisp (e.g. single `global`), symbol resolution, and partial output semantics; too large for this feature.
- **Best-effort plisp from any gram**: Risk of emitting plisp that misrepresents the input; spec forbids that.

---

## 2. Combining raw data with evaluatable expressions / single `global`

**Question** (from spec): How to combine raw data with evaluatable expressions? Must data become variable declarations? Could it be a single `global`?

**Decision**: For **Phase 1**, this does not apply: we only support gram that is a full pattern-lisp program. When we **later** support gram that lacks program structure, we will adopt a **single global** (or analogous) binding for the parsed Pattern Subject data, following Clojure/Racket/Common Lisp conventions (see spec Research). The data will **not** be forced into one `define` per field; one structured value (e.g. a map or similar) that expressions can reference is the target design. The exact plisp shape (e.g. `(define *gram-data* ...)`) is left to that future design.

**Rationale**: Spec Research and Clarifications already summarize that a single global for structured data is idiomatic; we record that as the chosen direction for the deferred work only.

---

## 3. Value→plisp source (gram→plisp output)

**Question**: `gramToProgram` yields `[Value]`. We need to emit plisp source that would parse and evaluate back to equivalent values. No existing `Value → plisp text` exists.

**Decision**: Implement **`valueToPlispSource :: Value -> Either Error String`** (in `PatternLisp.Codec` or a dedicated helper used by Codec) that serializes `Value` to valid plisp. Implement **`exprToPlisp :: Expr -> String`** as a helper for expressions (e.g. closure bodies, nested structure). Use them when converting gram→plisp: `gramToProgram` → `[Value]` → map `valueToPlispSource` → join as `(begin v1 v2 ...)` or one expression per line (see Default output path and format below). Support at least: `VInteger`, `VString`, `VBoolean`, `VArray` (list), `VMap` (record `{ ... }`), `VSet` (`#{ ... }`), `VClosure` → `(lambda (params) body)` with `exprToPlisp` for body, `VKeyword`, `VDecimal`, `VSymbol`, `VPrimitive` (as symbol name), `VPattern` (defer or represent as `<pattern>` placeholder if needed). Emit syntax that the current Parser and Eval accept.

**Rationale**:
- gram→plisp cannot be implemented without a Value→plisp printer.
- Reusing `patternSubjectToExpr` is not sufficient: it works on Pattern Subject in “expr-like” form; gram stores Values in Value form (`:Closure`, `:Scope`, etc.). The inverse of `valueToPatternSubjectForGram` is `patternSubjectToValue`, which gives `Value`, not `Expr`. So we need a direct Value→plisp path.
- `exprToPlisp` supports closure bodies and any Expr that we reconstruct from Values (e.g. when we have `Closure` and its `body :: Expr`).

**Alternatives considered**:
- **Value → Pattern Subject (expr-shaped) → patternSubjectToExpr → print**: The Gram/Codec Pattern form for Values is not expr-shaped; we would need a second, expr-shaped encoding of Value. More indirect and error-prone than Value→plisp.
- **Reuse `formatValue`**: That is for display, not necessarily valid plisp; e.g. `"<closure>"` does not round-trip.

---

## 4. Default output path and .plisp.gram

**Decision**:
- **Plisp→gram**: If `-o`/`--output` is omitted, default output path is the input path with `.plisp` replaced by **`.plisp.gram`** (e.g. `foo.plisp` → `foo.plisp.gram`). If the user sets `-o out.gram`, that is used as-is.
- **Gram→plisp**: If `-o`/`--output` is omitted, default output path is the input path with `.gram` or `.plisp.gram` replaced by **`.plisp`** (e.g. `foo.plisp.gram` → `foo.plisp`, `x.gram` → `x.plisp`). If the user sets `-o out.plisp`, that is used as-is.
- **Multiple values (gram→plisp)**: Emit `(begin v1 v2 ...)` as a single plisp expression so that `parseFileContent` / `loadPlispFile` can read it and reproduce the same sequence of values (last value = result of `begin`; for full round-trip of [Value] we rely on the fact that programToGram/gramToProgram treat the program as a sequence and we emit one `begin` with all values, which matches the “one expression” convention for a file that represents a sequence).

**Rationale**: Aligns with FR-003, FR-006, and the `.plisp.gram` convention; `begin` is already used by `loadPlispFile` for multi-expr files.

---

## 5. CLI surface: subcommand vs flags

**Question**: Spec says “option or subcommand”. Existing pattern-lisp has flags `-e`, `-i`, `-h` and positional files.

**Decision**: Use **flags** (no subcommands): `--to-gram` and `--to-plisp`. Exactly one of them must be set when converting. One positional input file; optional `-o`/`--output`. Examples:
- `pattern-lisp --to-gram script.plisp`
- `pattern-lisp --to-gram script.plisp -o out.plisp.gram`
- `pattern-lisp --to-plisp out.plisp.gram`
- `pattern-lisp --to-plisp x.gram -o x.plisp`

If both `--to-gram` and `--to-plisp` are given, or neither, or no input file: print usage and exit with failure. `--to-gram`/`--to-plisp` are mutually exclusive with the existing “load files and eval” mode: when `--to-gram` or `--to-plisp` is present, the process runs in “convert mode” only (no `-e`/`-i`, no multi-file load). If `-e`/`-i` is combined with `--to-gram`/`--to-plisp`, error.

**Rationale**: Keeps a single binary and avoids a formal subcommand parser; flags are consistent with `-e`, `-i`, `-h`. Clear and easy to document.

**Alternatives considered**:
- **Subcommand** `convert to-gram` / `convert to-plisp`: More explicit but would require subcommand dispatch and different help; we can evolve to that later if needed.
