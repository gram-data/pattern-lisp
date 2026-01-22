# Data Model: Plisp–Gram Convert CLI

**Date**: 2025-01-30  
**Feature**: 008-plisp-gram-convert

## Entities

### ConvertMode

**Type**: Sum type (in implementation: `ToGram` | `ToPlisp`)

**Values**:
- `ToGram`: Convert plisp file → gram file.
- `ToPlisp`: Convert gram file → plisp file.

**Validation**: Exactly one of `--to-gram` or `--to-plisp` must be set when in convert mode. Mutually exclusive with eval/REPL modes (`-e`, `-i`).

---

### ConvertOptions

**Type**: Record / struct

**Fields**:
- `mode :: ConvertMode`
- `inputPath :: FilePath`
- `outputPath :: Maybe FilePath` — if `Nothing`, derive from `inputPath` and `mode` (see Default output path).

**Validation**:
- `inputPath` must be a single existing file (at runtime); extension should match mode (`.plisp` for `ToGram`, `.gram` or `.plisp.gram` for `ToPlisp`) for user clarity, but the implementation may only check at run time.
- `outputPath`: if `Just p`, use `p`; if `Nothing`, apply default rule.

---

### Default output path

**Rules** (from research.md):
- **ToGram**: `input.plisp` → `input.plisp.gram`. If the input has no `.plisp` suffix, append `.plisp.gram`.
- **ToPlisp**: `input.gram` → `input.plisp`; `input.plisp.gram` → `input.plisp`. If the input has neither `.gram` nor `.plisp.gram`, replace the final extension with `.plisp` or append `.plisp`.

**Implementation note**: Use `replaceExtension` / `dropExtension` or equivalent; exact behavior for edge names (e.g. `foo.plisp.bak`) can follow “replace `.plisp`” or “replace last extension” as decided in tasks.

---

### valueToPlispSource

**Type**: `Value -> Either Error String`

**Role**: Serialize a pattern-lisp `Value` to valid plisp source that parses and evaluates to an equivalent value. Used by gram→plisp when turning `gramToProgram`’s `[Value]` into plisp text.

**Input**: Any `Value` (VInteger, VString, VBoolean, VArray, VMap, VSet, VClosure, VKeyword, VDecimal, VSymbol, VPrimitive, etc.).

**Output**: `Right String` — plisp source (e.g. `"42"`, `"(lambda (x) (+ x 1))"`, `"{ a: 1 }"`, `"#{ 1 2 }"`). `Left Error` for values that cannot be serialized (e.g. VPattern if we defer that).

**Validation**: Emitted plisp must be parseable by `parseExpr` (or `parseFileContent` for `(begin ...)`) and, when evaluated, produce a value that is equivalent (per pattern-lisp equality) to the input. No `formatValue`-style placeholders like `"<closure>"` for round-trip.

---

### exprToPlisp

**Type**: `Expr -> String`

**Role**: Serialize an `Expr` to plisp source. Helper for `valueToPlispSource` (e.g. closure body) and any Expr that we need to print.

**Input**: `Expr` (List, Quote, Atom, RecordLiteral, etc.).

**Output**: `String` — plisp source that parses back to the same Expr (or equivalent).

**Validation**: Round-trip `parseExpr (exprToPlisp e) ≡ Right e` (or structurally equivalent) for all Expr forms we support.

---

## Data flow

### Plisp→gram

1. Read `inputPath` as text.
2. `parseFileContent` → `Expr`; on failure → `ParseError` to stderr, exit 1.
3. `evalExprWithEnv expr initialEnv` → `(Value, Env)`; on failure → `TypeMismatch`/etc. to stderr, exit 1.
4. `programToGram [value] initialEnv` → `String` (gram).
5. Write to `outputPath` (overwrite). On write failure → stderr, exit 1.

### Gram→plisp

1. Read `inputPath` as text.
2. `gramToProgram` → `Either Error ([Value], Env)`. On `Left` (parse, missing `kind: "Pattern Lisp"`, or `patternSubjectToValue` failure) → error to stderr, exit 1.
3. `mapM valueToPlispSource values` → `Either Error [String]`. On `Left` → stderr, exit 1.
4. Join as `(begin v1 v2 ...)` (or one expr per value; `begin` chosen in research).
5. Write to `outputPath` (overwrite). On write failure → stderr, exit 1.

---

## State and side effects

- **File system**: Read one input file; write one output file. Overwrite by default.
- **Process**: Exit 0 on success; exit 1 on any error. All errors to stderr; successful output (gram or plisp) to the output file only, not stdout.
