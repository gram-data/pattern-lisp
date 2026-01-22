# Contracts: Plisp–Gram Convert CLI

**Date**: 2025-01-30  
**Feature**: 008-plisp-gram-convert  
**Type**: CLI interface and library functions

## Overview

This document specifies the CLI contract for the convert modes and the library functions used to implement them. The convert feature adds `--to-gram` and `--to-plisp` flags to the pattern-lisp executable.

---

## CLI: Convert mode

### Invocation

```text
pattern-lisp --to-gram INPUT.plisp [-o OUTPUT]
pattern-lisp --to-plisp INPUT.gram   [-o OUTPUT]
```

- **`--to-gram`**: Convert plisp → gram. `INPUT` must be a `.plisp` file (or any path; read as plisp source).
- **`--to-plisp`**: Convert gram → plisp. `INPUT` must be a `.gram` or `.plisp.gram` file (or any path; read as gram).
- **`-o` / `--output`** (optional): Output path. If omitted, see [Default output paths](#default-output-paths).
- **`INPUT`**: One positional argument; the file to convert.

### Constraints

- Exactly one of `--to-gram` or `--to-plisp` must be present. If both or neither: print usage to stderr, exit 1.
- Convert mode is **mutually exclusive** with `-e`/`--eval`, `-i`/`--interactive`, and with “load files and eval” (no `--to-*`). If `--to-gram` or `--to-plisp` is present together with `-e` or `-i`: error, exit 1.
- Exactly one input file. Zero or more than one: error, exit 1.

### Default output paths

- **`--to-gram`**: `foo.plisp` → `foo.plisp.gram`; if the base name has no `.plisp`, append `.plisp.gram`.
- **`--to-plisp`**: `foo.gram` or `foo.plisp.gram` → `foo.plisp`; if the extension is neither, replace the final extension with `.plisp` or append `.plisp`.

### Exit codes

| Code | Meaning |
|------|---------|
| 0 | Conversion succeeded; output written to the output file. |
| 1 | Error: invalid args, missing file, parse error, eval error (plisp→gram), gram structure error (gram→plisp), or write failure. |

### Output and errors

- **Success**: No stdout. The output file contains the result (gram text or plisp text).
- **Errors**: All error messages to **stderr**. Human-readable; no JSON. Must include enough context (e.g. file path, “parse error”, “expected kind: Pattern Lisp”) to satisfy FR-008.

### Input/output formats

- **Plisp input** (for `--to-gram`): Pattern-lisp source; may be one or more expressions (as in `loadPlispFile`). The file is parsed and evaluated; the final result (one Value) is serialized as the program `[value]` via `programToGram`.
- **Gram input** (for `--to-plisp`): Gram notation. Must be a **pattern-lisp program**: first pattern has `{ kind: "Pattern Lisp" }`; remaining patterns are value patterns that `patternSubjectToValue` accepts. Any other gram (valid gram but not a pattern-lisp program) yields an error.
- **Gram output** (from `--to-gram`): Gram text with `{ kind: "Pattern Lisp" }` and one or more value patterns (one for the single result of the plisp file).
- **Plisp output** (from `--to-plisp`): Plisp text: `(begin v1 v2 ...)` where each `vi` is the plisp source for the corresponding `Value` in the program.

---

## Library contracts

### valueToPlispSource

**Signature**: `Value -> Either Error String`

**Pre**: None.

**Post**:
- `Right s`: `s` is valid plisp; `parseExpr s` or `parseFileContent s` succeeds and the evaluated value is equivalent to the input.
- `Left e`: `e` describes why the value cannot be serialized (e.g. unsupported type).

**Covers**: VInteger, VString, VBoolean, VArray, VMap, VSet, VClosure, VKeyword, VDecimal, VSymbol, VPrimitive. VPattern may be `Left` or a placeholder in this phase.

---

### exprToPlisp

**Signature**: `Expr -> String`

**Pre**: `Expr` is in the form produced by the parser and handled by the evaluator.

**Post**: The returned string is valid plisp and parses back to an equivalent `Expr`.

---

## Error messages (contract)

The following must be reported clearly on stderr (exact wording can vary):

- Missing or invalid `--to-gram` / `--to-plisp` usage (both, neither, or with `-e`/`-i`).
- Missing input file or wrong number of arguments.
- File not found / unreadable `INPUT`.
- **Plisp→gram**: Parse error; evaluation error (undefined variable, type mismatch, etc.).
- **Gram→plisp**: Gram parse error; file missing `kind: "Pattern Lisp"`; `patternSubjectToValue` failure; `valueToPlispSource` failure.
- Inability to write `OUTPUT` (permission, disk, etc.).
