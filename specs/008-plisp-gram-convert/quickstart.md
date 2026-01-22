# Quickstart: Plisp–Gram Convert CLI

**Date**: 2025-01-30  
**Feature**: 008-plisp-gram-convert

## Overview

The pattern-lisp CLI can convert a single plisp file to gram and a single gram file (that is a pattern-lisp program) to plisp. One file per run; output path is derived from the input if `-o` is omitted. The `.plisp.gram` suffix is used by default for plisp→gram.

## Prerequisites

- Built `pattern-lisp` executable (`cabal build`).
- A `.plisp` file (for `--to-gram`) or a `.gram` / `.plisp.gram` file that contains a pattern-lisp program (for `--to-plisp`).

## Convert plisp → gram

```bash
# Default output: script.plisp.gram
pattern-lisp --to-gram script.plisp

# Explicit output
pattern-lisp --to-gram script.plisp -o out.gram
pattern-lisp --to-gram script.plisp -o out.plisp.gram
```

- Reads `script.plisp`, parses and evaluates it, then writes gram (with `{ kind: "Pattern Lisp" }` and the value pattern) to the output.
- Fails with an error to stderr and exit 1 if the plisp does not parse or evaluate.

## Convert gram → plisp

```bash
# Default output: program.plisp (from program.plisp.gram or program.gram)
pattern-lisp --to-plisp program.plisp.gram
pattern-lisp --to-plisp state.gram

# Explicit output
pattern-lisp --to-plisp program.plisp.gram -o program.plisp
```

- Reads the gram file. It must be a pattern-lisp program (first pattern has `{ kind: "Pattern Lisp" }`, rest are value patterns).
- Writes plisp as `(begin v1 v2 ...)` so it parses and evaluates to the same sequence of values.
- Fails with an error to stderr and exit 1 if the gram is not a pattern-lisp program or a value cannot be turned into plisp.

## Round-trip

```bash
pattern-lisp --to-gram foo.plisp                    # creates foo.plisp.gram
pattern-lisp --to-plisp foo.plisp.gram              # creates foo.plisp
# foo.plisp should match the original (or be equivalent when run)
```

## Errors

All errors go to stderr. Exit 0 only on success. Common cases:

- `--to-gram` and `--to-plisp` both or neither: usage, exit 1.
- `-e` or `-i` used with `--to-gram`/`--to-plisp`: error, exit 1.
- Missing or extra input file: error, exit 1.
- Plisp parse or eval error: message to stderr, exit 1.
- Gram missing `kind: "Pattern Lisp"` or invalid value patterns: message to stderr, exit 1.
- Cannot write output file: message to stderr, exit 1.

## Help

```bash
pattern-lisp -h
# or
pattern-lisp --help
```

The help text will include `--to-gram`, `--to-plisp`, and `-o`/`--output` once the feature is implemented.
