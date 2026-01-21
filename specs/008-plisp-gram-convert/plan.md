# Implementation Plan: Plisp–Gram Convert CLI Option

**Branch**: `008-plisp-gram-convert` | **Date**: 2025-01-30 | **Spec**: [spec.md](./spec.md)  
**Input**: Feature specification from `specs/008-plisp-gram-convert/spec.md`

## Summary

Add CLI options to the pattern-lisp executable to convert a single plisp file to gram (`--to-gram`) and a single gram file to plisp (`--to-plisp`). Plisp→gram uses existing `programToGram` and load/eval; gram→plisp uses `gramToProgram` and a new Value→plisp printer. Default output paths are derived from the input (e.g. `foo.plisp` → `foo.plisp.gram` or `foo.gram`). The `.plisp.gram` convention is supported. Gram that does not contain a valid pattern-lisp program (`kind: "Pattern Lisp"` and expression sequence) fails with a clear error in this phase; the “parse into data, then evaluate” path is deferred (see research.md).

## Technical Context

**Language/Version**: Haskell (GHC 4.18+), Cabal 3.x  
**Primary Dependencies**: megaparsec, mtl, gram, pattern, subject, filepath (existing)  
**Storage**: File I/O only (read plisp/gram, write gram/plisp)  
**Testing**: hspec (existing); new tests in `test/PatternLisp/` and integration in `test/IntegrationSpec.hs`  
**Target Platform**: Same as pattern-lisp (OS-agnostic CLI)  
**Project Type**: Single executable (extend existing `app/Main.hs`)  
**Performance Goals**: Same as current CLI (subsecond for typical files)  
**Constraints**: One file per invocation; overwrite output by default  
**Scale/Scope**: Single-file convert; batch/recursion out of scope

## Constitution Check

*GATE: Must pass before Phase 0 research. Re-check after Phase 1 design.*

**Post–Phase 1**: No change. Conversion remains in-library + thin CLI; text I/O; TDD; round-trip and error contracts; errors to stderr.

| Principle | Status | Notes |
|-----------|--------|-------|
| **I. Library-First** | Pass | Conversion logic lives in library (Codec, new valueToPlispSource); CLI is a thin wrapper. |
| **II. CLI Interface** | Pass | Text in/out: file paths and -o; stdout=result, stderr=errors. Human-readable only (JSON not in scope for convert). |
| **III. Test-First** | Pass | TDD: tests for convert modes, default paths, .plisp.gram, errors; then implement. |
| **IV. Integration Testing** | Pass | Contract: convert subcommand; round-trip tests (plisp→gram→plisp). |
| **V. Observability** | Pass | Errors to stderr; structured messages (file, direction, kind of error). |

## Project Structure

### Documentation (this feature)

```text
specs/008-plisp-gram-convert/
├── plan.md
├── research.md
├── data-model.md
├── quickstart.md
├── contracts/
│   └── README.md
├── checklists/
│   └── requirements.md
└── tasks.md           # from /speckit.tasks, not /speckit.plan
```

### Source Code (repository root)

```text
app/
└── Main.hs            # Add --to-gram, --to-plisp, -o; convert logic

src/PatternLisp/
├── Codec.hs           # Add valueToPlispSource (or new module if preferred)
├── FileLoader.hs      # Reuse loadPlispFile; no change for gram load
├── Gram.hs            # Reuse patternToGram, gramToPattern
└── ...

test/
├── PatternLisp/
│   └── ConvertSpec.hs # New: convert-to-gram, convert-to-plisp, errors, .plisp.gram
├── IntegrationSpec.hs # Add round-trip: plisp→gram→plisp
└── Spec.hs            # Register ConvertSpec
```

**Structure Decision**: Extend existing `app/Main.hs` and `src/PatternLisp`; no new packages or top-level dirs. New `test/PatternLisp/ConvertSpec.hs` for convert-specific tests.

## Complexity Tracking

> No constitution violations. This section is empty.
