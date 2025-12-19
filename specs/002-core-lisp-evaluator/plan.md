# Implementation Plan: Core Lisp Evaluator

**Branch**: `002-core-lisp-evaluator` | **Date**: 2025-01-27 | **Spec**: [spec.md](spec.md)
**Input**: Feature specification from `/specs/002-core-lisp-evaluator/spec.md`

**Note**: This template is filled in by the `/speckit.plan` command. See `.specify/templates/commands/plan.md` for the execution workflow.

## Summary

Build a minimal Lisp interpreter with S-expression syntax, implementing core language forms (lambda, if, let, quote, begin, define), arithmetic and comparison operators, string primitives, and an interactive REPL. The interpreter uses environment-based evaluation with lexical scoping and closures. Implementation uses Haskell with megaparsec for parsing, following a library-first architecture with CLI interface. Property-based tests verify key evaluation laws (substitution semantics, closure behavior).

## Technical Context

**Language/Version**: Haskell with GHC 9.10.3 (as per existing project setup)  
**Primary Dependencies**: megaparsec (parsing), text (string handling), containers (data structures), mtl (monad transformers), gram (from gram-hs for future serialization), hspec (testing), QuickCheck (property-based testing)  
**Storage**: N/A (in-memory evaluation, no persistence required)  
**Testing**: hspec for unit/integration tests, QuickCheck for property-based tests verifying evaluation laws  
**Target Platform**: Cross-platform (Haskell/Cabal supports Linux, macOS, Windows)  
**Project Type**: single (library + executable, not web/mobile)  
**Performance Goals**: Parse S-expressions up to 1000 tokens within 100ms; REPL response time under 500ms for typical expressions  
**Constraints**: Must maintain lexical scoping semantics; closures must capture environment correctly; error messages must be clear and informative; must support nested expressions and function calls  
**Scale/Scope**: Core interpreter modules (Lisp.Syntax, Lisp.Parser, Lisp.Eval, Lisp.Primitives); REPL interface; property-based test suite; example programs

## Constitution Check

*GATE: Must pass before Phase 0 research. Re-check after Phase 1 design.*

### Pre-Research Check

### I. Library-First
✅ **PASS**: Core interpreter functionality is implemented as a library (`src/Lisp.*` modules). Library is self-contained with clear module boundaries (Syntax, Parser, Eval, Primitives). Library can be used independently of CLI.

### II. CLI Interface
✅ **PASS**: Executable component (`app/Main.hs`) provides REPL interface. Text I/O protocol: stdin for input, stdout for results, stderr for errors. Supports both interactive REPL and command-line expression evaluation.

### III. Test-First (NON-NEGOTIABLE)
✅ **PASS**: Test suite framework is configured (hspec, QuickCheck). Property-based tests for evaluation laws are specified in requirements. Test-first development will be enforced during implementation.

### IV. Integration Testing
✅ **PASS**: Property-based tests verify integration between parser, evaluator, and environment. Tests verify substitution semantics and closure behavior which require multiple components working together.

### V. Observability
✅ **PASS**: Text I/O ensures debuggability (REPL shows expressions and results). Error messages are required to be clear and informative (FR-017). Structured error reporting through stderr.

**Pre-Research Gate Result**: ✅ **PASS** - All applicable principles satisfied. Architecture aligns with constitution requirements.

---

### Post-Design Check (After Phase 1)

### I. Library-First
✅ **PASS**: Design confirms library-first approach. `data-model.md` documents library module organization (`Lisp.Syntax`, `Lisp.Parser`, `Lisp.Eval`, `Lisp.Primitives`). Each module is self-contained and independently testable.

### II. CLI Interface
✅ **PASS**: Design includes CLI executable entry point (`app/Main.hs`) with REPL functionality. Text I/O protocol defined: stdin → parse → eval → stdout, errors → stderr. `quickstart.md` includes REPL usage examples.

### III. Test-First (NON-NEGOTIABLE)
✅ **PASS**: Test framework (hspec, QuickCheck) is configured. Property-based tests for substitution and closure semantics are specified. `quickstart.md` includes test execution verification. Test-first development will be enforced when implementing each module.

### IV. Integration Testing
✅ **PASS**: Property-based tests verify integration between parser, evaluator, and environment. Tests verify substitution semantics and closure behavior which require multiple components working together correctly.

### V. Observability
✅ **PASS**: Text I/O ensures debuggability (REPL shows expressions and results). Error messages are required to be clear and informative. Structured error reporting through stderr with context information.

**Post-Design Gate Result**: ✅ **PASS** - All applicable principles satisfied. Design artifacts confirm alignment with constitution requirements. No violations detected.

## Project Structure

### Documentation (this feature)

```text
specs/002-core-lisp-evaluator/
├── plan.md              # This file (/speckit.plan command output)
├── research.md          # Phase 0 output (/speckit.plan command)
├── data-model.md        # Phase 1 output (/speckit.plan command)
├── quickstart.md        # Phase 1 output (/speckit.plan command)
├── contracts/           # Phase 1 output (/speckit.plan command)
└── tasks.md             # Phase 2 output (/speckit.tasks command - NOT created by /speckit.plan)
```

### Source Code (repository root)

```text
pattern-lisp/
├── src/
│   └── Lisp/
│       ├── Syntax.hs    # AST data types (Expr, Value)
│       ├── Parser.hs    # S-expression parser
│       ├── Eval.hs      # Environment-based evaluator
│       └── Primitives.hs # Built-in functions
├── app/
│   └── Main.hs          # REPL/CLI entry point
├── test/
│   ├── Spec.hs          # Test suite entry point
│   ├── Lisp/
│   │   ├── ParserSpec.hs
│   │   ├── EvalSpec.hs
│   │   └── PrimitivesSpec.hs
│   └── Properties.hs    # Property-based tests
├── examples/
│   └── *.lisp           # Example Lisp programs
├── cabal.project
└── pattern-lisp.cabal
```

**Structure Decision**: Single project structure following standard Cabal conventions. Library modules in `src/Lisp/` with clear separation of concerns (Syntax, Parser, Eval, Primitives). Test files mirror library structure in `test/Lisp/`. Examples directory for demonstration programs. This structure aligns with Cabal best practices and supports the library-first principle.

## Complexity Tracking

> **Fill ONLY if Constitution Check has violations that must be justified**

| Violation | Why Needed | Simpler Alternative Rejected Because |
|-----------|------------|-------------------------------------|
| N/A | N/A | N/A |
