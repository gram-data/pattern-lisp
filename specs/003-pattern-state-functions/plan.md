# Implementation Plan: Pattern State Functions

**Branch**: `003-pattern-state-functions` | **Date**: 2025-01-27 | **Spec**: [spec.md](spec.md)
**Input**: Feature specification from `/specs/003-pattern-state-functions/spec.md`

**Note**: This template is filled in by the `/speckit.plan` command. See `.specify/templates/commands/plan.md` for the execution workflow.

## Summary

Integrate Pattern as a first-class value type in Pattern Lisp, where all state is represented as Pattern Subject values. Programs follow the canonical form `(lambda (state) ...)` transforming Pattern Subject → Pattern Subject. Implement complete serialization of all values (including closures and primitives) to Subject representation, enabling code-as-data capabilities. Add pattern construction, query, and predicate primitives. Create runtime system for tool validation, execution, and state management with execution tracing.

## Technical Context

**Language/Version**: Haskell with GHC 9.10.3 (as per existing project setup)  
**Primary Dependencies**: 
- gram-data (Pattern and Subject types)
- Existing dependencies: megaparsec, text, containers, mtl, gram
- Testing: hspec, QuickCheck

**Storage**: In-memory Pattern Subject structures; runtime state can be serialized to Subject for persistence  
**Testing**: hspec for unit/integration tests, QuickCheck for property-based tests verifying serialization round-trips and closure preservation  
**Target Platform**: Cross-platform (Haskell/Cabal supports Linux, macOS, Windows)  
**Project Type**: single (library + executable extension, not web/mobile)  
**Performance Goals**: 
- Pattern operations (construction, query) complete in under 10ms for typical patterns (< 100 nodes)
- Serialization/deserialization round-trips complete in under 100ms for typical values
- Tool validation and execution complete in under 1 second for typical tools (state size < 1000 nodes)

**Constraints**: 
- Must maintain 1:1 Gram correspondence (all values serializable to Subject)
- Closures must remain executable after serialization round-trip
- Primitives must remain functional after serialization round-trip
- Variable names use property-based representation (not Gram identifiers)
- Canonical form enforcement: tools must be `(lambda (state) ...)`

**Scale/Scope**: 
- Extend existing `Lisp.*` modules (or reorganize to `PatternLisp.*`)
- New modules: `PatternLisp.Subject` (serialization), `PatternLisp.PatternPrimitives` (pattern operations), `PatternLisp.Runtime` (tool execution)
- Extend evaluator with pattern primitives
- Runtime system for tool management
- Comprehensive test suite for serialization and pattern operations

## Constitution Check

*GATE: Must pass before Phase 0 research. Re-check after Phase 1 design.*

### Pre-Research Check

### I. Library-First
✅ **PASS**: Feature extends existing library (`Lisp.*` or `PatternLisp.*` modules). New functionality (Pattern values, serialization, runtime) is implemented as library modules. Library is self-contained and independently testable. Each module has clear purpose (Subject serialization, Pattern primitives, Runtime management).

### II. CLI Interface
✅ **PASS**: Feature extends existing CLI/REPL (`app/Main.hs`). Runtime commands (`:load`, `:exec`, `:state`, `:trace`, `:save`, `:restore`) follow text I/O protocol: stdin for commands, stdout for results, stderr for errors. Supports both interactive REPL and file-based tool loading.

### III. Test-First (NON-NEGOTIABLE)
✅ **PASS**: Test suite framework is configured (hspec, QuickCheck). Comprehensive test requirements specified for serialization round-trips, pattern operations, tool validation, and runtime execution. Test-first development will be enforced during implementation.

### IV. Integration Testing
✅ **PASS**: Integration tests specified for tool validation, tool execution, state threading, runtime serialization. Property-based tests verify serialization round-trips which require multiple components (serialization, deserialization, evaluation) working together correctly.

### V. Observability
✅ **PASS**: Text I/O ensures debuggability (REPL shows state, execution traces). Error messages required to be clear and actionable (FR-019, FR-020, FR-006). Execution traces provide visibility into state transformations. Structured error reporting through stderr.

**Pre-Research Gate Result**: ✅ **PASS** - All applicable principles satisfied. Architecture aligns with constitution requirements.

---

### Post-Design Check (After Phase 1)

### I. Library-First
✅ **PASS**: Design confirms library-first approach. `data-model.md` documents library module organization (`PatternLisp.Syntax`, `PatternLisp.Subject`, `PatternLisp.PatternPrimitives`, `PatternLisp.Runtime`). Each module is self-contained and independently testable. Modules serve distinct purposes (serialization, pattern operations, runtime management).

### II. CLI Interface
✅ **PASS**: Design includes CLI executable extension (`app/Main.hs`) with runtime commands (`:load`, `:exec`, `:state`, `:trace`, `:save`, `:restore`). Text I/O protocol defined: stdin → commands → stdout/stderr. `quickstart.md` includes REPL usage examples and command documentation.

### III. Test-First (NON-NEGOTIABLE)
✅ **PASS**: Test framework (hspec, QuickCheck) is configured. Comprehensive test requirements specified in `data-model.md` and `contracts/README.md`. Property-based tests for serialization round-trips are specified. `quickstart.md` includes test execution verification. Test-first development will be enforced when implementing each module.

### IV. Integration Testing
✅ **PASS**: Integration tests specified for tool validation, tool execution, state threading, runtime serialization. Property-based tests verify serialization round-trips which require multiple components (serialization, deserialization, evaluation) working together correctly. `contracts/README.md` documents integration points.

### V. Observability
✅ **PASS**: Text I/O ensures debuggability (REPL shows state, execution traces). Error messages required to be clear and actionable. Execution traces provide visibility into state transformations. Structured error reporting through stderr with context information. `quickstart.md` includes troubleshooting section.

**Post-Design Gate Result**: ✅ **PASS** - All applicable principles satisfied. Design artifacts confirm alignment with constitution requirements. No violations detected.

## Project Structure

### Documentation (this feature)

```text
specs/003-pattern-state-functions/
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
│   └── PatternLisp/     # Reorganized from Lisp.* (or extend Lisp.*)
│       ├── Syntax.hs    # Extended with VPattern, pattern primitives
│       ├── Parser.hs    # (unchanged, or updated imports)
│       ├── Eval.hs      # Extended with pattern primitive evaluation
│       ├── Primitives.hs # Extended with pattern primitives in initialEnv
│       ├── Subject.hs   # NEW: Value ↔ Subject serialization
│       ├── PatternPrimitives.hs # NEW: Pattern operation implementations
│       └── Runtime.hs   # NEW: Tool validation, execution, state management
├── app/
│   └── Main.hs          # Extended with runtime commands (:load, :exec, :state, :trace, :save, :restore)
├── test/
│   ├── Spec.hs          # Test suite entry point
│   ├── PatternLisp/      # Reorganized from Lisp.* (or extend Lisp.*)
│   │   ├── ParserSpec.hs
│   │   ├── EvalSpec.hs
│   │   ├── PrimitivesSpec.hs
│   │   ├── SubjectSpec.hs      # NEW: Serialization round-trip tests
│   │   ├── PatternSpec.hs      # NEW: Pattern primitive tests
│   │   └── RuntimeSpec.hs      # NEW: Runtime and tool execution tests
│   └── Properties.hs    # Property-based tests for serialization
└── examples/
    ├── basic-patterns.plisp
    ├── hello-tool.plisp
    ├── state-reader.plisp
    ├── filter-tool.plisp
    ├── composition.plisp
    └── closure-in-state.plisp
```

**Structure Decision**: Extend existing `Lisp.*` modules (or reorganize to `PatternLisp.*` per TODO document). New modules added for serialization (`Subject.hs`), pattern operations (`PatternPrimitives.hs`), and runtime management (`Runtime.hs`). Test structure mirrors source structure. Examples directory contains tool definition files.

## Design Decisions

### CLI State Initialization

**Status**: ✅ **DECISION MADE**

**Selected Approach**: **Option F (Convention-Based Auto-Loading)**

**Decision**: All non-flag CLI arguments are files processed by convention:
- `.plisp` files → Functions (lambda or named function, bound to filename-derived name)
- `.gram` files → State variables (Pattern Subject, bound to filename-derived name)

**Implementation Details**:
- No directory scanning - files provided explicitly as CLI arguments
- Filename-to-identifier: Remove extension (`hello-tool.plisp` → `hello-tool`)
- File processing order: `.gram` files first (states), then `.plisp` files (functions)
- Lambda naming: If file contains `(lambda (state) ...)`, bind to filename-derived name
- Named functions: If file contains `(define name ...)`, use that name (filename ignored)
- Function invocation: `(tool-name state-var)` - no special `:exec` command needed
- Non-interactive: `-e/--eval <expr>` or stdin for expression evaluation
- REPL commands: `:reload` (re-process files), `:env` (show environment), `:state` (show state if using single-state model)

**Impact**: This decision affects:
- CLI argument parsing (process all non-flag args as files)
- File loading logic (`.plisp` vs `.gram` handling)
- Environment population (functions and states from files)
- REPL command set (minimal, since functions are first-class)
- Documentation and examples

**Ready for Implementation**: This decision enables implementation of Phase 1B (Runtime Module) and Phase 9 (Update REPL for Tool Execution).

---

## Complexity Tracking

> **Fill ONLY if Constitution Check has violations that must be justified**

No violations detected. All new modules serve distinct purposes:
- `Subject.hs`: Serialization layer (required for code-as-data)
- `PatternPrimitives.hs`: Pattern operations (required for state manipulation)
- `Runtime.hs`: Tool execution infrastructure (required for canonical form enforcement)
