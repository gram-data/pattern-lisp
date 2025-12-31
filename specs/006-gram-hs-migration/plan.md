# Implementation Plan: Gram-HS Library Migration

**Branch**: `006-gram-hs-migration` | **Date**: 2025-01-28 | **Spec**: [spec.md](./spec.md)
**Input**: Feature specification from `/specs/006-gram-hs-migration/spec.md`

**Note**: This template is filled in by the `/speckit.plan` command. See `.specify/templates/commands/plan.md` for the execution workflow.

## Summary

Migrate Pattern Lisp codebase to use updated gram-hs library constructor API. The breaking change renames constructors: `patternWith` → `pattern` (for patterns with elements) and `pattern` → `point` (for atomic patterns). This is a straightforward refactoring task requiring systematic find-and-replace across source and test modules, including updates to code comments and documentation examples that reference the old API. Migration is followed by verification through compilation and test execution.

## Technical Context

**Language/Version**: Haskell (GHC 9.10.3, base >=4.18 && <5)  
**Primary Dependencies**: gram-hs library (pattern, subject, gram packages), Cabal build system  
**Storage**: N/A (in-memory pattern structures)  
**Testing**: hspec, QuickCheck, cabal test  
**Target Platform**: Linux/macOS (Haskell cross-platform)  
**Project Type**: Library (Haskell library with CLI executable)  
**Performance Goals**: No performance impact - API migration only  
**Constraints**: Must maintain 100% test pass rate, zero compilation errors  
**Scale/Scope**: ~50 occurrences across 7 source modules and 4 test modules, plus code comments and documentation examples

## Constitution Check

*GATE: Must pass before Phase 0 research. Re-check after Phase 1 design.*

### I. Library-First
✅ **PASS**: This is a migration of an existing library. No new library creation required.

### II. CLI Interface
✅ **PASS**: No CLI changes required. Existing CLI functionality preserved.

### III. Test-First (NON-NEGOTIABLE)
✅ **PASS**: Migration will be verified by existing test suite. All tests must pass after migration.

### IV. Integration Testing
✅ **PASS**: Existing integration tests will verify pattern serialization/deserialization still works correctly.

### V. Observability
✅ **PASS**: No observability changes required. Text I/O and logging unchanged.

**Gate Status**: ✅ **ALL GATES PASS** - Proceed to Phase 0

### Post-Phase 1 Re-check

*Re-evaluated after Phase 1 design completion.*

### I. Library-First
✅ **PASS**: No new libraries created. Existing library structure maintained.

### II. CLI Interface
✅ **PASS**: No CLI changes. Existing interface preserved.

### III. Test-First (NON-NEGOTIABLE)
✅ **PASS**: Migration verified by existing test suite. All tests must pass.

### IV. Integration Testing
✅ **PASS**: Integration tests verify serialization/deserialization correctness.

### V. Observability
✅ **PASS**: No observability changes. Text I/O unchanged.

**Post-Phase 1 Gate Status**: ✅ **ALL GATES PASS** - Ready for Phase 2 (task creation)

## Project Structure

### Documentation (this feature)

```text
specs/006-gram-hs-migration/
├── plan.md              # This file (/speckit.plan command output)
├── research.md          # Phase 0 output (/speckit.plan command)
├── data-model.md        # Phase 1 output (/speckit.plan command)
├── quickstart.md        # Phase 1 output (/speckit.plan command)
├── contracts/           # Phase 1 output (/speckit.plan command)
└── tasks.md             # Phase 2 output (/speckit.tasks command - NOT created by /speckit.plan)
```

### Source Code (repository root)

```text
src/PatternLisp/
├── Codec.hs              # 30+ patternWith occurrences, imports Pattern.Core
├── PatternPrimitives.hs   # 4 patternWith occurrences, imports Pattern.Core
├── Gram.hs                # 1 pattern occurrence (atomic), imports Pattern.Core
├── Eval.hs                # Qualified import only (PatternCore)
└── [other modules]        # No Pattern.Core usage

test/PatternLisp/
├── CodecSpec.hs          # Imports Pattern.Core
├── GramSpec.hs           # 1 patternWith occurrence, imports Pattern.Core
├── GramSerializationSpec.hs  # 1 patternWith occurrence, imports Pattern.Core
├── RuntimeSpec.hs        # 1 pattern occurrence (atomic), imports Pattern.Core
└── [other test modules]   # No Pattern.Core usage
```

**Structure Decision**: Existing single-project Haskell library structure. Migration affects specific modules that import and use Pattern.Core constructors, including:
- Source code: Constructor function calls
- Code comments: Haddock examples and inline comments referencing old API
- Module documentation: Example code snippets in module headers

## Complexity Tracking

> **No violations** - This is a straightforward API migration refactoring task with no complexity additions.
