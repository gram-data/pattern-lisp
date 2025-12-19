# Data Model: Pattern Lisp Project Initialization

**Date**: 2025-12-19  
**Feature**: Pattern Lisp Project Initialization

## Overview

This phase involves project setup and configuration, not runtime data entities. The "data model" here refers to the structural organization of the project itself rather than application data.

## Project Structure Entities

### Directory Structure

The project follows a hierarchical directory structure that represents the organization of code and resources:

```
pattern-lisp/
├── src/              # Library code modules
├── app/              # Executable entry point
├── test/             # Test files
├── examples/         # Example programs
├── cabal.project     # Project configuration
└── pattern-lisp.cabal # Package definition
```

**Properties**:
- Each directory has a specific purpose (library, executable, tests, examples)
- Directories follow Cabal conventions
- Structure is flat (no nested package structure)

### Cabal Configuration Files

#### `cabal.project`

**Purpose**: Multi-package configuration file that defines project-wide settings and source repository dependencies.

**Key Sections**:
- `packages:` - Lists local packages (`.` for single package)
- `source-repository-package` - Defines external git dependencies (gram-hs)

**Properties**:
- Text file in Cabal project format
- Defines dependency sources
- Project-level configuration

#### `pattern-lisp.cabal`

**Purpose**: Package definition file specifying library, executable, and test suite components.

**Key Sections**:
- `cabal-version` - Cabal specification version
- `name` - Package name
- `version` - Package version
- `library` - Library component definition
  - `exposed-modules` - Public API modules (`Lisp.*`)
  - `build-depends` - Library dependencies
- `executable` - CLI executable definition
  - `main-is` - Entry point file
  - `build-depends` - Executable dependencies
- `test-suite` - Test framework definition
  - `type` - Test framework type (e.g., `exitcode-stdio-1.0`)
  - `main-is` - Test entry point
  - `build-depends` - Test dependencies

**Properties**:
- Defines three components: library, executable, test-suite
- Specifies dependencies for each component
- Follows Cabal package specification format

### Module Organization

#### Library Modules (`src/Lisp.*`)

**Purpose**: Core library code organized by functionality.

**Initial Structure**:
- Modules will be added in subsequent phases
- Namespace: `Lisp.*` (e.g., `Lisp.Syntax`, `Lisp.Parser`, `Lisp.Eval`)

**Properties**:
- Each module is self-contained
- Modules follow Haskell naming conventions
- Exposed modules form the public API

#### Executable Entry Point (`app/Main.hs`)

**Purpose**: CLI/REPL executable entry point.

**Properties**:
- Single entry point file
- Imports library modules
- Provides command-line interface

#### Test Files (`test/`)

**Purpose**: Test specifications and property-based tests.

**Properties**:
- Organized by module being tested
- Uses hspec for unit/integration tests
- Uses QuickCheck for property-based tests

## Relationships

- `cabal.project` → references `pattern-lisp.cabal` (via `packages: .`)
- `pattern-lisp.cabal` → defines components that use `src/`, `app/`, `test/`
- Library modules (`src/`) → can import gram-hs modules
- Executable (`app/`) → depends on library modules
- Test suite (`test/`) → tests library and executable components

## Validation Rules

1. **Directory Structure**: All required directories (`src/`, `app/`, `test/`, `examples/`) must exist
2. **Cabal Files**: Both `cabal.project` and `pattern-lisp.cabal` must be valid Cabal syntax
3. **Dependency Resolution**: `cabal update` must successfully resolve gram-hs dependency
4. **Build Success**: `cabal build all` must complete without errors
5. **Test Execution**: `cabal test` must execute test framework (even with no tests)

## State Transitions

**Project Setup States**:

1. **Uninitialized** → No project structure exists
2. **Structure Created** → Directories exist but no files
3. **Configuration Added** → Cabal files created
4. **Dependencies Resolved** → `cabal update` succeeds
5. **Buildable** → `cabal build all` succeeds
6. **Testable** → `cabal test` executes successfully
7. **Complete** → All verification steps pass

## Notes

- This is a structural/organizational model, not a runtime data model
- Actual Lisp interpreter data structures (AST, values, environments) will be defined in Phase 1 of development
- The model focuses on project organization and build configuration

