# Feature Specification: Gram-HS Library Migration

**Feature Branch**: `006-gram-hs-migration`  
**Created**: 2025-01-28  
**Status**: Draft  
**Input**: User description: "Migrate to use updated gram-hs library, particularly the breaking change to constructors. Read ../gram-hs/docs/users/migration/rename-constructors.md for details"

## User Scenarios & Testing *(mandatory)*

### User Story 1 - Update Pattern Constructor Imports (Priority: P1)

As a developer maintaining the Pattern Lisp codebase, I need to update all imports to use the new constructor names (`point` and `pattern`) so that the code compiles with the updated gram-hs library.

**Why this priority**: Without updated imports, the code will fail to compile, blocking all other work.

**Independent Test**: Can be fully tested by updating imports in all affected modules and verifying the build succeeds with `cabal build`.

**Acceptance Scenarios**:

1. **Given** a module that imports `Pattern.Core (pattern, patternWith)`, **When** the import is updated to `Pattern.Core (point, pattern)`, **Then** the module compiles successfully
2. **Given** all source modules, **When** imports are updated, **Then** no compilation errors related to missing constructors occur

---

### User Story 2 - Replace Atomic Pattern Constructors (Priority: P1)

As a developer maintaining the Pattern Lisp codebase, I need to replace all atomic `pattern` constructor calls with `point` so that atomic patterns are created correctly with the new API.

**Why this priority**: Atomic pattern construction is fundamental to the system and must work correctly for all functionality to operate.

**Independent Test**: Can be fully tested by searching for atomic `pattern` calls (where `pattern` takes a single argument, not a list), replacing them with `point`, and verifying the build and tests pass.

**Acceptance Scenarios**:

1. **Given** code that creates atomic patterns like `pattern "atom"` or `pattern 42`, **When** these are replaced with `point "atom"` or `point 42`, **Then** the code compiles and atomic patterns are created correctly
2. **Given** all atomic pattern constructions in the codebase, **When** they are migrated to use `point`, **Then** the code compiles and uses the new API correctly

---

### User Story 3 - Replace Pattern-With-Elements Constructors (Priority: P1)

As a developer maintaining the Pattern Lisp codebase, I need to replace all `patternWith` constructor calls with `pattern` so that patterns with elements are created correctly with the new API.

**Why this priority**: Patterns with elements are used extensively throughout the codebase for serialization, closures, and data structures. This must work correctly for the system to function.

**Independent Test**: Can be fully tested by searching for all `patternWith` calls, replacing them with `pattern`, and verifying the build and tests pass.

**Acceptance Scenarios**:

1. **Given** code that creates patterns with elements like `patternWith decoration elements`, **When** this is replaced with `pattern decoration elements`, **Then** the code compiles and patterns are created correctly
2. **Given** nested pattern constructions that mix atomic and non-atomic patterns, **When** atomic patterns use `point` and non-atomic patterns use `pattern`, **Then** nested structures are created correctly
3. **Given** all `patternWith` calls in the codebase, **When** they are migrated to use `pattern`, **Then** the code compiles and uses the new API correctly

---

### User Story 4 - Verify Migration Completeness (Priority: P2)

As a developer maintaining the Pattern Lisp codebase, I need to verify that all constructor calls have been migrated and no old API usage remains, ensuring the codebase is fully compatible with the updated library.

**Why this priority**: Incomplete migration could lead to runtime errors or subtle bugs that are difficult to diagnose.

**Independent Test**: Can be fully tested by searching the codebase for any remaining `patternWith` calls or incorrect `pattern` usage, running the full test suite, and verifying all tests pass.

**Acceptance Scenarios**:

1. **Given** the migrated codebase, **When** searching for `patternWith`, **Then** no occurrences are found (except possibly in comments or documentation)
2. **Given** the migrated codebase, **When** searching for atomic `pattern` calls (single argument, not list), **Then** all have been replaced with `point`
3. **Given** the full test suite, **When** running `cabal test`, **Then** all tests pass without errors
4. **Given** example programs, **When** running them through the interpreter, **Then** they produce the same results as before migration

---

### Edge Cases

- What happens when a pattern construction is deeply nested with mixed atomic and non-atomic patterns?
- How does the system handle patterns created in closures that capture patterns?
- What happens when serializing and deserializing patterns after migration?
- How are patterns with zero elements handled (empty lists, empty maps)?
- What happens when patterns are constructed dynamically at runtime?

## Requirements *(mandatory)*

### Functional Requirements

- **FR-001**: System MUST update all imports from `Pattern.Core (pattern, patternWith)` to `Pattern.Core (point, pattern)`
- **FR-002**: System MUST replace all atomic pattern constructions (`pattern value`) with `point value`
- **FR-003**: System MUST replace all pattern-with-elements constructions (`patternWith decoration elements`) with `pattern decoration elements`
- **FR-004**: System MUST ensure nested pattern constructions correctly use `point` for atomic patterns and `pattern` for patterns with elements
- **FR-005**: System MUST update both direct gram-hs API usage AND pattern-lisp's reflection of patterns to use the new API with no backwards compatibility allowances
- **FR-010**: System MUST update code comments and documentation examples that reference the old constructor API to use the new API names
- **FR-006**: System MUST compile successfully with the updated gram-hs library
- **FR-007**: System MUST pass all existing tests after migration
- **FR-008**: System MUST correctly serialize and deserialize patterns using the new constructors
- **FR-009**: System MUST handle all pattern types correctly: atomic values, lists, maps, sets, closures, and nested structures

### Key Entities *(include if feature involves data)*

- **Pattern Constructor**: The function used to create Pattern values. Changed from `pattern`/`patternWith` to `point`/`pattern`
- **Atomic Pattern**: A pattern with no elements, created with `point` in the new API
- **Pattern with Elements**: A pattern containing child patterns, created with `pattern` in the new API
- **Migration Scope**: All modules that import or use Pattern constructors, including Codec, PatternPrimitives, Gram, and any test modules

## Success Criteria *(mandatory)*

### Measurable Outcomes

- **SC-001**: All source files compile successfully with zero errors related to Pattern constructors
- **SC-002**: All existing tests pass with 100% success rate after migration
- **SC-003**: All example programs execute successfully using the new API (behavioral equivalence expected, but API usage must be fully migrated)
- **SC-004**: Codebase contains zero occurrences of `patternWith` function calls, and code comments/documentation examples have been updated to use the new API
- **SC-005**: All atomic pattern constructions use `point` instead of `pattern`
- **SC-006**: Pattern serialization and deserialization round-trips work correctly for all pattern types
- **SC-007**: Migration is completed within a single development session (no partial migration state)

## Assumptions

- The updated gram-hs library is available and compatible with the current GHC version
- The migration guide accurately describes all breaking changes
- Existing test coverage is sufficient to verify migration correctness
- No changes to pattern semantics are required beyond constructor name changes
- The migration can be done incrementally by module, but all modules must be migrated before the feature is complete
- This is a breaking change migration with no backwards compatibility requirements - both gram-hs API usage and pattern-lisp's reflection of patterns must be fully updated

## Dependencies

- Updated gram-hs library with renamed constructors
- Access to migration guide at `../gram-hs/docs/users/migration/rename-constructors.md`
- Existing test suite to verify migration correctness

## Out of Scope

- Backwards compatibility with old constructor names or patterns
- Maintaining support for old gram-hs API usage
- Gradual migration strategies or compatibility layers
- Changes to pattern semantics or behavior (beyond API changes)
- Performance optimizations
- New features or functionality
- Documentation updates beyond migration notes (code comments and examples within source files are included in migration scope)
- Changes to other gram-hs library APIs beyond constructor names

## Clarifications

### Session 2025-01-28

- Q: Should backwards compatibility be maintained for this migration? → A: No backwards compatibility required. Both gram-hs API usage and pattern-lisp's reflection of patterns must be fully updated with no allowance for backwards compatibility.
