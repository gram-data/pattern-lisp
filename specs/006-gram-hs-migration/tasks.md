# Tasks: Gram-HS Library Migration

**Input**: Design documents from `/specs/006-gram-hs-migration/`
**Prerequisites**: plan.md (required), spec.md (required for user stories), research.md, data-model.md, contracts/

**Tests**: No new tests required - existing test suite verifies migration correctness.

**Organization**: Tasks are organized by user story to enable independent implementation and testing of each story.

## Format: `[ID] [P?] [Story] Description`

- **[P]**: Can run in parallel (different files, no dependencies)
- **[Story]**: Which user story this task belongs to (e.g., US1, US2, US3)
- Include exact file paths in descriptions

## Path Conventions

- **Single project**: `src/`, `test/` at repository root
- Paths shown below use repository root structure

---

## Phase 1: Setup (Prerequisites)

**Purpose**: Verify prerequisites and understand migration requirements

- [ ] T001 Verify updated gram-hs library is available and compatible with current GHC version
- [ ] T002 Read migration guide at `../gram-hs/docs/users/migration/rename-constructors.md` and understand breaking changes
- [ ] T003 Verify existing test suite passes before migration: `cabal test`

**Checkpoint**: Prerequisites verified - ready to begin migration

---

## Phase 2: User Story 1 - Update Pattern Constructor Imports (Priority: P1) 🎯 MVP

**Goal**: Update all imports from `Pattern.Core (pattern, patternWith)` to `Pattern.Core (point, pattern)` so code compiles with updated library.

**Independent Test**: Update imports in all affected modules and verify build succeeds with `cabal build`.

### Implementation for User Story 1

- [ ] T004 [P] [US1] Update import in `src/PatternLisp/Codec.hs`: Change `import Pattern.Core (pattern, patternWith)` to `import Pattern.Core (point, pattern)`
- [ ] T005 [P] [US1] Update import in `src/PatternLisp/PatternPrimitives.hs`: Change `import Pattern.Core (pattern, patternWith)` to `import Pattern.Core (point, pattern)`
- [ ] T006 [P] [US1] Update import in `src/PatternLisp/Gram.hs`: Change `import Pattern.Core (pattern)` to `import Pattern.Core (point, pattern)`
- [ ] T007 [P] [US1] Update import in `test/PatternLisp/CodecSpec.hs`: Change `import Pattern.Core (pattern, patternWith)` to `import Pattern.Core (point, pattern)`
- [ ] T008 [P] [US1] Update import in `test/PatternLisp/GramSpec.hs`: Change `import Pattern.Core (pattern, patternWith)` to `import Pattern.Core (point, pattern)`
- [ ] T009 [P] [US1] Update import in `test/PatternLisp/GramSerializationSpec.hs`: Change `import Pattern.Core (pattern, patternWith)` to `import Pattern.Core (point, pattern)`
- [ ] T010 [P] [US1] Update import in `test/PatternLisp/RuntimeSpec.hs`: Change `import Pattern.Core (pattern)` to `import Pattern.Core (point, pattern)`
- [ ] T011 [US1] Verify all imports updated: Run `grep -r "patternWith" src/ test/ | grep -i import` to confirm no old imports remain
- [ ] T012 [US1] Verify compilation: Run `cabal build` to ensure no import-related errors

**Checkpoint**: All imports updated - code should compile (may have errors from old constructor usage, which is expected)

---

## Phase 3: User Story 2 - Replace Atomic Pattern Constructors (Priority: P1)

**Goal**: Replace all atomic `pattern` constructor calls with `point` so atomic patterns are created correctly with the new API.

**Independent Test**: Search for atomic `pattern` calls (single argument, not list), replace with `point`, and verify build and tests pass.

### Implementation for User Story 2

- [ ] T013 [P] [US2] Replace atomic `pattern` calls in `src/PatternLisp/Codec.hs`: Find all `pattern $` and `pattern subject` (single argument) and replace with `point $` and `point subject`
- [ ] T014 [P] [US2] Replace atomic `pattern` calls in `src/PatternLisp/PatternPrimitives.hs`: Find all `pattern subject` (single argument) and replace with `point subject`
- [ ] T015 [P] [US2] Replace atomic `pattern` calls in `src/PatternLisp/Gram.hs`: Replace `pattern subject` with `point subject` in `exprToGram` function
- [ ] T016 [P] [US2] Replace atomic `pattern` calls in `test/PatternLisp/RuntimeSpec.hs`: Replace any atomic `pattern` calls with `point`
- [ ] T017 [US2] Verify atomic patterns migrated: Run `grep -r "pattern \$" src/ test/` and `grep -r "pattern [^W]" src/ test/` to check for remaining atomic patterns (excluding `patternWith`)
- [ ] T018 [US2] Verify compilation: Run `cabal build` to ensure atomic pattern replacements are correct

**Checkpoint**: All atomic patterns use `point` - compilation should succeed for atomic patterns

---

## Phase 4: User Story 3 - Replace Pattern-With-Elements Constructors (Priority: P1)

**Goal**: Replace all `patternWith` constructor calls with `pattern` so patterns with elements are created correctly with the new API.

**Independent Test**: Search for all `patternWith` calls, replace with `pattern`, and verify build and tests pass.

### Implementation for User Story 3

- [ ] T019 [P] [US3] Replace `patternWith` calls in `src/PatternLisp/Codec.hs`: Replace all ~30+ occurrences of `patternWith` with `pattern`
- [ ] T020 [P] [US3] Replace `patternWith` calls in `src/PatternLisp/PatternPrimitives.hs`: Replace all 4 occurrences of `patternWith` with `pattern`
- [ ] T021 [P] [US3] Replace `patternWith` calls in `test/PatternLisp/GramSpec.hs`: Replace `patternWith` with `pattern`
- [ ] T022 [P] [US3] Replace `patternWith` calls in `test/PatternLisp/GramSerializationSpec.hs`: Replace `patternWith` with `pattern`
- [ ] T023 [US3] Verify all `patternWith` replaced: Run `grep -r "patternWith" src/ test/` to confirm no function calls remain (comments may still reference)
- [ ] T024 [US3] Verify compilation: Run `cabal build` to ensure all `patternWith` replacements are correct
- [ ] T025 [US3] Verify nested patterns: Check that nested patterns correctly use `point` for atomic children and `pattern` for non-atomic children

**Checkpoint**: All `patternWith` calls replaced with `pattern` - full migration of constructor calls complete

---

## Phase 5: User Story 4 - Verify Migration Completeness (Priority: P2)

**Goal**: Verify that all constructor calls have been migrated and no old API usage remains, ensuring codebase is fully compatible with updated library.

**Independent Test**: Search codebase for remaining `patternWith` calls or incorrect `pattern` usage, run full test suite, and verify all tests pass.

### Implementation for User Story 4

- [ ] T026 [US4] Search for remaining `patternWith` function calls: Run `grep -r "patternWith" src/ test/` and verify only comments/documentation references remain
- [ ] T027 [US4] Search for incorrect atomic `pattern` usage: Run `grep -r "pattern \$" src/ test/` and verify all are intentional (not atomic patterns that should be `point`)
- [ ] T028 [US4] Verify compilation: Run `cabal build` and ensure zero errors related to Pattern constructors
- [ ] T029 [US4] Run full test suite: Execute `cabal test` and verify 100% test pass rate
- [ ] T030 [US4] Test example programs: Run example programs through interpreter and verify they produce same results as before migration
- [ ] T031 [US4] Verify serialization round-trips: Test pattern serialization/deserialization for all pattern types (atomic, lists, maps, sets, closures, nested)

**Checkpoint**: Migration verified complete - all tests pass, no old API usage remains

---

## Phase 6: Polish & Cross-Cutting Concerns

**Purpose**: Update code comments and documentation examples to reflect new API

- [ ] T032 [P] Update Haddock examples in `src/PatternLisp/PatternPrimitives.hs`: Replace `evalPatternWith` example comments to use new API if they reference constructors
- [ ] T033 [P] Update inline comments in `src/PatternLisp/Codec.hs`: Review and update any comments that reference old constructor names
- [ ] T034 [P] Update module documentation examples: Review module headers for example code snippets referencing old API
- [ ] T035 [P] Update comments in `test/PatternLisp/` files: Review test file comments for old API references
- [ ] T036 Verify comment updates: Run final `grep -r "patternWith" src/ test/` to confirm only historical references remain (if any)
- [ ] T037 Final compilation check: Run `cabal build` one final time
- [ ] T038 Final test check: Run `cabal test` one final time to ensure everything still works

**Checkpoint**: Migration complete - code, tests, and documentation all updated

---

## Dependencies & Execution Order

### Phase Dependencies

- **Setup (Phase 1)**: No dependencies - can start immediately
- **User Story 1 (Phase 2)**: Depends on Setup completion - BLOCKS all other migration work
- **User Story 2 (Phase 3)**: Depends on User Story 1 completion (imports must be updated first)
- **User Story 3 (Phase 4)**: Depends on User Story 1 completion (imports must be updated first)
  - Can be done in parallel with User Story 2 (different files)
- **User Story 4 (Phase 5)**: Depends on User Stories 2 and 3 completion (all constructor calls must be migrated)
- **Polish (Phase 6)**: Depends on User Story 4 completion (verification must pass first)

### User Story Dependencies

- **User Story 1 (P1)**: Can start after Setup - No dependencies on other stories
- **User Story 2 (P1)**: Depends on User Story 1 - Imports must be updated before replacing atomic patterns
- **User Story 3 (P1)**: Depends on User Story 1 - Imports must be updated before replacing `patternWith`
  - Can run in parallel with User Story 2 (different files, no conflicts)
- **User Story 4 (P2)**: Depends on User Stories 2 and 3 - All constructor calls must be migrated before verification

### Within Each User Story

- Import updates can be done in parallel (different files)
- Atomic pattern replacements can be done in parallel (different files)
- `patternWith` replacements can be done in parallel (different files)
- Verification tasks must run after all replacements complete

### Parallel Opportunities

- **Phase 2 (US1)**: All import updates (T004-T010) can run in parallel
- **Phase 3 (US2)**: All atomic pattern replacements (T013-T016) can run in parallel
- **Phase 4 (US3)**: All `patternWith` replacements (T019-T022) can run in parallel
- **Phase 6 (Polish)**: All comment updates (T032-T035) can run in parallel
- **Cross-phase**: User Stories 2 and 3 can be worked on in parallel after User Story 1 completes

---

## Parallel Example: User Story 1

```bash
# Launch all import updates together (different files, no dependencies):
Task: "Update import in src/PatternLisp/Codec.hs"
Task: "Update import in src/PatternLisp/PatternPrimitives.hs"
Task: "Update import in src/PatternLisp/Gram.hs"
Task: "Update import in test/PatternLisp/CodecSpec.hs"
Task: "Update import in test/PatternLisp/GramSpec.hs"
Task: "Update import in test/PatternLisp/GramSerializationSpec.hs"
Task: "Update import in test/PatternLisp/RuntimeSpec.hs"
```

---

## Parallel Example: User Stories 2 and 3

```bash
# After User Story 1 completes, User Stories 2 and 3 can run in parallel:

# Developer A: User Story 2 (Atomic patterns)
Task: "Replace atomic pattern calls in src/PatternLisp/Codec.hs"
Task: "Replace atomic pattern calls in src/PatternLisp/PatternPrimitives.hs"
Task: "Replace atomic pattern calls in src/PatternLisp/Gram.hs"

# Developer B: User Story 3 (patternWith replacements)
Task: "Replace patternWith calls in src/PatternLisp/Codec.hs"
Task: "Replace patternWith calls in src/PatternLisp/PatternPrimitives.hs"
Task: "Replace patternWith calls in test/PatternLisp/GramSpec.hs"
```

---

## Implementation Strategy

### MVP First (User Story 1 Only)

1. Complete Phase 1: Setup
2. Complete Phase 2: User Story 1 (Update imports)
3. **STOP and VALIDATE**: Verify code compiles (may have constructor errors, which is expected)
4. This provides a working checkpoint where imports are correct

### Incremental Delivery

1. Complete Setup → Prerequisites verified
2. Add User Story 1 → Imports updated → Verify compilation
3. Add User Story 2 → Atomic patterns migrated → Verify compilation
4. Add User Story 3 → `patternWith` migrated → Verify compilation
5. Add User Story 4 → Verification complete → All tests pass
6. Add Polish → Comments updated → Final verification

### Parallel Team Strategy

With multiple developers:

1. Team completes Setup together
2. Once Setup is done:
   - Developer A: User Story 1 (all imports) - BLOCKS others
3. Once User Story 1 completes:
   - Developer A: User Story 2 (atomic patterns)
   - Developer B: User Story 3 (`patternWith` replacements)
4. Once User Stories 2 and 3 complete:
   - Developer A: User Story 4 (verification)
   - Developer B: Polish (comment updates)
5. Final verification together

---

## Notes

- [P] tasks = different files, no dependencies
- [Story] label maps task to specific user story for traceability
- Each user story should be independently completable and testable
- Commit after each phase or logical group
- Stop at any checkpoint to validate story independently
- Avoid: replacing variable names that happen to be called `pattern` or `patternWith`
- Be careful: `pattern` is now used for patterns with elements, so distinguish from atomic `point` usage
- Verification tasks (T026-T031) should catch any missed replacements

---

## Task Summary

**Total Tasks**: 38

**Tasks per User Story**:
- User Story 1 (Imports): 9 tasks
- User Story 2 (Atomic patterns): 6 tasks
- User Story 3 (patternWith): 7 tasks
- User Story 4 (Verification): 6 tasks
- Polish: 7 tasks
- Setup: 3 tasks

**Parallel Opportunities Identified**:
- 7 import updates can run in parallel
- 4 atomic pattern replacements can run in parallel
- 4 `patternWith` replacements can run in parallel
- 4 comment updates can run in parallel
- User Stories 2 and 3 can run in parallel after User Story 1

**Independent Test Criteria**:
- **US1**: Build succeeds after import updates (constructor errors expected)
- **US2**: Build succeeds, atomic patterns use `point`
- **US3**: Build succeeds, all `patternWith` replaced with `pattern`
- **US4**: All tests pass, no old API usage remains

**Suggested MVP Scope**: User Story 1 (Update imports) - provides working checkpoint

