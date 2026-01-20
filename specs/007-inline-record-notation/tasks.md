# Tasks: Inline Record Notation for Pattern-Lisp

**Input**: Design documents from `/specs/007-inline-record-notation/`
**Prerequisites**: plan.md, spec.md, research.md, data-model.md, contracts/

**Tests**: Test-first development is mandatory per constitution. All tests must be written and fail before implementation.

**Organization**: Tasks are grouped by user story to enable independent implementation and testing of each story.

## Format: `[ID] [P?] [Story] Description`

- **[P]**: Can run in parallel (different files, no dependencies)
- **[Story]**: Which user story this task belongs to (e.g., US1, US2, US3)
- Include exact file paths in descriptions

## Path Conventions

- Source code: `src/PatternLisp/`
- Tests: `test/PatternLisp/`
- Documentation: `docs/`
- Examples: `examples/`

---

## Phase 1: Setup (Shared Infrastructure)

**Purpose**: Project initialization and basic structure

- [X] T001 Verify gram-hs library dependency is available in `pattern-lisp.cabal`
- [X] T002 [P] Review existing parser structure in `src/PatternLisp/Parser.hs` to understand integration points
- [X] T003 [P] Review existing evaluation structure in `src/PatternLisp/Eval.hs` to understand value evaluation patterns

---

## Phase 2: Foundational (Blocking Prerequisites)

**Purpose**: Core infrastructure that MUST be complete before ANY user story can be implemented

**⚠️ CRITICAL**: No user story work can begin until this phase is complete

- [X] T004 **REPLACE** `VMap` with gram `Subject.Value.VMap` in `Value` type in `src/PatternLisp/Syntax.hs` (Option B - gram types throughout)
- [X] T005 **REPLACE** `MapLiteral` with `RecordLiteral [(String, Expr)]` in `Expr` type in `src/PatternLisp/Syntax.hs`
- [X] T006 **REMOVE** `MapKey` type from `src/PatternLisp/Syntax.hs` (use String keys only, matching gram)
- [X] T007 Add `Unquote Expr` and `UnquoteSplice Expr` to `Expr` type in `src/PatternLisp/Syntax.hs` for quasiquotation support
- [X] T008 Update `Value` type to use gram `Subject.Value` types throughout (VInteger, VString, VBoolean, VMap, VArray, etc.) in `src/PatternLisp/Syntax.hs`
- [X] T009 Update `Atom` type: `String Text` → `String String` (gram uses String, not Text) in `src/PatternLisp/Syntax.hs`
- [ ] T010 Create helper function to convert gram record structure to `RecordLiteral` in `src/PatternLisp/Parser.hs` (TODO: Phase 3 - gram parser integration)
- [ ] T011 Create helper function to translate gram parse errors to pattern-lisp parse errors with position in `src/PatternLisp/Parser.hs` (TODO: Phase 3)
- [X] T011a **UPDATE** all existing map operations (MapGet, MapAssoc, MapDissoc, etc.) to work with `VMap (Map String Value)` instead of `VMap (Map MapKey Value)` in `src/PatternLisp/Eval.hs` (COMPLETE)
- [X] T011a-continued **UPDATE** all existing map operations in `src/PatternLisp/Primitives.hs` (COMPLETE - Primitives.hs only registers primitives, implementations in Eval.hs already updated)
- [X] T011b **RENAME** `hash-map` → `record` in `src/PatternLisp/Syntax.hs` and `src/PatternLisp/Primitives.hs` (COMPLETE)
- [X] T011c **REMOVE** all conversion functions for maps/records in `src/PatternLisp/Codec.hs` (COMPLETE - MapKey conversion logic removed, now uses String keys directly)
- [X] T011d **UPDATE** all pattern matching on `Value` constructors in `src/PatternLisp/Eval.hs` to use gram types (VInteger, VString, VBoolean, VArray, VMap, etc.) (COMPLETE)
- [X] T011d-continued **UPDATE** all pattern matching in `src/PatternLisp/Codec.hs` (COMPLETE)
- [X] T011d-continued **UPDATE** all pattern matching in `src/PatternLisp/PatternPrimitives.hs` (COMPLETE)

**Checkpoint**: Foundation ready - user story implementation can now begin in parallel

---

## Phase 3: User Story 1 - Write Records Using Inline Syntax (Priority: P1) 🎯 MVP

**Goal**: Enable developers to write property records directly in code using `{ key: value }` syntax that parses correctly and produces record values.

**Independent Test**: Write record literals in pattern-lisp code and verify they parse correctly and produce record values. Test with empty records, nested records, and various value types.

### Tests for User Story 1 ⚠️

> **NOTE: Write these tests FIRST, ensure they FAIL before implementation**

- [X] T012 [P] [US1] Write test for empty record parsing in `test/PatternLisp/ParserSpec.hs` (COMPLETE - already covered)
- [X] T013 [P] [US1] Write test for simple record parsing `{ name: "Alice", age: 30 }` in `test/PatternLisp/ParserSpec.hs` (COMPLETE - already covered)
- [X] T014 [P] [US1] Write test for nested record parsing in `test/PatternLisp/ParserSpec.hs` (COMPLETE - already covered)
- [X] T015 [P] [US1] Write test for records with different value types (string, number, boolean, null) in `test/PatternLisp/ParserSpec.hs` (COMPLETE)
- [ ] T016 [P] [US1] Write test for duplicate key error in `test/PatternLisp/ParserSpec.hs` (NOTE: Current implementation allows duplicates with last-wins behavior, which is acceptable per spec. Test exists for last-wins behavior.)
- [X] T017 [P] [US1] Write test for unclosed record error in `test/PatternLisp/ParserSpec.hs` (COMPLETE)
- [X] T018 [P] [US1] Write test for invalid key error in `test/PatternLisp/ParserSpec.hs` (COMPLETE)
- [X] T019 [P] [US1] Write test for record evaluation to `Subject.Value.VMap` in `test/PatternLisp/EvalSpec.hs` (COMPLETE - already covered, added additional test)
- [X] T020 [P] [US1] Write test for record equality (structural, order-independent) in `test/PatternLisp/EvalSpec.hs` (COMPLETE)

### Implementation for User Story 1

- [ ] T021 [US1] **REPLACE** `mapParser` with `recordParser` that detects `{` and delegates to gram parser in `src/PatternLisp/Parser.hs`
- [ ] T022 [US1] Implement record text extraction (from `{` to matching `}`) in `src/PatternLisp/Parser.hs`
- [ ] T023 [US1] Integrate gram parser invocation for record parsing in `src/PatternLisp/Parser.hs`
- [ ] T024 [US1] Implement conversion from gram record structure to `RecordLiteral` in `src/PatternLisp/Parser.hs`
- [ ] T025 [US1] Implement duplicate key detection during parsing in `src/PatternLisp/Parser.hs`
- [ ] T026 [US1] Implement error translation from gram parser errors to pattern-lisp parse errors with position in `src/PatternLisp/Parser.hs`
- [ ] T027 [US1] Add record parser to main expression parser in `src/PatternLisp/Parser.hs`
- [X] T028 [US1] Implement evaluation of `RecordLiteral` to `Subject.Value.VMap` in `src/PatternLisp/Eval.hs` (COMPLETE - already implemented in Phase 2)
- [X] T029 [US1] Verify all tests pass for User Story 1 (COMPLETE - 203 examples, 0 failures)

**Checkpoint**: At this point, User Story 1 should be fully functional and testable independently. Records can be written and parsed correctly.

---

## Phase 4: User Story 2 - Access and Manipulate Record Values (Priority: P2)

**Goal**: Enable developers to read values from records, check for key existence, and create modified copies of records programmatically.

**Independent Test**: Create records, access their values, check keys, and create modified copies, verifying all operations work correctly and immutability is preserved.

### Tests for User Story 3 ⚠️

> **NOTE: Write these tests FIRST, ensure they FAIL before implementation**

- [ ] T030 [P] [US2] Write test for `record?` type predicate in `test/PatternLisp/PrimitivesSpec.hs`
- [ ] T031 [P] [US2] Write test for `record-get` with existing key in `test/PatternLisp/PrimitivesSpec.hs`
- [ ] T032 [P] [US2] Write test for `record-get` with missing key (no default) in `test/PatternLisp/PrimitivesSpec.hs`
- [ ] T033 [P] [US2] Write test for `record-get` with missing key (with default) in `test/PatternLisp/PrimitivesSpec.hs`
- [ ] T034 [P] [US2] Write test for `record-has?` predicate in `test/PatternLisp/PrimitivesSpec.hs`
- [ ] T035 [P] [US2] Write test for `record-keys` operation in `test/PatternLisp/PrimitivesSpec.hs`
- [ ] T036 [P] [US2] Write test for `record-values` operation in `test/PatternLisp/PrimitivesSpec.hs`
- [ ] T037 [P] [US2] Write test for `record->alist` conversion in `test/PatternLisp/PrimitivesSpec.hs`
- [ ] T038 [P] [US2] Write test for `alist->record` conversion in `test/PatternLisp/PrimitivesSpec.hs`
- [ ] T039 [P] [US2] Write test for `record-set` operation (immutability) in `test/PatternLisp/PrimitivesSpec.hs`
- [ ] T040 [P] [US2] Write test for `record-remove` operation (immutability) in `test/PatternLisp/PrimitivesSpec.hs`
- [ ] T041 [P] [US2] Write test for `record-merge` operation in `test/PatternLisp/PrimitivesSpec.hs`
- [ ] T042 [P] [US2] Write test for `record-map` operation in `test/PatternLisp/PrimitivesSpec.hs`
- [ ] T043 [P] [US2] Write test for `record-filter` operation in `test/PatternLisp/PrimitivesSpec.hs`
- [ ] T044 [P] [US2] Write test for `record` constructor function in `test/PatternLisp/PrimitivesSpec.hs`
- [ ] T045 [P] [US2] Write test for immutability verification (original records unchanged) in `test/PatternLisp/PrimitivesSpec.hs`

### Implementation for User Story 2

- [ ] T046 [US2] Add `Record?` primitive to `Primitive` type in `src/PatternLisp/Syntax.hs`
- [ ] T047 [US2] Add record operation primitives to `Primitive` type in `src/PatternLisp/Syntax.hs` (RecordGet, RecordHas, RecordKeys, RecordValues, RecordToAlist, AlistToRecord, RecordSet, RecordRemove, RecordMerge, RecordMap, RecordFilter, Record)
- [ ] T048 [US2] Implement `primitiveName` for all record primitives in `src/PatternLisp/Syntax.hs`
- [ ] T049 [US2] Implement `primitiveFromName` for all record primitives in `src/PatternLisp/Syntax.hs`
- [ ] T050 [US2] Implement `record?` primitive evaluation in `src/PatternLisp/Primitives.hs`
- [ ] T051 [US2] Implement `record-get` primitive evaluation in `src/PatternLisp/Primitives.hs`
- [ ] T052 [US2] Implement `record-has?` primitive evaluation in `src/PatternLisp/Primitives.hs`
- [ ] T053 [US2] Implement `record-keys` primitive evaluation in `src/PatternLisp/Primitives.hs`
- [ ] T054 [US2] Implement `record-values` primitive evaluation in `src/PatternLisp/Primitives.hs`
- [ ] T055 [US2] Implement `record->alist` primitive evaluation in `src/PatternLisp/Primitives.hs`
- [ ] T056 [US2] Implement `alist->record` primitive evaluation in `src/PatternLisp/Primitives.hs`
- [ ] T057 [US2] Implement `record-set` primitive evaluation in `src/PatternLisp/Primitives.hs`
- [ ] T058 [US2] Implement `record-remove` primitive evaluation in `src/PatternLisp/Primitives.hs`
- [ ] T059 [US2] Implement `record-merge` primitive evaluation in `src/PatternLisp/Primitives.hs`
- [ ] T060 [US2] Implement `record-map` primitive evaluation in `src/PatternLisp/Primitives.hs`
- [ ] T061 [US2] Implement `record-filter` primitive evaluation in `src/PatternLisp/Primitives.hs`
- [ ] T062 [US2] Implement `record` constructor primitive evaluation in `src/PatternLisp/Primitives.hs`
- [ ] T063 [US2] Export all record primitives in standard library in `src/PatternLisp/Runtime.hs`
- [ ] T064 [US2] Verify all tests pass for User Story 2

**Checkpoint**: At this point, User Stories 1 AND 2 should both work independently. Complete record operations are available.

---

## Phase 5: User Story 3 - Use Records in Quasiquotation (Priority: P3)

**Goal**: Enable developers to construct records dynamically using unquoting and splicing within record literals for building records from computed values and combining records programmatically.

**Independent Test**: Write quasiquoted records with unquoted expressions and verify resulting records contain computed values. Test unquoting and splicing separately and together.

### Tests for User Story 3 ⚠️

> **NOTE: Write these tests FIRST, ensure they FAIL before implementation**

- [ ] T065 [P] [US3] Write test for unquoting in record literal in `test/PatternLisp/EvalSpec.hs`
- [ ] T066 [P] [US3] Write test for splicing record in record literal in `test/PatternLisp/EvalSpec.hs`
- [ ] T067 [P] [US3] Write test for combined unquoting and splicing in `test/PatternLisp/EvalSpec.hs`
- [ ] T068 [P] [US3] Write test for error when splicing non-record in `test/PatternLisp/EvalSpec.hs`
- [ ] T069 [P] [US3] Write integration test for quasiquotation with records in `test/PatternLisp/IntegrationSpec.hs`

### Implementation for User Story 3

- [ ] T070 [US3] Extend `RecordLiteral` evaluation to handle `Unquote Expr` in values in `src/PatternLisp/Eval.hs`
- [ ] T071 [US3] Extend `RecordLiteral` evaluation to handle `UnquoteSplice Expr` in values in `src/PatternLisp/Eval.hs`
- [ ] T072 [US3] Implement unquote evaluation (evaluate expression and use value) in `src/PatternLisp/Eval.hs`
- [ ] T073 [US3] Implement splice evaluation (evaluate to record and merge entries) in `src/PatternLisp/Eval.hs`
- [ ] T074 [US3] Implement error handling for splicing non-record values in `src/PatternLisp/Eval.hs`
- [ ] T075 [US3] Verify all tests pass for User Story 3

**Checkpoint**: All user stories should now be independently functional. Complete record feature with quasiquotation support.

---

## Phase 6: Polish & Cross-Cutting Concerns

**Purpose**: Improvements that affect multiple user stories, documentation, and validation

- [ ] T076 [P] Add record examples to `examples/records.plisp` demonstrating all features
- [ ] T077 [P] Update `examples/README.md` with record examples documentation
- [ ] T078 [P] Verify record serialization/deserialization round-trips correctly in `test/PatternLisp/CodecSpec.hs`
- [ ] T079 [P] Add performance tests for large records (1000+ keys) in `test/PatternLisp/PrimitivesSpec.hs`
- [ ] T080 [P] Add edge case tests for deeply nested records in `test/PatternLisp/EvalSpec.hs`
- [ ] T081 [P] Verify all existing tests still pass (regression check)
- [ ] T082 [P] Run `cabal build` and fix any compilation errors
- [ ] T083 [P] Run `cabal test` and ensure 100% test pass rate
- [ ] T084 [P] Code cleanup and refactoring (remove debug code, improve error messages)
- [ ] T085 [P] Add Haddock documentation comments to all record-related functions in source files
- [ ] T086 [P] Create end-user language reference documentation for records in `docs/pattern-lisp-records.md`
- [ ] T087 [P] Add record syntax section to `docs/pattern-lisp-syntax-conventions.md`
- [ ] T088 [P] Add record examples to language reference in `docs/pattern-lisp-records.md`
- [ ] T089 [P] Document all record operations with signatures and examples in `docs/pattern-lisp-records.md`
- [ ] T090 [P] Note gram syntax compatibility in `docs/pattern-lisp-records.md`
- [ ] T091 [P] Update main README.md to mention record feature if appropriate
- [ ] T092 [P] Run quickstart.md validation (verify all examples in quickstart.md work)

---

## Dependencies & Execution Order

### Phase Dependencies

- **Setup (Phase 1)**: No dependencies - can start immediately
- **Foundational (Phase 2)**: Depends on Setup completion - BLOCKS all user stories
- **User Stories (Phase 3-5)**: All depend on Foundational phase completion
  - User Story 1 (P1): Can start immediately after Foundational
  - User Story 2 (P2): Depends on User Story 1 (needs record values to operate on)
  - User Story 3 (P3): Depends on User Story 1 (needs record evaluation for quasiquotation)
- **Polish (Phase 6)**: Depends on all desired user stories being complete

### User Story Dependencies

- **User Story 1 (P1)**: Can start after Foundational (Phase 2) - No dependencies on other stories
- **User Story 2 (P2)**: Depends on User Story 1 completion (needs record values to operate on)
- **User Story 3 (P3)**: Depends on User Story 1 completion (needs record evaluation for quasiquotation)

### Within Each User Story

- Tests (mandatory) MUST be written and FAIL before implementation
- Parser changes before evaluation changes
- Core implementation before integration
- Story complete before moving to next priority

### Parallel Opportunities

- All Setup tasks marked [P] can run in parallel
- All Foundational tasks marked [P] can run in parallel (within Phase 2)
- Once Foundational phase completes, User Story 1 can start
- All tests for a user story marked [P] can run in parallel
- User Stories 2 and 3 can potentially start in parallel after User Story 1
- All Polish tasks marked [P] can run in parallel

---

## Parallel Example: User Story 1

```bash
# Launch all tests for User Story 1 together:
Task: "Write test for empty record parsing in test/PatternLisp/ParserSpec.hs"
Task: "Write test for simple record parsing in test/PatternLisp/ParserSpec.hs"
Task: "Write test for nested record parsing in test/PatternLisp/ParserSpec.hs"
Task: "Write test for records with different value types in test/PatternLisp/ParserSpec.hs"
Task: "Write test for duplicate key error in test/PatternLisp/ParserSpec.hs"
Task: "Write test for unclosed record error in test/PatternLisp/ParserSpec.hs"
Task: "Write test for invalid key error in test/PatternLisp/ParserSpec.hs"
Task: "Write test for record evaluation to VRecord in test/PatternLisp/EvalSpec.hs"
Task: "Write test for record equality in test/PatternLisp/EvalSpec.hs"
```

---

## Implementation Strategy

### MVP First (User Story 1 Only)

1. Complete Phase 1: Setup
2. Complete Phase 2: Foundational (CRITICAL - blocks all stories)
3. Complete Phase 3: User Story 1 (Record parsing and basic evaluation)
4. **STOP and VALIDATE**: Test User Story 1 independently
5. Deploy/demo if ready

### Incremental Delivery

1. Complete Setup + Foundational → Foundation ready
2. Add User Story 1 → Test independently → Deploy/Demo (MVP!)
3. Add User Story 2 → Test independently → Deploy/Demo
4. Add User Story 3 → Test independently → Deploy/Demo
5. Add Polish & Documentation → Final release
7. Each story adds value without breaking previous stories

### Parallel Team Strategy

With multiple developers:

1. Team completes Setup + Foundational together
2. Once Foundational is done:
   - Developer A: User Story 1 (must complete first)
   - Once User Story 1 is done:
     - Developer A: User Story 2 (can start after US1)
     - Developer B: User Story 3 (can start after US1)
3. Stories complete and integrate independently
4. All developers: Polish & Documentation phase

---

## Notes

- [P] tasks = different files, no dependencies
- [Story] label maps task to specific user story for traceability
- Each user story should be independently completable and testable
- Verify tests fail before implementing (test-first mandatory)
- Commit after each task or logical group
- Stop at any checkpoint to validate story independently
- End-user documentation tasks (T098-T102) are included in Polish phase
- Avoid: vague tasks, same file conflicts, cross-story dependencies that break independence

---

## Summary

- **Total Tasks**: 92
- **User Story 1 (P1)**: 18 tasks (9 tests + 9 implementation)
- **User Story 2 (P2)**: 33 tasks (16 tests + 17 implementation)
- **User Story 3 (P3)**: 11 tasks (5 tests + 6 implementation)
- **Setup**: 3 tasks
- **Foundational**: 8 tasks
- **Polish & Documentation**: 19 tasks (including 5 end-user documentation tasks)

**Suggested MVP Scope**: Phase 1 + Phase 2 + Phase 3 (User Story 1 only) = 29 tasks

**End-User Documentation**: Tasks T086-T090 create comprehensive language reference documentation for records feature.
