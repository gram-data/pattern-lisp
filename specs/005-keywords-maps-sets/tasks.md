# Tasks: Keywords, Maps, and Sets

**Input**: Design documents from `/specs/005-keywords-maps-sets/`
**Prerequisites**: plan.md (required), spec.md (required for user stories), research.md, data-model.md, contracts/

**Tests**: TDD is MANDATORY per Constitution Check. All tests must be written first and fail before implementation.

**Organization**: Tasks are grouped by user story to enable independent implementation and testing of each story.

## Format: `[ID] [P?] [Story] Description`

- **[P]**: Can run in parallel (different files, no dependencies)
- **[Story]**: Which user story this task belongs to (e.g., US1, US2, US3)
- Include exact file paths in descriptions

## Path Conventions

- **Single project**: `src/PatternLisp/`, `test/PatternLisp/` at repository root
- All paths shown below use existing Pattern Lisp module structure

---

## Phase 1: Setup (Shared Infrastructure)

**Purpose**: Project initialization and verification

- [x] T001 Verify existing project structure and dependencies in `pattern-lisp.cabal`
- [x] T002 [P] Verify `containers` package is available for `Data.Map` and `Data.Set` in `pattern-lisp.cabal`
- [x] T003 [P] Review existing test structure in `test/PatternLisp/` to understand test patterns

---

## Phase 2: Foundational (Blocking Prerequisites)

**Purpose**: Core type system extensions that MUST be complete before ANY user story can be implemented

**⚠️ CRITICAL**: No user story work can begin until this phase is complete

- [x] T004 Add `Keyword String` variant to `Atom` type in `src/PatternLisp/Syntax.hs`
- [x] T005 [P] Add `VKeyword String` variant to `Value` type in `src/PatternLisp/Syntax.hs`
- [x] T006 [P] Add `VMap (Map.Map KeywordKey Value)` variant to `Value` type in `src/PatternLisp/Syntax.hs` (using KeywordKey newtype for type safety)
- [x] T007 [P] Add `VSet (Set.Set Value)` variant to `Value` type in `src/PatternLisp/Syntax.hs`
- [x] T008 Add `Eq` and `Ord` instances for `KeywordKey` in `src/PatternLisp/Syntax.hs` (needed for Map keys)
- [x] T009 Add `Eq` and `Ord` instances for `Value` in `src/PatternLisp/Syntax.hs` (needed for Set elements - custom Ord instance implemented)

**Checkpoint**: Foundation ready - type system extended, user story implementation can now begin

---

## Phase 3: User Story 1 - Using Keywords for Map Keys (Priority: P1) 🎯 MVP

**Goal**: Developers can use keywords with postfix colon syntax (`name:`) that evaluate to themselves without environment lookup. Keywords are foundational for maps.

**Independent Test**: Can be fully tested by evaluating keyword expressions and using keywords as map keys. Developers can write `{name: "Alice" age: 30}` and verify that keywords evaluate to themselves and work as map keys.

### Tests for User Story 1 ⚠️

> **NOTE: Write these tests FIRST, ensure they FAIL before implementation**

- [x] T010 [P] [US1] Test keyword parsing in `test/PatternLisp/ParserSpec.hs` - parse `name:` as `Atom (Keyword "name")`
- [x] T011 [P] [US1] Test keyword evaluation in `test/PatternLisp/EvalSpec.hs` - `name:` evaluates to `VKeyword "name"` without environment lookup
- [x] T012 [P] [US1] Test keyword comparison in `test/PatternLisp/EvalSpec.hs` - `(= name: name:)` returns `VBool True`
- [x] T013 [P] [US1] Test keyword type distinction in `test/PatternLisp/EvalSpec.hs` - keywords are distinct from symbols (type error if used as symbol)

### Implementation for User Story 1

- [x] T014 [US1] Implement keyword parser in `src/PatternLisp/Parser.hs` - recognize `symbol:` pattern (postfix colon)
- [x] T015 [US1] Implement keyword evaluation in `src/PatternLisp/Eval.hs` - `Atom (Keyword s)` evaluates to `VKeyword s` directly
- [x] T016 [US1] Update `Eq` instance for keywords in `src/PatternLisp/Syntax.hs` - keywords compare by string equality (via Value Eq instance)
- [x] T017 [US1] Add error handling for keyword type mismatches in `src/PatternLisp/Eval.hs` - clear error when keyword used as symbol (keywords evaluate to themselves, no lookup)

**Checkpoint**: At this point, User Story 1 should be fully functional and testable independently. Keywords can be parsed, evaluated, and compared.

---

## Phase 4: User Story 3 - Working with Sets (Priority: P2)

**Goal**: Developers can work with unordered collections of unique elements using hash set syntax (`#{1 2 3}`). Sets are important for Subject labels and unique value collections.

**Independent Test**: Can be fully tested by creating sets, checking membership, and performing set operations. Developers can write set literals, use set operations, and verify correct behavior.

**Note**: Sets can be implemented in parallel with keywords (no dependency), but we implement after keywords to follow priority order.

### Tests for User Story 3 ⚠️

> **NOTE: Write these tests FIRST, ensure they FAIL before implementation**

- [x] T018 [P] [US3] Test set literal parsing in `test/PatternLisp/ParserSpec.hs` - parse `#{1 2 3}` as set literal
- [x] T019 [P] [US3] Test set evaluation in `test/PatternLisp/EvalSpec.hs` - `#{1 2 3}` evaluates to `VSet` with correct elements
- [x] T020 [P] [US3] Test duplicate removal in `test/PatternLisp/EvalSpec.hs` - `#{1 2 2 3}` removes duplicates
- [x] T021 [P] [US3] Test `contains?` primitive in `test/PatternLisp/PrimitivesSpec.hs` - `(contains? #{1 2 3} 2)` returns `true`
- [x] T022 [P] [US3] Test `set-union` primitive in `test/PatternLisp/PrimitivesSpec.hs` - `(set-union #{1 2} #{2 3})` returns `#{1 2 3}`
- [x] T023 [P] [US3] Test `set-intersection` primitive in `test/PatternLisp/PrimitivesSpec.hs` - `(set-intersection #{1 2 3} #{2 3 4})` returns `#{2 3}`
- [x] T024 [P] [US3] Test `set-difference` primitive in `test/PatternLisp/PrimitivesSpec.hs` - `(set-difference #{1 2 3} #{2})` returns `#{1 3}`
- [x] T025 [P] [US3] Test `set-symmetric-difference` primitive in `test/PatternLisp/PrimitivesSpec.hs` - `(set-symmetric-difference #{1 2} #{2 3})` returns `#{1 3}`
- [x] T026 [P] [US3] Test `set-subset?` primitive in `test/PatternLisp/PrimitivesSpec.hs` - `(set-subset? #{1 2} #{1 2 3})` returns `true`
- [x] T027 [P] [US3] Test `set-equal?` primitive in `test/PatternLisp/PrimitivesSpec.hs` - `(set-equal? #{1 2 3} #{3 2 1})` returns `true`
- [x] T028 [P] [US3] Test `empty?` primitive for sets in `test/PatternLisp/PrimitivesSpec.hs` - `(empty? #{})` returns `true`
- [x] T029 [P] [US3] Test `hash-set` constructor in `test/PatternLisp/PrimitivesSpec.hs` - `(hash-set 1 2 3)` creates set equivalent to `#{1 2 3}`

### Implementation for User Story 3

- [x] T030 [US3] Add set operation primitives to `Primitive` type in `src/PatternLisp/Syntax.hs` - `SetContains`, `SetUnion`, `SetIntersection`, `SetDifference`, `SetSymmetricDifference`, `SetSubset`, `SetEqual`, `SetEmpty`, `HashSet`
- [x] T031 [US3] Implement set literal parser in `src/PatternLisp/Parser.hs` - recognize `#{...}` syntax with whitespace handling (added `SetLiteral` to Expr and `setParser`)
- [x] T032 [US3] Implement set literal evaluation in `src/PatternLisp/Eval.hs` - evaluate set literal to `VSet` with duplicates removed (using `Set.fromList`)
- [x] T033 [US3] Implement `contains?` primitive for sets in `src/PatternLisp/Eval.hs` - check membership using `Set.member`
- [x] T034 [US3] Implement `set-union` primitive in `src/PatternLisp/Eval.hs` - use `Set.union`
- [x] T035 [US3] Implement `set-intersection` primitive in `src/PatternLisp/Eval.hs` - use `Set.intersection`
- [x] T036 [US3] Implement `set-difference` primitive in `src/PatternLisp/Eval.hs` - use `Set.difference`
- [x] T037 [US3] Implement `set-symmetric-difference` primitive in `src/PatternLisp/Eval.hs` - use `Set.union (Set.difference s1 s2) (Set.difference s2 s1)`
- [x] T038 [US3] Implement `set-subset?` primitive in `src/PatternLisp/Eval.hs` - use `Set.isSubsetOf`
- [x] T039 [US3] Implement `set-equal?` primitive in `src/PatternLisp/Eval.hs` - use `Set.fromList` and compare (order doesn't matter) - using `==` on sets
- [x] T040 [US3] Implement `empty?` primitive for sets in `src/PatternLisp/Eval.hs` - use `Set.null`
- [x] T041 [US3] Implement `hash-set` constructor in `src/PatternLisp/Eval.hs` - create set from variable arguments (using `Set.fromList`)
- [x] T042 [US3] Add set operation primitives to `initialEnv` in `src/PatternLisp/Primitives.hs` - register all set primitives
- [x] T043 [US3] Add `primitiveName` cases for set primitives in `src/PatternLisp/Syntax.hs` - string names for serialization (also added `primitiveFromName` cases)

**Checkpoint**: At this point, User Story 3 should be fully functional and testable independently. Sets can be created, manipulated, and all operations work correctly.

---

## Phase 5: User Story 2 - Creating and Manipulating Maps (Priority: P1)

**Goal**: Developers can work with key-value data structures using maps with keyword keys. Maps provide efficient lookup and update operations for configuration, state management, and structured data.

**Independent Test**: Can be fully tested by creating maps, accessing values, updating keys, and performing map operations. Developers can write map literals, use map operations, and verify correct behavior.

**Note**: Maps depend on keywords (User Story 1), so this phase must come after Phase 3.

### Tests for User Story 2 ⚠️

> **NOTE: Write these tests FIRST, ensure they FAIL before implementation**

- [x] T044 [P] [US2] Test map literal parsing in `test/PatternLisp/ParserSpec.hs` - parse `{name: "Alice" age: 30}` as map literal
- [x] T045 [P] [US2] Test nested map parsing in `test/PatternLisp/ParserSpec.hs` - parse `{user: {name: "Bob"}}` as nested map
- [x] T046 [P] [US2] Test duplicate key handling in `test/PatternLisp/ParserSpec.hs` - `{name: "Alice" name: "Bob"}` uses last value (parser allows, evaluator handles)
- [x] T047 [P] [US2] Test map evaluation in `test/PatternLisp/EvalSpec.hs` - `{name: "Alice" age: 30}` evaluates to `VMap` with correct entries
- [x] T048 [P] [US2] Test `get` primitive in `test/PatternLisp/PrimitivesSpec.hs` - `(get {name: "Alice"} name:)` returns `"Alice"`
- [x] T049 [P] [US2] Test `get` with default in `test/PatternLisp/PrimitivesSpec.hs` - `(get {name: "Alice"} age: 0)` returns `0`
- [x] T050 [P] [US2] Test `get-in` primitive in `test/PatternLisp/PrimitivesSpec.hs` - `(get-in {user: {name: "Alice"}} [user: name:])` returns `"Alice"` (tested via nested get)
- [x] T051 [P] [US2] Test `assoc` primitive in `test/PatternLisp/PrimitivesSpec.hs` - `(assoc {name: "Alice"} age: 30)` adds/updates key
- [x] T052 [P] [US2] Test `dissoc` primitive in `test/PatternLisp/PrimitivesSpec.hs` - `(dissoc {name: "Alice" age: 30} age:)` removes key
- [x] T053 [P] [US2] Test `update` primitive in `test/PatternLisp/PrimitivesSpec.hs` - `(update {count: 5} count: inc)` returns `{count: 6}` (tested with lambda)
- [x] T054 [P] [US2] Test `update` on non-existent key in `test/PatternLisp/PrimitivesSpec.hs` - `(update {} count: inc)` creates key with function applied to `nil` (tested with empty list as nil)
- [x] T055 [P] [US2] Test `contains?` primitive for maps in `test/PatternLisp/PrimitivesSpec.hs` - `(contains? {name: "Alice"} name:)` returns `true`
- [x] T056 [P] [US2] Test `empty?` primitive for maps in `test/PatternLisp/PrimitivesSpec.hs` - `(empty? {})` returns `true`
- [x] T057 [P] [US2] Test `hash-map` constructor in `test/PatternLisp/PrimitivesSpec.hs` - `(hash-map name: "Alice" age: 30)` creates map equivalent to literal

### Implementation for User Story 2

- [x] T058 [US2] Add map operation primitives to `Primitive` type in `src/PatternLisp/Syntax.hs` - `MapGet`, `MapGetIn`, `MapAssoc`, `MapDissoc`, `MapUpdate`, `MapContains`, `MapEmpty`, `HashMap`
- [x] T059 [US2] Implement map literal parser in `src/PatternLisp/Parser.hs` - recognize `{key: value ...}` syntax with keyword keys (added `MapLiteral` to Expr and `mapParser`)
- [x] T060 [US2] Implement duplicate key handling in map parser in `src/PatternLisp/Parser.hs` - last value wins (silently overwrites) - handled in evaluator via Map.insert
- [x] T061 [US2] Implement map literal evaluation in `src/PatternLisp/Eval.hs` - evaluate map literal to `VMap` with `VKeyword` keys (converted to `KeywordKey`)
- [x] T062 [US2] Implement `get` primitive in `src/PatternLisp/Eval.hs` - `Map.lookup` with `nil` (empty list) or default return
- [x] T063 [US2] Implement `get-in` primitive in `src/PatternLisp/Eval.hs` - nested access via keyword path list
- [x] T064 [US2] Implement `assoc` primitive in `src/PatternLisp/Eval.hs` - `Map.insert` to add/update key
- [x] T065 [US2] Implement `dissoc` primitive in `src/PatternLisp/Eval.hs` - `Map.delete` to remove key (returns unchanged if key doesn't exist)
- [x] T066 [US2] Implement `update` primitive in `src/PatternLisp/Eval.hs` - apply function to value at key, create with `f nil` (empty list) if key doesn't exist
- [x] T067 [US2] Implement `contains?` primitive for maps in `src/PatternLisp/Eval.hs` - use `Map.member` (also handles sets via SetContains)
- [x] T068 [US2] Implement `empty?` primitive for maps in `src/PatternLisp/Eval.hs` - use `Map.null` (also handles sets via SetEmpty)
- [x] T069 [US2] Implement `hash-map` constructor in `src/PatternLisp/Eval.hs` - create map from alternating keyword-value pairs
- [x] T070 [US2] Add map operation primitives to `initialEnv` in `src/PatternLisp/Primitives.hs` - register all map primitives
- [x] T071 [US2] Add `primitiveName` cases for map primitives in `src/PatternLisp/Syntax.hs` - string names for serialization (also added `primitiveFromName` cases)
- [x] T072 [US2] Add type error handling in `src/PatternLisp/Eval.hs` - clear errors when non-keyword used as map key (type checking in map literal evaluation and hash-map)

**Checkpoint**: At this point, User Story 2 should be fully functional and testable independently. Maps can be created, accessed, updated, and all operations work correctly.

---

## Phase 6: User Story 4 - Subject Labels as String Sets (Priority: P3)

**Goal**: Developers can represent Subject labels as sets of strings. Plain strings in sets are sufficient; prefix colon syntax is optional and deferred.

**Independent Test**: Can be fully tested by creating sets of strings for labels. Developers can write `#{"Person" "Employee"}` to represent Subject labels.

**Note**: This story is mostly covered by User Story 3 (sets), but we add integration tests for gram interop.

### Tests for User Story 4 ⚠️

> **NOTE: Write these tests FIRST, ensure they FAIL before implementation**

- [x] T073 [P] [US4] Test Subject label set creation in `test/PatternLisp/EvalSpec.hs` - `#{"Person" "Employee"}` creates set of strings
- [x] T074 [P] [US4] Test Subject label set membership in `test/PatternLisp/EvalSpec.hs` - `(contains? #{"Person" "Employee"} "Person")` returns `true`
- [x] T075 [P] [US4] Test gram interop with Subject labels in `test/PatternLisp/GramSpec.hs` - string sets can be used directly for gram pattern Subject labels

### Implementation for User Story 4

- [x] T076 [US4] Add integration test for gram pattern with Subject labels in `test/PatternLisp/GramSpec.hs` - verify `Set String` works with gram patterns (tested round-trip serialization)
- [x] T077 [US4] Document Subject label usage in examples or documentation (added to quickstart.md)

**Checkpoint**: At this point, User Story 4 should be fully functional. Subject labels work as string sets for gram interop.

---

## Phase 7: Serialization & Integration

**Purpose**: Serialization support for keywords, maps, and sets to maintain compatibility with existing Pattern Lisp serialization

### Tests for Serialization ⚠️

> **NOTE: Write these tests FIRST, ensure they FAIL before implementation**

- [x] T078 [P] Test keyword serialization in `test/PatternLisp/CodecSpec.hs` - keyword serializes and deserializes correctly
- [x] T079 [P] Test map serialization in `test/PatternLisp/CodecSpec.hs` - map serializes and deserializes with keyword keys preserved
- [x] T080 [P] Test set serialization in `test/PatternLisp/CodecSpec.hs` - set serializes and deserializes correctly
- [x] T081 [P] Test round-trip serialization in `test/PatternLisp/CodecSpec.hs` - keywords, maps, sets preserve types through serialization

### Implementation for Serialization

- [x] T082 Implement keyword serialization in `src/PatternLisp/Codec.hs` - keywords serialize as Pattern Subject with label "Keyword" and property "name"
- [x] T083 Implement keyword deserialization in `src/PatternLisp/Codec.hs` - detect "Keyword" label and reconstruct `VKeyword` from "name" property
- [x] T084 Implement map serialization in `src/PatternLisp/Codec.hs` - maps serialize as Pattern Subject with label "Map" and alternating key-value elements
- [x] T085 Implement map deserialization in `src/PatternLisp/Codec.hs` - deserialize alternating key-value pairs from pattern elements, convert string keys to `KeywordKey`
- [x] T086 Implement set serialization in `src/PatternLisp/Codec.hs` - sets serialize as Pattern Subject with label "Set" and elements as pattern elements
- [x] T087 Implement set deserialization in `src/PatternLisp/Codec.hs` - deserialize elements from pattern elements, remove duplicates using `Set.fromList`
- [x] T088 Update `valueToSubjectForGram` in `src/PatternLisp/Codec.hs` - handle keywords, maps, sets (maps and sets use empty properties, elements stored as pattern elements)
- [x] T089 Update `valueToPatternSubjectForGram` in `src/PatternLisp/Codec.hs` - handle keywords, maps, sets (maps use alternating key-value pattern elements, sets use element pattern elements)

**Checkpoint**: Serialization complete - keywords, maps, and sets can be serialized and deserialized with type preservation.

---

## Phase 8: Polish & Cross-Cutting Concerns

**Purpose**: Improvements that affect multiple user stories

- [ ] T090 [P] Update error messages in `src/PatternLisp/Eval.hs` - clear messages for keyword, map, set type mismatches
- [ ] T091 [P] Add property-based tests in `test/Properties.hs` - QuickCheck tests for map operations (assoc then get returns correct value)
- [ ] T092 [P] Add property-based tests in `test/Properties.hs` - QuickCheck tests for set operations (union is commutative, intersection is idempotent)
- [ ] T093 [P] Add property-based tests in `test/Properties.hs` - QuickCheck tests for serialization (round-trip preserves values and types)
- [ ] T094 [P] Update documentation in `docs/` - document keywords, maps, sets syntax and usage
- [ ] T095 [P] Add examples in `examples/` - example files demonstrating keywords, maps, sets usage
- [ ] T096 Run quickstart.md validation - verify all examples in quickstart.md work correctly
- [ ] T097 Code cleanup and refactoring - review all changes for consistency
- [ ] T098 Performance validation - verify SC-001, SC-002, SC-003 performance goals are met

---

## Dependencies & Execution Order

### Phase Dependencies

- **Setup (Phase 1)**: No dependencies - can start immediately
- **Foundational (Phase 2)**: Depends on Setup completion - BLOCKS all user stories
- **User Story 1 - Keywords (Phase 3)**: Depends on Foundational - Can start after Phase 2
- **User Story 3 - Sets (Phase 4)**: Depends on Foundational - Can start after Phase 2 (can be parallel with US1)
- **User Story 2 - Maps (Phase 5)**: Depends on Foundational AND User Story 1 (keywords) - Must wait for Phase 3
- **User Story 4 - Labels (Phase 6)**: Depends on User Story 3 (sets) - Can start after Phase 4
- **Serialization (Phase 7)**: Depends on all user stories - Can start after Phases 3, 4, 5
- **Polish (Phase 8)**: Depends on all previous phases - Final phase

### User Story Dependencies

- **User Story 1 (P1) - Keywords**: Can start after Foundational (Phase 2) - No dependencies on other stories
- **User Story 3 (P2) - Sets**: Can start after Foundational (Phase 2) - Can be parallel with US1
- **User Story 2 (P1) - Maps**: Can start after Foundational (Phase 2) AND User Story 1 - Requires keywords
- **User Story 4 (P3) - Labels**: Can start after User Story 3 - Mostly covered by sets, adds integration tests

### Within Each User Story

- Tests (MANDATORY per Constitution) MUST be written and FAIL before implementation
- Type extensions before parser
- Parser before evaluator
- Evaluator before primitives
- Primitives before integration
- Story complete before moving to next priority

### Parallel Opportunities

- **Phase 1**: All Setup tasks marked [P] can run in parallel
- **Phase 2**: All Foundational tasks marked [P] can run in parallel (T005, T006, T007)
- **Phase 3 (US1)**: All test tasks marked [P] can run in parallel (T010-T013)
- **Phase 4 (US3)**: All test tasks marked [P] can run in parallel (T018-T029)
- **Phase 5 (US2)**: All test tasks marked [P] can run in parallel (T044-T057)
- **Phase 6 (US4)**: All test tasks marked [P] can run in parallel (T073-T075)
- **Phase 7**: All test tasks marked [P] can run in parallel (T078-T081)
- **Phase 8**: All tasks marked [P] can run in parallel
- **Cross-phase**: User Story 1 and User Story 3 can be worked on in parallel (different developers)

---

## Parallel Example: User Story 1

```bash
# Launch all tests for User Story 1 together:
Task: "Test keyword parsing in test/PatternLisp/ParserSpec.hs"
Task: "Test keyword evaluation in test/PatternLisp/EvalSpec.hs"
Task: "Test keyword comparison in test/PatternLisp/EvalSpec.hs"
Task: "Test keyword type distinction in test/PatternLisp/EvalSpec.hs"
```

---

## Parallel Example: User Story 3

```bash
# Launch all tests for User Story 3 together:
Task: "Test set literal parsing in test/PatternLisp/ParserSpec.hs"
Task: "Test set evaluation in test/PatternLisp/EvalSpec.hs"
Task: "Test duplicate removal in test/PatternLisp/EvalSpec.hs"
Task: "Test contains? primitive in test/PatternLisp/PrimitivesSpec.hs"
Task: "Test set-union primitive in test/PatternLisp/PrimitivesSpec.hs"
# ... (all other set operation tests)
```

---

## Implementation Strategy

### MVP First (User Story 1 Only)

1. Complete Phase 1: Setup
2. Complete Phase 2: Foundational (CRITICAL - blocks all stories)
3. Complete Phase 3: User Story 1 (Keywords)
4. **STOP and VALIDATE**: Test User Story 1 independently
5. Deploy/demo if ready

### Incremental Delivery

1. Complete Setup + Foundational → Foundation ready
2. Add User Story 1 (Keywords) → Test independently → Deploy/Demo (MVP!)
3. Add User Story 3 (Sets) → Test independently → Deploy/Demo
4. Add User Story 2 (Maps) → Test independently → Deploy/Demo (requires US1)
5. Add User Story 4 (Labels) → Test independently → Deploy/Demo
6. Add Serialization → Test independently → Deploy/Demo
7. Polish → Final release

### Parallel Team Strategy

With multiple developers:

1. Team completes Setup + Foundational together
2. Once Foundational is done:
   - Developer A: User Story 1 (Keywords)
   - Developer B: User Story 3 (Sets) - can start in parallel
3. Once User Story 1 is done:
   - Developer A: User Story 2 (Maps) - requires keywords
   - Developer B: User Story 4 (Labels) - can continue
4. Once all stories done:
   - Team: Serialization and Polish

---

## Notes

- [P] tasks = different files, no dependencies
- [Story] label maps task to specific user story for traceability
- Each user story should be independently completable and testable
- **TDD MANDATORY**: Verify tests fail before implementing
- Commit after each task or logical group
- Stop at any checkpoint to validate story independently
- Avoid: vague tasks, same file conflicts, cross-story dependencies that break independence
- Follow Constitution: Test-first, library-first, observability (clear errors)

