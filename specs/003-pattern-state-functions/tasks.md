# Tasks: Pattern Lisp Serialization to Gram

**Input**: Design documents from `/specs/003-pattern-state-functions/` and `/docs/plisp-serialization-design.md`
**Prerequisites**: plan.md, spec.md, data-model.md, plisp-serialization-design.md, plisp-serialization-design-review-todos.md

**Tests**: Tests are included per the feature specification requirements for comprehensive coverage of serialization, pattern operations, and runtime execution.

**Organization**: Tasks are grouped by user story to enable independent implementation and testing of each story.

## Format: `[ID] [P?] [Story] Description`

- **[P]**: Can run in parallel (different files, no dependencies)
- **[Story]**: Which user story this task belongs to (e.g., US1, US2, US3)
- Include exact file paths in descriptions

## Path Conventions

- **Single project**: `src/`, `test/` at repository root
- Source modules: `src/PatternLisp/`
- Test modules: `test/PatternLisp/`
- Examples: `examples/`

---

## Phase 1: Setup (Shared Infrastructure)

**Purpose**: Project initialization, module verification, and dependency setup

- [X] T001 Create `src/PatternLisp/` directory structure
- [X] T002 [P] Move `src/Lisp/Syntax.hs` to `src/PatternLisp/Syntax.hs` and update module declaration
- [X] T003 [P] Move `src/Lisp/Parser.hs` to `src/PatternLisp/Parser.hs` and update module declaration
- [X] T004 [P] Move `src/Lisp/Eval.hs` to `src/PatternLisp/Eval.hs` and update module declaration
- [X] T005 [P] Move `src/Lisp/Primitives.hs` to `src/PatternLisp/Primitives.hs` and update module declaration
- [X] T006 [P] Move test files from `test/Lisp/` to `test/PatternLisp/` and update module declarations
- [X] T007 Update all imports across codebase to use `PatternLisp.*` namespace
- [X] T008 Update `pattern-lisp.cabal` exposed-modules to reflect new module structure
- [X] T009 Update `app/Main.hs` imports to use `PatternLisp.*` modules
- [X] T010 Add gram-data dependencies (`pattern` and `subject`) to `pattern-lisp.cabal` build-depends section
- [X] T011 Verify build: `cabal build all`
- [X] T012 Verify existing tests still pass: `cabal test`

**Checkpoint**: Module reorganization complete, project builds successfully

---

## Phase 2: Foundational (Blocking Prerequisites)

**Purpose**: Core type extensions and basic pattern support that ALL user stories depend on

**⚠️ CRITICAL**: No user story work can begin until this phase is complete

- [X] T013 Import `Subject.Core (Subject)` and `Pattern (Pattern)` in `src/PatternLisp/Syntax.hs`
- [X] T014 Add `VPattern (Pattern Subject)` constructor to `Value` type in `src/PatternLisp/Syntax.hs`
- [X] T015 Add pattern primitive constructors to `Primitive` type in `src/PatternLisp/Syntax.hs`
- [X] T016 Implement `primitiveName :: Primitive -> String` function in `src/PatternLisp/Syntax.hs`
- [X] T017 Implement `primitiveFromName :: String -> Maybe Primitive` function in `src/PatternLisp/Syntax.hs`
- [X] T018 Create `src/PatternLisp/PatternPrimitives.hs` module with module header and exports
- [X] T019 Implement `evalPatternCreate :: Value -> EvalM Value` in `src/PatternLisp/PatternPrimitives.hs`
- [X] T020 Implement `evalPatternWith :: Value -> [Value] -> EvalM Value` in `src/PatternLisp/PatternPrimitives.hs`
- [X] T021 Import `PatternLisp.PatternPrimitives` in `src/PatternLisp/Eval.hs`
- [X] T022 Add `applyPrimitive PatternCreate` case in `src/PatternLisp/Eval.hs` to call `PatternPrimitives.evalPatternCreate`
- [X] T023 Add `applyPrimitive PatternWith` case in `src/PatternLisp/Eval.hs` to call `PatternPrimitives.evalPatternWith`
- [X] T024 Add pattern primitives to `initialEnv` in `src/PatternLisp/Primitives.hs`
- [X] T025 Verify types compile: `cabal build`
- [X] T026 Test basic pattern construction: Added tests in `test/PatternLisp/PrimitivesSpec.hs`

**Checkpoint**: Foundation ready - Pattern values can be created, basic pattern operations available. User story implementation can now begin.

---

## Phase 3: User Story 1 - Pattern as First-Class Value Type (Priority: P1) 🎯 MVP

**Goal**: Developers can use Pattern values directly in Pattern Lisp programs, where Pattern values are decorated with Subject types to enable complete serialization and code-as-data capabilities.

**Independent Test**: Can be fully tested by creating Pattern values from various data types (numbers, strings, lists) and verifying they can be constructed, queried, and transformed. Delivers the ability to represent and manipulate state as Pattern structures.

### Tests for User Story 1

> **NOTE: Write these tests FIRST, ensure they FAIL before implementation**

- [X] T027 [P] [US1] Create test file `test/PatternLisp/PatternSpec.hs` with module header and imports
- [X] T028 [P] [US1] Add test "pattern construction creates atomic pattern" in `test/PatternLisp/PatternSpec.hs`
- [X] T029 [P] [US1] Add test "pattern-with creates pattern with elements" in `test/PatternLisp/PatternSpec.hs`
- [X] T030 [P] [US1] Add test "pattern-value extracts decoration correctly" in `test/PatternLisp/PatternSpec.hs`
- [X] T031 [P] [US1] Add test "pattern-elements returns list of VPattern elements" in `test/PatternLisp/PatternSpec.hs`
- [X] T032 [P] [US1] Add test "pattern-length returns correct direct element count" in `test/PatternLisp/PatternSpec.hs`
- [X] T033 [P] [US1] Add test "pattern-size counts all nodes recursively" in `test/PatternLisp/PatternSpec.hs`
- [X] T034 [P] [US1] Add test "pattern-depth returns max depth correctly" in `test/PatternLisp/PatternSpec.hs`
- [X] T035 [P] [US1] Add test "pattern-values flattens all values" in `test/PatternLisp/PatternSpec.hs`
- [X] T036 [P] [US1] Add test "nested patterns work correctly" in `test/PatternLisp/PatternSpec.hs`
- [X] T037 [P] [US1] Add test "type errors for non-pattern values" in `test/PatternLisp/PatternSpec.hs`
- [X] T038 [P] [US1] Add test "arity errors for wrong argument counts" in `test/PatternLisp/PatternSpec.hs`

### Implementation for User Story 1

- [X] T039 [US1] Implement `evalPatternValue :: Pattern Subject -> EvalM Value` in `src/PatternLisp/PatternPrimitives.hs`
- [X] T040 [US1] Implement `evalPatternElements :: Pattern Subject -> EvalM Value` in `src/PatternLisp/PatternPrimitives.hs`
- [X] T041 [US1] Implement `evalPatternLength :: Pattern Subject -> EvalM Value` in `src/PatternLisp/PatternPrimitives.hs`
- [X] T042 [US1] Implement `evalPatternSize :: Pattern Subject -> EvalM Value` in `src/PatternLisp/PatternPrimitives.hs`
- [X] T043 [US1] Implement `evalPatternDepth :: Pattern Subject -> EvalM Value` in `src/PatternLisp/PatternPrimitives.hs`
- [X] T044 [US1] Implement `evalPatternValues :: Pattern Subject -> EvalM Value` in `src/PatternLisp/PatternPrimitives.hs`
- [X] T045 [US1] Add helper function `expectPattern :: Value -> EvalM (Pattern Subject)` in `src/PatternLisp/PatternPrimitives.hs`
- [X] T046 [US1] Add `applyPrimitive PatternValue` case in `src/PatternLisp/Eval.hs` with type checking
- [X] T047 [US1] Add `applyPrimitive PatternElements` case in `src/PatternLisp/Eval.hs` with type checking
- [X] T048 [US1] Add `applyPrimitive PatternLength` case in `src/PatternLisp/Eval.hs` with type checking
- [X] T049 [US1] Add `applyPrimitive PatternSize` case in `src/PatternLisp/Eval.hs` with type checking
- [X] T050 [US1] Add `applyPrimitive PatternDepth` case in `src/PatternLisp/Eval.hs` with type checking
- [X] T051 [US1] Add `applyPrimitive PatternValues` case in `src/PatternLisp/Eval.hs` with type checking
- [X] T052 [US1] Add pattern query primitives to `initialEnv` in `src/PatternLisp/Primitives.hs`
- [X] T053 [US1] Add arity checking for all pattern query primitives in `src/PatternLisp/Eval.hs`
- [X] T054 [US1] Add type checking error messages for pattern operations in `src/PatternLisp/Eval.hs`
- [X] T055 [US1] Verify all tests pass: `cabal test`

**Checkpoint**: At this point, User Story 1 should be fully functional and testable independently. Pattern values can be created, queried, and transformed.

---

## Phase 4: User Story 2 - Pure Function State Transformation (Priority: P1) 🎯 MVP

**Goal**: Developers can write Pattern Lisp programs that are pure functions accepting a single state parameter (Pattern Subject) and returning a transformed Pattern Subject, following the canonical form `(lambda (state) ...)`.

**Independent Test**: Can be fully tested by writing a lambda expression with a single "state" parameter, evaluating it with a Pattern Subject input, and verifying it returns a Pattern Subject output. Delivers the ability to define tools as pure state transformations.

### Tests for User Story 2

> **NOTE: Write these tests FIRST, ensure they FAIL before implementation**

- [X] T056 [P] [US2] Create test file `test/PatternLisp/RuntimeSpec.hs` with module header and imports
- [X] T057 [P] [US2] Add test "valid tool: lambda with single state parameter" in `test/PatternLisp/RuntimeSpec.hs`
- [X] T058 [P] [US2] Add test "invalid tool: wrong parameter count" in `test/PatternLisp/RuntimeSpec.hs`
- [X] T059 [P] [US2] Add test "invalid tool: wrong parameter name" in `test/PatternLisp/RuntimeSpec.hs`
- [X] T060 [P] [US2] Add test "invalid tool: not a lambda" in `test/PatternLisp/RuntimeSpec.hs`
- [X] T061 [P] [US2] Add test "tool execution: identity tool returns same state" in `test/PatternLisp/RuntimeSpec.hs`
- [X] T062 [P] [US2] Add test "tool execution: tool transforms state correctly" in `test/PatternLisp/RuntimeSpec.hs`
- [X] T063 [P] [US2] Add test "tool execution: tool returns non-pattern should error" in `test/PatternLisp/RuntimeSpec.hs`
- [X] T064 [P] [US2] Add test "tool composition: sequential tool execution" in `test/PatternLisp/RuntimeSpec.hs`

### Implementation for User Story 2

- [X] T065 [US2] Create `src/PatternLisp/Runtime.hs` module with module header and exports
- [X] T066 [US2] Define `RuntimeState` type in `src/PatternLisp/Runtime.hs` with `environment :: Env` and `executionTrace :: [(String, Pattern Subject, Pattern Subject)]`
- [X] T067 [US2] Implement `validateTool :: Expr -> Env -> Either Error Value` in `src/PatternLisp/Runtime.hs` to check canonical form
- [X] T068 [US2] Implement `executeTool :: Value -> Pattern Subject -> Env -> Either Error (Pattern Subject)` in `src/PatternLisp/Runtime.hs`
- [X] T069 [US2] Add clear error messages for tool validation failures in `src/PatternLisp/Runtime.hs`
- [X] T070 [US2] Add error handling for non-Pattern tool results in `src/PatternLisp/Runtime.hs`
- [X] T071 [US2] Verify tool validation tests pass: `cabal test RuntimeSpec`

**Checkpoint**: At this point, User Stories 1 AND 2 should both work independently. Tools can be validated and executed with state transformations.

---

## Phase 5: User Story 3 - Complete Value Serialization to Gram (Priority: P2)

**Goal**: All Pattern Lisp values (including closures and primitives) are serializable to Gram notation following the design in `docs/plisp-serialization-design.md`, enabling code-as-data, persistence, and complete introspection of program state.

**Independent Test**: Can be fully tested by converting any Pattern Lisp value (number, string, list, Pattern, closure, primitive) to Gram notation and back, verifying the round-trip preserves functionality. Delivers the ability to serialize and deserialize all program state including code.

### Tests for User Story 3

> **NOTE: Write these tests FIRST, ensure they FAIL before implementation**

- [X] T072 [P] [US3] Create test file `test/PatternLisp/GramSerializationSpec.hs` with module header and imports
- [X] T073 [P] [US3] Add test "round-trip numbers" in `test/PatternLisp/GramSerializationSpec.hs`
- [X] T074 [P] [US3] Add test "round-trip strings" in `test/PatternLisp/GramSerializationSpec.hs`
- [X] T075 [P] [US3] Add test "round-trip booleans" in `test/PatternLisp/GramSerializationSpec.hs`
- [X] T076 [P] [US3] Add test "round-trip lists" in `test/PatternLisp/GramSerializationSpec.hs`
- [X] T077 [P] [US3] Add test "round-trip patterns" in `test/PatternLisp/GramSerializationSpec.hs`
- [X] T078 [P] [US3] Add test "round-trip simple closure" in `test/PatternLisp/GramSerializationSpec.hs`
- [X] T079 [P] [US3] Add test "round-trip closure with captured environment" in `test/PatternLisp/GramSerializationSpec.hs`
- [X] T080 [P] [US3] Add test "round-trip nested closures" in `test/PatternLisp/GramSerializationSpec.hs`
- [X] T081 [P] [US3] Add test "round-trip mutually recursive closures" in `test/PatternLisp/GramSerializationSpec.hs`
- [X] T082 [P] [US3] Add test "round-trip self-recursive closure" in `test/PatternLisp/GramSerializationSpec.hs`
- [X] T083 [P] [US3] Add test "round-trip primitives" in `test/PatternLisp/GramSerializationSpec.hs`
- [X] T084 [P] [US3] Add test "closure remains executable after round-trip" in `test/PatternLisp/GramSerializationSpec.hs`
- [X] T085 [P] [US3] Add test "primitive remains functional after round-trip" in `test/PatternLisp/GramSerializationSpec.hs`
- [X] T086 [P] [US3] Add test "pattern containing closures round-trips" in `test/PatternLisp/GramSerializationSpec.hs`
- [X] T087 [P] [US3] Add test "program with file-level metadata round-trips" in `test/PatternLisp/GramSerializationSpec.hs`
- [X] T088 [P] [US3] Add test "program with environment section round-trips" in `test/PatternLisp/GramSerializationSpec.hs`
- [X] T089 [P] [US3] Add test "binding deduplication works correctly" in `test/PatternLisp/GramSerializationSpec.hs`
- [X] T090 [P] [US3] Add test "parameters vs bound values distinction preserved" in `test/PatternLisp/GramSerializationSpec.hs`
- [X] T091 [P] [US3] Add test "special form labels preserved (If, Let, Begin, Define, Quote)" in `test/PatternLisp/GramSerializationSpec.hs`
- [X] T092 [P] [US3] Add test "standard library bindings filtered from environment" in `test/PatternLisp/GramSerializationSpec.hs`
- [X] T093 [P] [US3] Add test "missing primitive in registry errors correctly" in `test/PatternLisp/GramSerializationSpec.hs`
- [ ] T094 [P] [US3] Create property-based test file `test/Properties.hs` for serialization round-trips
- [ ] T095 [P] [US3] Add QuickCheck property "all values serialize and deserialize correctly" in `test/Properties.hs`

### Implementation for User Story 3

#### Core Serialization Module

- [X] T096 [US3] Create `src/PatternLisp/Codec.hs` module (or extend existing) with module header documenting Gram serialization approach
- [X] T097 [US3] Implement `valueToGram :: Value -> String` in `src/PatternLisp/Codec.hs` that serializes values to Gram notation (via valueToPatternSubjectForGram + toGram)
- [X] T098 [US3] Implement `gramToValue :: String -> Either Error Value` in `src/PatternLisp/Codec.hs` that deserializes Gram notation to values (via fromGram + patternSubjectToValue)
- [X] T099 [US3] Implement `programToGram :: [Value] -> Env -> String` in `src/PatternLisp/Codec.hs` that creates file-level structure with metadata (basic implementation done, TODO: file-level metadata and environment section)
- [X] T100 [US3] Implement `gramToProgram :: String -> Either Error ([Value], Env)` in `src/PatternLisp/Codec.hs` that parses file-level structure (basic implementation done, TODO: file-level parsing and environment section)

#### Value Serialization

- [X] T101 [US3] Implement `valueToPatternSubject :: Value -> Pattern Subject` for VNumber in `src/PatternLisp/Codec.hs`
- [X] T102 [US3] Implement `valueToPatternSubject :: Value -> Pattern Subject` for VString in `src/PatternLisp/Codec.hs`
- [X] T103 [US3] Implement `valueToPatternSubject :: Value -> Pattern Subject` for VBool in `src/PatternLisp/Codec.hs`
- [X] T104 [US3] Implement `valueToPatternSubject :: Value -> Pattern Subject` for VList in `src/PatternLisp/Codec.hs`
- [X] T105 [US3] Implement `valueToPatternSubject :: Value -> Pattern Subject` for VPattern in `src/PatternLisp/Codec.hs`
- [X] T106 [US3] Implement `valueToPatternSubject :: Value -> Pattern Subject` for VPrimitive in `src/PatternLisp/Codec.hs` (as `[:Primitive {name: "..."}]`)
- [X] T107 [US3] Implement `patternSubjectToValue :: Pattern Subject -> Either Error Value` for all value types in `src/PatternLisp/Codec.hs`

#### Closure Serialization

- [X] T108 [US3] Implement `closureToPatternSubject :: Closure -> Pattern Subject` in `src/PatternLisp/Codec.hs` using `[:Closure | [:Env | ...], [:Lambda | [:Parameters | ...], [:Body | ...]]]` structure (basic structure done, TODO: binding collection and deduplication)
- [X] T109 [US3] Implement `patternSubjectToClosure :: Pattern Subject -> Either Error Closure` in `src/PatternLisp/Codec.hs` that reconstructs closure from Gram structure (basic extraction done, TODO: binding resolution from environment section)
- [ ] T110 [US3] Implement binding collection and deduplication in `closureToPatternSubject` in `src/PatternLisp/Codec.hs` (collect all bindings from all closures)
- [ ] T111 [US3] Implement binding deduplication algorithm (unique (name, value) pairs) in `src/PatternLisp/Codec.hs`
- [ ] T112 [US3] Implement identifier assignment for bindings in `src/PatternLisp/Codec.hs` (assign Gram identifiers like `b1`, `b2`, etc.)
- [ ] T113 [US3] Implement environment section construction in `programToGram` in `src/PatternLisp/Codec.hs` (create `[:Environment | ...]` section)
- [ ] T114 [US3] Implement parameter extraction from closure in `closureToPatternSubject` in `src/PatternLisp/Codec.hs` (extract params to `[:Parameters | ...]`)
- [ ] T115 [US3] Implement body serialization in `closureToPatternSubject` in `src/PatternLisp/Codec.hs` (convert body Expr to Pattern Subject)
- [ ] T116 [US3] Implement bound value reference resolution in `patternSubjectToClosure` in `src/PatternLisp/Codec.hs` (resolve identifiers to bindings)
- [ ] T117 [US3] Implement recursive closure support in `closureToPatternSubject` in `src/PatternLisp/Codec.hs` (handle forward references)

#### Expression Serialization (for Closure Bodies)

- [ ] T118 [US3] Implement `exprToPatternSubject :: Expr -> Pattern Subject` for all Expr forms in `src/PatternLisp/Codec.hs`
- [ ] T119 [US3] Implement `patternSubjectToExpr :: Pattern Subject -> Either Error Expr` for all Expr forms in `src/PatternLisp/Codec.hs`
- [ ] T120 [US3] Implement special form labels in `exprToPatternSubject` in `src/PatternLisp/Codec.hs` (`:If`, `:Let`, `:Begin`, `:Define`, `:Quote`)
- [ ] T121 [US3] Implement special form recognition in `patternSubjectToExpr` in `src/PatternLisp/Codec.hs` (deserialize labels back to special forms)
- [ ] T122 [US3] Implement parameter reference handling in `exprToPatternSubject` in `src/PatternLisp/Codec.hs` (parameters referenced directly, not as identifiers)
- [ ] T123 [US3] Implement bound value reference handling in `exprToPatternSubject` in `src/PatternLisp/Codec.hs` (bound values as identifiers)

#### Program Structure Serialization

- [ ] T124 [US3] Implement file-level property record creation in `programToGram` in `src/PatternLisp/Codec.hs` (`{kind: "Pattern Lisp", ...}`)
- [ ] T125 [US3] Implement optional environment section in `programToGram` in `src/PatternLisp/Codec.hs` (omit if empty)
- [ ] T126 [US3] Implement expressions as file sequence in `programToGram` in `src/PatternLisp/Codec.hs` (whitespace delimited patterns)
- [ ] T127 [US3] Implement file-level property record parsing in `gramToProgram` in `src/PatternLisp/Codec.hs`
- [ ] T128 [US3] Implement environment section parsing in `gramToProgram` in `src/PatternLisp/Codec.hs` (optional, extract bindings)
- [ ] T129 [US3] Implement expressions parsing in `gramToProgram` in `src/PatternLisp/Codec.hs` (remaining patterns in file)

#### Standard Library Filtering

- [ ] T130 [US3] Implement standard library binding filter in `programToGram` in `src/PatternLisp/Codec.hs` (exclude standard library bindings from Environment section)
- [ ] T131 [US3] Implement standard library merge in `gramToProgram` in `src/PatternLisp/Codec.hs` (merge serialized bindings with standard library)
- [ ] T132 [US3] Add primitive registry lookup in `patternSubjectToValue` for VPrimitive deserialization in `src/PatternLisp/Codec.hs`
- [ ] T133 [US3] Add error handling for missing primitives in `src/PatternLisp/Codec.hs`

#### Error Handling and Edge Cases

- [ ] T134 [US3] Add error handling for invalid Pattern Subject structures in `src/PatternLisp/Codec.hs`
- [ ] T135 [US3] Add error handling for malformed Gram notation in `src/PatternLisp/Codec.hs`
- [ ] T136 [US3] Add error handling for circular references in binding graph in `src/PatternLisp/Codec.hs`
- [ ] T137 [US3] Document serialization format in `src/PatternLisp/Codec.hs` module header referencing design document
- [ ] T138 [US3] Document binding deduplication semantics in `src/PatternLisp/Codec.hs`
- [ ] T139 [US3] Document parameters vs bound values distinction in `src/PatternLisp/Codec.hs`
- [ ] T140 [US3] Verify all serialization tests pass: `cabal test GramSerializationSpec Properties`

**Checkpoint**: At this point, User Stories 1, 2, AND 3 should all work independently. Complete serialization enables code-as-data capabilities.

---

## Phase 6: User Story 4 - Pattern Primitives for State Operations (Priority: P2)

**Goal**: Developers need primitive operations to construct, query, and transform Pattern values, enabling programs to read from and write to state effectively.

**Independent Test**: Can be fully tested by using pattern construction primitives (pattern, pattern-with), query primitives (pattern-value, pattern-elements, pattern-size), and predicate primitives (pattern-find, pattern-any?) to manipulate state. Delivers the ability to read and transform state structures.

### Tests for User Story 4

> **NOTE: Write these tests FIRST, ensure they FAIL before implementation**

- [X] T141 [P] [US4] Add test "pattern-find finds matching subpattern" in `test/PatternLisp/PatternSpec.hs`
- [X] T142 [P] [US4] Add test "pattern-find returns nothing if no match" in `test/PatternLisp/PatternSpec.hs`
- [X] T143 [P] [US4] Add test "pattern-any? checks existence correctly" in `test/PatternLisp/PatternSpec.hs`
- [X] T144 [P] [US4] Add test "pattern-all? checks universal property correctly" in `test/PatternLisp/PatternSpec.hs`
- [X] T145 [P] [US4] Add test "pattern predicates work with closures" in `test/PatternLisp/PatternSpec.hs`
- [X] T146 [P] [US4] Add test "pattern-find type error for non-closure predicate" in `test/PatternLisp/PatternSpec.hs`

### Implementation for User Story 4

- [X] T147 [US4] Implement `evalPatternFind :: Pattern Subject -> Value -> EvalM Value` in `src/PatternLisp/Eval.hs`
- [X] T148 [US4] Implement `evalPatternAny :: Pattern Subject -> Value -> EvalM Value` in `src/PatternLisp/Eval.hs`
- [X] T149 [US4] Implement `evalPatternAll :: Pattern Subject -> Value -> EvalM Value` in `src/PatternLisp/Eval.hs`
- [X] T150 [US4] Add `applyPrimitive PatternFind` case in `src/PatternLisp/Eval.hs` with type checking
- [X] T151 [US4] Add `applyPrimitive PatternAny` case in `src/PatternLisp/Eval.hs` with type checking
- [X] T152 [US4] Add `applyPrimitive PatternAll` case in `src/PatternLisp/Eval.hs` with type checking
- [X] T153 [US4] Add pattern predicate primitives to `initialEnv` in `src/PatternLisp/Primitives.hs`
- [X] T154 [US4] Add arity checking for pattern predicate primitives in `src/PatternLisp/Eval.hs`
- [X] T155 [US4] Add error handling for non-closure predicates in `src/PatternLisp/Eval.hs`
- [X] T156 [US4] Verify all pattern predicate tests pass: `cabal test PatternSpec`

**Checkpoint**: At this point, User Stories 1, 2, 3, AND 4 should all work independently. Full pattern primitive support enables complex state transformations.

---

## Phase 7: Polish & Cross-Cutting Concerns

**Purpose**: Documentation, examples, and final integration polish

- [ ] T157 [P] Create `examples/basic-patterns.plisp` with pattern construction examples
- [ ] T158 [P] Create `examples/hello-tool.plisp` with simple tool definition
- [ ] T159 [P] Create `examples/state-reader.plisp` with state querying tool
- [ ] T160 [P] Create `examples/filter-tool.plisp` with pattern predicate usage
- [ ] T161 [P] Create `examples/composition.plisp` with tool composition examples
- [ ] T162 [P] Create `examples/closure-in-state.plisp` demonstrating code-as-data
- [ ] T163 [P] Create `examples/serialization-demo.plisp` demonstrating Gram serialization
- [ ] T164 [P] Create `examples/recursive-closures.plisp` demonstrating recursive closure serialization
- [ ] T165 [P] Update `examples/README.md` with overview and usage instructions
- [ ] T166 [P] Update `README.md` with Pattern Lisp serialization design overview
- [ ] T167 [P] Add section on Gram serialization format to `README.md`
- [ ] T168 [P] Add examples of closure serialization to `README.md`
- [ ] T169 [P] Add examples of program structure (file-level metadata, environment section) to `README.md`
- [ ] T170 [P] Create `docs/gram-serialization-reference.md` with comprehensive Gram format reference
- [ ] T171 [P] Create `docs/tool-development.md` with tool development guide
- [ ] T172 [P] Update `docs/plisp-serialization-design.md` with implementation notes if needed
- [ ] T173 Code cleanup and refactoring across all modules
- [ ] T174 Run quickstart.md validation: verify all examples work
- [ ] T175 Run full test suite: `cabal test`
- [ ] T176 Verify no regressions in existing functionality

---

## Dependencies & Execution Order

### Phase Dependencies

- **Setup (Phase 1)**: No dependencies - can start immediately
- **Foundational (Phase 2)**: Depends on Setup completion - BLOCKS all user stories
- **User Stories (Phase 3-6)**: All depend on Foundational phase completion
  - User stories can then proceed in parallel (if staffed)
  - Or sequentially in priority order (P1 → P2 → P3)
- **Polish (Phase 8)**: Depends on all desired user stories being complete

### User Story Dependencies

- **User Story 1 (P1)**: Can start after Foundational (Phase 2) - No dependencies on other stories
- **User Story 2 (P1)**: Can start after Foundational (Phase 2) - Depends on US1 for Pattern values
- **User Story 3 (P2)**: Can start after Foundational (Phase 2) - Depends on US1 for Pattern serialization, US2 for closure serialization
- **User Story 4 (P2)**: Can start after Foundational (Phase 2) - Depends on US1 for basic pattern operations

### Within Each User Story

- Tests (if included) MUST be written and FAIL before implementation
- Core implementation before integration
- Story complete before moving to next priority

### Parallel Opportunities

- All Setup tasks marked [P] can run in parallel (T002-T006)
- All Foundational tasks marked [P] can run in parallel (within Phase 2)
- Once Foundational phase completes, user stories can start in parallel (if team capacity allows)
- All tests for a user story marked [P] can run in parallel
- Different user stories can be worked on in parallel by different team members
- All Polish tasks marked [P] can run in parallel

---

## Implementation Strategy

### MVP First (User Stories 1 & 2 Only)

1. Complete Phase 1: Setup
2. Complete Phase 2: Foundational (CRITICAL - blocks all stories)
3. Complete Phase 3: User Story 1 (Pattern as First-Class Value)
4. Complete Phase 4: User Story 2 (Pure Function State Transformation)
5. **STOP and VALIDATE**: Test User Stories 1 & 2 independently
6. Deploy/demo if ready

### Incremental Delivery

1. Complete Setup + Foundational → Foundation ready
2. Add User Story 1 → Test independently → Deploy/Demo (Basic Pattern Support)
3. Add User Story 2 → Test independently → Deploy/Demo (Tool Execution)
4. Add User Story 3 → Test independently → Deploy/Demo (Gram Serialization)
5. Add User Story 4 → Test independently → Deploy/Demo (Full Pattern Primitives)
6. Each story adds value without breaking previous stories

### Parallel Team Strategy

With multiple developers:

1. Team completes Setup + Foundational together
2. Once Foundational is done:
   - Developer A: User Story 1 (Pattern as First-Class)
   - Developer B: User Story 2 (Tool Execution) - after US1
   - Developer C: User Story 3 (Gram Serialization) - can start in parallel with US1/US2
   - Developer D: User Story 4 (Pattern Primitives) - after US1

---

## Notes

- [P] tasks = different files, no dependencies
- [Story] label maps task to specific user story for traceability
- Each user story should be independently completable and testable
- Verify tests fail before implementing
- Commit after each task or logical group
- Stop at any checkpoint to validate story independently
- Avoid: vague tasks, same file conflicts, cross-story dependencies that break independence
- CLI state initialization uses Option F (Convention-Based Auto-Loading) - files provided as CLI arguments
- All values must be serializable to Gram notation (including closures and primitives) for code-as-data capabilities
- Serialization follows design in `docs/plisp-serialization-design.md`:
  - File-level property records for metadata
  - Optional Environment section for shared bindings
  - Expressions as patterns in file sequence
  - Closure structure: `[:Closure | [:Env | ...], [:Lambda | [:Parameters | ...], [:Body | ...]]]`
  - Special form labels: `:If`, `:Let`, `:Begin`, `:Define`, `:Quote`
  - Parameters vs bound values distinction
  - Binding deduplication by (name, value) pairs
  - Recursive closure support via forward references
  - Standard library filtering
