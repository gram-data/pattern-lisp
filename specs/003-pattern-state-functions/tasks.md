# Tasks: Pattern State Functions

**Input**: Design documents from `/specs/003-pattern-state-functions/`
**Prerequisites**: plan.md, spec.md, research.md, data-model.md, contracts/

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

**Purpose**: Project initialization, module reorganization, and dependency setup

- [X] T001 Create `src/PatternLisp/` directory structure
- [X] T002 [P] Move `src/Lisp/Syntax.hs` to `src/PatternLisp/Syntax.hs` and update module declaration
- [X] T003 [P] Move `src/Lisp/Parser.hs` to `src/PatternLisp/Parser.hs` and update module declaration
- [X] T004 [P] Move `src/Lisp/Eval.hs` to `src/PatternLisp/Eval.hs` and update module declaration
- [X] T005 [P] Move `src/Lisp/Primitives.hs` to `src/PatternLisp/Primitives.hs` and update module declaration
- [X] T006 [P] Move test files from `test/Lisp/` to `test/PatternLisp/` and update module declarations
- [X] T007 Update all imports across codebase to use `PatternLisp.*` namespace
- [X] T008 Update `pattern-lisp.cabal` exposed-modules to reflect new module structure
- [X] T009 Update `app/Main.hs` imports to use `PatternLisp.*` modules
- [X] T010 Add gram-data dependency to `pattern-lisp.cabal` build-depends section (commented out for now, will be enabled in Phase 2)
- [X] T011 Verify build: `cabal build all`
- [X] T012 Verify existing tests still pass: `cabal test`

**Checkpoint**: Module reorganization complete, project builds successfully

---

## Phase 2: Foundational (Blocking Prerequisites)

**Purpose**: Core type extensions and basic pattern support that ALL user stories depend on

**⚠️ CRITICAL**: No user story work can begin until this phase is complete

- [ ] T013 Import `Gram.Subject (Subject)` and `Pattern.Core (Pattern)` in `src/PatternLisp/Syntax.hs`
- [ ] T014 Add `VPattern (Pattern Subject)` constructor to `Value` type in `src/PatternLisp/Syntax.hs`
- [ ] T015 Add pattern primitive constructors to `Primitive` type in `src/PatternLisp/Syntax.hs` (PatternCreate, PatternWith, PatternValue, PatternElements, PatternLength, PatternSize, PatternDepth, PatternValues, PatternFind, PatternAny, PatternAll)
- [ ] T016 Implement `primitiveName :: Primitive -> String` function in `src/PatternLisp/Syntax.hs`
- [ ] T017 Implement `primitiveFromName :: String -> Maybe Primitive` function in `src/PatternLisp/Syntax.hs`
- [ ] T018 Create `src/PatternLisp/PatternPrimitives.hs` module with module header and exports
- [ ] T019 Implement `evalPatternCreate :: Value -> EvalM Value` in `src/PatternLisp/PatternPrimitives.hs`
- [ ] T020 Implement `evalPatternWith :: Value -> [Value] -> EvalM Value` in `src/PatternLisp/PatternPrimitives.hs`
- [ ] T021 Import `PatternLisp.PatternPrimitives` in `src/PatternLisp/Eval.hs`
- [ ] T022 Add `applyPrimitive PatternCreate` case in `src/PatternLisp/Eval.hs` to call `PatternPrimitives.evalPatternCreate`
- [ ] T023 Add `applyPrimitive PatternWith` case in `src/PatternLisp/Eval.hs` to call `PatternPrimitives.evalPatternWith`
- [ ] T024 Add pattern primitives to `initialEnv` in `src/PatternLisp/Primitives.hs` ("pattern" → PatternCreate, "pattern-with" → PatternWith)
- [ ] T025 Verify types compile: `cabal build`
- [ ] T026 Test basic pattern construction in REPL: `(pattern "hello")` and `(pattern-with "root" (list))`

**Checkpoint**: Foundation ready - Pattern values can be created, basic pattern operations available. User story implementation can now begin.

---

## Phase 3: User Story 1 - Pattern as First-Class Value Type (Priority: P1) 🎯 MVP

**Goal**: Developers can use Pattern values directly in Pattern Lisp programs, where Pattern values are decorated with Subject types to enable complete serialization and code-as-data capabilities.

**Independent Test**: Can be fully tested by creating Pattern values from various data types (numbers, strings, lists) and verifying they can be constructed, queried, and transformed. Delivers the ability to represent and manipulate state as Pattern structures.

### Tests for User Story 1

> **NOTE: Write these tests FIRST, ensure they FAIL before implementation**

- [ ] T027 [P] [US1] Create test file `test/PatternLisp/PatternSpec.hs` with module header and imports
- [ ] T028 [P] [US1] Add test "pattern construction creates atomic pattern" in `test/PatternLisp/PatternSpec.hs`
- [ ] T029 [P] [US1] Add test "pattern-with creates pattern with elements" in `test/PatternLisp/PatternSpec.hs`
- [ ] T030 [P] [US1] Add test "pattern-value extracts decoration correctly" in `test/PatternLisp/PatternSpec.hs`
- [ ] T031 [P] [US1] Add test "pattern-elements returns list of VPattern elements" in `test/PatternLisp/PatternSpec.hs`
- [ ] T032 [P] [US1] Add test "pattern-length returns correct direct element count" in `test/PatternLisp/PatternSpec.hs`
- [ ] T033 [P] [US1] Add test "pattern-size counts all nodes recursively" in `test/PatternLisp/PatternSpec.hs`
- [ ] T034 [P] [US1] Add test "pattern-depth returns max depth correctly" in `test/PatternLisp/PatternSpec.hs`
- [ ] T035 [P] [US1] Add test "pattern-values flattens all values" in `test/PatternLisp/PatternSpec.hs`
- [ ] T036 [P] [US1] Add test "nested patterns work correctly" in `test/PatternLisp/PatternSpec.hs`
- [ ] T037 [P] [US1] Add test "type errors for non-pattern values" in `test/PatternLisp/PatternSpec.hs`
- [ ] T038 [P] [US1] Add test "arity errors for wrong argument counts" in `test/PatternLisp/PatternSpec.hs`

### Implementation for User Story 1

- [ ] T039 [US1] Implement `evalPatternValue :: Pattern Subject -> EvalM Value` in `src/PatternLisp/PatternPrimitives.hs`
- [ ] T040 [US1] Implement `evalPatternElements :: Pattern Subject -> EvalM Value` in `src/PatternLisp/PatternPrimitives.hs`
- [ ] T041 [US1] Implement `evalPatternLength :: Pattern Subject -> EvalM Value` in `src/PatternLisp/PatternPrimitives.hs`
- [ ] T042 [US1] Implement `evalPatternSize :: Pattern Subject -> EvalM Value` in `src/PatternLisp/PatternPrimitives.hs`
- [ ] T043 [US1] Implement `evalPatternDepth :: Pattern Subject -> EvalM Value` in `src/PatternLisp/PatternPrimitives.hs`
- [ ] T044 [US1] Implement `evalPatternValues :: Pattern Subject -> EvalM Value` in `src/PatternLisp/PatternPrimitives.hs`
- [ ] T045 [US1] Add helper function `expectPattern :: Value -> EvalM (Pattern Subject)` in `src/PatternLisp/Eval.hs`
- [ ] T046 [US1] Add `applyPrimitive PatternValue` case in `src/PatternLisp/Eval.hs` with type checking
- [ ] T047 [US1] Add `applyPrimitive PatternElements` case in `src/PatternLisp/Eval.hs` with type checking
- [ ] T048 [US1] Add `applyPrimitive PatternLength` case in `src/PatternLisp/Eval.hs` with type checking
- [ ] T049 [US1] Add `applyPrimitive PatternSize` case in `src/PatternLisp/Eval.hs` with type checking
- [ ] T050 [US1] Add `applyPrimitive PatternDepth` case in `src/PatternLisp/Eval.hs` with type checking
- [ ] T051 [US1] Add `applyPrimitive PatternValues` case in `src/PatternLisp/Eval.hs` with type checking
- [ ] T052 [US1] Add pattern query primitives to `initialEnv` in `src/PatternLisp/Primitives.hs` ("pattern-value", "pattern-elements", "pattern-length", "pattern-size", "pattern-depth", "pattern-values")
- [ ] T053 [US1] Add arity checking for all pattern query primitives in `src/PatternLisp/Eval.hs`
- [ ] T054 [US1] Add type checking error messages for pattern operations in `src/PatternLisp/Eval.hs`
- [ ] T055 [US1] Verify all tests pass: `cabal test PatternSpec`

**Checkpoint**: At this point, User Story 1 should be fully functional and testable independently. Pattern values can be created, queried, and transformed.

---

## Phase 4: User Story 2 - Pure Function State Transformation (Priority: P1) 🎯 MVP

**Goal**: Developers can write Pattern Lisp programs that are pure functions accepting a single state parameter (Pattern Subject) and returning a transformed Pattern Subject, following the canonical form `(lambda (state) ...)`.

**Independent Test**: Can be fully tested by writing a lambda expression with a single "state" parameter, evaluating it with a Pattern Subject input, and verifying it returns a Pattern Subject output. Delivers the ability to define tools as pure state transformations.

### Tests for User Story 2

> **NOTE: Write these tests FIRST, ensure they FAIL before implementation**

- [ ] T056 [P] [US2] Create test file `test/PatternLisp/RuntimeSpec.hs` with module header and imports
- [ ] T057 [P] [US2] Add test "valid tool: lambda with single state parameter" in `test/PatternLisp/RuntimeSpec.hs`
- [ ] T058 [P] [US2] Add test "invalid tool: wrong parameter count" in `test/PatternLisp/RuntimeSpec.hs`
- [ ] T059 [P] [US2] Add test "invalid tool: wrong parameter name" in `test/PatternLisp/RuntimeSpec.hs`
- [ ] T060 [P] [US2] Add test "invalid tool: not a lambda" in `test/PatternLisp/RuntimeSpec.hs`
- [ ] T061 [P] [US2] Add test "tool execution: identity tool returns same state" in `test/PatternLisp/RuntimeSpec.hs`
- [ ] T062 [P] [US2] Add test "tool execution: tool transforms state correctly" in `test/PatternLisp/RuntimeSpec.hs`
- [ ] T063 [P] [US2] Add test "tool execution: tool returns non-pattern should error" in `test/PatternLisp/RuntimeSpec.hs`
- [ ] T064 [P] [US2] Add test "tool composition: sequential tool execution" in `test/PatternLisp/RuntimeSpec.hs`

### Implementation for User Story 2

- [ ] T065 [US2] Create `src/PatternLisp/Runtime.hs` module with module header and exports
- [ ] T066 [US2] Define `RuntimeState` type in `src/PatternLisp/Runtime.hs` with `environment :: Env` and `executionTrace :: [(String, Pattern Subject, Pattern Subject)]`
- [ ] T067 [US2] Implement `validateTool :: Expr -> Env -> Either Error Value` in `src/PatternLisp/Runtime.hs` to check canonical form
- [ ] T068 [US2] Implement `executeTool :: Value -> Pattern Subject -> Env -> Either Error (Pattern Subject)` in `src/PatternLisp/Runtime.hs`
- [ ] T069 [US2] Add clear error messages for tool validation failures in `src/PatternLisp/Runtime.hs`
- [ ] T070 [US2] Add error handling for non-Pattern tool results in `src/PatternLisp/Runtime.hs`
- [ ] T071 [US2] Verify tool validation tests pass: `cabal test RuntimeSpec`

**Checkpoint**: At this point, User Stories 1 AND 2 should both work independently. Tools can be validated and executed with state transformations.

---

## Phase 5: User Story 3 - Complete Value Serialization (Priority: P2)

**Goal**: All Pattern Lisp values (including closures and primitives) are serializable to Subject representation, enabling code-as-data, persistence, and complete introspection of program state.

**Independent Test**: Can be fully tested by converting any Pattern Lisp value (number, string, list, Pattern, closure, primitive) to Subject representation and back, verifying the round-trip preserves functionality. Delivers the ability to serialize and deserialize all program state including code.

### Tests for User Story 3

> **NOTE: Write these tests FIRST, ensure they FAIL before implementation**

- [ ] T072 [P] [US3] Create test file `test/PatternLisp/SubjectSpec.hs` with module header and imports
- [ ] T073 [P] [US3] Add test "round-trip numbers" in `test/PatternLisp/SubjectSpec.hs`
- [ ] T074 [P] [US3] Add test "round-trip strings" in `test/PatternLisp/SubjectSpec.hs`
- [ ] T075 [P] [US3] Add test "round-trip booleans" in `test/PatternLisp/SubjectSpec.hs`
- [ ] T076 [P] [US3] Add test "round-trip lists" in `test/PatternLisp/SubjectSpec.hs`
- [ ] T077 [P] [US3] Add test "round-trip patterns" in `test/PatternLisp/SubjectSpec.hs`
- [ ] T078 [P] [US3] Add test "round-trip simple closure" in `test/PatternLisp/SubjectSpec.hs`
- [ ] T079 [P] [US3] Add test "round-trip closure with captured environment" in `test/PatternLisp/SubjectSpec.hs`
- [ ] T080 [P] [US3] Add test "round-trip nested closures" in `test/PatternLisp/SubjectSpec.hs`
- [ ] T081 [P] [US3] Add test "round-trip primitives" in `test/PatternLisp/SubjectSpec.hs`
- [ ] T082 [P] [US3] Add test "closure remains executable after round-trip" in `test/PatternLisp/SubjectSpec.hs`
- [ ] T083 [P] [US3] Add test "primitive remains functional after round-trip" in `test/PatternLisp/SubjectSpec.hs`
- [ ] T084 [P] [US3] Add test "pattern containing closures round-trips" in `test/PatternLisp/SubjectSpec.hs`
- [ ] T085 [P] [US3] Add test "exprToSubject and subjectToExpr round-trip" in `test/PatternLisp/SubjectSpec.hs`
- [ ] T086 [P] [US3] Add test "variable names use property-based representation" in `test/PatternLisp/SubjectSpec.hs`
- [ ] T087 [P] [US3] Add test "missing primitive in registry errors correctly" in `test/PatternLisp/SubjectSpec.hs`
- [ ] T088 [P] [US3] Create property-based test file `test/Properties.hs` for serialization round-trips
- [ ] T089 [P] [US3] Add QuickCheck property "all values serialize and deserialize correctly" in `test/Properties.hs`

### Implementation for User Story 3

- [ ] T090 [US3] Create `src/PatternLisp/Subject.hs` module with module header and exports
- [ ] T091 [US3] Implement `valueToSubject :: Value -> Subject` for VNumber in `src/PatternLisp/Subject.hs`
- [ ] T092 [US3] Implement `valueToSubject :: Value -> Subject` for VString in `src/PatternLisp/Subject.hs`
- [ ] T093 [US3] Implement `valueToSubject :: Value -> Subject` for VBool in `src/PatternLisp/Subject.hs`
- [ ] T094 [US3] Implement `valueToSubject :: Value -> Subject` for VList in `src/PatternLisp/Subject.hs`
- [ ] T095 [US3] Implement `valueToSubject :: Value -> Subject` for VPattern in `src/PatternLisp/Subject.hs`
- [ ] T096 [US3] Implement `valueToSubject :: Value -> Subject` for VClosure in `src/PatternLisp/Subject.hs` (with params, env, body)
- [ ] T097 [US3] Implement `valueToSubject :: Value -> Subject` for VPrimitive in `src/PatternLisp/Subject.hs` (by name)
- [ ] T098 [US3] Implement `subjectToValue :: Subject -> Either Error Value` for all value types in `src/PatternLisp/Subject.hs`
- [ ] T099 [US3] Implement environment serialization in closure serialization (envSubject with bindings) in `src/PatternLisp/Subject.hs`
- [ ] T100 [US3] Implement `exprToSubject :: Expr -> Subject` for all Expr forms in `src/PatternLisp/Subject.hs`
- [ ] T101 [US3] Implement `subjectToExpr :: Subject -> Either Error Expr` for all Expr forms in `src/PatternLisp/Subject.hs`
- [ ] T102 [US3] Add primitive registry lookup in `subjectToValue` for VPrimitive deserialization in `src/PatternLisp/Subject.hs`
- [ ] T103 [US3] Add error handling for missing primitives in `src/PatternLisp/Subject.hs`
- [ ] T104 [US3] Add error handling for invalid Subject structures in `src/PatternLisp/Subject.hs`
- [ ] T105 [US3] Document serialization format in `src/PatternLisp/Subject.hs` module header
- [ ] T106 [US3] Document variable name property-based representation in `src/PatternLisp/Subject.hs`
- [ ] T107 [US3] Verify all serialization tests pass: `cabal test SubjectSpec Properties`

**Checkpoint**: At this point, User Stories 1, 2, AND 3 should all work independently. Complete serialization enables code-as-data capabilities.

---

## Phase 6: User Story 4 - Pattern Primitives for State Operations (Priority: P2)

**Goal**: Developers need primitive operations to construct, query, and transform Pattern values, enabling programs to read from and write to state effectively.

**Independent Test**: Can be fully tested by using pattern construction primitives (pattern, pattern-with), query primitives (pattern-value, pattern-elements, pattern-size), and predicate primitives (pattern-find, pattern-any?) to manipulate state. Delivers the ability to read and transform state structures.

### Tests for User Story 4

> **NOTE: Write these tests FIRST, ensure they FAIL before implementation**

- [ ] T108 [P] [US4] Add test "pattern-find finds matching subpattern" in `test/PatternLisp/PatternSpec.hs`
- [ ] T109 [P] [US4] Add test "pattern-find returns nothing if no match" in `test/PatternLisp/PatternSpec.hs`
- [ ] T110 [P] [US4] Add test "pattern-any? checks existence correctly" in `test/PatternLisp/PatternSpec.hs`
- [ ] T111 [P] [US4] Add test "pattern-all? checks universal property correctly" in `test/PatternLisp/PatternSpec.hs`
- [ ] T112 [P] [US4] Add test "pattern predicates work with closures" in `test/PatternLisp/PatternSpec.hs`
- [ ] T113 [P] [US4] Add test "pattern-find type error for non-closure predicate" in `test/PatternLisp/PatternSpec.hs`

### Implementation for User Story 4

- [ ] T114 [US4] Implement `evalPatternFind :: Pattern Subject -> Value -> EvalM Value` in `src/PatternLisp/PatternPrimitives.hs`
- [ ] T115 [US4] Implement `evalPatternAny :: Pattern Subject -> Value -> EvalM Value` in `src/PatternLisp/PatternPrimitives.hs`
- [ ] T116 [US4] Implement `evalPatternAll :: Pattern Subject -> Value -> EvalM Value` in `src/PatternLisp/PatternPrimitives.hs`
- [ ] T117 [US4] Add `applyPrimitive PatternFind` case in `src/PatternLisp/Eval.hs` with type checking
- [ ] T118 [US4] Add `applyPrimitive PatternAny` case in `src/PatternLisp/Eval.hs` with type checking
- [ ] T119 [US4] Add `applyPrimitive PatternAll` case in `src/PatternLisp/Eval.hs` with type checking
- [ ] T120 [US4] Add pattern predicate primitives to `initialEnv` in `src/PatternLisp/Primitives.hs` ("pattern-find", "pattern-any?", "pattern-all?")
- [ ] T121 [US4] Add arity checking for pattern predicate primitives in `src/PatternLisp/Eval.hs`
- [ ] T122 [US4] Add error handling for non-closure predicates in `src/PatternLisp/Eval.hs`
- [ ] T123 [US4] Verify all pattern predicate tests pass: `cabal test PatternSpec`

**Checkpoint**: At this point, User Stories 1, 2, 3, AND 4 should all work independently. Full pattern primitive support enables complex state transformations.

---

## Phase 7: User Story 5 - Runtime State Management (Priority: P3)

**Goal**: Developers need a runtime system that manages Pattern Subject state, validates tool definitions, executes tools, and maintains execution traces for debugging and replay.

**Independent Test**: Can be fully tested by creating a runtime with initial state, loading tool definitions, executing tools sequentially, and verifying state transitions and execution traces are recorded correctly. Delivers the ability to run tools in a managed environment with state persistence.

### Tests for User Story 5

> **NOTE: Write these tests FIRST, ensure they FAIL before implementation**

- [ ] T124 [P] [US5] Add test "loadPlispFile loads function from file" in `test/PatternLisp/RuntimeSpec.hs`
- [ ] T125 [P] [US5] Add test "loadGramFile loads state from file" in `test/PatternLisp/RuntimeSpec.hs`
- [ ] T126 [P] [US5] Add test "processFiles handles .gram files first, then .plisp" in `test/PatternLisp/RuntimeSpec.hs`
- [ ] T127 [P] [US5] Add test "filename-to-identifier conversion works correctly" in `test/PatternLisp/RuntimeSpec.hs`
- [ ] T128 [P] [US5] Add test "execution trace records state transformations" in `test/PatternLisp/RuntimeSpec.hs`
- [ ] T129 [P] [US5] Add test "serializeRuntime converts runtime to Subject" in `test/PatternLisp/RuntimeSpec.hs`
- [ ] T130 [P] [US5] Add test "deserializeRuntime reconstructs runtime from Subject" in `test/PatternLisp/RuntimeSpec.hs`
- [ ] T131 [P] [US5] Add test "runtime round-trip preserves functionality" in `test/PatternLisp/RuntimeSpec.hs`

### Implementation for User Story 5

- [ ] T132 [US5] Implement `loadPlispFile :: FilePath -> Env -> IO (Either Error Env)` in `src/PatternLisp/Runtime.hs`
- [ ] T133 [US5] Implement `loadGramFile :: FilePath -> IO (Either Error (String, Pattern Subject))` in `src/PatternLisp/Runtime.hs`
- [ ] T134 [US5] Implement `processFiles :: [FilePath] -> Env -> IO (Either Error Env)` in `src/PatternLisp/Runtime.hs` (process .gram first, then .plisp)
- [ ] T135 [US5] Implement filename-to-identifier conversion (remove extension) in `src/PatternLisp/Runtime.hs`
- [ ] T136 [US5] Implement lambda naming from filename in `loadPlispFile` in `src/PatternLisp/Runtime.hs`
- [ ] T137 [US5] Implement `serializeRuntime :: RuntimeState -> Subject` in `src/PatternLisp/Runtime.hs`
- [ ] T138 [US5] Implement `deserializeRuntime :: Subject -> Either Error RuntimeState` in `src/PatternLisp/Runtime.hs`
- [ ] T139 [US5] Add error handling for file read errors in `src/PatternLisp/Runtime.hs`
- [ ] T140 [US5] Add error handling for duplicate identifiers in `src/PatternLisp/Runtime.hs`
- [ ] T141 [US5] Import `PatternLisp.Runtime` in `app/Main.hs`
- [ ] T142 [US5] Update CLI argument parsing in `app/Main.hs` to process all non-flag arguments as files
- [ ] T143 [US5] Implement file loading on REPL startup in `app/Main.hs` using `processFiles`
- [ ] T144 [US5] Add REPL command `:reload` to re-process files in `app/Main.hs`
- [ ] T145 [US5] Add REPL command `:env` to display environment in `app/Main.hs`
- [ ] T146 [US5] Add REPL command `:state` to display current state in `app/Main.hs` (if using single-state model)
- [ ] T147 [US5] Add REPL command `:trace` to show execution trace in `app/Main.hs`
- [ ] T148 [US5] Add REPL command `:save <file>` to persist runtime in `app/Main.hs`
- [ ] T149 [US5] Add REPL command `:restore <file>` to load runtime in `app/Main.hs`
- [ ] T150 [US5] Implement non-interactive mode with `-e/--eval` flag in `app/Main.hs`
- [ ] T151 [US5] Implement stdin evaluation support in `app/Main.hs`
- [ ] T152 [US5] Update help text (`:help`) with all new commands in `app/Main.hs`
- [ ] T153 [US5] Implement pretty-printing for Pattern values in `app/Main.hs`
- [ ] T154 [US5] Verify all runtime tests pass: `cabal test RuntimeSpec`

**Checkpoint**: At this point, all user stories should be independently functional. Runtime system provides complete tool execution and state management.

---

## Phase 8: Polish & Cross-Cutting Concerns

**Purpose**: Documentation, examples, and final integration polish

- [ ] T155 [P] Create `examples/basic-patterns.plisp` with pattern construction examples
- [ ] T156 [P] Create `examples/hello-tool.plisp` with simple tool definition
- [ ] T157 [P] Create `examples/state-reader.plisp` with state querying tool
- [ ] T158 [P] Create `examples/filter-tool.plisp` with pattern predicate usage
- [ ] T159 [P] Create `examples/composition.plisp` with tool composition examples
- [ ] T160 [P] Create `examples/closure-in-state.plisp` demonstrating code-as-data
- [ ] T161 [P] Update `examples/README.md` with overview and usage instructions
- [ ] T162 [P] Update `README.md` with Pattern State design overview
- [ ] T163 [P] Add section on `Pattern Subject -> Pattern Subject` canonical form to `README.md`
- [ ] T164 [P] Add examples of pattern operations to `README.md`
- [ ] T165 [P] Add examples of tool definitions to `README.md`
- [ ] T166 [P] Add closure serialization example to `README.md`
- [ ] T167 [P] Add code-as-data example to `README.md`
- [ ] T168 [P] Create `docs/pattern-primitives.md` with comprehensive primitive reference
- [ ] T169 [P] Create `docs/tool-development.md` with tool development guide
- [ ] T170 [P] Create `docs/serialization.md` with serialization format documentation
- [ ] T171 [P] Update `docs/pattern-state-lisp-design.md` with implementation details
- [ ] T172 Code cleanup and refactoring across all modules
- [ ] T173 Run quickstart.md validation: verify all examples work
- [ ] T174 Run full test suite: `cabal test`
- [ ] T175 Verify no regressions in existing functionality

---

## Dependencies & Execution Order

### Phase Dependencies

- **Setup (Phase 1)**: No dependencies - can start immediately
- **Foundational (Phase 2)**: Depends on Setup completion - BLOCKS all user stories
- **User Stories (Phase 3-7)**: All depend on Foundational phase completion
  - User stories can then proceed in parallel (if staffed)
  - Or sequentially in priority order (P1 → P2 → P3)
- **Polish (Phase 8)**: Depends on all desired user stories being complete

### User Story Dependencies

- **User Story 1 (P1)**: Can start after Foundational (Phase 2) - No dependencies on other stories
- **User Story 2 (P1)**: Can start after Foundational (Phase 2) - Depends on US1 for Pattern values
- **User Story 3 (P2)**: Can start after Foundational (Phase 2) - Depends on US1 for Pattern serialization
- **User Story 4 (P2)**: Can start after Foundational (Phase 2) - Depends on US1 for basic pattern operations
- **User Story 5 (P3)**: Can start after Foundational (Phase 2) - Depends on US2 for tool execution, US3 for serialization

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

## Parallel Example: User Story 1

```bash
# Launch all tests for User Story 1 together:
Task T027: "Create test file test/PatternLisp/PatternSpec.hs"
Task T028: "Add test 'pattern construction creates atomic pattern'"
Task T029: "Add test 'pattern-with creates pattern with elements'"
Task T030: "Add test 'pattern-value extracts decoration correctly'"
Task T031: "Add test 'pattern-elements returns list of VPattern elements'"
Task T032: "Add test 'pattern-length returns correct direct element count'"
Task T033: "Add test 'pattern-size counts all nodes recursively'"
Task T034: "Add test 'pattern-depth returns max depth correctly'"
Task T035: "Add test 'pattern-values flattens all values'"
Task T036: "Add test 'nested patterns work correctly'"
Task T037: "Add test 'type errors for non-pattern values'"
Task T038: "Add test 'arity errors for wrong argument counts'"
```

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
4. Add User Story 3 → Test independently → Deploy/Demo (Serialization)
5. Add User Story 4 → Test independently → Deploy/Demo (Full Pattern Primitives)
6. Add User Story 5 → Test independently → Deploy/Demo (Runtime Management)
7. Each story adds value without breaking previous stories

### Parallel Team Strategy

With multiple developers:

1. Team completes Setup + Foundational together
2. Once Foundational is done:
   - Developer A: User Story 1 (Pattern as First-Class)
   - Developer B: User Story 2 (Tool Execution) - after US1
   - Developer C: User Story 3 (Serialization) - can start in parallel
   - Developer D: User Story 4 (Pattern Primitives) - after US1
   - Developer E: User Story 5 (Runtime) - after US2 and US3

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
- All values must be serializable (including closures and primitives) for code-as-data capabilities

