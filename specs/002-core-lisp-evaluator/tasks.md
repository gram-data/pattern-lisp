# Tasks: Core Lisp Evaluator

**Input**: Design documents from `/specs/002-core-lisp-evaluator/`
**Prerequisites**: plan.md (required), spec.md (required for user stories), research.md, data-model.md, contracts/

**Tests**: Tests are REQUIRED per constitution (Test-First principle). All test tasks must be written FIRST and must FAIL before implementation.

**Organization**: Tasks are grouped by user story to enable independent implementation and testing of each story.

## Format: `[ID] [P?] [Story] Description`

- **[P]**: Can run in parallel (different files, no dependencies)
- **[Story]**: Which user story this task belongs to (e.g., US1, US2, US3)
- Include exact file paths in descriptions

## Path Conventions

- **Single project**: `src/`, `test/` at repository root
- Paths follow Cabal conventions: `src/Lisp/`, `app/`, `test/Lisp/`

---

## Phase 1: Setup (Shared Infrastructure)

**Purpose**: Project structure verification and test framework setup

- [x] T001 Verify project structure exists (src/Lisp/, app/, test/, examples/)
- [x] T002 [P] Verify test framework configuration in pattern-lisp.cabal (hspec, QuickCheck)
- [x] T003 [P] Create test directory structure: test/Lisp/ directory

---

## Phase 2: Foundational (Blocking Prerequisites)

**Purpose**: Core data structures that MUST be complete before ANY user story can be implemented

**⚠️ CRITICAL**: No user story work can begin until this phase is complete

- [x] T004 Create Expr data type in src/Lisp/Syntax.hs (Atom, List, Quote constructors)
- [x] T005 [P] Create Atom data type in src/Lisp/Syntax.hs (Symbol, Number, String, Bool constructors)
- [x] T006 [P] Create Value data type in src/Lisp/Syntax.hs (VNumber, VString, VBool, VList, VClosure, VPrimitive constructors)
- [x] T007 [P] Create Closure data type in src/Lisp/Syntax.hs (params, body, env fields)
- [x] T008 [P] Create Primitive data type in src/Lisp/Syntax.hs (Add, Sub, Mul, Div, Gt, Lt, Eq, Ne, StringAppend, StringLength, Substring)
- [x] T009 Create Env type alias in src/Lisp/Syntax.hs (Map String Value)
- [x] T010 [P] Create Error data type in src/Lisp/Syntax.hs (UndefinedVar, TypeMismatch, ArityMismatch, DivisionByZero, ParseError constructors)
- [x] T011 [P] Add Show instances for Expr, Atom, Value, Closure, Primitive, Error in src/Lisp/Syntax.hs
- [x] T012 [P] Add Eq instances for Expr, Atom, Value, Closure, Primitive in src/Lisp/Syntax.hs
- [x] T013 Export all data types and types from Lisp.Syntax module in src/Lisp/Syntax.hs

**Checkpoint**: Foundation ready - all core data types defined. User story implementation can now begin.

---

## Phase 3: User Story 1 - Parse S-Expressions (Priority: P1) 🎯 MVP

**Goal**: Parse S-expression strings into abstract syntax tree representation that can be evaluated.

**Independent Test**: Provide various S-expression strings and verify they parse correctly into expected AST structure. Can be tested independently without evaluation.

### Tests for User Story 1 ⚠️

> **NOTE: Write these tests FIRST, ensure they FAIL before implementation**

- [x] T014 [P] [US1] Create test file test/Lisp/ParserSpec.hs with hspec structure
- [x] T015 [P] [US1] Add test for parsing simple S-expression "(+ 1 2)" in test/Lisp/ParserSpec.hs
- [x] T016 [P] [US1] Add test for parsing nested S-expression "(if (> x 0) (+ x 1) (- x 1))" in test/Lisp/ParserSpec.hs
- [x] T017 [P] [US1] Add test for parsing quoted expression "(quote (a b c))" in test/Lisp/ParserSpec.hs
- [x] T018 [P] [US1] Add test for parsing invalid S-expression with error message in test/Lisp/ParserSpec.hs
- [x] T019 [P] [US1] Add test for parsing empty list "()" in test/Lisp/ParserSpec.hs
- [x] T020 [P] [US1] Add test for parsing single-element list "(x)" in test/Lisp/ParserSpec.hs
- [x] T021 [P] [US1] Add test for parsing numbers (positive, negative, zero) in test/Lisp/ParserSpec.hs
- [x] T022 [P] [US1] Add test for parsing strings with quotes and escapes in test/Lisp/ParserSpec.hs
- [x] T023 [P] [US1] Add test for parsing symbols (valid identifiers) in test/Lisp/ParserSpec.hs
- [x] T024 [P] [US1] Add test for parsing booleans (#t, #f) in test/Lisp/ParserSpec.hs
- [x] T025 [US1] Update test/Spec.hs to include ParserSpec tests

### Implementation for User Story 1

- [x] T026 [US1] Create Lisp.Parser module in src/Lisp/Parser.hs with parseExpr function signature
- [x] T027 [US1] Implement atom parser (symbol, number, string, bool) in src/Lisp/Parser.hs
- [x] T028 [US1] Implement list parser (parentheses, whitespace handling) in src/Lisp/Parser.hs
- [x] T029 [US1] Implement quote parser (quote form, single quote syntax) in src/Lisp/Parser.hs
- [x] T030 [US1] Implement main expr parser combining atom, list, quote in src/Lisp/Parser.hs
- [x] T031 [US1] Add error handling with position information in src/Lisp/Parser.hs
- [x] T032 [US1] Export parseExpr function from Lisp.Parser module in src/Lisp/Parser.hs
- [x] T033 [US1] Run tests and verify all ParserSpec tests pass

**Checkpoint**: At this point, User Story 1 should be fully functional. Can parse S-expressions into AST independently of evaluation.

---

## Phase 4: User Story 2 - Evaluate Basic Expressions (Priority: P1)

**Goal**: Evaluate basic Lisp expressions including arithmetic operations, comparison operators, and primitive function calls.

**Independent Test**: Evaluate arithmetic expressions, comparisons, and primitive function calls and verify correct results. Can be tested independently with manual AST construction.

### Tests for User Story 2 ⚠️

> **NOTE: Write these tests FIRST, ensure they FAIL before implementation**

- [x] T034 [P] [US2] Create test file test/Lisp/PrimitivesSpec.hs with hspec structure
- [x] T035 [P] [US2] Add test for arithmetic addition "(+ 1 2 3)" in test/Lisp/PrimitivesSpec.hs
- [x] T036 [P] [US2] Add test for arithmetic subtraction "(- 10 3)" in test/Lisp/PrimitivesSpec.hs
- [x] T037 [P] [US2] Add test for arithmetic multiplication "(* 4 5)" in test/Lisp/PrimitivesSpec.hs
- [x] T038 [P] [US2] Add test for arithmetic division "(/ 15 3)" in test/Lisp/PrimitivesSpec.hs
- [x] T039 [P] [US2] Add test for comparison greater than "(> 5 3)" in test/Lisp/PrimitivesSpec.hs
- [x] T040 [P] [US2] Add test for comparison less than "(< 3 5)" in test/Lisp/PrimitivesSpec.hs
- [x] T041 [P] [US2] Add test for comparison equal "(= 5 5)" in test/Lisp/PrimitivesSpec.hs
- [x] T042 [P] [US2] Add test for comparison not equal "(/= 5 3)" in test/Lisp/PrimitivesSpec.hs
- [x] T043 [P] [US2] Add test for string-append "(string-append \"hello\" \" world\")" in test/Lisp/PrimitivesSpec.hs
- [x] T044 [P] [US2] Add test for string-length "(string-length \"hello\")" in test/Lisp/PrimitivesSpec.hs
- [x] T045 [P] [US2] Add test for substring "(substring \"hello\" 1 3)" in test/Lisp/PrimitivesSpec.hs
- [x] T046 [P] [US2] Add test for nested expression "(+ (* 2 3) 4)" in test/Lisp/PrimitivesSpec.hs
- [x] T047 [P] [US2] Add test for division by zero error handling in test/Lisp/PrimitivesSpec.hs
- [x] T048 [P] [US2] Add test for type mismatch error handling in test/Lisp/PrimitivesSpec.hs
- [x] T049 [P] [US2] Add test for arity mismatch error handling in test/Lisp/PrimitivesSpec.hs
- [x] T050 [US2] Update test/Spec.hs to include PrimitivesSpec tests

### Implementation for User Story 2

- [x] T051 [US2] Create Lisp.Primitives module in src/Lisp/Primitives.hs
- [x] T052 [US2] Implement arithmetic primitives (add, sub, mul, div) in src/Lisp/Primitives.hs
- [x] T053 [US2] Implement comparison primitives (gt, lt, eq, ne) in src/Lisp/Primitives.hs
- [x] T054 [US2] Implement string primitives (string-append, string-length, substring) in src/Lisp/Primitives.hs
- [x] T055 [US2] Create initialEnv function returning Env with all primitives registered in src/Lisp/Primitives.hs
- [x] T056 [US2] Create Lisp.Eval module in src/Lisp/Eval.hs with evalExpr function signature
- [x] T057 [US2] Implement EvalM monad (ReaderT Env (Either Error)) in src/Lisp/Eval.hs
- [x] T058 [US2] Implement atom evaluation (self-evaluating atoms) in src/Lisp/Eval.hs
- [x] T059 [US2] Implement primitive function application in src/Lisp/Eval.hs
- [x] T060 [US2] Implement nested expression evaluation (evaluate arguments first) in src/Lisp/Eval.hs
- [x] T061 [US2] Add error handling for division by zero in src/Lisp/Eval.hs
- [x] T062 [US2] Add error handling for type mismatches in src/Lisp/Eval.hs
- [x] T063 [US2] Add error handling for arity mismatches in src/Lisp/Eval.hs
- [x] T064 [US2] Export evalExpr and evalExprWithEnv functions from Lisp.Eval module in src/Lisp/Eval.hs
- [x] T065 [US2] Run tests and verify all PrimitivesSpec tests pass

**Checkpoint**: At this point, User Story 2 should be fully functional. Can evaluate basic expressions independently.

---

## Phase 5: User Story 3 - Support Core Language Forms (Priority: P1)

**Goal**: Use core Lisp language forms (lambda, if, let, quote, begin, define) to write functional programs.

**Independent Test**: Write programs using each core form and verify they execute correctly. Can be tested independently with complete parser and evaluator.

### Tests for User Story 3 ⚠️

> **NOTE: Write these tests FIRST, ensure they FAIL before implementation**

- [x] T066 [P] [US3] Create test file test/Lisp/EvalSpec.hs with hspec structure
- [x] T067 [P] [US3] Add test for lambda expression "((lambda (x) (+ x 1)) 5)" in test/Lisp/EvalSpec.hs
- [x] T068 [P] [US3] Add test for if expression "(if (> x 0) 'positive 'negative)" with x=5 in test/Lisp/EvalSpec.hs
- [x] T069 [P] [US3] Add test for let expression "(let ((x 10) (y 20)) (+ x y))" in test/Lisp/EvalSpec.hs
- [x] T070 [P] [US3] Add test for quote expression "(quote (a b c))" in test/Lisp/EvalSpec.hs
- [x] T071 [P] [US3] Add test for single quote syntax "'(a b c)" in test/Lisp/EvalSpec.hs
- [x] T072 [P] [US3] Add test for begin expression "(begin (define x 5) (+ x 1))" in test/Lisp/EvalSpec.hs
- [x] T073 [P] [US3] Add test for define expression "(define square (lambda (x) (* x x)))" and call "(square 4)" in test/Lisp/EvalSpec.hs
- [x] T074 [P] [US3] Add test for nested let bindings with shadowing in test/Lisp/EvalSpec.hs
- [x] T075 [P] [US3] Add test for closure capturing lexical environment in test/Lisp/EvalSpec.hs
- [x] T076 [P] [US3] Add test for lambda with multiple parameters in test/Lisp/EvalSpec.hs
- [x] T077 [P] [US3] Add test for if with false condition in test/Lisp/EvalSpec.hs
- [x] T078 [P] [US3] Add test for begin with multiple expressions in test/Lisp/EvalSpec.hs
- [x] T079 [US3] Update test/Spec.hs to include EvalSpec tests

### Implementation for User Story 3

- [x] T080 [US3] Implement lambda form evaluation (create closure) in src/Lisp/Eval.hs
- [x] T081 [US3] Implement closure application (extend captured environment) in src/Lisp/Eval.hs
- [x] T082 [US3] Implement if form evaluation (condition, then, else) in src/Lisp/Eval.hs
- [x] T083 [US3] Implement let form evaluation (extend environment, evaluate body) in src/Lisp/Eval.hs
- [x] T084 [US3] Implement quote form evaluation (return quoted expression) in src/Lisp/Eval.hs
- [x] T085 [US3] Implement begin form evaluation (sequential evaluation, return last) in src/Lisp/Eval.hs
- [x] T086 [US3] Implement define form evaluation (add to environment, return name) in src/Lisp/Eval.hs
- [x] T087 [US3] Update evalExprWithEnv to handle define (return updated environment) in src/Lisp/Eval.hs
- [x] T088 [US3] Add error handling for undefined variables in src/Lisp/Eval.hs
- [x] T089 [US3] Add error handling for lambda arity mismatches in src/Lisp/Eval.hs
- [x] T090 [US3] Add error handling for invalid special form structure in src/Lisp/Eval.hs
- [x] T091 [US3] Run tests and verify all EvalSpec tests pass

**Checkpoint**: At this point, User Story 3 should be fully functional. All core language forms work correctly.

---

## Phase 6: User Story 4 - Interactive REPL (Priority: P2)

**Goal**: Interactive read-eval-print loop to experiment with Lisp expressions and see results immediately.

**Independent Test**: Launch REPL, enter expressions, verify they are evaluated and results displayed. Can be tested independently with complete parser and evaluator.

### Tests for User Story 4 ⚠️

> **NOTE: Write these tests FIRST, ensure they FAIL before implementation**

- [x] T092 [P] [US4] Create test file test/REPLSpec.hs with hspec structure
- [x] T093 [P] [US4] Add test for REPL parsing and evaluating simple expression in test/REPLSpec.hs
- [x] T094 [P] [US4] Add test for REPL handling define and using defined variable in test/REPLSpec.hs
- [x] T095 [P] [US4] Add test for REPL error handling (parse errors) in test/REPLSpec.hs
- [x] T096 [P] [US4] Add test for REPL error handling (evaluation errors) in test/REPLSpec.hs
- [x] T097 [P] [US4] Add test for REPL exit command in test/REPLSpec.hs
- [x] T098 [US4] Update test/Spec.hs to include REPLSpec tests

### Implementation for User Story 4

- [x] T099 [US4] Create REPL loop function in app/Main.hs (read, eval, print)
- [x] T100 [US4] Implement prompt display and input reading in app/Main.hs
- [x] T101 [US4] Integrate parser (parseExpr) in REPL loop in app/Main.hs
- [x] T102 [US4] Integrate evaluator (evalExpr) in REPL loop in app/Main.hs
- [x] T103 [US4] Implement result printing to stdout in app/Main.hs
- [x] T104 [US4] Implement error printing to stderr in app/Main.hs
- [x] T105 [US4] Implement exit command handling (":quit", ":q") in app/Main.hs
- [x] T106 [US4] Implement help command handling (":help", ":h") in app/Main.hs
- [x] T107 [US4] Implement environment persistence across REPL iterations in app/Main.hs
- [x] T108 [US4] Handle EOF gracefully (Ctrl+D) in app/Main.hs
- [x] T109 [US4] Update main function to start REPL in app/Main.hs
- [x] T110 [US4] Run tests and verify all REPLSpec tests pass

**Checkpoint**: At this point, User Story 4 should be fully functional. Interactive REPL works correctly.

---

## Phase 7: User Story 5 - Property-Based Testing (Priority: P2)

**Goal**: Property-based tests verifying key evaluation laws (substitution semantics, closure behavior) to ensure correctness.

**Independent Test**: Run property-based test suites and verify all tests pass. Can be tested independently with complete interpreter.

### Tests for User Story 5 ⚠️

> **NOTE: Write these tests FIRST, ensure they FAIL before implementation**

- [x] T111 [P] [US5] Create test file test/Properties.hs with QuickCheck structure
- [x] T112 [P] [US5] Add Arbitrary instance for Expr in test/Properties.hs
- [x] T113 [P] [US5] Add Arbitrary instance for Value in test/Properties.hs
- [x] T114 [P] [US5] Add property test for substitution semantics in test/Properties.hs
- [x] T115 [P] [US5] Add property test for closure capturing lexical environment in test/Properties.hs
- [x] T116 [P] [US5] Add property test for evaluation order (arguments evaluated before application) in test/Properties.hs
- [x] T117 [P] [US5] Add property test for let binding shadowing in test/Properties.hs
- [x] T118 [P] [US5] Add property test for closure environment isolation in test/Properties.hs
- [x] T119 [US5] Update test/Spec.hs to include Properties tests

### Implementation for User Story 5

- [x] T120 [US5] Implement substitution function for expressions in test/Properties.hs (helper for property tests)
- [x] T121 [US5] Configure QuickCheck to run 1000+ test cases in test/Properties.hs
- [x] T122 [US5] Run property tests and verify all pass consistently
- [x] T123 [US5] Document property test results and edge cases found

**Checkpoint**: At this point, User Story 5 should be fully functional. Property-based tests verify interpreter correctness.

---

## Phase 8: User Story 6 - Example Programs (Priority: P3)

**Goal**: Example Lisp programs demonstrating language capabilities and serving as learning resources.

**Independent Test**: Load and execute example programs and verify they produce expected results. Can be tested independently with complete interpreter.

### Tests for User Story 6 ⚠️

> **NOTE: Write these tests FIRST, ensure they FAIL before implementation**

- [x] T124 [P] [US6] Create test file test/ExamplesSpec.hs with hspec structure
- [x] T125 [P] [US6] Add test for factorial example program in test/ExamplesSpec.hs
- [x] T126 [P] [US6] Add test for list operations example in test/ExamplesSpec.hs
- [x] T127 [US6] Update test/Spec.hs to include ExamplesSpec tests

### Implementation for User Story 6

- [x] T128 [US6] Create examples/factorial.plisp with factorial function definition
- [x] T129 [US6] Create examples/lists.plisp demonstrating list operations
- [x] T130 [US6] Create examples/arithmetic.plisp demonstrating arithmetic operations
- [x] T131 [US6] Create examples/functions.plisp demonstrating function definitions and calls
- [x] T132 [US6] Create examples/conditionals.plisp demonstrating if expressions
- [x] T133 [US6] Create examples/scoping.plisp demonstrating let and closure scoping
- [x] T134 [US6] Add README.md in examples/ directory explaining example programs
- [x] T135 [US6] Run tests and verify all ExamplesSpec tests pass

**Checkpoint**: At this point, User Story 6 should be fully functional. Example programs demonstrate language capabilities.

---

## Phase 9: Polish & Cross-Cutting Concerns

**Purpose**: Improvements that affect multiple user stories

- [ ] T136 [P] Add comprehensive error messages with context in src/Lisp/Eval.hs
- [ ] T137 [P] Add comprehensive error messages with position in src/Lisp/Parser.hs
- [ ] T138 [P] Update module documentation in all src/Lisp/*.hs files
- [ ] T139 [P] Add Haddock comments for all exported functions
- [ ] T140 [P] Verify all success criteria from spec.md are met
- [ ] T141 [P] Run quickstart.md validation (verify all examples work)
- [ ] T142 Code cleanup and refactoring (remove unused code, improve naming)
- [ ] T143 Performance verification (parse <100ms for 1000 tokens, REPL <500ms response)
- [ ] T144 [P] Add integration tests combining parser, evaluator, and REPL in test/IntegrationSpec.hs
- [ ] T145 Update README.md with usage examples and quickstart link

---

## Dependencies & Execution Order

### Phase Dependencies

- **Setup (Phase 1)**: No dependencies - can start immediately
- **Foundational (Phase 2)**: Depends on Setup completion - BLOCKS all user stories
- **User Stories (Phase 3-8)**: All depend on Foundational phase completion
  - US1 (Parser) must complete before US2, US3, US4, US5, US6
  - US2 (Basic Eval) must complete before US3, US4, US5, US6
  - US3 (Core Forms) must complete before US4, US5, US6
  - US4 (REPL) can start after US1, US2, US3 complete
  - US5 (Property Tests) can start after US1, US2, US3 complete
  - US6 (Examples) can start after US1, US2, US3 complete
- **Polish (Phase 9)**: Depends on all desired user stories being complete

### User Story Dependencies

- **User Story 1 (P1) - Parser**: Can start after Foundational (Phase 2) - No dependencies on other stories
- **User Story 2 (P1) - Basic Eval**: Depends on US1 (Parser) - Needs parser to test evaluation
- **User Story 3 (P1) - Core Forms**: Depends on US2 (Basic Eval) - Builds on evaluation infrastructure
- **User Story 4 (P2) - REPL**: Depends on US1, US2, US3 - Needs complete interpreter
- **User Story 5 (P2) - Property Tests**: Depends on US1, US2, US3 - Needs complete interpreter
- **User Story 6 (P3) - Examples**: Depends on US1, US2, US3 - Needs complete interpreter

### Within Each User Story

- Tests (REQUIRED) MUST be written and FAIL before implementation
- Data types before functions
- Core implementation before integration
- Story complete before moving to next priority

### Parallel Opportunities

- All Setup tasks marked [P] can run in parallel
- All Foundational tasks marked [P] can run in parallel (within Phase 2)
- All test tasks within a user story marked [P] can run in parallel
- Data type creation tasks marked [P] can run in parallel
- Once US1, US2, US3 complete, US4, US5, US6 can proceed in parallel

---

## Parallel Example: User Story 1

```bash
# Launch all tests for User Story 1 together:
Task: "Create test file test/Lisp/ParserSpec.hs with hspec structure"
Task: "Add test for parsing simple S-expression \"(+ 1 2)\" in test/Lisp/ParserSpec.hs"
Task: "Add test for parsing nested S-expression \"(if (> x 0) (+ x 1) (- x 1))\" in test/Lisp/ParserSpec.hs"
Task: "Add test for parsing quoted expression \"(quote (a b c))\" in test/Lisp/ParserSpec.hs"
# ... (all test tasks can be written in parallel)
```

---

## Parallel Example: User Story 2

```bash
# Launch all primitive tests together:
Task: "Add test for arithmetic addition \"(+ 1 2 3)\" in test/Lisp/PrimitivesSpec.hs"
Task: "Add test for arithmetic subtraction \"(- 10 3)\" in test/Lisp/PrimitivesSpec.hs"
Task: "Add test for arithmetic multiplication \"(* 4 5)\" in test/Lisp/PrimitivesSpec.hs"
# ... (all primitive tests can be written in parallel)
```

---

## Implementation Strategy

### MVP First (User Story 1 Only)

1. Complete Phase 1: Setup
2. Complete Phase 2: Foundational (CRITICAL - blocks all stories)
3. Complete Phase 3: User Story 1 (Parser)
4. **STOP and VALIDATE**: Test User Story 1 independently - can parse S-expressions
5. Deploy/demo if ready

### Incremental Delivery

1. Complete Setup + Foundational → Foundation ready
2. Add User Story 1 (Parser) → Test independently → Deploy/Demo (MVP!)
3. Add User Story 2 (Basic Eval) → Test independently → Deploy/Demo
4. Add User Story 3 (Core Forms) → Test independently → Deploy/Demo
5. Add User Story 4 (REPL) → Test independently → Deploy/Demo
6. Add User Story 5 (Property Tests) → Test independently → Deploy/Demo
7. Add User Story 6 (Examples) → Test independently → Deploy/Demo
8. Each story adds value without breaking previous stories

### Parallel Team Strategy

With multiple developers:

1. Team completes Setup + Foundational together
2. Once Foundational is done:
   - Developer A: User Story 1 (Parser)
   - Developer B: Prepare tests for User Story 2
3. Once US1 is done:
   - Developer A: User Story 2 (Basic Eval)
   - Developer B: User Story 3 (Core Forms) - can start after US2 foundation
4. Once US2 and US3 are done:
   - Developer A: User Story 4 (REPL)
   - Developer B: User Story 5 (Property Tests)
   - Developer C: User Story 6 (Examples)
5. Stories complete and integrate independently

---

## Notes

- [P] tasks = different files, no dependencies
- [Story] label maps task to specific user story for traceability
- Each user story should be independently completable and testable
- **CRITICAL**: Verify tests fail before implementing (Test-First principle)
- Commit after each task or logical group
- Stop at any checkpoint to validate story independently
- Avoid: vague tasks, same file conflicts, cross-story dependencies that break independence
- All tests are REQUIRED per constitution (Test-First is NON-NEGOTIABLE)

