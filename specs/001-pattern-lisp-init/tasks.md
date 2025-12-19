# Tasks: Pattern Lisp Project Initialization

**Input**: Design documents from `/specs/001-pattern-lisp-init/`
**Prerequisites**: plan.md (required), spec.md (required for user stories), research.md, data-model.md, contracts/

**Tests**: Tests are NOT requested in the feature specification. This is a project setup phase focusing on structure and configuration.

**Organization**: Tasks are grouped by user story to enable independent implementation and testing of each story.

## Format: `[ID] [P?] [Story] Description`

- **[P]**: Can run in parallel (different files, no dependencies)
- **[Story]**: Which user story this task belongs to (e.g., US1, US2, US3, US4)
- Include exact file paths in descriptions

## Path Conventions

- **Single project**: `src/`, `app/`, `test/`, `examples/` at repository root
- Paths follow Cabal conventions as specified in plan.md

---

## Phase 1: Setup (Shared Infrastructure)

**Purpose**: Project initialization and basic structure

- [ ] T001 Create directory structure with src/, app/, test/, and examples/ directories at repository root
- [ ] T002 [P] Create .gitignore file with Cabal build artifacts (dist-newstyle/, .cabal-sandbox/, etc.)

---

## Phase 2: Foundational (Blocking Prerequisites)

**Purpose**: Core infrastructure that MUST be complete before ANY user story can be implemented

**⚠️ CRITICAL**: No user story work can begin until this phase is complete

- [ ] T003 Create cabal.project file with packages: . and source-repository-package configuration for gram-hs
- [ ] T004 Create pattern-lisp.cabal file with cabal-version, name, version, and build-type sections
- [ ] T005 [P] Configure library component in pattern-lisp.cabal with exposed-modules (Lisp.* placeholder) and build-depends
- [ ] T006 [P] Configure executable component in pattern-lisp.cabal with main-is: app/Main.hs and build-depends
- [ ] T007 [P] Configure test-suite component in pattern-lisp.cabal with type, main-is, and test dependencies (hspec, QuickCheck)

**Checkpoint**: Foundation ready - user story implementation can now begin in parallel

---

## Phase 3: User Story 1 - Project Structure Setup (Priority: P1) 🎯 MVP

**Goal**: Initialize a new Haskell project with the correct directory structure for Pattern Lisp, including separate directories for library code, CLI executable, tests, and examples.

**Independent Test**: Verify that all required directories exist and follow the expected structure by running `ls -la` and checking for `src/`, `app/`, `test/`, and `examples/` directories.

### Implementation for User Story 1

- [ ] T008 [US1] Verify src/ directory exists and is ready for library modules
- [ ] T009 [US1] Verify app/ directory exists and is ready for CLI executable
- [ ] T010 [US1] Verify test/ directory exists and is ready for test files
- [ ] T011 [US1] Verify examples/ directory exists and is ready for example programs
- [ ] T012 [US1] Validate directory structure follows standard Cabal project conventions

**Checkpoint**: At this point, User Story 1 should be fully functional and testable independently. All required directories exist and follow Cabal conventions.

---

## Phase 4: User Story 2 - Cabal Configuration (Priority: P1)

**Goal**: Configure the Cabal project files (cabal.project and pattern-lisp.cabal) to define the library, executable, test suite, and external dependencies including the gram-hs source repository.

**Independent Test**: Run `cabal build` and verify that Cabal successfully parses the configuration and resolves dependencies. Check that `cabal update` successfully fetches gram-hs dependency.

### Implementation for User Story 2

- [ ] T013 [US2] Configure cabal.project with source-repository-package for gram-hs (location: https://github.com/gram-data/gram-hs, tag: main)
- [ ] T014 [US2] Configure pattern-lisp.cabal library section with exposed-modules placeholder (Lisp.*) and dependencies (base, text, containers, megaparsec, mtl, gram-hs)
- [ ] T015 [US2] Configure pattern-lisp.cabal executable section with main-is: app/Main.hs and dependencies (library component, base)
- [ ] T016 [US2] Configure pattern-lisp.cabal test-suite section with type: exitcode-stdio-1.0, main-is, and dependencies (library component, hspec, QuickCheck)
- [ ] T017 [US2] Run cabal update to verify gram-hs dependency can be resolved
- [ ] T018 [US2] Verify cabal.project and pattern-lisp.cabal files are valid Cabal syntax

**Checkpoint**: At this point, User Story 2 should be fully functional and testable independently. Cabal configuration is complete and dependencies can be resolved.

---

## Phase 5: User Story 3 - Build Verification (Priority: P2)

**Goal**: Verify that the project builds successfully and all components (library, executable, tests) compile without errors.

**Independent Test**: Run `cabal build all` and `cabal test` and verify successful completion. Attempt to import gram-hs modules in library code to verify import resolution.

### Implementation for User Story 3

- [ ] T019 [US3] Create minimal library module placeholder in src/Lisp/Test.hs to verify library builds
- [ ] T020 [US3] Create minimal executable entry point in app/Main.hs that imports library and provides basic CLI output
- [ ] T021 [US3] Create minimal test file in test/Spec.hs with hspec framework setup
- [ ] T022 [US3] Run cabal build all and verify all components compile successfully
- [ ] T023 [US3] Run cabal test and verify test framework executes successfully
- [ ] T024 [US3] Verify gram-hs modules can be imported in library code (add import statement in src/Lisp/Test.hs)

**Checkpoint**: At this point, User Story 3 should be fully functional and testable independently. Project builds successfully and all components compile.

---

## Phase 6: User Story 4 - Git Repository Initialization (Priority: P3)

**Goal**: Initialize a git repository for the project and prepare it for version control and remote repository synchronization.

**Independent Test**: Verify that `.git` directory exists and basic git operations work. Check that `.gitignore` contains appropriate Cabal build artifacts.

### Implementation for User Story 4

- [ ] T025 [US4] Initialize git repository in project root (git init)
- [ ] T026 [US4] Verify .gitignore file contains Cabal build artifacts (dist-newstyle/, .cabal-sandbox/, cabal.project.local, etc.)
- [ ] T027 [US4] Add all project files to git staging area (git add)
- [ ] T028 [US4] Verify git status shows project files ready for commit

**Checkpoint**: At this point, User Story 4 should be fully functional and testable independently. Git repository is initialized and ready for version control.

---

## Phase 7: Polish & Cross-Cutting Concerns

**Purpose**: Improvements that affect multiple user stories

- [ ] T029 [P] Run quickstart.md validation steps to verify complete project setup
- [ ] T030 [P] Verify all success criteria from spec.md are met (SC-001 through SC-006)
- [ ] T031 [P] Update README.md with project setup instructions and build commands
- [ ] T032 [P] Document any deviations from research.md decisions in implementation notes

---

## Dependencies & Execution Order

### Phase Dependencies

- **Setup (Phase 1)**: No dependencies - can start immediately
- **Foundational (Phase 2)**: Depends on Setup completion - BLOCKS all user stories
- **User Stories (Phase 3+)**: All depend on Foundational phase completion
  - User stories can then proceed in parallel (if staffed)
  - Or sequentially in priority order (P1 → P2 → P3)
- **Polish (Final Phase)**: Depends on all desired user stories being complete

### User Story Dependencies

- **User Story 1 (P1)**: Can start after Foundational (Phase 2) - No dependencies on other stories
- **User Story 2 (P1)**: Can start after Foundational (Phase 2) - May integrate with US1 but should be independently testable
- **User Story 3 (P2)**: Depends on US1 and US2 completion (needs directory structure and Cabal config to build)
- **User Story 4 (P3)**: Can start after US3 completion (git init after build verification)

### Within Each User Story

- Directory structure before file creation
- Configuration files before build verification
- Build verification before git initialization
- Story complete before moving to next priority

### Parallel Opportunities

- All Setup tasks marked [P] can run in parallel
- All Foundational tasks marked [P] (T005, T006, T007) can run in parallel (within Phase 2)
- User Stories 1 and 2 (both P1) can start in parallel after Foundational phase completes
- All Polish tasks marked [P] can run in parallel

---

## Parallel Example: Foundational Phase

```bash
# Launch all Cabal configuration tasks together:
Task: "Configure library component in pattern-lisp.cabal with exposed-modules (Lisp.* placeholder) and build-depends"
Task: "Configure executable component in pattern-lisp.cabal with main-is: app/Main.hs and build-depends"
Task: "Configure test-suite component in pattern-lisp.cabal with type, main-is, and test dependencies (hspec, QuickCheck)"
```

---

## Implementation Strategy

### MVP First (User Stories 1 & 2 Only)

1. Complete Phase 1: Setup
2. Complete Phase 2: Foundational (CRITICAL - blocks all stories)
3. Complete Phase 3: User Story 1 (Project Structure)
4. Complete Phase 4: User Story 2 (Cabal Configuration)
5. **STOP and VALIDATE**: Test User Stories 1 & 2 independently
6. Verify cabal update and cabal build work

### Incremental Delivery

1. Complete Setup + Foundational → Foundation ready
2. Add User Story 1 → Test independently → Verify structure (MVP foundation!)
3. Add User Story 2 → Test independently → Verify configuration
4. Add User Story 3 → Test independently → Verify build
5. Add User Story 4 → Test independently → Verify git setup
6. Each story adds value without breaking previous stories

### Parallel Team Strategy

With multiple developers:

1. Team completes Setup + Foundational together
2. Once Foundational is done:
   - Developer A: User Story 1 (Project Structure)
   - Developer B: User Story 2 (Cabal Configuration)
3. Once US1 and US2 are complete:
   - Developer A: User Story 3 (Build Verification)
   - Developer B: User Story 4 (Git Repository)
4. Stories complete and integrate independently

---

## Notes

- [P] tasks = different files, no dependencies
- [Story] label maps task to specific user story for traceability
- Each user story should be independently completable and testable
- Commit after each task or logical group
- Stop at any checkpoint to validate story independently
- Avoid: vague tasks, same file conflicts, cross-story dependencies that break independence
- This is a project setup phase - no runtime functionality is implemented
- Tests are NOT included as they are not requested in the specification

