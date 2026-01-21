# Tasks: Plisp–Gram Convert CLI

**Input**: Design documents from `specs/008-plisp-gram-convert/`
**Prerequisites**: plan.md, spec.md, research.md, data-model.md, contracts/, quickstart.md

**Tests**: TDD—tests are written first and must fail before implementation (plan.md, constitution III. Test-First).

**Organization**: Tasks are grouped by user story to enable independent implementation and testing. Includes gram-hs update and breakage checks per user request.

## Format: `[ID] [P?] [Story] Description`

- **[P]**: Can run in parallel (different files, no dependencies)
- **[Story]**: User story (US1, US2, US3) for story-phase tasks only; omit for Setup, Foundational, Polish
- Include exact file paths in descriptions

## Path Conventions

- **Source**: `src/PatternLisp/`, `app/`
- **Tests**: `test/`, `test/PatternLisp/`
- **Config**: `cabal.project` at repository root

---

## Phase 1: Setup (gram-hs update and breakage check)

**Purpose**: Update to latest gram-hs, ensure build and tests pass; fix any breaking changes in Gram serialization and parsing.

**Logical model (gram document)**: A gram file is a **list of patterns**. The **first** may be a header-like pattern (optional root record `{k:v}`); the **rest** are content. Analogous to CSV with an optional header. `toGram [p1, p2, ...]` serializes: if `p1` is header-like → bare `{k:v}` then newline-separated content; otherwise all as `serializePattern`. `fromGram` returns `[Pattern]`; a leading bare root becomes the first (header-like) pattern. `fromGramWithHeader` yields `(Maybe (Map String V.Value), [Pattern])` with only content patterns in the list.

- [x] T001 Update gram-hs to latest: run `cabal update`; verify or update `cabal.project` source-repository-package entries for `libs/gram`, `libs/pattern`, `libs/subject` (tag: main or a newer release tag if available); run `cabal build` in project root.

- [x] T002 Run `cabal test`; fix any breaking changes from gram-hs in **serialization** (`toGram`, `valueToPatternSubjectForGram`, `programToGram` in `src/PatternLisp/Codec.hs`; `patternToGram` in `src/PatternLisp/Gram.hs`) and **parsing** (`fromGram`, `gramToPattern`, `gramToProgram` in `src/PatternLisp/Codec.hs`; `Gram.Parse` in `src/PatternLisp/Gram.hs`; `Gram.Parse` in `test/PatternLisp/RecordGramCompatibilitySpec.hs`). Update `test/PatternLisp/CodecSpec.hs`, `test/PatternLisp/GramSerializationSpec.hs`, `test/Properties.hs` if their use of these functions breaks.

- [x] T002b Refactor `gramToProgram` in `src/PatternLisp/Codec.hs` to follow the gram document model: parse the **entire** input with `fromGram` (no line-splitting or per-line `fromGram`). Expect `[Pattern]`: at least one; first = header (require `kind: "Pattern Lisp"` in its subject properties); **rest** = value patterns. Run `mapM patternSubjectToValue` on the rest. Optionally consider `fromGramWithHeader` (root as `Maybe (Map ...)`, list = content only). Handle empty/whitespace → `Right []` as "Empty Gram file" (or equivalent); only-header no-values → `[]` value patterns is allowed.

---

## Phase 2: Foundational (Blocking Prerequisites)

**Purpose**: Value→plisp and Expr→plisp serialization required for gram→plisp. MUST be complete before US2.

**Checkpoint**: Foundation ready—US1 can start (US1 does not need valueToPlispSource); US2 and US3 need it.

- [ ] T003 Implement `exprToPlisp :: Expr -> String` in `src/PatternLisp/Codec.hs` per data-model.md and contracts; support List, Quote, Atom, RecordLiteral, and other Expr forms the parser produces; ensure round-trip `parseExpr (exprToPlisp e) ≡ Right e` (or equivalent). Export from Codec.

- [ ] T004 Implement `valueToPlispSource :: Value -> Either Error String` in `src/PatternLisp/Codec.hs` per data-model.md and contracts; use `exprToPlisp` for closure bodies. Support VInteger, VString, VBoolean, VArray, VMap, VSet, VClosure, VKeyword, VDecimal, VSymbol, VPrimitive; VPattern may be `Left` or placeholder for now. Emit plisp that parses and evaluates to an equivalent value. Export from Codec.

---

## Phase 3: User Story 1 – Convert plisp to gram (Priority: P1) — MVP

**Goal**: User runs `pattern-lisp --to-gram foo.plisp` and gets gram written to `foo.plisp.gram` (or `-o` path). Valid plisp always succeeds; invalid plisp fails with a clear error.

**Independent Test**: `pattern-lisp --to-gram examples/arithmetic.plisp` produces `examples/arithmetic.plisp.gram`; `gramToProgram` on that file yields equivalent values.

### Tests for User Story 1 (TDD: write first, see them fail)

- [ ] T005 [P] [US1] Create `test/PatternLisp/ConvertSpec.hs` with failing describe blocks for plisp→gram: valid `.plisp` converts and writes gram; default output path `foo.plisp` → `foo.plisp.gram`; invalid plisp reports error and no output; explicit `-o` is used.

### Implementation for User Story 1

- [ ] T006 [US1] In `app/Main.hs`, parse `--to-gram`, `--to-plisp`, `-o`/`--output`, and one positional input; implement default output path for both ToGram (`foo.plisp` → `foo.plisp.gram`, else append `.plisp.gram`) and ToPlisp (`foo.gram`/`foo.plisp.gram` → `foo.plisp`, else replace or append `.plisp`) per research.md and contracts. T008 will enforce both/neither; T010 will use ToPlisp default.

- [ ] T007 [US1] In `app/Main.hs`, implement `--to-gram` flow: `loadPlispFile` with `initialEnv`, on success `programToGram [loadResultValue] env`, write to output path; on parse/eval error, stderr and exit 1. Import `PatternLisp.Codec` for `programToGram`; use `PatternLisp.FileLoader.loadPlispFile`.

- [ ] T008 [US1] In `app/Main.hs`, enforce convert-mode rules: exactly one of `--to-gram` or `--to-plisp`; mutually exclusive with `-e`/`-i` and with “load files and eval”; exactly one input file; on violation print usage to stderr and exit 1.

**Checkpoint**: `pattern-lisp --to-gram script.plisp` works; ConvertSpec plisp→gram tests pass.

---

## Phase 4: User Story 2 – Convert gram to plisp when pattern-lisp (Priority: P2)

**Goal**: User runs `pattern-lisp --to-plisp foo.plisp.gram` and gets plisp `(begin v1 v2 ...)` written to `foo.plisp`. Gram that is not a pattern-lisp program fails with a clear error.

**Independent Test**: Convert a `.plisp.gram` from US1 to plisp, then convert that plisp back to gram; the second gram matches the first in meaning.

### Tests for User Story 2 (TDD: write first, see them fail)

- [ ] T009 [US2] Add failing describe blocks in `test/PatternLisp/ConvertSpec.hs` for gram→plisp: valid pattern-lisp gram converts to `(begin ...)`; default output `foo.plisp.gram` → `foo.plisp`; gram missing `kind: "Pattern Lisp"` or invalid value patterns yields clear error and no output.

### Implementation for User Story 2

- [ ] T010 [US2] In `app/Main.hs`, implement `--to-plisp` flow: read file, `gramToProgram`, `mapM valueToPlispSource` on `[Value]`, join as `(begin v1 v2 ...)`, write to output path. Use default output from T006 when `-o` is omitted. Import `PatternLisp.Codec` for `gramToProgram`, `valueToPlispSource`.

- [ ] T011 [US2] Ensure gram→plisp reports a clear error when `gramToProgram` returns `Left` (gram parse error, missing `kind: "Pattern Lisp"`, or `patternSubjectToValue` failure). Verify or update error messages in `src/PatternLisp/Codec.hs` and/or `app/Main.hs` to satisfy FR-008 and contracts.

**Checkpoint**: `pattern-lisp --to-plisp program.plisp.gram` works; gram-not-program fails with clear message; ConvertSpec gram→plisp tests pass.

---

## Phase 5: User Story 3 – Filename convention .plisp.gram (Priority: P3)

**Goal**: `.plisp.gram` is the default for plisp→gram output; gram→plisp accepts `.plisp.gram` as input. Convention is documented and tested.

**Independent Test**: plisp→gram defaults to `example.plisp.gram`; gram→plisp with `example.plisp.gram` defaults to `example.plisp`; both directions work.

### Tests for User Story 3

- [ ] T012 [P] [US3] Add failing describe blocks in `test/PatternLisp/ConvertSpec.hs` for `.plisp.gram`: plisp→gram default output is `*.plisp.gram`; gram→plisp with `*.plisp.gram` input uses `*.plisp` as default output and converts successfully.

### Implementation for User Story 3

- [ ] T013 [US3] Verify default output in `app/Main.hs` produces `*.plisp.gram` for `--to-gram` and `*.plisp` for `--to-plisp` when input is `*.plisp.gram` or `*.gram`; fix if not. Help text for `.plisp.gram` is in T015.

**Checkpoint**: .plisp.gram convention works and is tested; help mentions it.

---

## Phase 6: Polish & Cross-Cutting Concerns

**Purpose**: Wire tests, usage, round-trip, and quickstart.

- [ ] T014 Register `PatternLisp.ConvertSpec` in `test/Spec.hs` (import and describe block).

- [ ] T015 Add `--to-gram`, `--to-plisp`, `-o`/`--output`, and `.plisp.gram` convention to usage/help in `app/Main.hs`.

- [ ] T016 Add round-trip test (plisp→gram→plisp) in `test/IntegrationSpec.hs` using `programToGram`/`gramToProgram` and `valueToPlispSource`; assert plisp equivalence.

- [ ] T017 Run `specs/008-plisp-gram-convert/quickstart.md` validation (convert plisp→gram, gram→plisp, round-trip, and error cases).

---

## Dependencies & Execution Order

### Phase Dependencies

- **Phase 1 (Setup)**: No dependencies. Must finish before Phase 2 (foundational) and any story.
- **Phase 2 (Foundational)**: Depends on Phase 1. Blocks US2 and US3; US1 can start after Phase 1.
- **Phase 3 (US1)**: Depends on Phase 1. Does not need `valueToPlispSource`/`exprToPlisp`.
- **Phase 4 (US2)**: Depends on Phase 2 and Phase 3 (convert options and `--to-gram` wiring in Main).
- **Phase 5 (US3)**: Depends on Phase 3 and Phase 4 (default paths and both convert modes).
- **Phase 6 (Polish)**: Depends on Phase 3, 4, 5.

### User Story Dependencies

- **US1 (P1)**: After Setup. No dependency on US2/US3.
- **US2 (P2)**: After Foundational and US1 (Main already has convert option parsing and mutual exclusivity).
- **US3 (P3)**: After US1 and US2 (default path behavior and both modes).

### Within Each User Story

- Tests (T005, T009, T012) MUST be written and MUST fail before implementation.
- US1: T005 → T006–T008. US2: T009 → T010–T011. US3: T012 → T013.

### Parallel Opportunities

- T005 (create ConvertSpec) is [P] within US1.
- T012 (add .plisp.gram tests) is [P] within US3.
- T003 and T004 are sequential (T004 uses exprToPlisp).

---

## Parallel Example: User Story 1

```bash
# After T005 (ConvertSpec with failing US1 tests), run:
cabal test --test-options='--match "Plisp.*gram"'
# Then implement T006–T008 until plisp→gram tests pass.
```

---

## Implementation Strategy

### MVP First (User Story 1 Only)

1. Phase 1: Setup (T001–T002b)—gram-hs update, breakage fixes, and gramToProgram refactor to whole-document fromGram.
2. Phase 2: Foundational (T003–T004)—needed for US2; can be deferred if doing MVP-only, but then US2 cannot start.
3. Phase 3: US1 (T005–T008)—plisp→gram. Stop and validate.
4. Deploy/demo plisp→gram.

### Incremental Delivery

1. Setup + Foundational → gram-hs current, valueToPlispSource/exprToPlisp ready.
2. US1 → plisp→gram. Test independently.
3. US2 → gram→plisp. Test independently; round-trip.
4. US3 → .plisp.gram convention and docs.
5. Polish → Spec.hs, usage, IntegrationSpec round-trip, quickstart.

### Suggested MVP Scope

- **Minimum**: Phase 1 + Phase 3 (US1). Omit Phase 2 until gram→plisp is in scope.
- **With round-trip**: Phase 1 + Phase 2 + Phase 3 + Phase 4 + T016 (round-trip in IntegrationSpec).

---

## Notes

- [P] = different files or no deps on incomplete work.
- [USn] maps to spec.md user stories for traceability.
- Each user story is independently testable.
- Verify tests fail before implementing (TDD).
- Commit after each task or coherent group.
- gram-hs: `Gram.Serialize` (toGram), `Gram.Parse` (fromGram, ParseError). Breakage may show as compile or test failures in Codec, Gram, CodecSpec, GramSerializationSpec, RecordGramCompatibilitySpec, Properties.

---

## Summary

| Phase        | Task IDs   | Count |
|-------------|------------|-------|
| 1 Setup     | T001–T002b | 3     |
| 2 Foundational | T003–T004 | 2   |
| 3 US1       | T005–T008 | 4     |
| 4 US2       | T009–T011 | 3     |
| 5 US3       | T012–T013 | 2     |
| 6 Polish    | T014–T017 | 4     |
| **Total**   |           | **18**|

| Story | Tasks       | Count |
|-------|-------------|-------|
| US1   | T005–T008   | 4     |
| US2   | T009–T011   | 3     |
| US3   | T012–T013   | 2     |

- **Parallel**: T005, T012.
- **Independent test criteria**: US1—plisp→gram produces valid gram; US2—gram→plisp produces `(begin ...)` and round-trip; US3—.plisp.gram default and input.
- **Format**: All tasks use `- [ ] [Txxx] [P?] [USn?] Description with file path`.
