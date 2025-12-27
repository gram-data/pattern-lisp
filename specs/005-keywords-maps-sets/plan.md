# Implementation Plan: Keywords, Maps, and Sets

**Branch**: `005-keywords-maps-sets` | **Date**: 2025-01-27 | **Spec**: [spec.md](./spec.md)
**Input**: Feature specification from `/specs/005-keywords-maps-sets/spec.md`

**Note**: This template is filled in by the `/speckit.plan` command. See `.specify/templates/commands/plan.md` for the execution workflow.

## Summary

Add foundational language features to Pattern Lisp: keywords (postfix colon syntax), maps (curly brace syntax with keyword keys), and sets (hash set syntax). These features enable structured data representation, configuration maps, and Subject label support for gram notation interop. Implementation follows TDD approach with incremental feature addition: keywords first (required for maps), sets in parallel, then maps.

## Technical Context

**Language/Version**: Haskell (GHC 9.10.3)  
**Primary Dependencies**: 
- `base >=4.18 && <5` - Standard library
- `text` - Text handling
- `containers` - Data.Map and Data.Set
- `megaparsec` - Parser combinator library
- `mtl` - Monad transformers (ReaderT, Except)
- `gram`, `pattern`, `subject` - Domain libraries for gram notation

**Storage**: N/A - In-memory data structures only  
**Testing**: Hspec with QuickCheck for property-based testing  
**Target Platform**: GHC (cross-platform)  
**Project Type**: Single library project (language interpreter)  
**Performance Goals**: 
- Map literal creation: <1s for 10 key-value pairs (SC-001)
- Nested map access: <10ms for 3 levels deep (SC-002)
- Set union: <100ms for 1000 elements (SC-003)

**Constraints**: 
- Immutable data structures (no mutation)
- Must maintain compatibility with existing Pattern Lisp serialization
- Must support gram notation interop (Subject labels are `Set String`)

**Scale/Scope**: 
- Core language feature (foundational)
- No external dependencies required
- Affects parser, evaluator, and serialization modules

## Constitution Check

*GATE: Must pass before Phase 0 research. Re-check after Phase 1 design.*

### I. Library-First ✅
**Status**: PASS  
**Rationale**: This feature extends the existing Pattern Lisp library. No new library is being created - we're adding foundational language features to the existing interpreter. The feature is self-contained within the Pattern Lisp library and can be tested independently.

### II. CLI Interface ✅
**Status**: PASS  
**Rationale**: Pattern Lisp already has a CLI interface via the `pattern-lisp` executable. Keywords, maps, and sets will be accessible through the existing REPL and file execution interfaces. No new CLI commands needed.

### III. Test-First (NON-NEGOTIABLE) ✅
**Status**: PASS  
**Rationale**: TDD will be strictly followed. Tests will be written first for:
- Keyword parsing and evaluation
- Map literal parsing, evaluation, and operations
- Set literal parsing, evaluation, and operations
- Serialization/deserialization round-trips
- Error handling for invalid usage

### IV. Integration Testing ✅
**Status**: PASS  
**Rationale**: Integration tests required for:
- Gram notation interop (Subject labels as `Set String`)
- Serialization contract (keywords, maps, sets must serialize correctly)
- Cross-module behavior (parser → evaluator → serialization)

### V. Observability ✅
**Status**: PASS  
**Rationale**: Error messages will be clear and include context:
- Type mismatches (e.g., "Expected keyword, got symbol")
- Invalid syntax (parser errors with position)
- Operation errors (e.g., "Cannot use non-keyword as map key")

**Gate Status**: ✅ ALL GATES PASS - Proceed to Phase 0

## Project Structure

### Documentation (this feature)

```text
specs/005-keywords-maps-sets/
├── plan.md              # This file (/speckit.plan command output)
├── research.md          # Phase 0 output (/speckit.plan command)
├── data-model.md        # Phase 1 output (/speckit.plan command)
├── quickstart.md        # Phase 1 output (/speckit.plan command)
├── contracts/           # Phase 1 output (/speckit.plan command)
└── tasks.md             # Phase 2 output (/speckit.tasks command - NOT created by /speckit.plan)
```

### Source Code (repository root)

```text
src/PatternLisp/
├── Syntax.hs            # Add Keyword, VKeyword, VMap, VSet to types
├── Parser.hs            # Add keyword, map, set literal parsers
├── Eval.hs              # Add keyword evaluation, map/set operations
├── Primitives.hs        # Add map/set operation primitives
├── Codec.hs             # Add serialization for keywords, maps, sets
├── PatternPrimitives.hs # (existing)
├── Runtime.hs           # (existing)
├── Gram.hs              # (existing)
└── FileLoader.hs        # (existing)

test/PatternLisp/
├── ParserSpec.hs        # Add tests for keyword, map, set parsing
├── EvalSpec.hs          # Add tests for evaluation
├── PrimitivesSpec.hs    # Add tests for map/set operations
├── CodecSpec.hs         # Add tests for serialization
└── [other existing specs]
```

**Structure Decision**: Single library project structure. All changes are within the existing `PatternLisp` module hierarchy. No new modules required - extend existing ones.

## Complexity Tracking

> **Fill ONLY if Constitution Check has violations that must be justified**

No violations - all gates pass.

---

## Phase 0: Research ✅

**Status**: Complete  
**Output**: [research.md](./research.md)

**Findings**:
- Keywords: Use `Keyword String` and `VKeyword String` (not interned)
- Maps: Use `Data.Map.Strict` with `VKeyword` keys
- Sets: Use `Data.Set` with `Value` elements
- Parser: Extend Megaparsec parser with new syntax
- Serialization: Convert to/from Subject representation
- Labels: Deferred (use plain strings in sets)

**All NEEDS CLARIFICATION items resolved.**

---

## Phase 1: Design & Contracts ✅

**Status**: Complete  
**Outputs**:
- [data-model.md](./data-model.md) - Entity definitions and relationships
- [contracts/README.md](./contracts/README.md) - Function contracts and behavior
- [quickstart.md](./quickstart.md) - Developer quickstart guide

**Design Decisions**:
- Type hierarchy: `VKeyword`, `VMap`, `VSet` added to `Value` type
- Immutable semantics: All operations return new data structures
- Serialization: Round-trip must preserve types
- Error handling: Clear type mismatch and syntax errors

**Agent Context**: Updated with Haskell (GHC 9.10.3) and project type.

---

## Phase 2: Tasks

**Status**: Pending  
**Next Command**: `/speckit.tasks`

Tasks will be generated from the plan and spec, breaking down implementation into testable units following TDD approach.
