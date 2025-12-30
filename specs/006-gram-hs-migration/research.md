# Research: Gram-HS Constructor Migration

**Date**: 2025-01-28  
**Status**: Complete

## Migration Guide Analysis

**Source**: `../gram-hs/docs/users/migration/rename-constructors.md`

### Key Findings

1. **Constructor Renaming**:
   - `patternWith v es` → `pattern v es` (patterns with elements)
   - `pattern v` → `point v` (atomic patterns)

2. **Import Changes**:
   - Old: `import Pattern.Core (pattern, patternWith, ...)`
   - New: `import Pattern.Core (point, pattern, ...)`

3. **Migration Strategy**:
   - Manual migration recommended (not automated) due to ambiguity between `pattern` as function vs variable name
   - Step 1: Update imports
   - Step 2: Replace all `patternWith` → `pattern`
   - Step 3: Review each `pattern` call to determine if atomic (use `point`) or has elements (keep `pattern`)

## Codebase Analysis

### Affected Modules

**Source Modules**:
- `src/PatternLisp/Codec.hs`: 30+ `patternWith` occurrences, imports `Pattern.Core (pattern, patternWith)`
- `src/PatternLisp/PatternPrimitives.hs`: 4 `patternWith` occurrences, imports `Pattern.Core (pattern, patternWith)`
- `src/PatternLisp/Gram.hs`: 1 atomic `pattern` occurrence, imports `Pattern.Core (pattern)`
- `src/PatternLisp/Eval.hs`: Qualified import only (`PatternCore`), no direct constructor usage

**Test Modules**:
- `test/PatternLisp/CodecSpec.hs`: Imports `Pattern.Core (pattern, patternWith)`
- `test/PatternLisp/GramSpec.hs`: 1 `patternWith` occurrence, imports `Pattern.Core (pattern, patternWith)`
- `test/PatternLisp/GramSerializationSpec.hs`: 1 `patternWith` occurrence, imports `Pattern.Core (pattern, patternWith)`
- `test/PatternLisp/RuntimeSpec.hs`: 1 atomic `pattern` occurrence, imports `Pattern.Core (pattern)`

### Migration Pattern

**Atomic Pattern Identification**:
- `pattern subject` where `subject` is a single value (not a list)
- Examples: `pattern $ valueToSubjectForGram (VNumber n)`, `pattern subject`

**Pattern-with-Elements Identification**:
- `patternWith decoration elements` where `elements` is a list
- Examples: `patternWith decoration elementPatterns`, `patternWith (valueToSubjectForGram (VMap Map.empty)) elements`

## Decisions

### Decision 1: Migration Approach
**Chosen**: Manual, module-by-module migration with verification after each module

**Rationale**: 
- Migration guide recommends manual migration due to ambiguity
- Module-by-module allows incremental verification
- Type checker will catch errors immediately

**Alternatives Considered**:
- Automated find-and-replace: Rejected due to risk of replacing variable names
- All-at-once migration: Rejected due to difficulty in debugging if errors occur

### Decision 2: Verification Strategy
**Chosen**: Compile and test after each module migration

**Rationale**:
- Ensures migration correctness incrementally
- Type checker provides immediate feedback
- Test suite validates functional correctness

**Alternatives Considered**:
- Migrate all then verify: Rejected due to difficulty in locating errors

### Decision 3: Atomic Pattern Detection
**Chosen**: Manual review of each `pattern` call to determine if it's atomic or has elements

**Rationale**:
- Type signature: `point :: v -> Pattern v` (one argument)
- Type signature: `pattern :: v -> [Pattern v] -> Pattern v` (two arguments)
- Compiler will catch incorrect usage

**Alternatives Considered**:
- Automated detection: Rejected due to complexity of distinguishing function calls from variable names

### Decision 4: Comments and Documentation Updates
**Chosen**: Update code comments and documentation examples to use new API

**Rationale**:
- Ensures consistency across codebase
- Prevents confusion for developers reading code
- Aligns with breaking change migration (no backwards compatibility)
- Haddock examples should reflect current API

**Alternatives Considered**:
- Leave comments unchanged: Rejected - would create inconsistency and confusion
- Update only critical comments: Rejected - partial updates create maintenance burden

## Migration Checklist

1. ✅ Migration guide reviewed and understood
2. ✅ Affected modules identified
3. ✅ Migration strategy determined
4. ✅ Verification approach defined

## Open Questions

None - all technical questions resolved through migration guide and codebase analysis.

