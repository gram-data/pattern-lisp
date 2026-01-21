# Implementation Plan: Inline Record Notation for Pattern-Lisp

**Branch**: `007-inline-record-notation` | **Date**: 2025-01-30 | **Spec**: [spec.md](./spec.md)
**Input**: Feature specification from `/specs/007-inline-record-notation/spec.md`

**Note**: This template is filled in by the `/speckit.plan` command. See `.specify/templates/commands/plan.md` for the execution workflow.

## Summary

**This is a refactoring**: Replace existing map support (`VMap`, `MapLiteral`, `hash-map`) with gram-compatible record support (`VRecord`, `RecordLiteral`, `record`). This is NOT a new feature - it's a migration/refactoring of existing functionality.

**Refactoring Scope**:
- **MAJOR**: Replace entire `Value` type system with gram `Subject.Value` types (Option B)
  - `VNumber Integer` → `Subject.Value.VInteger Integer`
  - `VString Text` → `Subject.Value.VString String`
  - `VBool Bool` → `Subject.Value.VBoolean Bool`
  - `VMap` → `Subject.Value.VMap (Map String Value)` (gram's PropertyRecord for records)
- **Replace** `MapLiteral [Expr]` → `RecordLiteral [(String, Expr)]`
- **Replace** `hash-map` function → `record` function
- **Replace** space-separated syntax `{name: "Alice" age: 30}` → comma-separated syntax `{name: "Alice", age: 30}`
- **Replace** custom map parser → gram parser delegation
- **Remove** `MapKey` type (use `String` keys only, matching gram)
- **Update** all map operations to work with gram `VMap` (records)
- **Remove** all conversion functions in `Codec.hs` (types already aligned)

**Clean Break - No Backward Compatibility**:
- Existing code using `{name: "Alice" age: 30}` syntax will break
- Existing code using `hash-map` will break
- Existing code using `VMap` will break
- All map operations will work with records instead

**Value Type Strategy**: Use gram types throughout (Option B) - BOLD BREAKING CHANGE
- **Replace** all pattern-lisp `Value` types with gram `Subject.Value` types throughout runtime
- **Replace** `VNumber Integer` → `Subject.Value.VInteger Integer`
- **Replace** `VString Text` → `Subject.Value.VString String` (Text → String conversion in parser/atoms)
- **Replace** `VBool Bool` → `Subject.Value.VBoolean Bool`
- **Replace** `VMap` → `Subject.Value.VMap (Map String Value)` for records (gram's PropertyRecord)
- **Keep** `VPattern`, `VClosure`, `VPrimitive` (pattern-lisp specific, not in gram)
- **Consider** `VList [Value]` → `Subject.Value.VArray [Value]` (gram uses VArray)
- **Consider** `VSet (Set.Set Value)` → keep or convert to `VArray` (gram doesn't have Set)
- **Remove** all conversion functions in `Codec.hs` (no longer needed - types already aligned)
- **Update** all pattern-lisp code to use gram types directly
- **Impact**: Large refactoring, aligns completely with gram, eliminates conversion layer, users see gram types in errors

**✅ Gram Schema Validation**: Verified using `gramref schema` - records are fully supported:
- `Subject.properties` is `Map String Value` (PropertyRecord) - exactly what records need
- `Value` type includes `object/map` with string keys and recursive Value values
- Nested records fully supported (maps can contain maps)
- All value types can be used as record values
- See `gram-schema-analysis.md` for detailed validation

**Important Syntax Distinction**:
- **Pattern-lisp syntax** (what you write in `.plisp` files): `{ name: "Alice", age: 30 }` - comma-separated (gram-compatible)
- **Pattern-lisp uses parentheses** `()` for function calls, NOT square brackets `[]`
- **Gram notation** (serialization format): `[Person {name: "Alice"}]` - output when serializing pattern-lisp values
- Records use gram-compatible syntax for consistency

## Technical Context

**Language/Version**: Haskell (GHC 9.10.3, base >=4.18 && <5)  
**Primary Dependencies**: gram-hs library (gram, pattern, subject packages), Megaparsec (parsing), Data.Map.Strict (record representation), Cabal build system  
**Storage**: N/A (in-memory record structures)  
**Testing**: hspec, QuickCheck, cabal test  
**Target Platform**: Linux/macOS (Haskell cross-platform)  
**Project Type**: Library (Haskell library with CLI executable)  
**Performance Goals**: Record operations complete in under 100ms for records with up to 1000 key-value pairs (interactive development use case)  
**Constraints**: Must maintain 100% test pass rate, zero compilation errors, parse errors must include accurate line/column information  
**Scale/Scope**: 
- **Major refactoring**: Replace entire `Value` type system with gram `Subject.Value` types
- **Map → Record migration**: Replace `VMap`/`MapLiteral` with gram-compatible records
- **Parser integration**: Gram parser delegation for comma-separated record syntax
- **~15 core operations**: Migrated from map operations to record operations
- **Integration**: Records with patterns and quasiquotation
- **Text → String conversion**: Update parser and atoms to use `String` instead of `Text`
- **Codebase-wide changes**: Update all files that use `Value` type (Eval.hs, Primitives.hs, Codec.hs, all tests)

## Constitution Check

*GATE: Must pass before Phase 0 research. Re-check after Phase 1 design.*

### I. Library-First
✅ **PASS**: This refactoring extends the existing pattern-lisp library. No new library creation required. Records replace maps within the existing library structure.

### II. CLI Interface
✅ **PASS**: No CLI changes required. Records are parsed and evaluated through existing CLI interface. Existing text I/O protocol preserved.

### III. Test-First (NON-NEGOTIABLE)
✅ **PASS**: All record functionality will be developed test-first. Tests written before implementation for parser, operations, and integration scenarios.

### IV. Integration Testing
✅ **PASS**: Integration tests required for: record parsing integration with gram parser, record operations, quasiquotation with records, records used with pattern construction functions.

### V. Observability
✅ **PASS**: Parse errors include position information (line/column). Record operations use existing error handling. Text I/O and logging unchanged.

**Gate Status**: ✅ **ALL GATES PASS** - Proceed to Phase 0

### Post-Phase 1 Re-check

*Re-evaluated after Phase 1 design completion.*

### I. Library-First
✅ **PASS**: Feature extends existing pattern-lisp library. No new library creation required.

### II. CLI Interface
✅ **PASS**: No CLI changes required. Records parsed and evaluated through existing CLI interface.

### III. Test-First (NON-NEGOTIABLE)
✅ **PASS**: All record functionality will be developed test-first. Tests written before implementation.

### IV. Integration Testing
✅ **PASS**: Integration tests required for: record parsing with gram parser, records used with pattern functions, quasiquotation support.

### V. Observability
✅ **PASS**: Parse errors include position information. Record operations use existing error handling.

**Post-Phase 1 Gate Status**: ✅ **ALL GATES PASS** - Ready for Phase 2 (task creation)

## Project Structure

### Documentation (this feature)

```text
specs/[###-feature]/
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
├── Syntax.hs              # Add Record type to Value, update Expr if needed
├── Parser.hs              # Add record parser integration with gram parser
├── Eval.hs                # Add record evaluation, operations
├── Primitives.hs          # Add record operation primitives
├── Runtime.hs             # Export record operations in standard library
├── Codec.hs               # Record serialization/deserialization if needed
└── [other existing modules]

test/PatternLisp/
├── ParserSpec.hs          # Record parsing tests
├── EvalSpec.hs            # Record evaluation tests
├── PrimitivesSpec.hs      # Record operation tests
├── IntegrationSpec.hs     # Record integration with patterns, quasiquotation
└── [other existing test modules]
```

**Structure Decision**: Existing single-project Haskell library structure. Major refactoring changes:

**Type System Refactoring (Option B - Gram Types Throughout)**:
- **Replace** `Value` type in `Syntax.hs` to use `Subject.Value` types directly:
  - `VNumber Integer` → `Subject.Value.VInteger Integer`
  - `VString Text` → `Subject.Value.VString String`
  - `VBool Bool` → `Subject.Value.VBoolean Bool`
  - `VMap` → `Subject.Value.VMap (Map String Value)` (for records)
  - Keep `VPattern`, `VClosure`, `VPrimitive` (pattern-lisp specific)
  - Consider `VList` → `Subject.Value.VArray`, `VSet` → keep or `VArray`
- **Update** `Atom` type: `String Text` → `String String` (Text → String conversion)
- **Remove** `MapKey` type (use `String` keys only, matching gram)
- **Replace** `MapLiteral` with `RecordLiteral [(String, Expr)]` in `Syntax.hs`
- **Import** `Subject.Value` module in `Syntax.hs`

**Parser Changes**:
- **Replace** `mapParser` with `recordParser` in `Parser.hs` (delegates to gram parser for comma-separated syntax)
- **Update** `atomParser` to produce `String` instead of `Text` (gram uses `String`)

**Evaluator Changes**:
- **Update** `Eval.hs` to evaluate `RecordLiteral` → `Subject.Value.VMap` (gram's PropertyRecord)
- **Update** all value constructors to use gram types
- **Update** all pattern matching on `Value` to use gram constructors

**Primitives Changes**:
- **Update** all map operations in `Primitives.hs` to work with `Subject.Value.VMap` instead of `VMap`
- **Update** all value type checks to use gram constructors
- **Rename** `hash-map` → `record` in `Runtime.hs` and `Syntax.hs`

**Codec Changes**:
- **Remove** conversion functions (no longer needed - types already aligned)
- **Simplify** serialization (direct use of gram types)

**Test Changes**:
- **Update** all tests to use gram types and record syntax
- **Update** all test expectations to use gram constructors

Records are values - no special subject notation syntax needed

## Complexity Tracking

> **MAJOR REFACTORING** - This is a bold breaking change that:
> 1. **Replaces entire Value type system** with gram `Subject.Value` types throughout runtime
> 2. **Replaces map types** with gram-compatible records using `Subject.Value.VMap`
> 3. **Removes conversion layer** - types already aligned, no conversion needed
> 4. **Updates entire codebase** - all files using `Value` type need updates
> 5. **Text → String migration** - parser and atoms convert to `String` (gram uses `String`)
> 
> **Benefits**:
> - Complete alignment with gram type system
> - No conversion overhead at serialization
> - Future gram features integrate seamlessly
> - Single source of truth for value types
> - Eliminates maintenance burden of conversion layer
> 
> **Trade-offs**:
> - Large refactoring effort upfront
> - Users see gram type names in errors (`VInteger` vs `VNumber`)
> - All existing code breaks (but clean break, no compatibility layer)
> 
> All design decisions follow established patterns and maintain consistency with gram's type system.

## Refactoring Strategy

**Immediate Replacement (No Phased Migration)**:
1. Remove `VMap`, `MapKey`, `MapLiteral` types immediately
2. Add `VRecord`, `RecordLiteral` types
3. Replace parser, evaluator, and all operations in single pass
4. Update all tests to use new syntax and types
5. No backward compatibility layer

**Value Type Conversion Strategy (Option B - Gram Types Throughout)**:
- **Runtime**: Use gram `Subject.Value` types directly (`VInteger`, `VString`, `VBoolean`, `VMap`, etc.)
- **Serialization**: No conversion needed - types already aligned with gram
- **Conversion Functions**: **REMOVE** all conversion functions in `Codec.hs` (no longer needed)
- **Type Aliases**: Consider type alias `type Value = Subject.Value.Value` in `Syntax.hs` for convenience
- **Impact**: 
  - **Large refactoring**: Update all pattern matching, constructors, and type checks throughout codebase
  - **Text → String**: Convert `Text` to `String` in parser/atoms (gram uses `String`)
  - **Complete alignment**: Pattern-lisp runtime types match gram types exactly
  - **No conversion overhead**: Direct use of gram types eliminates conversion layer
  - **User-facing**: Error messages will show gram type names (`VInteger` instead of `VNumber`)
  - **Long-term benefit**: Future gram features integrate seamlessly, no conversion layer to maintain
