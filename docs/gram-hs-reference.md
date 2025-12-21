# Gram-HS Reference: Pattern and Subject Types

This document provides reference information about the gram-hs library types and their relationship, based on the actual implementation in `../gram-hs`.

## Core Types

### Pattern v

**Type**: `Pattern v` from `Pattern.Core`

**Structure**:
```haskell
data Pattern v = Pattern
  { value    :: v              -- Decoration about what kind of pattern it is
  , elements :: [Pattern v]     -- The pattern itself, as a sequence of elements
  }
```

**Key Points**:
- **Generic type**: `Pattern v` can be decorated with any type `v`
- **Recursive structure**: Elements are themselves `Pattern v`, enabling arbitrary nesting
- **Sequence semantic**: The `elements` field IS the pattern - it contains the sequence
- **Tree implementation**: Represented as a recursive tree in memory

**Operations**:
- `pattern :: v -> Pattern v` - Create atomic pattern (no elements)
- `patternWith :: v -> [Pattern v] -> Pattern v` - Create pattern with elements
- `value :: Pattern v -> v` - Extract decoration
- `elements :: Pattern v -> [Pattern v]` - Extract elements

### Subject

**Type**: `Subject` from `Subject.Core`

**Structure**:
```haskell
data Subject = Subject
  { identity   :: Symbol        -- Unique identifier (always required)
  , labels     :: Set String    -- Classification labels
  , properties :: PropertyRecord -- Key-value map (Map String Value)
  }
```

**Key Points**:
- **Serializable value type**: Subject is designed for serialization to gram notation
- **No elements field**: Subject does NOT have an `elements` field
- **Properties only**: Nested data must be stored in the `properties` map
- **PropertyRecord**: Alias for `Map String Value` where `Value` is from `Subject.Value`

**Subject.Value Types**:
- `VInteger Integer`
- `VDecimal Double`
- `VBoolean Bool`
- `VString String`
- `VSymbol String`
- `VTaggedString String String`
- `VArray [Value]` - For arrays of values
- `VMap (Map String Value)` - For nested maps
- `VRange RangeValue`
- `VMeasurement String Double`

### Pattern Subject

**Type**: `Pattern Subject` - The specific combination that gram notation supports

**Key Points**:
- **Gram notation limitation**: Gram notation ONLY supports `Pattern Subject`, not generic `Pattern v`
- **Serialization**: `Pattern Subject` can be serialized directly to gram notation
- **Structure**: Pattern decorated with Subject values, where elements are also `Pattern Subject`

## Serialization Strategy

### Pattern Subject → Gram Notation

Gram notation serializes `Pattern Subject` directly:
- The Pattern structure (value + elements) is preserved
- The decoration (`Subject`) is serialized with its identity, labels, and properties
- Elements are serialized recursively as `Pattern Subject`

### Pattern Lisp Value → Subject

When serializing Pattern Lisp values (including `VPattern (Pattern Subject)`) to Subject representation:

**Challenge**: Subject doesn't have an `elements` field, so we cannot directly represent the Pattern structure.

**Solution**: Store Pattern structure in Subject properties:
- Use `VMap` to represent nested structures
- Use `VArray` to represent lists of values
- Recursively serialize Pattern elements as Subject values stored in properties

**Serialization Format for VPattern**:
- Label: "Pattern"
- Properties:
  - `decoration`: Subject (the Pattern's decoration)
  - `elements`: VArray of Subject values (recursively serialized Pattern elements)

**Deserialization**: 
- Extract decoration from `decoration` property
- Extract elements from `elements` property (VArray)
- Reconstruct Pattern recursively

## Important Distinctions

1. **Pattern vs Subject**:
   - Pattern is a generic recursive structure with `elements`
   - Subject is a serializable value type without `elements`
   - `Pattern Subject` combines both: Pattern structure with Subject decorations

2. **Gram Notation**:
   - Only supports `Pattern Subject`, not generic patterns
   - Serializes Pattern structure directly (not as Subject properties)
   - Pattern Lisp serialization to Subject is different from gram notation

3. **Serialization Context**:
   - **Gram serialization**: `Pattern Subject → String` (gram notation)
   - **Pattern Lisp serialization**: `Value → Subject` (for code-as-data, persistence)

## References

- `../gram-hs/libs/pattern/src/Pattern/Core.hs` - Pattern type definition
- `../gram-hs/libs/subject/src/Subject/Core.hs` - Subject type definition
- `../gram-hs/libs/gram/src/Gram/Serialize.hs` - Gram notation serialization
- `../gram-hs/design/DESIGN.md` - Pattern design philosophy

