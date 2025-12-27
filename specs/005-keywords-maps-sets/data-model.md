# Data Model: Keywords, Maps, and Sets

**Date**: 2025-01-27  
**Feature**: Keywords, Maps, and Sets

## Entities

### Keyword

**Type**: `Atom` variant and `Value` variant

**Structure**:
```haskell
data Atom = ...
         | Keyword String    -- Postfix colon syntax: name:

data Value = ...
          | VKeyword String  -- Self-evaluating keyword value
```

**Properties**:
- `String`: The keyword name (without colon, e.g., `"name"` for `name:`)

**Validation Rules**:
- Keywords must have postfix colon syntax (`symbol:`)
- Keywords are self-evaluating (no environment lookup)
- Keywords are distinct from symbols (type safety)

**State Transitions**: None - keywords are immutable values

**Relationships**:
- Used as keys in maps (`VMap (Map VKeyword Value)`)
- Can be elements in sets (`VSet (Set Value)`)

---

### Map

**Type**: `Value` variant

**Structure**:
```haskell
data Value = ...
          | VMap (Map.Map VKeyword Value)  -- Map with keyword keys
```

**Properties**:
- Keys: `VKeyword` (only keywords allowed as keys)
- Values: `Value` (any value type, including nested maps)

**Validation Rules**:
- Keys must be keywords (type checked)
- Duplicate keys: last value wins (silently overwrites)
- Maps are immutable (operations return new maps)

**State Transitions**: None - maps are immutable. Operations (`assoc`, `dissoc`, `update`) return new maps.

**Relationships**:
- Keys are keywords (`VKeyword`)
- Values can be any `Value` type (including other maps for nesting)
- Can be elements in sets (`VSet (Set Value)`)

**Operations**:
- `get`: Retrieve value by keyword key
- `get-in`: Nested access via keyword path
- `assoc`: Add/update key-value pair
- `dissoc`: Remove key
- `update`: Apply function to value at key
- `contains?`: Check if key exists
- `empty?`: Check if map is empty
- `hash-map`: Constructor function

---

### Set

**Type**: `Value` variant

**Structure**:
```haskell
data Value = ...
          | VSet (Set.Set Value)  -- Set of any values
```

**Properties**:
- Elements: `Value` (any value type: numbers, strings, keywords, maps, other sets)

**Validation Rules**:
- Sets are unordered (order not preserved)
- Duplicate elements automatically removed
- Sets are immutable (operations return new sets)

**State Transitions**: None - sets are immutable. Operations (`set-union`, `set-intersection`, etc.) return new sets.

**Relationships**:
- Elements can be any `Value` type
- Used for Subject labels (`Set String` - sets of strings)
- Can contain other sets (nested sets)

**Operations**:
- `contains?`: Check membership
- `set-union`: Union of two sets
- `set-intersection`: Intersection of two sets
- `set-difference`: Elements in first but not second
- `set-symmetric-difference`: Elements in either but not both
- `set-subset?`: Check if first is subset of second
- `set-equal?`: Check if sets contain same elements
- `empty?`: Check if set is empty
- `hash-set`: Constructor function

---

### Subject Label

**Type**: `String` (not a separate entity)

**Structure**: Plain strings in sets (`Set String`)

**Properties**:
- String value (e.g., `"Person"`, `"Employee"`)

**Validation Rules**:
- Subject labels are `Set String` (unordered, unique)
- Plain strings in sets are sufficient: `#{"Person" "Employee"}`
- Prefix colon syntax (`:Person`) is optional and deferred

**State Transitions**: None - strings are immutable

**Relationships**:
- Used in sets: `VSet (Set.Set Value)` where elements are `VString`
- Subject labels in gram patterns are `Set String`

---

## Type Hierarchy

```
Value
├── VNumber Integer
├── VString Text
├── VBool Bool
├── VKeyword String          [NEW]
├── VMap (Map VKeyword Value) [NEW]
├── VSet (Set Value)         [NEW]
├── VList [Value]
├── VPattern (Pattern Subject)
├── VClosure Closure
└── VPrimitive Primitive
```

**Key Constraints**:
- Map keys must be `VKeyword` (type safety)
- Set elements can be any `Value` type
- Maps and sets are immutable (functional semantics)

---

## Serialization Model

### Keywords
- **To Subject**: Convert to `VString` (may use marker like `":keyword:name"` or `VTaggedString`)
- **From Subject**: Detect marker and reconstruct `VKeyword`

### Maps
- **To Subject**: Convert `Map VKeyword Value` to `Map String Value` (keywords → strings)
- **From Subject**: Convert `Map String Value` to `Map VKeyword Value` (strings → keywords, validate)

### Sets
- **To Subject**: Convert `Set Value` to `VArray [Value]` (set → list → array)
- **From Subject**: Convert `VArray [Value]` to `Set Value` (array → list → set, remove duplicates)

**Round-trip Requirement**: Serialization must preserve types (keywords remain keywords, not strings).

---

## Error Conditions

### Type Errors
- Using non-keyword as map key: `TypeMismatch "Expected keyword, got symbol"`
- Type mismatch in operations: `TypeMismatch "Expected map, got list"`

### Syntax Errors
- Invalid keyword syntax: `ParseError "Expected symbol: for keyword"`
- Invalid map syntax: `ParseError "Expected {key: value ...}"`
- Invalid set syntax: `ParseError "Expected #{...}"`

### Runtime Errors
- Key not found (with `get`): Returns `nil` (not an error)
- Invalid operation: `TypeMismatch` with descriptive message

