# Data Model: Inline Record Notation

**Date**: 2025-01-30  
**Feature**: Inline Record Notation

## Entities

### Record

**Type**: Uses gram `Subject.Value.VMap (Map String Value)` directly (Option B - gram types throughout)

**Structure**:
```haskell
-- Records use gram's VMap type directly (no separate VRecord type)
-- Subject.Value.VMap (Map String Value)  -- This replaces VMap
```

**Properties**:
- Keys: `String` (converted from gram identifiers during parsing)
- Values: `Value` (any value type: strings, numbers, booleans, null, nested records)

**Validation Rules**:
- Keys must be valid identifiers according to gram syntax rules
- Duplicate keys produce parse error at parse time (not allowed)
- Records are immutable (operations return new records)
- Structural equality: two records are equal if they have same keys with equal values (regardless of key ordering)
- Keys are strings (not symbols) for gram compatibility

**State Transitions**: None - records are immutable. Operations (`record-set`, `record-remove`, `record-merge`) return new records.

**Relationships**:
- Keys are strings (converted from gram identifiers)
- Values are gram `Subject.Value` types (including other records for nesting)
- Used as property records in Pattern Subjects (`Subject.properties`)
- Can be elements in sets (if `VSet` is kept) or arrays (`Subject.Value.VArray`)
- **Replaces** the old `VMap (Map MapKey Value)` type

**Operations**:
- `record?`: Type predicate to check if value is a record
- `record-get`: Get value by key (with optional default)
- `record-has?`: Check if key exists
- `record-keys`: Get all keys
- `record-values`: Get all values
- `record->alist`: Convert to association list
- `alist->record`: Convert from association list
- `record-set`: Add/update key-value pair (returns new record)
- `record-remove`: Remove key (returns new record)
- `record-merge`: Merge two records (right takes precedence, returns new record)
- `record-map`: Map function over values
- `record-filter`: Filter entries by predicate

---

### Record Literal

**Type**: `Expr` variant (AST representation)

**Structure**:
```haskell
data Expr = ...
          | RecordLiteral [(String, Expr)]  -- Keys and value expressions
```

**Properties**:
- Keys: `String` (parsed from gram identifiers)
- Values: `Expr` (may contain unquotes for quasiquotation)

**Validation Rules**:
- Keys must be valid identifiers
- Duplicate keys produce parse error
- Values are expressions (evaluated during evaluation phase)
- Supports unquote (`Unquote Expr`) and unquote-splice (`UnquoteSplice Expr`) for quasiquotation

**State Transitions**: 
- Parsing: Gram identifiers → String keys
- Evaluation: `RecordLiteral` → `Subject.Value.VMap (Map String Value)` (gram's PropertyRecord)

**Relationships**:
- Parsed by gram parser (delegated from Megaparsec)
- Evaluated to `VRecord` during evaluation phase
- Can be used as values anywhere in pattern-lisp code
- Can appear in quasiquotation contexts
- Can be passed as arguments to pattern construction functions

---

## Using Records with Patterns

Records are `Subject.Value.VMap` values (gram's PropertyRecord) that can be used with existing pattern-lisp functions. When records are used with pattern construction functions, they can be used directly as `Subject.properties` for gram serialization (no conversion needed - types already aligned).

**Note**: Pattern-lisp uses function calls with parentheses `()`, not square bracket notation `[]`. Square brackets like `[Label { ... }]` are gram notation (serialization format), not pattern-lisp syntax.

---

## Type Hierarchy

```
Value (gram Subject.Value types - Option B)
├── VInteger Integer              [REPLACES VNumber]
├── VString String                [REPLACES VString Text]
├── VBoolean Bool                 [REPLACES VBool]
├── VSymbol String                [for symbols]
├── VMap (Map String Value)       [REPLACES VMap - used for records]
├── VArray [Value]                [REPLACES VList]
├── VSet (Set Value)              [may keep or convert to VArray]
├── VPattern (Pattern Subject)    [pattern-lisp specific]
├── VClosure Closure              [pattern-lisp specific]
└── VPrimitive Primitive          [pattern-lisp specific]

Expr
├── Atom Atom
├── List [Expr]
├── SetLiteral [Expr]
├── RecordLiteral [(String, Expr)]  [REPLACES MapLiteral]
├── Quote Expr
├── Unquote Expr                    [NEW - for quasiquotation]
└── UnquoteSplice Expr              [NEW - for splicing]
```

**Key Constraints**:
- Record keys are strings (not symbols, not keywords) for gram compatibility
- Records are immutable (functional semantics)
- Structural equality works regardless of key ordering
- Records can be nested (values can be other records)

---

## Serialization Model

### Records
- **To Subject**: Records ARE `PropertyRecord` (`Subject.Value.VMap (Map String Value)`) - no conversion needed
- **From Subject**: `PropertyRecord` IS the record type - direct use
- **To Gram**: Records serialize as gram property records: `{ key: value, ... }` (direct serialization, no conversion)
- **From Gram**: Gram property records parse to `Subject.Value.VMap` (records)

**Round-trip Requirement**: Records must serialize to valid gram notation and parse back correctly.

### Records in Pattern Serialization
- **To Gram**: Pattern-lisp `Subject.Value.VMap` values (records) ARE `Subject.properties` - direct use, no conversion
- **From Gram**: Gram property records `{key: value}` parse directly to `Subject.Value.VMap` (records)

**Round-trip Requirement**: Records must serialize to valid gram notation and parse back correctly. Note: Pattern-lisp syntax uses spaces after colons (`{ key: value }`), while gram notation typically omits them (`{key: value}`), but both are valid.

---

## Error Conditions

### Parse Errors
- Duplicate keys in record: `ParseError "Duplicate key 'key' in record"` with position
- Invalid record syntax: `ParseError "Expected {key: value, ...}"` with position
- Unclosed record: `ParseError "Unclosed record, expected '}'"` with position
- Invalid key (not identifier): `ParseError "Invalid record key"` with position
- Gram parser errors: Translated to pattern-lisp parse errors with position information

### Type Errors
- Using non-record in record operation: `TypeMismatch "Expected record, got <type>"`
- Splicing non-record in quasiquotation: `TypeMismatch "Expected record for splicing, got <type>"`

### Runtime Errors
- Key not found (without default): Returns `nil` or error depending on operation
- Invalid operation: `TypeMismatch` with descriptive message
- Nested record access: Supported via `record-get` and nested operations

---

## Validation Rules Summary

1. **Keys**: Must be valid gram identifiers, converted to strings
2. **Duplicate Keys**: Parse error at parse time
3. **Immutability**: All operations return new records
4. **Equality**: Structural equality (same keys, equal values, order-independent)
5. **Nesting**: Records can contain other records as values
6. **Quasiquotation**: Unquote and splice supported in record literals
7. **Subject Integration**: Records attach as `Subject.properties`
8. **Serialization**: Records serialize to gram notation, round-trip correctly
