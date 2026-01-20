# Gram Schema Analysis: Record Support Validation

**Date**: 2025-01-30  
**Purpose**: Validate that records and record values are fully supported in gram notation schema

## Schema Overview

The gram schema defines a `Pattern<Subject>` structure where:
- **Subject** has `properties` field: `Map String Value` (PropertyRecord)
- **Value** is a union type supporting multiple value types including maps/objects

## Record Support Analysis

### 1. Subject.properties (PropertyRecord)

From schema:
```json
"properties": {
    "additionalProperties": {
        "$ref": "#/$defs/Value"
    },
    "default": {},
    "description": "Map of property names to values",
    "type": "object"
}
```

✅ **FULLY SUPPORTED**: 
- Properties is a map/object with string keys
- Values can be any `Value` type
- This is exactly `PropertyRecord = Map String Value` in gram-hs

### 2. Value Type - Map/Object Support

From schema:
```json
{
    "additionalProperties": {
        "$ref": "#/$defs/Value"
    },
    "description": "Map of string keys to values (no 'type' discriminator)",
    "not": {
        "required": ["type"]
    },
    "type": "object"
}
```

✅ **FULLY SUPPORTED**:
- Maps are plain objects (not discriminated union types)
- String keys only
- Values are recursive `Value` types
- **No "type" field required** - this is a plain map, not a tagged type
- This matches `Subject.Value.VMap (Map String Value)` in gram-hs

### 3. Nested Records

✅ **FULLY SUPPORTED**:
- Value can be a map
- Map values can be Values
- Values can be maps
- **Recursive nesting is fully supported**

Example (valid gram):
```json
{
    "subject": {
        "identity": "",
        "labels": ["Person"],
        "properties": {
            "name": "Alice",
            "address": {
                "street": "123 Main",
                "city": "NYC"
            }
        }
    },
    "elements": []
}
```

### 4. All Value Types Supported

The Value union type includes:
- ✅ `integer` → `Subject.Value.VInteger Integer`
- ✅ `number` → `Subject.Value.VDecimal Double` (decimal/floating-point)
- ✅ `boolean` → `Subject.Value.VBoolean Bool`
- ✅ `string` → `Subject.Value.VString String`
- ✅ `Symbol` → `Subject.Value.VSymbol String`
- ✅ `TaggedString` → `Subject.Value.VTaggedString String String`
- ✅ `array` → `Subject.Value.VArray [Value]`
- ✅ `object/map` → `Subject.Value.VMap (Map String Value)` **← Records use this**
- ✅ `Range` → `Subject.Value.VRange RangeValue`
- ✅ `Measurement` → `Subject.Value.VMeasurement String Double`

### 5. Pattern-Lisp Value Type Mapping

**Current Plan (Option B - Gram Types Throughout)**:

| Pattern-Lisp (Current) | Gram Schema | Gram-HS Type | Status |
|------------------------|-------------|--------------|--------|
| `VNumber Integer` | `integer` | `VInteger Integer` | ✅ Supported |
| `VString Text` | `string` | `VString String` | ✅ Supported (Text→String) |
| `VBool Bool` | `boolean` | `VBoolean Bool` | ✅ Supported |
| `VMap` | `object` | `VMap (Map String Value)` | ✅ Supported (for records) |
| `VList [Value]` | `array` | `VArray [Value]` | ✅ Supported |
| `VSet (Set Value)` | `array` | `VArray [Value]` | ⚠️ Conversion needed |
| `VPattern` | `Pattern` | `Pattern Subject` | ✅ Supported |
| `VClosure` | N/A | N/A | ⚠️ Pattern-lisp specific |
| `VPrimitive` | N/A | N/A | ⚠️ Pattern-lisp specific |

### 6. Record Syntax Validation

**Pattern-Lisp Record Syntax**:
```lisp
{name: "Alice", age: 30, address: {street: "123 Main", city: "NYC"}}
```

**Gram Serialization** (as PropertyRecord):
```json
{
    "name": "Alice",
    "age": 30,
    "address": {
        "street": "123 Main",
        "city": "NYC"
    }
}
```

✅ **FULLY SUPPORTED**: 
- Comma-separated key-value pairs
- String keys (from identifiers)
- Nested records (maps within maps)
- All value types as record values

### 7. Edge Cases

✅ **Empty Records**: `{}` → `{}` (empty object) - **Supported**
✅ **Duplicate Keys**: Should cause parse error (gram parser handles this)
✅ **Nested Depth**: No limit in schema - **Fully recursive**
✅ **Mixed Value Types**: All Value types can be record values - **Supported**

## Conclusion

✅ **RECORDS ARE FULLY SUPPORTED** in gram notation schema:

1. **PropertyRecord**: `Subject.properties` is `Map String Value` - exactly what records need
2. **Record Values**: `Value` type includes `object/map` type with string keys
3. **Nested Records**: Fully recursive - maps can contain maps
4. **All Value Types**: All gram value types can be used as record values
5. **No Type Discriminator**: Maps are plain objects, not tagged types - perfect for records

## Validation for Refactoring Plan

**Option B (Gram Types Throughout) is VALIDATED**:
- ✅ `Subject.Value.VMap (Map String Value)` matches gram schema `object` type
- ✅ `Subject.Value.VInteger`, `VString`, `VBoolean` match gram schema value types
- ✅ Nested records fully supported via recursive Value type
- ✅ All pattern-lisp value types can map to gram value types (except pattern-lisp specific: Closure, Primitive)

**Recommendation**: Proceed with Option B refactoring - gram schema fully supports records and all value types.
