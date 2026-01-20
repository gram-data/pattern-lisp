# Contracts: Inline Record Notation

**Date**: 2025-01-30  
**Feature**: Inline Record Notation  
**Type**: **REFACTORING** - Replace existing map support with gram-compatible records

## Overview

This refactoring replaces existing map support with gram-style property records in Pattern Lisp. Contracts are defined as function signatures and behavior specifications for record syntax and operations. Records use gram `Subject.Value.VMap` directly (Option B).

## Syntax Contracts

### Record Literals

**Syntax**: `{ key1: value1, key2: value2 }`

**Contract**:
- Input: Curly braces with comma-separated key-value pairs (gram-style notation) - replaces space-separated map syntax
- Output: `Subject.Value.VMap (Map String Value)` (gram's PropertyRecord type)
- Error: `ParseError` if syntax invalid, duplicate keys, or invalid identifiers

**Examples**:
```lisp
{ name: "Alice", age: 30 }                    ;; => VMap (gram type) with 2 entries
{ person: { name: "Bob" } }                  ;; => Nested record
{ }                                           ;; => Empty record
```

**Duplicate Keys**: Parse error at parse time (not allowed)

**Key Rules**:
- Keys are identifiers (unquoted, gram syntax)
- Keys converted to strings internally
- Values are literals (strings, numbers, booleans, null) or nested records

---

## Using Records with Patterns

Records are values that can be used with existing pattern-lisp functions. Pattern-lisp uses function calls with parentheses `()`, not square bracket notation `[]`.

**Note**: Square brackets like `[Person { ... }]` are gram notation (serialization format), not pattern-lisp syntax. In pattern-lisp, you use function calls like `(pure ...)` or `(pattern ...)` to create patterns.

---

### Quasiquotation with Records

**Syntax**: `` `{ key: ,expr, key2: ,@record } ``

**Contract**:
- Input: Quasiquoted record with unquotes (`,expr`) and splices (`,@record`)
- Output: `Subject.Value.VMap` with evaluated expressions and merged records
- Error: `TypeMismatch` if splicing non-record, evaluation errors propagate

**Examples**:
```lisp
(let ((name "Alice") (age 30))
  `{ name: ,name, age: ,age })  ;; => { name: "Alice", age: 30 }

(let ((base { role: "Engineer" }))
  `{ name: "Alice", ,@base })   ;; => { name: "Alice", role: "Engineer" }
```

---

## Primitive Function Contracts

### Type Predicate

#### `record?`

**Signature**: `(record? value)`

**Contract**:
- Input: Any value
- Output: `true` if value is a record, `false` otherwise
- Error: None

**Examples**:
```lisp
(record? { name: "Alice" })  ;; => true
(record? 42)                 ;; => false
```

---

### Access Operations

#### `record-get`

**Signature**: `(record-get record key)` or `(record-get record key default)`

**Contract**:
- Input: Record, string key, optional default value
- Output: Value at key, or `nil`/default if key doesn't exist
- Error: `TypeMismatch` if first arg is not a record or second arg is not a string

**Examples**:
```lisp
(record-get { name: "Alice" } "name")              ;; => "Alice"
(record-get { name: "Alice" } "age")               ;; => nil
(record-get { name: "Alice" } "age" 0)              ;; => 0 (default)
```

---

#### `record-has?`

**Signature**: `(record-has? record key)`

**Contract**:
- Input: Record, string key
- Output: `true` if key exists, `false` otherwise
- Error: `TypeMismatch` if first arg is not a record or second arg is not a string

**Examples**:
```lisp
(record-has? { name: "Alice" } "name")  ;; => true
(record-has? { name: "Alice" } "age")   ;; => false
```

---

#### `record-keys`

**Signature**: `(record-keys record)`

**Contract**:
- Input: Record
- Output: List of all keys (strings)
- Error: `TypeMismatch` if arg is not a record

**Examples**:
```lisp
(record-keys { name: "Alice", age: 30 })  ;; => ("name" "age")
```

---

#### `record-values`

**Signature**: `(record-values record)`

**Contract**:
- Input: Record
- Output: List of all values
- Error: `TypeMismatch` if arg is not a record

**Examples**:
```lisp
(record-values { name: "Alice", age: 30 })  ;; => ("Alice" 30)
```

---

#### `record->alist`

**Signature**: `(record->alist record)`

**Contract**:
- Input: Record
- Output: Association list `((key1 . value1) (key2 . value2) ...)`
- Error: `TypeMismatch` if arg is not a record

**Examples**:
```lisp
(record->alist { name: "Alice", age: 30 })  ;; => (("name" . "Alice") ("age" . 30))
```

---

### Construction Operations

#### `record`

**Signature**: `(record :key1 value1 :key2 value2 ...)`

**Contract**:
- Input: Alternating keyword keys and values
- Output: Record equivalent to literal `{ key1: value1, key2: value2 }`
- Error: `ArityMismatch` if odd number of args, `TypeMismatch` if non-keywords used as keys

**Examples**:
```lisp
(record :name "Alice" :age 30)  ;; => { name: "Alice", age: 30 }
```

---

#### `alist->record`

**Signature**: `(alist->record alist)`

**Contract**:
- Input: Association list `((key1 . value1) (key2 . value2) ...)`
- Output: Record with same key-value pairs
- Error: `TypeMismatch` if arg is not an association list

**Examples**:
```lisp
(alist->record '(("name" . "Alice") ("age" . 30)))  ;; => { name: "Alice", age: 30 }
```

---

### Transformation Operations

#### `record-set`

**Signature**: `(record-set record key value)`

**Contract**:
- Input: Record, string key, value
- Output: New record with key updated/added (original unchanged)
- Error: `TypeMismatch` if first arg is not a record or second arg is not a string

**Examples**:
```lisp
(record-set { name: "Alice" } "age" 30)  ;; => { name: "Alice", age: 30 }
```

---

#### `record-remove`

**Signature**: `(record-remove record key)`

**Contract**:
- Input: Record, string key
- Output: New record with key removed (original unchanged, unchanged if key doesn't exist)
- Error: `TypeMismatch` if first arg is not a record or second arg is not a string

**Examples**:
```lisp
(record-remove { name: "Alice", age: 30 } "age")  ;; => { name: "Alice" }
```

---

#### `record-merge`

**Signature**: `(record-merge record1 record2)`

**Contract**:
- Input: Two records
- Output: New record containing all keys from both (record2 values take precedence for duplicates)
- Error: `TypeMismatch` if either arg is not a record

**Examples**:
```lisp
(record-merge { name: "Alice" } { age: 30 })           ;; => { name: "Alice", age: 30 }
(record-merge { name: "Alice" } { name: "Bob" })       ;; => { name: "Bob" }
```

---

#### `record-map`

**Signature**: `(record-map function record)`

**Contract**:
- Input: Function `(lambda (key value) new-value)`, record
- Output: New record with function applied to each value (keys unchanged)
- Error: `TypeMismatch` if second arg is not a record, `ArityMismatch` if function doesn't accept 2 args

**Examples**:
```lisp
(record-map (lambda (k v) (* v 2)) { count: 5, total: 10 })  ;; => { count: 10, total: 20 }
```

---

#### `record-filter`

**Signature**: `(record-filter predicate record)`

**Contract**:
- Input: Predicate `(lambda (key value) bool)`, record
- Output: New record containing only entries where predicate returns `true`
- Error: `TypeMismatch` if second arg is not a record, `ArityMismatch` if predicate doesn't accept 2 args

**Examples**:
```lisp
(record-filter (lambda (k v) (> v 5)) { a: 10, b: 3, c: 7 })  ;; => { a: 10, c: 7 }
```

---

## Serialization Contract

### Round-trip Requirement

**Contract**: Records must serialize to valid gram notation and parse back correctly:
- Records serialize as gram property records: `{ key: value, ... }` (direct serialization, no conversion)
- Parsing gram property records produces `Subject.Value.VMap` (records)
- Round-trip preserves all key-value pairs and nested structures

**Error Conditions**:
- Invalid serialization format: `ParseError`
- Type mismatch during deserialization: `TypeMismatch`

---

## Error Contract

All operations follow Pattern Lisp's error handling:
- `TypeMismatch`: Wrong type for operation (e.g., non-record in record operation)
- `ArityMismatch`: Wrong number of arguments
- `ParseError`: Invalid syntax (with position information: line, column)

**Parse Error Examples**:
- Duplicate keys: `ParseError "Duplicate key 'key' in record at line 5, column 10"`
- Unclosed record: `ParseError "Unclosed record, expected '}' at line 3, column 15"`
- Invalid key: `ParseError "Invalid record key at line 2, column 8"`

Error messages must be clear and suggest correct usage.

---

## Immutability Contract

**Contract**: All record operations return new records. Original records are never modified.

**Verification**:
```lisp
(let ((r { name: "Alice" }))
  (record-set r "age" 30)
  r)  ;; => { name: "Alice" } (unchanged)
```

---

## Equality Contract

**Contract**: Records use structural equality. Two records are equal if they have the same keys with equal values, regardless of key ordering.

**Examples**:
```lisp
(= { name: "Alice", age: 30 } { age: 30, name: "Alice" })  ;; => true
(= { name: "Alice" } { name: "Bob" })                     ;; => false
```
