# Contracts: Keywords, Maps, and Sets

**Date**: 2025-01-27  
**Feature**: Keywords, Maps, and Sets

## Overview

This feature adds foundational language constructs to Pattern Lisp. Since this is a language feature (not an API), contracts are defined as function signatures and behavior specifications for the new primitives and syntax.

## Syntax Contracts

### Keywords

**Syntax**: `symbol:` (postfix colon)

**Contract**:
- Input: Symbol followed by colon (e.g., `name:`)
- Output: `VKeyword String` (self-evaluating, no environment lookup)
- Error: `ParseError` if syntax invalid

**Examples**:
```lisp
name:        ;; => VKeyword "name"
age:         ;; => VKeyword "age"
```

---

### Map Literals

**Syntax**: `{key1: value1 key2: value2 ...}`

**Contract**:
- Input: Curly braces with key-value pairs (keywords as keys)
- Output: `VMap (Map VKeyword Value)`
- Error: `ParseError` if syntax invalid, `TypeMismatch` if non-keyword used as key

**Examples**:
```lisp
{name: "Alice" age: 30}                    ;; => VMap with 2 entries
{user: {name: "Bob" email: "bob@example"}}  ;; => Nested map
```

**Duplicate Keys**: Last value wins (silently overwrites earlier values)

---

### Set Literals

**Syntax**: `#{element1 element2 ...}`

**Contract**:
- Input: Hash set syntax with elements
- Output: `VSet (Set Value)` (duplicates removed, unordered)
- Error: `ParseError` if syntax invalid

**Examples**:
```lisp
#{1 2 3}                    ;; => VSet with numbers
#{"Person" "Employee"}      ;; => VSet with strings (Subject labels)
#{name: age: active:}        ;; => VSet with keywords
```

---

## Primitive Function Contracts

### Map Operations

#### `get`

**Signature**: `(get map key:)` or `(get map key: default)`

**Contract**:
- Input: Map, keyword key, optional default value
- Output: Value at key, or `nil`/default if key doesn't exist
- Error: `TypeMismatch` if first arg is not a map or second arg is not a keyword

**Examples**:
```lisp
(get {name: "Alice"} name:)              ;; => "Alice"
(get {name: "Alice"} age:)               ;; => nil
(get {name: "Alice"} age: 0)             ;; => 0 (default)
```

---

#### `get-in`

**Signature**: `(get-in map [key1: key2: ...])`

**Contract**:
- Input: Map, list of keyword keys (path)
- Output: Value at end of path, or `nil` if any key in path doesn't exist
- Error: `TypeMismatch` if first arg is not a map or path is not a list of keywords

**Examples**:
```lisp
(get-in {user: {name: "Alice"}} [user: name:])  ;; => "Alice"
(get-in {user: {name: "Alice"}} [user: age:])  ;; => nil
```

---

#### `assoc`

**Signature**: `(assoc map key: value)`

**Contract**:
- Input: Map, keyword key, value
- Output: New map with key updated/added
- Error: `TypeMismatch` if first arg is not a map or second arg is not a keyword

**Examples**:
```lisp
(assoc {name: "Alice"} age: 30)  ;; => {name: "Alice" age: 30}
```

---

#### `dissoc`

**Signature**: `(dissoc map key:)`

**Contract**:
- Input: Map, keyword key
- Output: New map with key removed (unchanged if key doesn't exist)
- Error: `TypeMismatch` if first arg is not a map or second arg is not a keyword

**Examples**:
```lisp
(dissoc {name: "Alice" age: 30} age:)  ;; => {name: "Alice"}
```

---

#### `update`

**Signature**: `(update map key: function)`

**Contract**:
- Input: Map, keyword key, function
- Output: New map with key updated by applying function to current value (creates key with function applied to `nil` if key doesn't exist)
- Error: `TypeMismatch` if first arg is not a map or second arg is not a keyword

**Examples**:
```lisp
(update {count: 5} count: inc)  ;; => {count: 6}
(update {} count: inc)          ;; => {count: 1} (if inc treats nil as 0)
```

---

#### `contains?` (map)

**Signature**: `(contains? map key:)`

**Contract**:
- Input: Map, keyword key
- Output: `true` if key exists, `false` otherwise
- Error: `TypeMismatch` if first arg is not a map or second arg is not a keyword

**Examples**:
```lisp
(contains? {name: "Alice"} name:)  ;; => true
(contains? {name: "Alice"} age:)   ;; => false
```

---

#### `empty?` (map)

**Signature**: `(empty? map)`

**Contract**:
- Input: Map
- Output: `true` if map has no keys, `false` otherwise
- Error: `TypeMismatch` if arg is not a map

**Examples**:
```lisp
(empty? {})           ;; => true
(empty? {name: "A"})  ;; => false
```

---

#### `hash-map`

**Signature**: `(hash-map key1: val1 key2: val2 ...)`

**Contract**:
- Input: Alternating keyword keys and values
- Output: Map equivalent to literal `{key1: val1 key2: val2 ...}`
- Error: `ArityMismatch` if odd number of args, `TypeMismatch` if non-keywords used as keys

**Examples**:
```lisp
(hash-map name: "Alice" age: 30)  ;; => {name: "Alice" age: 30}
```

---

### Set Operations

#### `contains?` (set)

**Signature**: `(contains? set element)`

**Contract**:
- Input: Set, element
- Output: `true` if element in set, `false` otherwise
- Error: `TypeMismatch` if first arg is not a set

**Examples**:
```lisp
(contains? #{1 2 3} 2)  ;; => true
(contains? #{1 2 3} 4)  ;; => false
```

---

#### `set-union`

**Signature**: `(set-union set1 set2)`

**Contract**:
- Input: Two sets
- Output: Set containing all elements from both sets
- Error: `TypeMismatch` if either arg is not a set

**Examples**:
```lisp
(set-union #{1 2} #{2 3})  ;; => #{1 2 3}
```

---

#### `set-intersection`

**Signature**: `(set-intersection set1 set2)`

**Contract**:
- Input: Two sets
- Output: Set containing elements present in both sets
- Error: `TypeMismatch` if either arg is not a set

**Examples**:
```lisp
(set-intersection #{1 2 3} #{2 3 4})  ;; => #{2 3}
```

---

#### `set-difference`

**Signature**: `(set-difference set1 set2)`

**Contract**:
- Input: Two sets
- Output: Set containing elements in set1 but not in set2
- Error: `TypeMismatch` if either arg is not a set

**Examples**:
```lisp
(set-difference #{1 2 3} #{2})  ;; => #{1 3}
```

---

#### `set-symmetric-difference`

**Signature**: `(set-symmetric-difference set1 set2)`

**Contract**:
- Input: Two sets
- Output: Set containing elements in either set but not both
- Error: `TypeMismatch` if either arg is not a set

**Examples**:
```lisp
(set-symmetric-difference #{1 2} #{2 3})  ;; => #{1 3}
```

---

#### `set-subset?`

**Signature**: `(set-subset? set1 set2)`

**Contract**:
- Input: Two sets
- Output: `true` if all elements of set1 are in set2, `false` otherwise
- Error: `TypeMismatch` if either arg is not a set

**Examples**:
```lisp
(set-subset? #{1 2} #{1 2 3})  ;; => true
```

---

#### `set-equal?`

**Signature**: `(set-equal? set1 set2)`

**Contract**:
- Input: Two sets
- Output: `true` if sets contain same elements (order doesn't matter), `false` otherwise
- Error: `TypeMismatch` if either arg is not a set

**Examples**:
```lisp
(set-equal? #{1 2 3} #{3 2 1})  ;; => true
```

---

#### `empty?` (set)

**Signature**: `(empty? set)`

**Contract**:
- Input: Set
- Output: `true` if set has no elements, `false` otherwise
- Error: `TypeMismatch` if arg is not a set

**Examples**:
```lisp
(empty? #{})        ;; => true
(empty? #{1 2 3})   ;; => false
```

---

#### `hash-set`

**Signature**: `(hash-set element1 element2 ...)`

**Contract**:
- Input: Elements (any number)
- Output: Set equivalent to literal `#{element1 element2 ...}`
- Error: None (empty set if no args)

**Examples**:
```lisp
(hash-set 1 2 3)  ;; => #{1 2 3}
```

---

## Serialization Contract

### Round-trip Requirement

**Contract**: Serialization and deserialization must preserve types:
- Keywords remain keywords (not converted to strings)
- Maps remain maps (keys remain keywords)
- Sets remain sets (order may change, duplicates removed)

**Error Conditions**:
- Invalid serialization format: `ParseError`
- Type mismatch during deserialization: `TypeMismatch`

---

## Error Contract

All operations follow Pattern Lisp's error handling:
- `TypeMismatch`: Wrong type for operation
- `ArityMismatch`: Wrong number of arguments
- `ParseError`: Invalid syntax (with position information)

Error messages must be clear and suggest correct usage.

