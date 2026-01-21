# Pattern Lisp Records

**Last Updated**: 2025-01-30

Records are immutable key-value structures in Pattern Lisp, using gram-compatible notation. Records replace the previous map syntax and provide a complete set of operations for data manipulation.

## Syntax

Records use curly braces with comma-separated key-value pairs:

```lisp
{name: "Alice", age: 30}                    ;; Basic record
{user: {name: "Bob", email: "bob@example.com"}}  ;; Nested record
{}                                           ;; Empty record
```

### Key Syntax

Keys can be:
- **Identifiers**: `{name: "Alice"}` - automatically converted to strings
- **Quoted strings**: `{"user-id": 123}` - useful for keys with special characters
- **Mixed**: `{name: "Alice", "user-id": 123}` - both styles in one record

Both single (`:`) and double (`::`) colons are supported for gram compatibility:
- `{name: "Alice"}` - single colon
- `{name:: "Alice"}` - double colon (gram-compatible)

### Value Types

Record values can be any Pattern Lisp value:
- Numbers: `{count: 42}`
- Strings: `{name: "Alice"}`
- Booleans: `{active: true}`
- Arrays: `{tags: ["admin", "user"]}`
- Nested records: `{user: {name: "Bob"}}`
- Any other Pattern Lisp value

## Basic Operations

### Type Checking

```lisp
(record? {name: "Alice"})    ;; => true
(record? "not a record")      ;; => false
```

### Accessing Values

```lisp
(get {name: "Alice", age: 30} "name")           ;; => "Alice"
(get {name: "Alice"} "email" "unknown")         ;; => "unknown" (default value)
(get {name: "Alice"} "email")                   ;; => () (nil if no default)
```

### Checking for Keys

```lisp
(has? {name: "Alice", age: 30} "name")   ;; => true
(has? {name: "Alice", age: 30} "email")  ;; => false
```

### Getting Keys and Values

```lisp
(keys {name: "Alice", age: 30})    ;; => ("name" "age")
(values {name: "Alice", age: 30})  ;; => ("Alice" 30)
```

## Modification Operations

All modification operations return **new records** - original records are immutable.

### Adding/Updating Keys

```lisp
(assoc {name: "Alice"} "age" 30)              ;; => {name: "Alice", age: 30}
(assoc {name: "Alice", age: 30} "age" 31)     ;; => {name: "Alice", age: 31}
```

### Removing Keys

```lisp
(dissoc {name: "Alice", age: 30} "age")  ;; => {name: "Alice"}
```

### Merging Records

```lisp
(merge {name: "Alice", age: 30} {age: 31, city: "NYC"})
;; => {name: "Alice", age: 31, city: "NYC"}
;; Note: Right record takes precedence
```

## Transformation Operations

### Mapping Over Records

```lisp
(map (lambda (k v) (* v 2)) {a: 1, b: 2, c: 3})
;; => {a: 2, b: 4, c: 6}
```

The mapping function receives two arguments: the key (as a string) and the value.

### Filtering Records

```lisp
(filter (lambda (k v) (> v 2)) {a: 1, b: 2, c: 3, d: 4})
;; => {c: 3, d: 4}
```

The filter function receives two arguments (key, value) and must return a boolean.

## Conversion Operations

### Record to Association List

```lisp
(record->alist {name: "Alice", age: 30})
;; => (("name" "Alice") ("age" 30))
```

### Association List to Record

```lisp
(alist->record (("name" "Alice") ("age" 30)))
;; => {name: "Alice", age: 30}
```

## Programmatic Creation

### Using the `record` Constructor

```lisp
(record "name" "Alice" "age" 30)
;; => {name: "Alice", age: 30}
```

Takes alternating key-value pairs. Requires an even number of arguments.

## Quasiquotation

Records support quasiquotation for dynamic construction:

### Unquoting Values

```lisp
(let ((name "Alice")
      (age 30))
  `{name: ,name, age: ,age})
;; => {name: "Alice", age: 30}
```

### Splicing Records

```lisp
(let ((base {role: "Engineer"})
      (extra {name: "Alice", age: 30}))
  `{,@base, ,@extra})
;; => {role: "Engineer", name: "Alice", age: 30}
```

### Combined Unquoting and Splicing

```lisp
(let ((name "Alice")
      (metadata {age: 30, city: "NYC"}))
  `{name: ,name, ,@metadata})
;; => {name: "Alice", age: 30, city: "NYC"}
```

## Nested Records

Records can be nested to any depth:

```lisp
{user: {name: "Bob", email: "bob@example.com"},
 config: {debug: true, verbose: false}}
```

Access nested values:

```lisp
(get (get {user: {name: "Bob"}} "user") "name")  ;; => "Bob"
```

## Real-World Examples

### User Profile

```lisp
{id: 1,
 name: "Alice",
 email: "alice@example.com",
 roles: ["admin", "user"],
 settings: {theme: "dark", notifications: true}}
```

### Configuration Object

```lisp
{server: {host: "localhost", port: 8080},
 database: {host: "db.example.com", port: 5432},
 features: {logging: true, caching: false}}
```

### Pattern Subject Representation

```lisp
{identity: "person-123",
 labels: ["Person", "Employee"],
 properties: {name: "Bob", department: "Engineering"}}
```

## Gram Syntax Compatibility

Records in Pattern Lisp are **100% gram-compatible**. You can copy records from `.gram` files into `.plisp` files with no changes:

```gram
{name: "Alice", age: 30, tags: ["admin", "user"]}
```

This exact syntax works in both gram and Pattern Lisp.

### Supported Gram Features

- ✅ Comma-separated key-value pairs
- ✅ Both single (`:`) and double (`::`) colons
- ✅ String keys (quoted)
- ✅ Identifier keys (unquoted)
- ✅ Nested records
- ✅ Arrays as values: `[1, 2, 3]`
- ✅ All gram value types (integers, decimals, strings, booleans, arrays, nested records)

### Pattern-Lisp Specific Features

These features work in Pattern Lisp but are not part of gram notation:
- **Unquotes**: `,expr` - evaluate expression dynamically
- **Splices**: `,@expr` - merge records dynamically

When records with these features are serialized to gram, the unquotes and splices are evaluated first, so the resulting gram contains only pure values.

## Complete Operation Reference

### Type Checking

| Function | Signature | Description |
|---------|-----------|-------------|
| `record?` | `(record? value)` | Returns `true` if value is a record |

### Access Operations

| Function | Signature | Description |
|---------|-----------|-------------|
| `get` | `(get record key [default])` | Get value by key, with optional default |
| `has?` | `(has? record key)` | Check if key exists |
| `keys` | `(keys record)` | Get all keys as array |
| `values` | `(values record)` | Get all values as array |

### Modification Operations

| Function | Signature | Description |
|---------|-----------|-------------|
| `assoc` | `(assoc record key value)` | Add/update key (returns new record) |
| `dissoc` | `(dissoc record key)` | Remove key (returns new record) |
| `merge` | `(merge record1 record2)` | Merge two records (right takes precedence) |

### Transformation Operations

| Function | Signature | Description |
|---------|-----------|-------------|
| `map` | `(map fn record)` | Apply function to each (key, value) pair |
| `filter` | `(filter pred record)` | Keep entries where predicate returns `true` |

### Conversion Operations

| Function | Signature | Description |
|---------|-----------|-------------|
| `record->alist` | `(record->alist record)` | Convert to association list |
| `alist->record` | `(alist->record pairs)` | Convert from association list |

### Constructor

| Function | Signature | Description |
|---------|-----------|-------------|
| `record` | `(record key1 val1 key2 val2 ...)` | Create record from key-value pairs |

## Notes

- **Immutability**: All operations return new records. Original records are never modified.
- **Key Order**: Keys maintain their insertion order (first to last).
- **Duplicate Keys**: If a record literal has duplicate keys, the last value wins.
- **Empty Records**: `{}` is a valid record with no entries.
- **Type**: Records are represented as `VMap (Map String Value)` internally, using gram's `Subject.Value.VMap` type directly.
