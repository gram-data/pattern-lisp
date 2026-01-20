# Quickstart: Inline Record Notation

**Date**: 2025-01-30  
**Feature**: Inline Record Notation  
**Type**: **REFACTORING** - Replace existing map support with gram-compatible records

## Overview

This refactoring replaces existing map support with gram-style property records in Pattern Lisp:
- **Records**: Immutable key-value structures using gram-compatible notation (`{ key: value }`) - replaces existing map syntax
- **Type**: Uses gram `Subject.Value.VMap` directly (Option B - gram types throughout)
- **Operations**: Complete set of operations for access, transformation, and manipulation
- **Quasiquotation**: Dynamic record construction with unquoting and splicing

**Important**: This is a refactoring - records replace maps. The syntax changes from space-separated `{name: "Alice" age: 30}` to comma-separated `{name: "Alice", age: 30}`. Records use gram `Subject.Value.VMap` directly (no separate VRecord type). Records are values that can be used anywhere in pattern-lisp code, including as arguments to existing pattern construction functions like `(pure ...)` and `(pattern ...)`.

## Record Literals

Records use gram's established notation with curly braces and comma-separated key-value pairs.

### Syntax

```lisp
{ name: "Alice", age: 30 }                    ;; record literal
{ person: { name: "Bob" } }                  ;; nested records
{ }                                           ;; empty record
```

### Basic Usage

```lisp
;; Records evaluate to Subject.Value.VMap (gram's PropertyRecord)
{ name: "Alice", age: 30 }                    ;; => VMap (gram type)

;; Nested records
{ user: { name: "Alice", role: "Engineer" } } ;; => nested record

;; Empty records
{ }                                           ;; => empty record
```

### Duplicate Keys

```lisp
{ name: "Alice", name: "Bob" }  ;; => ParseError (duplicate keys not allowed)
```

---

## Using Records with Patterns

Records are values that can be used with existing pattern-lisp functions. Pattern-lisp uses function calls (not square bracket notation) to create patterns.

### Usage with Pattern Functions

```lisp
;; Records are values - use them with existing pattern functions
(define person-record { name: "Alice", age: 30 })

;; Records can be used as arguments to pattern construction functions
;; (Note: actual pattern construction API depends on existing pattern-lisp functions)
```

**Note**: Pattern-lisp uses parentheses `()` for function calls, not square brackets `[]`. Square brackets like `[Person { ... }]` are gram notation (serialization format), not pattern-lisp syntax.

---

## Record Operations

### Type Predicate

```lisp
(record? { name: "Alice" })  ;; => true
(record? 42)                 ;; => false
```

### Access Operations

```lisp
;; Get value by key
(record-get { name: "Alice", age: 30 } "name")              ;; => "Alice"
(record-get { name: "Alice" } "age")                       ;; => nil
(record-get { name: "Alice" } "age" 0)                      ;; => 0 (default)

;; Check key existence
(record-has? { name: "Alice" } "name")                     ;; => true
(record-has? { name: "Alice" } "age")                      ;; => false

;; Get all keys
(record-keys { name: "Alice", age: 30 })                   ;; => ("name" "age")

;; Get all values
(record-values { name: "Alice", age: 30 })                 ;; => ("Alice" 30)

;; Convert to association list
(record->alist { name: "Alice", age: 30 })                 ;; => (("name" . "Alice") ("age" . 30))
```

### Construction Operations

```lisp
;; Programmatic construction
(record :name "Alice" :age 30)                             ;; => { name: "Alice", age: 30 }

;; From association list
(alist->record '(("name" . "Alice") ("age" . 30)))         ;; => { name: "Alice", age: 30 }
```

### Transformation Operations

```lisp
;; Add/update key-value pair (returns new record)
(record-set { name: "Alice" } "age" 30)                     ;; => { name: "Alice", age: 30 }

;; Remove key (returns new record)
(record-remove { name: "Alice", age: 30 } "age")           ;; => { name: "Alice" }

;; Merge two records (right takes precedence)
(record-merge { name: "Alice" } { age: 30 })                ;; => { name: "Alice", age: 30 }
(record-merge { name: "Alice" } { name: "Bob" })            ;; => { name: "Bob" }

;; Map over values
(record-map (lambda (k v) (* v 2)) { count: 5, total: 10 }) ;; => { count: 10, total: 20 }

;; Filter entries
(record-filter (lambda (k v) (> v 5)) { a: 10, b: 3, c: 7 }) ;; => { a: 10, c: 7 }
```

### Immutability

```lisp
;; All operations return new records, originals unchanged
(let ((r { name: "Alice" }))
  (record-set r "age" 30)
  r)  ;; => { name: "Alice" } (unchanged)
```

### Equality

```lisp
;; Structural equality (order-independent)
(= { name: "Alice", age: 30 } { age: 30, name: "Alice" })  ;; => true
(= { name: "Alice" } { name: "Bob" })                      ;; => false
```

---

## Quasiquotation

Records support unquoting and splicing for dynamic construction.

### Unquoting

```lisp
;; Unquote expressions
(let ((name "Alice") (age 30))
  `{ name: ,name, age: ,age })  ;; => { name: "Alice", age: 30 }
```

### Splicing

```lisp
;; Splice record entries
(let ((base { role: "Engineer" }))
  `{ name: "Alice", ,@base })   ;; => { name: "Alice", role: "Engineer" }
```

### Combined

```lisp
;; Combine unquoting and splicing
(let ((name "Alice")
      (base { role: "Engineer" }))
  `{ name: ,name, age: 30, ,@base })  ;; => { name: "Alice", age: 30, role: "Engineer" }
```

---

## Common Patterns

### Building Records Incrementally

```lisp
;; Start with empty record, add keys
(let ((r { }))
  (-> r
      (record-set "name" "Alice")
      (record-set "age" 30)
      (record-set "role" "Engineer")))  ;; => { name: "Alice", age: 30, role: "Engineer" }
```

### Merging Multiple Records

```lisp
;; Combine multiple records
(let ((personal { name: "Alice", age: 30 })
      (work { role: "Engineer", dept: "Engineering" }))
  (record-merge personal work))  ;; => { name: "Alice", age: 30, role: "Engineer", dept: "Engineering" }
```

### Filtering Records

```lisp
;; Keep only certain keys
(record-filter (lambda (k v) (member k '("name" "age"))) 
               { name: "Alice", age: 30, role: "Engineer" })
;; => { name: "Alice", age: 30 }
```

### Transforming Values

```lisp
;; Apply function to all values
(record-map (lambda (k v) (if (string? v) (string-upcase v) v))
            { name: "alice", age: 30 })  ;; => { name: "ALICE", age: 30 }
```

---

## Error Handling

### Parse Errors

```lisp
;; Duplicate keys
{ name: "Alice", name: "Bob" }  ;; => ParseError: "Duplicate key 'name' in record"

;; Unclosed record
{ name: "Alice"                 ;; => ParseError: "Unclosed record, expected '}'"
```

### Type Errors

```lisp
;; Non-record in record operation
(record-get 42 "name")          ;; => TypeMismatch: "Expected record, got number"

;; Non-string key
(record-get { name: "Alice" } 42) ;; => TypeMismatch: "Expected string key, got number"
```

---

## Next Steps

- See [data-model.md](./data-model.md) for detailed type definitions
- See [contracts/README.md](./contracts/README.md) for complete API reference
- See [research.md](./research.md) for implementation details
