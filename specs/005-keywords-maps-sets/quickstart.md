# Quickstart: Keywords, Maps, and Sets

**Date**: 2025-01-27  
**Feature**: Keywords, Maps, and Sets

## Overview

This feature adds three foundational language constructs to Pattern Lisp:
- **Keywords**: Self-evaluating symbols with postfix colon syntax (`name:`)
- **Maps**: Key-value data structures with keyword keys (`{name: "Alice" age: 30}`)
- **Sets**: Unordered collections of unique elements (`#{1 2 3}`)

## Keywords

Keywords are self-evaluating symbols used primarily as map keys.

### Syntax

```lisp
name:        ;; keyword
age:        ;; keyword
on-success: ;; keyword
```

### Usage

```lisp
;; Keywords evaluate to themselves
name:                    ;; => name: (VKeyword "name")

;; Keywords are used as map keys
{name: "Alice" age: 30}  ;; => map with keyword keys

;; Keywords are distinct from symbols
(= name: name:)          ;; => true (same keyword)
```

---

## Maps

Maps are key-value data structures using keywords as keys.

### Syntax

```lisp
{name: "Alice" age: 30}                    ;; map literal
{user: {name: "Bob" email: "bob@ex"}}     ;; nested maps
```

### Operations

```lisp
;; Access
(get {name: "Alice"} name:)                    ;; => "Alice"
(get {name: "Alice"} age:)                     ;; => nil
(get {name: "Alice"} age: 0)                   ;; => 0 (default)

;; Nested access
(get-in {user: {name: "Alice"}} [user: name:])  ;; => "Alice"

;; Update
(assoc {name: "Alice"} age: 30)              ;; => {name: "Alice" age: 30}
(dissoc {name: "Alice" age: 30} age:)       ;; => {name: "Alice"}
(update {count: 5} count: inc)               ;; => {count: 6}

;; Predicates
(contains? {name: "Alice"} name:)            ;; => true
(empty? {})                                  ;; => true

;; Constructor
(hash-map name: "Alice" age: 30)              ;; => {name: "Alice" age: 30}
```

### Duplicate Keys

```lisp
{name: "Alice" name: "Bob"}  ;; => {name: "Bob"} (last value wins)
```

---

## Sets

Sets are unordered collections of unique elements.

### Syntax

```lisp
#{1 2 3}                    ;; set of numbers
#{"Person" "Employee"}      ;; set of strings (Subject labels)
#{name: age: active:}         ;; set of keywords
```

### Operations

```lisp
;; Membership
(contains? #{1 2 3} 2)                      ;; => true

;; Set operations
(set-union #{1 2} #{2 3})                   ;; => #{1 2 3}
(set-intersection #{1 2 3} #{2 3 4})        ;; => #{2 3}
(set-difference #{1 2 3} #{2})               ;; => #{1 3}
(set-symmetric-difference #{1 2} #{2 3})     ;; => #{1 3}

;; Predicates
(set-subset? #{1 2} #{1 2 3})               ;; => true
(set-equal? #{1 2 3} #{3 2 1})               ;; => true
(empty? #{})                                 ;; => true

;; Constructor
(hash-set 1 2 3)                             ;; => #{1 2 3}
```

### Duplicate Elements

```lisp
#{1 2 2 3}  ;; => #{1 2 3} (duplicates removed)
```

---

## Subject Labels

Subject labels in gram patterns are represented as `Set String`. In Pattern Lisp, you can create sets of strings to represent Subject labels:

```lisp
;; Create a set of labels
#{"Person" "Employee"}  ;; => set of strings

;; Check membership
(contains? #{"Person" "Employee"} "Person")  ;; => true

;; Subject labels are Set String, matching the set representation
;; When creating gram patterns, labels are automatically handled as sets
```

**Key Points**:
- Subject labels are `Set String` (unordered, unique strings)
- Plain strings in sets (`#{"Person" "Employee"}`) are sufficient
- Sets automatically handle duplicates and are unordered
- Labels work seamlessly with gram pattern serialization
- Prefix colon syntax (`:Person`) is optional and deferred - use plain strings for now

---

## Common Patterns

### Configuration Maps

```lisp
(def config {host: "localhost" port: 8080 debug: true})

(get config host:)        ;; => "localhost"
(get config port:)        ;; => 8080
```

### Nested Data

```lisp
(def user {name: "Alice" 
           address: {street: "123 Main" city: "NYC"}})

(get-in user [address: city:])  ;; => "NYC"
```

### Set Operations

```lisp
(def admins #{"alice" "bob"})
(def users #{"alice" "bob" "charlie"})

(set-difference users admins)  ;; => #{"charlie"}
```

---

## Error Handling

```lisp
;; Type errors
(get "not-a-map" name:)  ;; => TypeMismatch "Expected map, got string"

;; Syntax errors
{name "Alice"}           ;; => ParseError "Expected keyword key"
```

---

## Next Steps

1. **Keywords**: Start with keyword parsing and evaluation
2. **Sets**: Implement sets (can be parallel with keywords)
3. **Maps**: Implement maps (requires keywords)
4. **Operations**: Add map and set operation primitives
5. **Serialization**: Add serialization support

See [tasks.md](./tasks.md) for detailed implementation tasks.

