# Pattern-Lisp Syntax Conventions

## Design Document v0.1

### Overview

This document specifies the syntactic conventions for pattern-lisp, covering symbols, keywords, namespaces, and their interaction with gram notation. A key design goal is clean interoperability between pattern-lisp code and gram's pattern syntax.

---

## Symbols

Symbols are names that refer to values in the environment.

### Basic Symbols

```lisp
x                   ;; simple symbol
my-function         ;; kebab-case symbol
parse-json          ;; another kebab-case symbol
user42              ;; alphanumeric
```

Symbols are looked up in the current lexical environment when evaluated.

### Quoted Symbols

Quoting prevents evaluation, yielding the symbol itself:

```lisp
'x                  ;; the symbol x, not its value
'my-function        ;; the symbol my-function
'(a b c)            ;; list of symbols (not evaluated)
```

### Naming Conventions

| Convention | Usage | Example |
|------------|-------|---------|
| `lowercase-kebab` | Functions, variables | `get-user`, `parse-json` |
| `UpperCamelCase` | Types, constructors | `HttpError`, `Some`, `None` |
| `*name*` | Dynamic/special variables | `*current-user*`, `*debug*` |

---

## Namespaced Symbols

Pattern-lisp uses the `namespace/symbol` convention (from Clojure) for organizing related functions:

```lisp
effect/succeed      ;; effect namespace, succeed function
effect/flatMap      ;; effect namespace, flatMap function
string/split        ;; string namespace, split function
string/join         ;; string namespace, join function
my-app/process      ;; application namespace
```

### Semantics

The slash is part of the symbol name—`effect/succeed` is a single symbol, not an access operation. This is purely a naming convention that provides:

- Visual organization of related functions
- Collision avoidance between libraries
- Clear provenance of functions

The host runtime may interpret namespace prefixes for module resolution, but pattern-lisp's core semantics treat namespaced symbols as ordinary symbols.

### Examples

```lisp
;; Using namespaced functions
(string/split "a,b,c" ",")        ;; => ["a" "b" "c"]
(string/join ["a" "b" "c"] "-")   ;; => "a-b-c"

;; Effect operations
(effect/succeed 42)
(effect/map inc)
(effect/flatMap fetch-user)

;; Can be bound locally like any symbol
(def succeed effect/succeed)
(succeed 42)                       ;; same as (effect/succeed 42)
```

---

## Keywords

Keywords are self-evaluating symbols used for keys, tags, and options. Pattern-lisp uses **postfix colon** notation:

```lisp
name:               ;; keyword
age:                ;; keyword  
on-success:         ;; keyword
tag:                ;; keyword
```

### Evaluation

Keywords evaluate to themselves—no lookup occurs:

```lisp
name:               ;; => name:
(def x name:)       
x                   ;; => name:
(= name: name:)     ;; => true
```

### Primary Uses

**Map keys:**
```lisp
{name: "Alice" age: 30 active: true}
```

**Option/configuration maps:**
```lisp
(effect/retry operation {times: 3 delay: 1000})
(http/request {method: "POST" url: "/api" body: data})
```

**Tagged unions (discriminants):**
```lisp
{tag: HttpError: status: 404 message: "Not found"}
{tag: Success: value: result}
```

**Keyword arguments:**
```lisp
(create-user "alice" email: "alice@example.com" admin: false)
```

### Keywords vs Symbols

| Aspect | Symbol | Keyword |
|--------|--------|---------|
| Syntax | `name` | `name:` |
| Evaluation | Looked up | Self-evaluating |
| Typical use | Bindings, functions | Data keys, options |
| Identity | By binding | By name (interned) |

```lisp
;; Symbols require definition
(def name "Alice")
name                    ;; => "Alice"

;; Keywords are immediate values
name:                   ;; => name:
(= name: name:)         ;; => true (same object)
```

---

## Labels (Gram Notation Interop)

In gram notation, **prefix colon** denotes node labels:

```gram
(a:Person {name: "Alice"})
(b:Person:Employee {name: "Bob" dept: "Engineering"})
(:Company {name: "Acme"})
```

This creates a clean syntactic distinction:

| Syntax | Meaning | Context |
|--------|---------|---------|
| `name:` | Keyword (key) | Map keys, options |
| `:Person` | Label (type) | Node categories, type annotations |

### Semantic Distinction

The prefix/postfix difference reflects a meaningful semantic distinction:

- **Keywords** (`name:`) are *keys*—they expect an associated value
- **Labels** (`:Person`) are *categories*—they classify or type an entity

```lisp
;; Keywords as keys (postfix)
{name: "Alice" age: 30}

;; Labels as types (prefix) in gram patterns
(a:Person {name: "Alice"})

;; In pattern-lisp code working with patterns
(def pattern (node 'a [:Person :Employee] {name: "Alice"}))
```

### Labels in Pattern-Lisp

When working with gram patterns programmatically, labels appear as prefix-colon symbols:

```lisp
;; Creating a pattern with labels
(gram/node 'a [:Person] {name: "Alice"})

;; Matching on labels
(match node
  ((labels :Person) (handle-person node))
  ((labels :Company) (handle-company node)))

;; Checking labels
(gram/has-label? node :Person)    ;; => true
```

### Why This Works

The parsing is unambiguous because context determines interpretation:

1. **Postfix `name:`** only appears in map position or keyword argument position
2. **Prefix `:Person`** only appears in label position (after node identifier or in label lists)

```lisp
;; Clear from context:
{name: "Alice"}           ;; name: is a keyword (map key position)
(a:Person {name: "Alice"}) ;; :Person is a label (after node id)
[:Person :Employee]        ;; labels in a label list
```

---

## Maps

Maps use keywords as keys with the postfix syntax:

```lisp
;; Map literal
{name: "Alice" age: 30 active: true}

;; Nested maps
{user: {name: "Alice" email: "alice@example.com"}
 settings: {theme: "dark" notifications: true}}

;; Access
(get user name:)          ;; => "Alice"
(get-in data [user: name:]) ;; => "Alice"
```

### Map Operations

```lisp
;; Construction
{a: 1 b: 2}               ;; literal
(hash-map a: 1 b: 2)      ;; function

;; Access
(get m key:)              ;; get with default nil
(get m key: default)      ;; get with explicit default

;; Update
(assoc m key: value)      ;; add/update key
(dissoc m key:)           ;; remove key
(update m key: f)         ;; apply f to value at key

;; Predicates
(contains? m key:)        ;; key present?
(empty? m)                ;; no keys?
```

---

## Comparison with Other Lisps

### Clojure

```clojure
;; Clojure uses prefix keywords
{:name "Alice" :age 30}
(:name user)              ; keyword as function
```

### Common Lisp

```lisp
;; Common Lisp uses prefix keywords
(list :name "Alice" :age 30)
(getf plist :name)
```

### Pattern-Lisp

```lisp
;; Pattern-lisp uses postfix keywords
{name: "Alice" age: 30}
(get user name:)
```

The postfix choice is unconventional for Lisps but serves pattern-lisp's specific need: compatibility with gram notation where prefix colon denotes labels.

---

## Grammar Summary

```
symbol          = letter (letter | digit | '-' | '_')*
namespaced-sym  = symbol '/' symbol
keyword         = symbol ':'
label           = ':' symbol

letter          = 'a'-'z' | 'A'-'Z'
digit           = '0'-'9'
```

### Token Examples

| Input | Token Type | Value |
|-------|------------|-------|
| `name` | symbol | `name` |
| `get-user` | symbol | `get-user` |
| `effect/succeed` | namespaced-symbol | `effect/succeed` |
| `name:` | keyword | `name:` |
| `:Person` | label | `:Person` |

---

## Appendix: Rationale

### Why Postfix Keywords?

1. **Gram notation compatibility**: Prefix colon is already used for labels in gram
2. **Semantic clarity**: Keys (postfix) vs types (prefix) reflects their different roles
3. **Visual similarity to JS/JSON**: `{name: value}` is familiar to web developers
4. **Unambiguous parsing**: Context always determines whether `:` is prefix or postfix

### Why Not Prefix Keywords?

In most Lisps, `:keyword` works because there's no collision. Pattern-lisp's tight integration with gram notation creates a conflict:

```
;; Ambiguous if both use prefix
(:Person {name: "Alice"})   ;; Is :Person a keyword or label?
```

Postfix keywords resolve this elegantly:

```
;; Unambiguous
(a:Person {name: "Alice"})  ;; :Person is label, name: is keyword
```

### Why Slash for Namespaces?

1. **Clojure convention**: Widely recognized from the most popular modern Lisp
2. **Visually distinct**: Clearly separates namespace from name
3. **No parsing ambiguity**: `/` doesn't conflict with other syntax
4. **Not overloaded**: Unlike `:` which now has two uses (labels, keywords)
