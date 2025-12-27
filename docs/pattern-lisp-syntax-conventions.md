# Pattern-Lisp Syntax Conventions

**⚠️ Future: Not Yet Implemented - Design Document v0.1**

This document describes planned syntax conventions for Pattern Lisp. The features described here (keywords, maps, sets, labels) are **not yet implemented** in the current codebase.

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
;; Note: Labels are sets (Set String), so use set syntax
(define pattern (node 'a #{:Person :Employee} {name: "Alice"}))
```

### Labels in Pattern-Lisp

When working with gram patterns programmatically, labels appear as prefix-colon symbols:

```lisp
;; Creating a pattern with labels (labels are sets)
(gram/node 'a #{:Person} {name: "Alice"})

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
2. **Prefix `:Person`** only appears in label position (after node identifier or in label sets)

```lisp
;; Clear from context:
{name: "Alice"}                    ;; name: is a keyword (map key position)
(a:Person {name: "Alice"})         ;; :Person is a label (after node id)
#{:Person :Employee}               ;; labels as a set (Subject labels are Set String)
```

**Note**: Subject labels are `Set String` (unordered, unique), so Pattern Lisp uses set syntax `#{:Person :Employee}`. Gram notation may serialize labels as `[:Person :Employee]` (order preserved in text), but semantically they form a set.

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
(get user name:)                    ;; => "Alice"
(get-in data [user: name:])         ;; => "Alice" (path is list of keywords)
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

**Note**: Maps use keywords as keys (postfix colon syntax). The `hash-map` function provides explicit construction when building maps programmatically, while `{...}` is a literal syntax for read-time construction.

---

## Sets

Sets are unordered collections of unique elements. Pattern-lisp uses **hash set syntax** (inspired by Clojure):

```lisp
;; Set literal
#{:Person :Employee}               ;; set of labels
#{1 2 3}                           ;; set of numbers
#{"a" "b" "c"}                     ;; set of strings
#{name: age: active:}              ;; set of keywords
```

### Evaluation

Sets evaluate to set values—no lookup occurs:

```lisp
#{:Person :Employee}               ;; => set containing :Person and :Employee
(def labels #{:Person :Employee})
labels                              ;; => set
```

### Primary Uses

**Subject labels** (which are `Set String`):
```lisp
;; Creating a pattern with label set
(gram/node 'a #{:Person :Employee} {name: "Alice"})

;; Working with label sets
(def person-labels #{:Person :Employee})
(gram/has-label? node :Person)      ;; => true
```

**Unique value collections**:
```lisp
;; Set of unique values
(def unique-ids #{1 2 3 4 5})

;; Set operations
(set-union #{1 2} #{2 3})          ;; => #{1 2 3}
(set-intersection #{1 2 3} #{2 3 4}) ;; => #{2 3}
(set-difference #{1 2 3} #{2})     ;; => #{1 3}
```

**Filtering duplicates**:
```lisp
;; Convert list to set (removes duplicates)
(def unique-list (set-from-list '(1 2 2 3 3 3))) ;; => #{1 2 3}
```

### Set Operations

```lisp
;; Construction
#{a b c}                            ;; literal
(hash-set a b c)                    ;; function (explicit constructor)

;; Membership
(contains? s element)               ;; element present?
(member? s element)                 ;; alternative name

;; Set operations
(set-union s1 s2)                   ;; union of two sets
(set-intersection s1 s2)            ;; intersection
(set-difference s1 s2)              ;; elements in s1 but not s2
(set-symmetric-difference s1 s2)    ;; elements in either but not both

;; Predicates
(empty? s)                          ;; no elements?
(set-subset? s1 s2)                 ;; s1 subset of s2?
(set-equal? s1 s2)                  ;; sets contain same elements?
```

### Sets vs Lists vs Maps

| Aspect | List | Map | Set |
|--------|------|-----|-----|
| Syntax | `(a b c)` or `'(a b c)` | `{a: 1 b: 2}` | `#{a b c}` |
| Order | Preserved | Key-based | Unordered |
| Duplicates | Allowed | Keys unique | Elements unique |
| Access | By index | By key | Membership test |
| Typical use | Sequences | Key-value data | Unique collections, labels |

```lisp
;; Lists: ordered sequences
'(1 2 3)                            ;; => (1 2 3) (order matters)

;; Maps: key-value pairs
{name: "Alice" age: 30}             ;; => map (keys are keywords)

;; Sets: unordered, unique
#{1 2 3}                            ;; => set (order doesn't matter, no duplicates)
#{1 2 2 3}                          ;; => #{1 2 3} (duplicates removed)
```

### Why Sets for Labels?

Subject labels are `Set String` in the implementation (confirmed in `docs/gram-hs-reference.md`):
- **Order doesn't matter**: `#{:Person :Employee}` = `#{:Employee :Person}`
- **No duplicates**: `#{:Person :Person}` = `#{:Person}`
- **Set operations**: Can use union, intersection, etc. on label sets

Gram notation may serialize labels as `[:Person :Employee]` (order preserved in text), but Pattern Lisp code should use set syntax `#{:Person :Employee}` to accurately reflect the underlying data structure.

---

## Comparison with Other Lisps

### Clojure

```clojure
;; Clojure uses prefix keywords
{:name "Alice" :age 30}              ;; map literal
(hash-map :name "Alice" :age 30)     ;; map constructor
#{:Person :Employee}                 ;; set literal
(hash-set :Person :Employee)          ;; set constructor
(:name user)                         ;; keyword as function
```

### Common Lisp

```lisp
;; Common Lisp uses prefix keywords
(list :name "Alice" :age 30)         ;; property list
(getf plist :name)                   ;; access
;; No standard set syntax (uses lists or hash tables)
```

### Pattern-Lisp

```lisp
;; Pattern-lisp uses postfix keywords
{name: "Alice" age: 30}              ;; map literal
(hash-map name: "Alice" age: 30)     ;; map constructor
#{:Person :Employee}                ;; set literal (for labels)
(hash-set :Person :Employee)        ;; set constructor
(get user name:)                     ;; map access
```

The postfix keyword choice is unconventional for Lisps but serves pattern-lisp's specific need: compatibility with gram notation where prefix colon denotes labels. The `#{}` set syntax follows Clojure convention and clearly distinguishes sets from lists and maps.

---

## Grammar Summary

```
symbol          = letter (letter | digit | '-' | '_')*
namespaced-sym  = symbol '/' symbol
keyword         = symbol ':'
label           = ':' symbol
map-literal     = '{' (keyword value)* '}'
set-literal     = '#{' (value)* '}'

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
| `{name: "Alice"}` | map-literal | map with key `name:` and value `"Alice"` |
| `#{:Person :Employee}` | set-literal | set containing `:Person` and `:Employee` |

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

### Why Hash Set Syntax (#{})?

1. **Clojure convention**: `#{}` is widely recognized as set notation
2. **Visual distinction**: 
   - `()` = lists (ordered sequences)
   - `{}` = maps (key-value pairs)
   - `#{}` = sets (unordered, unique)
3. **Subject labels alignment**: Subject labels are `Set String`, so `#{:Person :Employee}` naturally represents a set of labels
4. **Unambiguous**: Clearly communicates set semantics (unordered, unique) vs. list semantics (ordered, duplicates allowed)
5. **Constructor functions**: `hash-map` and `hash-set` provide explicit construction when needed (e.g., programmatic construction)

### Why hash-map and hash-set Functions?

Following Clojure convention:
- **Literals** (`{...}`, `#{...}`): Read-time construction, more efficient
- **Constructors** (`hash-map`, `hash-set`): Runtime construction, useful for programmatic building

Both create the same data structure, but literals are preferred when the structure is known at read time.
