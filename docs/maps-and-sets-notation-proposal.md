# Maps and Sets Notation Proposal for Pattern Lisp

**Date**: 2025-01-27  
**Status**: Proposal for Review  
**Related**: `docs/pattern-lisp-syntax-conventions.md`

## Research Summary: Lisp Conventions

### Sets in Lisp

**Common Lisp**:
- No standard literal syntax for sets
- Common approaches:
  - Lists treated as sets (order ignored, duplicates removed)
  - Hash tables with elements as keys (value = `t`)
  - Custom set data structures

**Clojure**:
- **Set literal**: `#{a b c}` - hash set literal syntax
- **Set constructor**: `(hash-set a b c)` - function to create sets
- Sets are unordered, no duplicates
- Membership: `(contains? my-set element)`

**Scheme**:
- No standard set syntax
- Typically uses lists or SRFI-113 sets (library extension)

**Emacs Lisp**:
- Lists used as sets
- Functions: `memq`, `delq` for membership and removal

### Maps in Lisp

**Common Lisp**:
- Hash tables: `(make-hash-table)` then `(setf (gethash key table) value)`
- Association lists (alists): `((key1 . value1) (key2 . value2))`
- Property lists (plists): `(:key1 value1 :key2 value2)`

**Clojure**:
- **Map literal**: `{:key value}` - map literal syntax (uses prefix keywords)
- **Map constructor**: `(hash-map :key value)` - function to create maps
- Maps are key-value associations
- Access: `(get map :key)` or `(:key map)` (keyword as function)

**Scheme**:
- Association lists (alists) or hash tables (SRFI-69)

### Key Observations

1. **Clojure distinguishes literals from constructors**:
   - `{:a 1 :b 2}` is a literal (read-time construction)
   - `(hash-map :a 1 :b 2)` is a function call (runtime construction)
   - Both create the same data structure, but literals are more efficient
   - **Note**: `hash-map` is commonly used in Clojure for programmatic construction

2. **Set notation is less standardized** than map notation
   - Clojure's `#{}` is the most widely recognized set literal syntax
   - Most Lisps use lists or hash tables for sets

3. **Subject labels are sets** (confirmed: `Set String` in gram-hs-reference.md)
   - Order doesn't matter
   - No duplicates
   - This aligns with set semantics
   - **Important**: The syntax conventions doc shows `[:Person :Employee]` as a label list, but Subject labels are actually a `Set String`, not a list. This is a semantic mismatch that should be addressed.

## Proposed Approaches for Pattern Lisp

### Approach 1: Clojure-Inspired (Recommended)

**Maps**: Use curly braces `{key: value}` (already in syntax conventions doc)
- Literal: `{name: "Alice" age: 30}`
- Constructor: `(hash-map name: "Alice" age: 30)` - explicit constructor function
- Rationale: Matches existing syntax conventions document, familiar to Clojure users

**Sets**: Use hash set syntax `#{element1 element2}`
- Literal: `#{:Person :Employee}` or `#{1 2 3}`
- Constructor: `(hash-set :Person :Employee)` - explicit constructor function
- Rationale: Clojure's `#{}` is widely recognized, clearly distinguishes from lists `[]`

**Pros**:
- Clear distinction: `[]` = list (ordered), `{}` = map (key-value), `#{}` = set (unordered, unique)
- Familiar to Clojure users
- `hash-map` and `hash-set` functions provide explicit construction when needed
- Aligns with Subject labels being sets (can use `#{:Person :Employee}` for label lists)

**Cons**:
- `#{}` syntax requires reader macro support (may need parser extension)
- Less common in other Lisp dialects

**Example**:
```lisp
;; Maps
{name: "Alice" age: 30}                    ;; literal
(hash-map name: "Alice" age: 30)          ;; constructor

;; Sets
#{:Person :Employee}                       ;; literal (for labels)
#{1 2 3}                                   ;; literal (for values)
(hash-set :Person :Employee)              ;; constructor

;; Subject labels (which are sets)
(def person-labels #{:Person :Employee})
```

---

### Approach 2: Minimal Extension (Conservative)

**Maps**: Use curly braces `{key: value}` (already in syntax conventions doc)
- Literal: `{name: "Alice" age: 30}`
- Constructor: `(hash-map name: "Alice" age: 30)` - explicit constructor function

**Sets**: Use tagged list syntax `(set element1 element2)`
- Literal: `(set :Person :Employee)` or `(set 1 2 3)`
- Constructor: `(hash-set :Person :Employee)` - explicit constructor function
- Rationale: No new syntax, uses existing list syntax with special form

**Pros**:
- No parser changes needed (uses existing list syntax)
- Familiar Lisp pattern (tagged lists)
- `hash-set` function provides explicit construction

**Cons**:
- Less visually distinct from lists
- Requires special form handling in evaluator
- May be confused with function calls

**Example**:
```lisp
;; Maps
{name: "Alice" age: 30}                    ;; literal
(hash-map name: "Alice" age: 30)          ;; constructor

;; Sets
(set :Person :Employee)                    ;; literal (special form)
(set 1 2 3)                                ;; literal
(hash-set :Person :Employee)              ;; constructor
```

---

### Approach 3: Square Brackets for Sets

**Maps**: Use curly braces `{key: value}` (already in syntax conventions doc)
- Literal: `{name: "Alice" age: 30}`
- Constructor: `(hash-map name: "Alice" age: 30)`

**Sets**: Use square brackets `[element1 element2]` (reuse existing list syntax)
- Literal: `[:Person :Employee]` or `[1 2 3]`
- Constructor: `(hash-set :Person :Employee)`
- Rationale: Reuse `[]` syntax, distinguish by context (labels vs. values)

**Pros**:
- No new syntax needed
- `[]` already used for label lists in gram notation
- Simple implementation

**Cons**:
- Ambiguous: `[1 2 3]` could be list or set depending on context
- Doesn't clearly communicate set semantics (unordered, unique)
- May conflict with existing list usage

**Example**:
```lisp
;; Maps
{name: "Alice" age: 30}                    ;; literal
(hash-map name: "Alice" age: 30)          ;; constructor

;; Sets (using [] for sets, () for lists)
[:Person :Employee]                       ;; set literal
[1 2 3]                                   ;; set literal
(hash-set :Person :Employee)             ;; constructor
```

---

### Approach 4: Explicit Set Type with Different Brackets

**Maps**: Use curly braces `{key: value}` (already in syntax conventions doc)
- Literal: `{name: "Alice" age: 30}`
- Constructor: `(hash-map name: "Alice" age: 30)`

**Sets**: Use angle brackets `⟨element1 element2⟩` or backticks `` `{element1 element2}``
- Literal: `⟨:Person :Employee⟩` or `` `{:Person :Employee}``
- Constructor: `(hash-set :Person :Employee)`
- Rationale: Visually distinct from lists and maps

**Pros**:
- Clear visual distinction
- No ambiguity with existing syntax

**Cons**:
- Non-standard syntax (angle brackets not common in Lisps)
- May be difficult to type on some keyboards
- Requires parser support for new syntax

**Example**:
```lisp
;; Maps
{name: "Alice" age: 30}                    ;; literal
(hash-map name: "Alice" age: 30)          ;; constructor

;; Sets
⟨:Person :Employee⟩                       ;; set literal (angle brackets)
(hash-set :Person :Employee)              ;; constructor
```

---

## Important: Label List vs Set Mismatch

**Current Issue**: The syntax conventions document shows label lists as `[:Person :Employee]` (using square brackets), but Subject labels are actually `Set String` in the implementation (confirmed in `docs/gram-hs-reference.md`).

**Semantic Mismatch**:
- Syntax conventions doc: `[:Person :Employee]` (suggests ordered list)
- Implementation: `Set String` (unordered, unique)
- Gram notation: Uses `[:Person :Employee]` for serialization (order preserved in text, but semantically a set)

**Resolution**:
- **Pattern Lisp code**: Use set syntax `#{:Person :Employee}` for labels (matches Subject's `Set String`)
- **Gram notation**: Can continue using `[:Person :Employee]` for serialization (order doesn't matter, duplicates removed)
- **Conversion**: When parsing gram notation `[:Person :Employee]`, convert to set `#{:Person :Employee}` in Pattern Lisp

This ensures Pattern Lisp code accurately reflects the underlying data structure (set, not list).

---

## Recommendation: Approach 1 (Clojure-Inspired)

### Rationale

1. **Clear Semantics**: `#{}` is widely recognized as set notation (Clojure standard)
2. **Visual Distinction**: 
   - `()` = lists (ordered sequences)
   - `{}` = maps (key-value pairs)
   - `#{}` = sets (unordered, unique)
3. **Subject Labels Alignment**: Subject labels are `Set String`, so `#{:Person :Employee}` naturally represents a set of labels
4. **Constructor Functions**: `hash-map` and `hash-set` provide explicit construction when needed (e.g., programmatic construction)
5. **Gram Notation Compatibility**: Label lists in gram use `[:Person :Employee]`, which can be parsed as a set literal `#{:Person :Employee}` in Pattern Lisp

### Implementation Notes

**Parser Changes**:
- Add reader macro for `#{}` syntax
- Parse `#{...}` as set literal
- Elements can be any values (keywords, labels, numbers, strings, etc.)

**Value Type Changes**:
- Add `VSet (Set Value)` variant to `Value` type
- Sets are unordered, no duplicates
- Set operations: `(contains? s element)`, `(set-union s1 s2)`, `(set-intersection s1 s2)`, `(set-difference s1 s2)`

**Map Type**:
- Use `VMap (Map String Value)` or `VMap (Map Keyword Value)` (if keywords implemented first)
- Maps use keywords as keys (postfix colon syntax)

**Subject Labels**:
- Subject labels are already `Set String` in the implementation (confirmed in gram-hs-reference.md)
- **Current mismatch**: Syntax conventions doc shows `[:Person :Employee]` as label list, but Subject uses `Set String`
- **Resolution**: 
  - Pattern Lisp code should use set syntax: `#{:Person :Employee}` (matches Subject's `Set String` type)
  - Gram notation can use `[:Person :Employee]` for serialization (order preserved in text, but semantically unordered)
  - When deserializing gram notation, convert `[:Person :Employee]` → `#{:Person :Employee}` (set)
- Pattern Lisp set literals `#{:Person :Employee}` naturally represent label sets
- Conversion: `#{:Person :Employee}` → `Set.fromList ["Person", "Employee"]` (stripping `:` prefix for labels)

### Syntax Summary

```lisp
;; Lists (ordered sequences)
'(1 2 3)                                   ;; quoted list
(list 1 2 3)                               ;; constructor

;; Maps (key-value pairs)
{name: "Alice" age: 30}                    ;; literal
(hash-map name: "Alice" age: 30)          ;; constructor

;; Sets (unordered, unique)
#{:Person :Employee}                       ;; literal (labels - matches Subject's Set String)
#{1 2 3}                                   ;; literal (values)
(hash-set :Person :Employee)              ;; constructor

;; Subject labels (which are sets, not lists!)
(def labels #{:Person :Employee})         ;; set of labels (correct)
;; NOT: [:Person :Employee]               ;; this suggests a list, but labels are sets
```

### Terminology

- **Map**: Key-value association (also called dictionary, hash table, associative array)
- **Set**: Unordered collection of unique elements
- **hash-map**: Function to construct maps programmatically
- **hash-set**: Function to construct sets programmatically
- **Literal syntax**: `{}` for maps, `#{}` for sets (read-time construction)
- **Constructor function**: `(hash-map ...)` and `(hash-set ...)` (runtime construction)

---

## Alternative: Approach 2 (Minimal Extension)

If `#{}` syntax is too complex to implement, **Approach 2** (tagged lists) is a reasonable fallback:

```lisp
;; Sets using special form
(set :Person :Employee)                    ;; set literal
(hash-set :Person :Employee)              ;; constructor
```

This requires:
- Special form `set` in evaluator
- No parser changes (uses existing list syntax)
- Less visually distinct but simpler to implement

---

## Questions for Review

1. **Set syntax preference**: `#{}` (Clojure-style) vs `(set ...)` (tagged list)?
2. **Map constructor naming**: `hash-map` vs `map` vs `make-map`?
   - **Recommendation**: `hash-map` (matches Clojure, already in syntax conventions doc)
3. **Set constructor naming**: `hash-set` vs `set` vs `make-set`?
   - **Recommendation**: `hash-set` (matches Clojure convention, parallels `hash-map`)
4. **Label syntax**: Should `[:Person :Employee]` (gram notation) be parsed as a set `#{:Person :Employee}` in Pattern Lisp?
   - **Recommendation**: Yes - Subject labels are `Set String`, so Pattern Lisp should use set syntax `#{:Person :Employee}`. Gram notation can use `[:Person :Employee]` for serialization, but Pattern Lisp code should use sets.
5. **Keyword vs String keys**: Should maps use `Keyword` or `String` for keys?
   - **Recommendation**: `Keyword` (self-evaluating, matches syntax conventions doc which shows `{name: "Alice"}` with keywords as keys)
6. **List vs Set distinction**: Should `()` be lists and `#{}` be sets, or should `[]` be used for sets?
   - **Recommendation**: Keep `()` for lists (ordered), use `#{}` for sets (unordered, unique). This provides clear semantic distinction.

---

## Next Steps

1. Review and decide on approach
2. Update `docs/pattern-lisp-syntax-conventions.md` with chosen notation
3. Update TODO.md implementation tasks with specific syntax decisions
4. Implement parser support for chosen syntax
5. Add set operations to primitives

