# Feature Specification: Keywords, Maps, and Sets

**Feature Branch**: `005-keywords-maps-sets`  
**Created**: 2025-01-27  
**Status**: Draft  
**Input**: User description: "Pattern Lisp should support keywords, maps and sets as described in @TODO.md"

## Clarifications

### Session 2025-01-27

- Q: Do we need prefix colon syntax for labels (`:Person`) or can we use plain strings in sets (`#{"Person" "Employee"}`)? → A: Use plain strings in sets - labels are just strings. Prefix colon is optional syntactic sugar for gram interop.
- Q: What happens when a map literal has duplicate keys? → A: Last value wins - silently overwrites earlier values.
- Q: What happens when `update` is called on a non-existent key? → A: Create the key with the function applied to `nil`.

## User Scenarios & Testing *(mandatory)*

### User Story 1 - Using Keywords for Map Keys (Priority: P1)

Developers need to create structured data using maps with named keys. Keywords provide a clean syntax for map keys that are self-evaluating and don't require lookup in the environment.

**Why this priority**: Keywords are foundational - they're required for maps to work. Maps are essential for configuration, tagged unions, and structured state representation. Without keywords, developers cannot use the map literal syntax.

**Independent Test**: Can be fully tested by evaluating keyword expressions and using keywords as map keys. Developers can write `{name: "Alice" age: 30}` and verify that keywords evaluate to themselves and work as map keys.

**Acceptance Scenarios**:

1. **Given** a Pattern Lisp program, **When** a developer writes `name:`, **Then** it evaluates to the keyword `name:` (self-evaluating, no environment lookup)
2. **Given** a Pattern Lisp program, **When** a developer writes `{name: "Alice" age: 30}`, **Then** it creates a map with keyword keys `name:` and `age:` mapping to their respective values
3. **Given** a Pattern Lisp program, **When** a developer uses a keyword in a context that expects a symbol, **Then** the keyword is treated as a distinct value type (not a symbol)
4. **Given** a Pattern Lisp program, **When** a developer compares two identical keywords (e.g., `(= name: name:)`), **Then** the comparison returns true

---

### User Story 2 - Creating and Manipulating Maps (Priority: P1)

Developers need to work with key-value data structures for configuration, state management, and structured data representation. Maps provide efficient lookup and update operations.

**Why this priority**: Maps are a fundamental data structure needed for many language features. They enable configuration maps, tagged unions, and structured state representation. Maps depend on keywords (P1), so they share the same priority.

**Independent Test**: Can be fully tested by creating maps, accessing values, updating keys, and performing map operations. Developers can write map literals, use map operations, and verify correct behavior.

**Acceptance Scenarios**:

1. **Given** a Pattern Lisp program, **When** a developer writes `{name: "Alice" age: 30 active: true}`, **Then** it creates a map with three key-value pairs
2. **Given** a map `m` with key `name:` mapping to `"Alice"`, **When** a developer writes `(get m name:)`, **Then** it returns `"Alice"` (or `nil` if key doesn't exist)
3. **Given** a map `m`, **When** a developer writes `(get m name: "Unknown")`, **Then** it returns the value at `name:` or `"Unknown"` if the key doesn't exist
4. **Given** a nested map structure, **When** a developer writes `(get-in data [user: name:])`, **Then** it navigates the nested path and returns the value at the end
5. **Given** a map `m`, **When** a developer writes `(assoc m age: 31)`, **Then** it returns a new map with `age:` updated to `31` (or added if it didn't exist)
6. **Given** a map `m`, **When** a developer writes `(dissoc m age:)`, **Then** it returns a new map with `age:` removed
7. **Given** a map `m` with key `count:` mapping to `5`, **When** a developer writes `(update m count: inc)`, **Then** it returns a new map with `count:` updated to `6`
8. **Given** a map `m`, **When** a developer writes `(contains? m name:)`, **Then** it returns `true` if `name:` is a key in the map, `false` otherwise
9. **Given** an empty map `{}`, **When** a developer writes `(empty? {})`, **Then** it returns `true`
10. **Given** a Pattern Lisp program, **When** a developer writes `(hash-map name: "Alice" age: 30)`, **Then** it creates a map equivalent to `{name: "Alice" age: 30}`

---

### User Story 3 - Working with Sets (Priority: P2)

Developers need to work with unordered collections of unique elements, particularly for Subject labels (which are sets) and for filtering duplicates or performing set operations.

**Why this priority**: Sets are important for gram notation interop (Subject labels are `Set String`) and for unique value collections. However, they can be implemented in parallel with keywords and don't block maps, so they have slightly lower priority than maps.

**Independent Test**: Can be fully tested by creating sets, checking membership, and performing set operations. Developers can write set literals, use set operations, and verify correct behavior.

**Acceptance Scenarios**:

1. **Given** a Pattern Lisp program, **When** a developer writes `#{:Person :Employee}`, **Then** it creates a set containing strings `"Person"` and `"Employee"` (if prefix colon syntax is implemented) OR the developer can write `#{"Person" "Employee"}` to create a set of strings directly
2. **Given** a Pattern Lisp program, **When** a developer writes `#{1 2 3}`, **Then** it creates a set containing the numbers `1`, `2`, and `3`
3. **Given** a set `s` containing `#{1 2 3}`, **When** a developer writes `(contains? s 2)`, **Then** it returns `true`
4. **Given** two sets `#{1 2}` and `#{2 3}`, **When** a developer writes `(set-union #{1 2} #{2 3})`, **Then** it returns `#{1 2 3}`
5. **Given** two sets `#{1 2 3}` and `#{2 3 4}`, **When** a developer writes `(set-intersection #{1 2 3} #{2 3 4})`, **Then** it returns `#{2 3}`
6. **Given** two sets `#{1 2 3}` and `#{2}`, **When** a developer writes `(set-difference #{1 2 3} #{2})`, **Then** it returns `#{1 3}`
7. **Given** two sets `#{1 2}` and `#{2 3}`, **When** a developer writes `(set-symmetric-difference #{1 2} #{2 3})`, **Then** it returns `#{1 3}` (elements in either but not both)
8. **Given** two sets `#{1 2}` and `#{1 2 3}`, **When** a developer writes `(set-subset? #{1 2} #{1 2 3})`, **Then** it returns `true`
9. **Given** two sets `#{1 2 3}` and `#{3 2 1}`, **When** a developer writes `(set-equal? #{1 2 3} #{3 2 1})`, **Then** it returns `true` (order doesn't matter)
10. **Given** an empty set `#{}`, **When** a developer writes `(empty? #{})`, **Then** it returns `true`
11. **Given** a Pattern Lisp program, **When** a developer writes `(hash-set 1 2 3)`, **Then** it creates a set equivalent to `#{1 2 3}`
12. **Given** a set literal with duplicates `#{1 2 2 3}`, **When** it is evaluated, **Then** duplicates are removed, resulting in `#{1 2 3}`

---

### User Story 4 - Subject Labels as String Sets (Priority: P3)

Developers working with gram patterns need to represent Subject labels, which are sets of strings (`Set String`). Plain strings in sets are sufficient; prefix colon syntax (`:Person`) is optional syntactic sugar for gram notation compatibility.

**Why this priority**: Subject labels are `Set String` in the underlying representation. Using plain strings (`#{"Person" "Employee"}`) is sufficient and simpler. Prefix colon syntax can be optional sugar for visual compatibility with gram notation, but is not required. This can be deferred if handled via gram interop only, so it has lower priority.

**Independent Test**: Can be fully tested by creating sets of strings for labels. Developers can write `#{"Person" "Employee"}` to represent Subject labels. Optional prefix colon syntax (`:Person`) may evaluate to the string `"Person"` for gram interop convenience.

**Acceptance Scenarios**:

1. **Given** a Pattern Lisp program, **When** a developer writes `#{"Person" "Employee"}`, **Then** it creates a set of strings representing Subject labels
2. **Given** a Pattern Lisp program, **When** a developer writes `(contains? #{"Person" "Employee"} "Person")`, **Then** it returns `true` (strings work as set elements)
3. **Given** Subject labels are `Set String`, **When** a developer creates a set `#{"Person" "Employee"}`, **Then** it can be used directly for gram pattern Subject labels without conversion
4. **Given** prefix colon syntax is implemented as optional sugar, **When** a developer writes `:Person`, **Then** it may evaluate to the string `"Person"` for gram interop convenience (implementation may defer this)

---

### Edge Cases

- **Duplicate map keys**: When a map literal has duplicate keys (e.g., `{name: "Alice" name: "Bob"}`), the last value wins and silently overwrites earlier values, resulting in `{name: "Bob"}`
- **Non-existent map key with `get`**: Returns `nil` if no default provided, or the provided default value
- **Non-existent path with `get-in`**: Returns `nil` if any key in the path doesn't exist (no default provided), or the provided default value
- **`update` on non-existent key**: Creates the key with the function applied to `nil` (e.g., `(update {} count: inc)` → `{count: 1}` assuming `inc` treats `nil` as 0)
- What happens when a set literal contains values of different types? - Should be allowed (sets can contain any values)
- What happens when comparing sets with different element types? - Should return `false` (sets are only equal if they contain the same elements)
- What happens when `dissoc` is called on a non-existent key? - Should return the map unchanged
- What happens when keywords are used in contexts where symbols are expected? - Should produce a clear error message
- What happens when parsing ambiguous syntax? - Parser should use context to disambiguate (postfix `name:` in map position vs prefix `:Person` if implemented as optional sugar)

## Requirements *(mandatory)*

### Functional Requirements

- **FR-001**: Pattern Lisp MUST support keywords with postfix colon syntax (e.g., `name:`, `age:`) that evaluate to themselves without environment lookup
- **FR-002**: Pattern Lisp MUST support map literals with curly brace syntax using keywords as keys (e.g., `{name: "Alice" age: 30}`)
- **FR-003**: Pattern Lisp MUST support nested maps (maps containing other maps as values)
- **FR-004**: Pattern Lisp MUST provide `(get m key:)` operation that returns the value at `key:` or `nil` if the key doesn't exist
- **FR-005**: Pattern Lisp MUST provide `(get m key: default)` operation that returns the value at `key:` or `default` if the key doesn't exist
- **FR-006**: Pattern Lisp MUST provide `(get-in data [key1: key2: ...])` operation for nested map access that returns the value at the end of the path or `nil` if any key in the path doesn't exist
- **FR-007**: Pattern Lisp MUST provide `(assoc m key: value)` operation that returns a new map with `key:` updated to `value` (or added if it didn't exist)
- **FR-008**: Pattern Lisp MUST provide `(dissoc m key:)` operation that returns a new map with `key:` removed
- **FR-009**: Pattern Lisp MUST provide `(update m key: f)` operation that returns a new map with `key:` updated by applying function `f` to its current value (if key doesn't exist, creates it with `f` applied to `nil`)
- **FR-010**: Pattern Lisp MUST provide `(contains? m key:)` operation that returns `true` if `key:` exists in map `m`, `false` otherwise
- **FR-011**: Pattern Lisp MUST provide `(empty? m)` operation that returns `true` if map `m` has no keys, `false` otherwise
- **FR-012**: Pattern Lisp MUST provide `(hash-map key1: val1 key2: val2 ...)` function that constructs a map from key-value pairs
- **FR-013**: Pattern Lisp MUST support set literals with hash set syntax (e.g., `#{:Person :Employee}`, `#{1 2 3}`)
- **FR-014**: Pattern Lisp MUST ensure sets are unordered collections where duplicate elements are automatically removed
- **FR-015**: Pattern Lisp MUST provide `(contains? s element)` operation that returns `true` if `element` exists in set `s`, `false` otherwise
- **FR-016**: Pattern Lisp MUST provide `(set-union s1 s2)` operation that returns a set containing all elements from both sets
- **FR-017**: Pattern Lisp MUST provide `(set-intersection s1 s2)` operation that returns a set containing elements present in both sets
- **FR-018**: Pattern Lisp MUST provide `(set-difference s1 s2)` operation that returns a set containing elements in `s1` but not in `s2`
- **FR-019**: Pattern Lisp MUST provide `(set-symmetric-difference s1 s2)` operation that returns a set containing elements in either set but not both
- **FR-020**: Pattern Lisp MUST provide `(set-subset? s1 s2)` operation that returns `true` if all elements of `s1` are in `s2`, `false` otherwise
- **FR-021**: Pattern Lisp MUST provide `(set-equal? s1 s2)` operation that returns `true` if sets contain the same elements (regardless of order), `false` otherwise
- **FR-022**: Pattern Lisp MUST provide `(empty? s)` operation that returns `true` if set `s` has no elements, `false` otherwise
- **FR-023**: Pattern Lisp MUST provide `(hash-set element1 element2 ...)` function that constructs a set from elements
- **FR-024**: Pattern Lisp MUST support Subject labels as sets of strings (e.g., `#{"Person" "Employee"}`) since Subject labels are `Set String`
- **FR-025**: Pattern Lisp MAY support prefix colon syntax (e.g., `:Person`) as optional syntactic sugar that evaluates to strings for gram notation interop (implementation may defer this)
- **FR-026**: Pattern Lisp MUST serialize and deserialize keywords, maps, and sets correctly (maintaining their distinct types)
- **FR-027**: Pattern Lisp MUST provide clear error messages when keywords or map/set operations are used incorrectly

### Key Entities *(include if feature involves data)*

- **Keyword**: A self-evaluating symbol with postfix colon syntax (e.g., `name:`) that evaluates to itself without environment lookup. Used primarily as map keys, option/configuration identifiers, and tagged union discriminants.

- **Map**: An associative data structure mapping keywords to values. Maps are unordered collections of key-value pairs where keys are unique keywords. Used for configuration, structured data, tagged unions, and state representation.

- **Set**: An unordered collection of unique elements. Sets automatically remove duplicates and do not preserve order. Used for Subject labels (which are `Set String`), unique value collections, and set-based operations.

- **Subject Label**: A string used in a set to represent node categories or types in gram patterns. Subject labels are `Set String` - plain strings in sets (e.g., `#{"Person" "Employee"}`) are sufficient. Prefix colon syntax (`:Person`) may be implemented as optional syntactic sugar that evaluates to strings for gram notation interop convenience.

## Success Criteria *(mandatory)*

### Measurable Outcomes

- **SC-001**: Developers can create map literals with at least 10 key-value pairs in under 1 second of evaluation time
- **SC-002**: Developers can access nested map values 3 levels deep using `get-in` with consistent performance (under 10ms for typical maps)
- **SC-003**: Developers can perform set union operations on sets containing up to 1000 elements in under 100ms
- **SC-004**: Developers can successfully parse and evaluate programs containing keywords, maps, and sets with 100% syntax correctness (no false positives or false negatives in parsing)
- **SC-005**: Developers can use keywords, maps, and sets in all contexts where they are semantically valid (map keys, set elements, function arguments) without encountering type errors
- **SC-006**: Serialization and deserialization of keywords, maps, and sets maintains data integrity (round-trip serialization preserves all values and types) for 100% of test cases
- **SC-007**: Error messages for incorrect usage of keywords, maps, or sets clearly identify the problem and suggest correct usage in 90% of common error scenarios
- **SC-008**: Developers can create and manipulate maps with up to 100 key-value pairs without performance degradation
- **SC-009**: Set operations (union, intersection, difference) produce correct results for sets containing mixed value types (numbers, strings, keywords) in 100% of test cases
- **SC-010**: Keywords evaluate to themselves (no environment lookup) in 100% of evaluation contexts

## Assumptions

- Keywords, maps, and sets are foundational language features with no external dependencies
- Maps use keywords as keys exclusively (not symbols or other types) for consistency and clarity
- Sets can contain any value type (numbers, strings, keywords, maps, other sets, etc.)
- Subject labels are `Set String` - plain strings in sets are sufficient (e.g., `#{"Person" "Employee"}`)
- Prefix colon syntax (`:Person`) is optional syntactic sugar that may evaluate to strings for gram interop convenience, but is not required
- Map and set operations return new data structures (immutable semantics) rather than mutating existing ones
- Duplicate keys in map literals will use the last value (silently overwrites earlier values)
- Duplicate elements in set literals are automatically removed during construction
- Serialization format for keywords, maps, and sets will be compatible with existing Pattern Lisp serialization mechanisms

## Dependencies

- None - Keywords, maps, and sets are foundational language features that don't depend on other features
- Implementation order: Keywords should be implemented first (needed for maps), Sets can be implemented in parallel with keywords, Maps require keywords, Prefix colon syntax for labels is optional and can be deferred

## Out of Scope

- Enhanced namespace resolution for namespaced symbols (already supported as regular symbols)
- Module/namespace system enhancements (deferred to future work)
- Pattern matching on structured data (deferred to future work)
- Effect system integration (depends on maps but is separate feature)
- Graph lens integration (depends on maps and sets but is separate feature)
- Host-call boundary (depends on maps but is separate feature)
