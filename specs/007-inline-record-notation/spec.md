# Feature Specification: Inline Record Notation for Pattern-Lisp

**Feature Branch**: `007-inline-record-notation`  
**Created**: 2025-01-30  
**Status**: Draft  
**Type**: **REFACTORING** - Replace existing map support with gram-compatible records  
**Input**: User description: "Refactor map support to use gram-style property records in pattern-lisp using { ... } syntax. Records are immutable key-value structures that integrate directly with pattern construction and manipulation."

## User Scenarios & Testing *(mandatory)*

### User Story 1 - Write Records Using Inline Syntax (Priority: P1)

As a pattern-lisp developer, I need to write property records directly in my code using familiar `{ key: value }` syntax so that I can express data structures naturally without verbose function calls.

**Why this priority**: This is the core value proposition - enabling direct expression of records in code. Without this, the feature provides no value.

**Independent Test**: Can be fully tested by writing record literals in pattern-lisp code and verifying they parse correctly and produce record values.

**Acceptance Scenarios**:

1. **Given** pattern-lisp code with `{ name: "Alice", age: 30 }`, **When** the code is parsed, **Then** a record value is created with keys "name" and "age" and corresponding values
2. **Given** pattern-lisp code with an empty record `{ }`, **When** the code is parsed, **Then** an empty record value is created
3. **Given** pattern-lisp code with nested records `{ person: { name: "Alice" } }`, **When** the code is parsed, **Then** a record containing a nested record is created correctly
4. **Given** pattern-lisp code with records containing different value types (strings, numbers, booleans, null), **When** the code is parsed, **Then** all value types are preserved correctly in the record

---

### User Story 2 - Access and Manipulate Record Values (Priority: P2)

As a pattern-lisp developer, I need to read values from records, check for key existence, and create modified copies of records so that I can work with record data programmatically.

**Why this priority**: While inline syntax enables creation, programmatic access and manipulation are necessary for records to be useful in real applications.

**Independent Test**: Can be fully tested by creating records, accessing their values, checking keys, and creating modified copies, verifying all operations work correctly.

**Acceptance Scenarios**:

1. **Given** a record `{ name: "Alice", age: 30 }`, **When** accessing the value for key "name", **Then** the value "Alice" is returned
2. **Given** a record `{ name: "Alice" }`, **When** checking if key "age" exists, **Then** false is returned
3. **Given** a record `{ name: "Alice" }`, **When** creating a new record with an additional key-value pair, **Then** a new record is returned with both the original and new key-value pairs, and the original record remains unchanged
4. **Given** a record `{ name: "Alice", age: 30 }`, **When** creating a new record with a key removed, **Then** a new record is returned without that key, and the original record remains unchanged
5. **Given** a record `{ name: "Alice" }` and another record `{ age: 30 }`, **When** merging the records, **Then** a new record is returned containing keys from both records

---

### User Story 3 - Use Records in Quasiquotation (Priority: P3)

As a pattern-lisp developer, I need to construct records dynamically using unquoting within record literals so that I can build records from computed values and combine records programmatically.

**Why this priority**: Dynamic record construction is useful for advanced use cases but not essential for basic functionality. This enables more sophisticated patterns.

**Independent Test**: Can be fully tested by writing quasiquoted records with unquoted expressions and verifying the resulting records contain the computed values.

**Acceptance Scenarios**:

1. **Given** pattern-lisp code with `(let ((name "Alice") (age 30)) `{ name: ,name, age: ,age })`, **When** the code is evaluated, **Then** a record is created with the values from the variables
2. **Given** pattern-lisp code with `(let ((base { role: "Engineer" })) `{ name: "Alice", ,@base })`, **When** the code is evaluated, **Then** a record is created containing both the explicit key-value pair and all entries from the base record

---

### Edge Cases

- What happens when a record contains duplicate keys (e.g., `{ a: 1, a: 2 }`)?
- How does the system handle records with very large numbers of key-value pairs?
- What happens when accessing a non-existent key without a default value?
- How are records with deeply nested structures (many levels of nesting) handled?
- What happens when a record key contains special characters or is not a valid identifier?
- How does the system handle records in error contexts (e.g., malformed syntax)?
- What happens when comparing records for equality with different key orderings?

## Requirements *(mandatory)*

### Functional Requirements

- **FR-001**: System MUST parse record literals written in `{ key: value }` syntax and produce record values
- **FR-002**: System MUST support empty records written as `{ }`
- **FR-003**: System MUST support nested records where values can be other records
- **FR-004**: System MUST support records containing values of type string, number, boolean, null, and nested records
- **FR-006**: System MUST treat records as immutable values - operations that modify records must return new records without changing the original
- **FR-007**: System MUST support structural equality for records - two records with the same keys and equal values must be considered equal regardless of key ordering
- **FR-008**: System MUST provide a type predicate to check if a value is a record
- **FR-009**: System MUST provide operations to get values from records by key, with support for default values when keys are missing
- **FR-010**: System MUST provide operations to check if a record contains a specific key
- **FR-011**: System MUST provide operations to get all keys from a record
- **FR-012**: System MUST provide operations to get all values from a record
- **FR-013**: System MUST provide operations to create new records by adding or updating key-value pairs
- **FR-014**: System MUST provide operations to create new records by removing key-value pairs
- **FR-015**: System MUST provide operations to merge two records into a new record
- **FR-016**: System MUST provide operations to convert records to and from association lists
- **FR-017**: System MUST support unquoting expressions within record literals in quasiquotation contexts
- **FR-018**: System MUST support splicing record entries into record literals in quasiquotation contexts
- **FR-019**: System MUST produce parse errors with clear location information when record syntax is malformed
- **FR-020**: System MUST handle records with keys that are valid identifiers according to gram syntax rules

### Key Entities *(include if feature involves data)*

- **Record**: An immutable key-value structure using `Subject.Value.VMap (Map String Value)` (gram's PropertyRecord type). Keys are identifiers converted to strings, and values are any gram `Subject.Value` type (strings, numbers, booleans, null, nested records). Records use structural equality and cannot be modified after creation. **This replaces the existing `VMap` type.**
- **Record Literal**: Source code syntax `{ key1: value1, key2: value2 }` that represents a record value. The syntax follows gram's established notation for property records (comma-separated, replacing the old space-separated map syntax). Records are values that can be used anywhere in pattern-lisp code, including as arguments to functions that create subjects/patterns. **This replaces the existing `MapLiteral` type.**

## Success Criteria *(mandatory)*

### Measurable Outcomes

- **SC-001**: Developers can write record literals using `{ key: value }` syntax and have them parse successfully with 100% syntax correctness for valid gram-style record notation
- **SC-003**: All record access operations (get, has-key, keys, values) complete successfully for valid records with performance suitable for interactive use (operations complete in under 100 milliseconds for records with up to 1000 key-value pairs)
- **SC-004**: Record transformation operations (set, remove, merge) produce correct new records while preserving immutability - original records remain unchanged in 100% of operations
- **SC-005**: Record equality comparison works correctly for structurally equivalent records regardless of key ordering, with 100% accuracy
- **SC-006**: Quasiquotation with unquoting and splicing works correctly for record construction, producing expected records in 100% of valid use cases
- **SC-007**: Parse errors for malformed record syntax include accurate line and column information, enabling developers to locate and fix syntax errors quickly
- **SC-008**: All example programs using record notation execute successfully and produce expected results, demonstrating the feature works end-to-end

## Assumptions

- The gram parser can be invoked to parse record syntax from within the pattern-lisp reader
- Record syntax follows gram's established notation exactly, ensuring consistency across the system
- Records are values that can be used anywhere in pattern-lisp code, including with pattern construction functions
- Immutability is a core requirement - records cannot be modified after creation
- Structural equality (not reference equality) is the appropriate comparison method for records
- Keys in records are identifiers (unquoted), matching gram's syntax rules
- The feature should maintain consistency with existing pattern-lisp syntax and conventions
- Performance requirements are suitable for interactive development use cases, not necessarily high-throughput production scenarios

## Dependencies

- Existing gram parser that can parse record syntax
- Pattern-lisp reader infrastructure that can integrate with gram parser
- Pattern construction functions that can accept record values as arguments
- Quasiquotation system that supports unquoting and splicing

## Out of Scope

- Record serialization and deserialization (handled by existing gram serialization)
- Record validation or schema enforcement
- Record operations beyond basic CRUD (create, read, update, delete) and transformation
- Performance optimizations for very large records (beyond basic requirements)
- Record type system beyond basic type predicates
- Integration with other data structures beyond association lists
- Changes to gram's record syntax specification
- Backwards compatibility with existing map structures - **clean break, no compatibility layer**

## Clarifications

### Session 2025-01-30

- Q: Should keys be symbols or strings internally? → A: Keys should be strings internally (matching gram's PropertyRecord), converted from gram identifiers during parsing. This aligns with Option B (gram types throughout).
- Q: What happens with duplicate keys like `{ a: 1, a: 2 }`? → A: Duplicate keys should produce a parse error at parse time, matching typical JSON parser behavior and preventing ambiguity.
- Q: Should records preserve insertion order? → A: No ordering guarantee is required, but implementations may preserve order as an implementation detail. Structural equality must work regardless of key ordering.
- Q: Should `(print rec)` produce valid gram notation? → A: Yes, print representation should produce valid gram notation, enabling round-tripping of record data.
