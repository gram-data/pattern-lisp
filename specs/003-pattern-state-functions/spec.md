# Feature Specification: Pattern State Functions

**Feature Branch**: `003-pattern-state-functions`  
**Created**: 2025-01-27  
**Status**: Draft  
**Input**: User description: "Refine the core concept of pattern lisp programs as pure functions which transform state, where state is represented with a Pattern Subject as described in @docs/pattern-state-lisp-design.md with suggested implementation approach described in @docs/pattern-state-integration-todo.md"

## User Scenarios & Testing *(mandatory)*

### User Story 1 - Pattern as First-Class Value Type (Priority: P1)

A developer needs to use Pattern values directly in Pattern Lisp programs, where Pattern values are decorated with Subject types to enable complete serialization and code-as-data capabilities.

**Why this priority**: Pattern values are the fundamental data structure for state representation. Without Pattern as a first-class value type, programs cannot work with state transformations. This is the foundational capability that enables all state transformation operations.

**Independent Test**: Can be fully tested by creating Pattern values from various data types (numbers, strings, lists) and verifying they can be constructed, queried, and transformed. Delivers the ability to represent and manipulate state as Pattern structures.

**Acceptance Scenarios**:

1. **Given** a Pattern Lisp program, **When** creating a Pattern value from a simple value like a string or number, **Then** the program can construct an atomic Pattern with Subject decoration
2. **Given** a Pattern Lisp program, **When** creating a Pattern with elements, **Then** the program can construct a Pattern containing other Patterns as elements
3. **Given** a Pattern value, **When** querying its structure, **Then** the program can extract the decoration value, elements, size, depth, and other structural properties
4. **Given** a Pattern value, **When** transforming it, **Then** the program returns a new Pattern without mutating the original

---

### User Story 2 - Pure Function State Transformation (Priority: P1)

A developer needs to write Pattern Lisp programs that are pure functions accepting a single state parameter (Pattern Subject) and returning a transformed Pattern Subject, following the canonical form `(lambda (state) ...)`.

**Why this priority**: The canonical form establishes the foundation for tool definitions and enables composability, testability, and traceability. Without this constraint, programs cannot be validated as tools or composed together. This is the core design principle that makes Pattern Lisp suitable for agent tool definitions.

**Independent Test**: Can be fully tested by writing a lambda expression with a single "state" parameter, evaluating it with a Pattern Subject input, and verifying it returns a Pattern Subject output. Delivers the ability to define tools as pure state transformations.

**Acceptance Scenarios**:

1. **Given** a Pattern Lisp program in canonical form `(lambda (state) ...)`, **When** evaluating it with a Pattern Subject input, **Then** it returns a Pattern Subject output
2. **Given** a Pattern Lisp program, **When** validating it as a tool, **Then** programs that don't match the canonical form (wrong parameter count, wrong parameter name, or non-lambda) are rejected with clear error messages
3. **Given** a tool definition, **When** executing it with state, **Then** the tool receives the current state as a Pattern Subject and returns a new Pattern Subject representing the transformed state
4. **Given** multiple tool definitions, **When** composing them sequentially, **Then** each tool receives the output of the previous tool as its input state

---

### User Story 3 - Complete Value Serialization (Priority: P2)

A developer needs all Pattern Lisp values (including closures and primitives) to be serializable to Subject representation, enabling code-as-data, persistence, and complete introspection of program state.

**Why this priority**: Complete serializability enables powerful capabilities like persistence, code distribution, self-modification, and debugging. While not required for basic state transformation, it's essential for the full vision of Pattern Lisp as a portable, inspectable tool definition language.

**Independent Test**: Can be fully tested by converting any Pattern Lisp value (number, string, list, Pattern, closure, primitive) to Subject representation and back, verifying the round-trip preserves functionality. Delivers the ability to serialize and deserialize all program state including code.

**Acceptance Scenarios**:

1. **Given** any basic Pattern Lisp value (number, string, boolean, list), **When** serializing to Subject and deserializing back, **Then** the value is preserved exactly
2. **Given** a closure value (lambda function), **When** serializing to Subject and deserializing back, **Then** the closure remains executable and behaves identically to the original
3. **Given** a primitive value (like +, -, pattern-value), **When** serializing to Subject and deserializing back, **Then** the primitive remains functional and can be used in evaluation
4. **Given** a Pattern containing closures as elements, **When** serializing the entire Pattern to Subject, **Then** all closures are preserved and remain executable after deserialization
5. **Given** a closure with captured environment variables, **When** serializing and deserializing, **Then** the captured environment is preserved and the closure executes correctly

---

### User Story 4 - Pattern Primitives for State Operations (Priority: P2)

A developer needs primitive operations to construct, query, and transform Pattern values, enabling programs to read from and write to state effectively.

**Why this priority**: Pattern primitives are the building blocks for state manipulation. Without them, programs cannot effectively work with Pattern state. While basic Pattern construction might be sufficient for simple cases, full primitive support enables complex state transformations.

**Independent Test**: Can be fully tested by using pattern construction primitives (pattern, pattern-with), query primitives (pattern-value, pattern-elements, pattern-size), and predicate primitives (pattern-find, pattern-any?) to manipulate state. Delivers the ability to read and transform state structures.

**Acceptance Scenarios**:

1. **Given** a Pattern Lisp program, **When** using `(pattern value)` primitive, **Then** it creates an atomic Pattern with the value as Subject decoration
2. **Given** a Pattern Lisp program, **When** using `(pattern-with value elements)` primitive, **Then** it creates a Pattern with the value as decoration and a list of Pattern elements
3. **Given** a Pattern value, **When** using `(pattern-value p)` primitive, **Then** it extracts the Subject decoration and converts it back to a Lisp value
4. **Given** a Pattern value, **When** using `(pattern-elements p)` primitive, **Then** it returns a list of Pattern elements
5. **Given** a Pattern value, **When** using `(pattern-find p predicate)` primitive, **Then** it finds the first subpattern matching the predicate closure
6. **Given** a Pattern value, **When** using query primitives like `(pattern-size p)` or `(pattern-depth p)`, **Then** it returns structural information about the Pattern

---

### User Story 5 - Runtime State Management (Priority: P3)

A developer needs a runtime system that manages Pattern Subject state, validates tool definitions, executes tools, and maintains execution traces for debugging and replay.

**Why this priority**: Runtime state management enables the practical use of Pattern Lisp for tool execution. While not required for basic evaluation, it provides the infrastructure needed for building agent systems with stateful tool execution and traceability.

**Independent Test**: Can be fully tested by creating a runtime with initial state, loading tool definitions, executing tools sequentially, and verifying state transitions and execution traces are recorded correctly. Delivers the ability to run tools in a managed environment with state persistence.

**Acceptance Scenarios**:

1. **Given** a runtime system, **When** loading a tool definition from a file, **Then** the tool is validated for canonical form and registered if valid
2. **Given** a runtime with current state, **When** executing a tool by name, **Then** the tool receives the current state, returns a new state, and the runtime updates its state
3. **Given** a runtime executing multiple tools, **When** each tool completes, **Then** the execution trace records the tool name, input state, and output state for each execution
4. **Given** a runtime with state and tools, **When** serializing the runtime to Subject, **Then** all state, tool definitions, and execution trace are preserved
5. **Given** a serialized runtime, **When** deserializing from Subject, **Then** the runtime can resume execution with the same state and tools

---

### Edge Cases

- What happens when a tool returns a non-Pattern value? (Should error with clear message)
- How does the system handle deeply nested Patterns (depth 10+)? (Should work correctly without stack overflow)
- What happens when serializing a closure with a very large captured environment? (Should handle gracefully)
- How does the system handle circular references in Pattern structures? (Should detect and error appropriately)
- What happens when a primitive is referenced by name that doesn't exist during deserialization? (Should error with helpful message)
- How does the system handle Pattern values with mixed types (numbers, strings, Patterns, closures) as elements? (Should preserve all types correctly)
- What happens when a tool validation fails due to wrong parameter name? (Should provide clear error message indicating expected "state" parameter)
- How does the system handle empty Patterns (no decoration, no elements)? (Should be valid and serializable)

## Requirements *(mandatory)*

### Functional Requirements

- **FR-001**: System MUST support Pattern as a first-class value type in Pattern Lisp, where Pattern values are decorated with Subject types
- **FR-002**: System MUST provide pattern construction primitives (`pattern`, `pattern-with`) that create Pattern values from Lisp values
- **FR-003**: System MUST provide pattern query primitives (`pattern-value`, `pattern-elements`, `pattern-length`, `pattern-size`, `pattern-depth`, `pattern-values`) that extract information from Pattern values
- **FR-004**: System MUST provide pattern predicate primitives (`pattern-find`, `pattern-any?`, `pattern-all?`) that search and filter Pattern structures using closure predicates
- **FR-005**: System MUST enforce canonical form for tool definitions: all valid tools MUST be lambda expressions with exactly one parameter named "state"
- **FR-006**: System MUST validate tool definitions at load time, rejecting programs that don't match canonical form with clear error messages
- **FR-007**: System MUST support complete serialization of all Pattern Lisp values (numbers, strings, booleans, lists, Patterns, closures, primitives) to Subject representation
- **FR-008**: System MUST support complete deserialization of Subject representations back to Pattern Lisp values, preserving functionality for closures and primitives
- **FR-009**: System MUST preserve closure executability after serialization and deserialization (closures must work identically before and after round-trip)
- **FR-010**: System MUST preserve primitive functionality after serialization and deserialization (primitives must work identically before and after round-trip)
- **FR-011**: System MUST support serialization of closures with captured environments, preserving all variable bindings
- **FR-012**: System MUST support serialization of expression ASTs (for closure bodies), enabling code-as-data capabilities
- **FR-013**: System MUST use property-based representation for variable names in serialization (not Gram identifiers), matching Lisp's name-based scoping
- **FR-014**: System MUST provide a runtime system that manages Pattern Subject state, tool definitions, and execution traces
- **FR-015**: System MUST support tool execution that takes current state as input and produces new state as output
- **FR-016**: System MUST maintain execution traces recording tool name, input state, and output state for each tool execution
- **FR-017**: System MUST support runtime serialization (converting entire runtime state including tools, current state, and trace to Subject)
- **FR-018**: System MUST support runtime deserialization (reconstructing runtime from Subject and resuming execution)
- **FR-019**: System MUST handle type errors gracefully (e.g., passing non-Pattern to pattern-value) with clear error messages
- **FR-020**: System MUST handle arity errors gracefully (e.g., wrong number of arguments to primitives) with clear error messages

### Key Entities *(include if feature involves data)*

- **Pattern Subject**: The core data structure representing state. A Pattern decorated with a Subject value, where Subject provides serializable properties and structure. Patterns can contain other Patterns as elements, forming tree structures.

- **Tool Definition**: A Pattern Lisp program in canonical form `(lambda (state) ...)` that transforms Pattern Subject state. Tools are validated at load time and must accept exactly one "state" parameter and return a Pattern Subject.

- **Closure Value**: A serializable function value containing parameter names, body expression, and captured environment. Closures can be stored in state, serialized, and remain executable after deserialization.

- **Primitive Value**: A serializable reference to a built-in operation (arithmetic, comparison, string, pattern operations). Primitives are serialized by name and looked up in a registry during deserialization.

- **Runtime State**: The complete execution context including current Pattern Subject state, registered tool definitions, and execution trace history. The runtime can be serialized and deserialized for persistence.

- **Execution Trace**: A sequence of records, each containing a tool name, input Pattern Subject state, and output Pattern Subject state. Enables debugging, replay, and analysis of tool execution history.

## Success Criteria *(mandatory)*

### Measurable Outcomes

- **SC-001**: Developers can write Pattern Lisp programs that transform Pattern Subject state using the canonical form `(lambda (state) ...)` with 100% validation success rate for correctly formatted tools
- **SC-002**: All Pattern Lisp values (including closures and primitives) can be serialized to Subject and deserialized back with 100% functional preservation (closures remain executable, primitives remain functional)
- **SC-003**: Developers can construct, query, and transform Pattern values using pattern primitives with zero data loss (all structural information preserved through operations)
- **SC-004**: Tool definitions can be loaded, validated, and executed with state transformations completing successfully in under 1 second for typical tool definitions (state size < 1000 nodes)
- **SC-005**: Runtime system can serialize and deserialize complete runtime state (including tools, current state, execution trace) with 100% preservation of functionality (tools remain executable after round-trip)
- **SC-006**: Execution traces accurately record state transformations with 100% fidelity (input and output states match actual tool execution results)
- **SC-007**: Error messages for invalid tool definitions (wrong parameter count, wrong parameter name, non-lambda) are clear and actionable, enabling developers to fix issues on first attempt in 90% of cases
- **SC-008**: Pattern operations (construction, query, predicates) handle nested structures up to depth 20 without performance degradation or errors
- **SC-009**: Serialization and deserialization of closures with captured environments preserves all variable bindings correctly, enabling closures to execute identically before and after round-trip in 100% of test cases

## Assumptions

- Pattern and Subject types are available from the gram-data dependency
- The existing Pattern Lisp evaluator can be extended to support Pattern values
- Variable name scoping in Lisp follows standard lexical scoping rules (no need for global identifier management)
- Gram identifiers (pattern variables) are only used for establishing identity relationships between elements, not for variable bindings in closures
- Host-call mechanism and Graph Lens integration are deferred to later phases (out of scope for this feature)
- All serialization uses Subject representation with property-based variable names (not Gram identifiers) for simplicity and Lisp compatibility

## Dependencies

- gram-data library providing Pattern and Subject types
- Existing Pattern Lisp parser and evaluator (from previous features)
- Pattern Core library for Pattern construction and query operations

## Out of Scope

- Host-call mechanism for side effects (deferred to later phase)
- Graph Lens integration for graph navigation (deferred to later phase)
- Gram notation parsing for tool definitions (currently only S-expressions supported)
- Multi-agent coordination
- Optimized pattern storage backends
- REPL enhancements beyond basic tool loading and execution (detailed REPL features deferred)
