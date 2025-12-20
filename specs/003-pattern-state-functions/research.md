# Research: Pattern State Functions

**Feature**: Pattern State Functions  
**Date**: 2025-01-27  
**Phase**: 0 - Research

## Research Tasks

### 1. Pattern and Subject Types from gram-data

**Task**: Understand Pattern and Subject type APIs from gram-data dependency

**Findings**:
- Pattern type: `Pattern v` where `v` is the decoration type (will be `Subject` for this feature)
- Subject type: Provides serializable structure with labels, properties, and elements
- Pattern construction: `pattern :: v -> Pattern v` (atomic), `patternWith :: v -> [Pattern v] -> Pattern v` (with elements)
- Pattern queries: Functions to extract decoration, elements, size, depth, etc.
- Pattern operations are provided by Pattern.Core library

**Decision**: Use `Pattern Subject` as the decoration type for all Pattern values in Pattern Lisp. This enables complete serialization since Subject is the serializable representation.

**Rationale**: Subject provides the serialization format needed for 1:1 Gram correspondence. Pattern with Subject decoration allows all state to be serializable while maintaining Pattern's tree structure.

**Alternatives considered**: 
- Using raw Subject without Pattern: Rejected because Pattern provides the tree structure needed for state representation
- Using custom serialization format: Rejected because Subject is the standard Gram representation

---

### 2. Serialization Patterns for Closures

**Task**: Research how to serialize closures (lambda functions) while preserving executability

**Findings**:
- Closures contain: parameter names, body expression (AST), captured environment
- Serialization approach: Convert each component to Subject representation
  - Parameters: List of strings (property)
  - Body: Expression AST converted to Subject via `exprToSubject`
  - Environment: Map of bindings, each binding as Subject with name and value
- Deserialization approach: Reconstruct closure from Subject components
  - Extract parameters from property
  - Reconstruct body expression via `subjectToExpr`
  - Reconstruct environment by deserializing binding values
  - Closure remains executable because body and environment are preserved

**Decision**: Serialize closures as Subject with:
- Label: "Closure"
- Property: `params` (list of parameter name strings)
- Elements: `[envSubject, bodySubject]`
  - `envSubject`: Label "Env", elements are bindings (Label "Binding", properties `{name: String, value: Subject}`)
  - `bodySubject`: Expression AST as Subject (via `exprToSubject`)

**Rationale**: This structure preserves all closure components in a serializable format. The evaluator can reconstruct closures from this representation and they remain executable.

**Alternatives considered**:
- Serializing closures as bytecode: Rejected because AST preservation is needed for code-as-data
- Not serializing closures: Rejected because complete serializability is a core requirement

---

### 3. Serialization Patterns for Primitives

**Task**: Research how to serialize primitive functions while preserving functionality

**Findings**:
- Primitives are built-in operations (+, -, pattern-value, etc.)
- Serialization approach: Store primitive by name (symbolic identifier)
- Deserialization approach: Look up primitive by name in registry
- Primitive registry: Map from string names to Primitive constructors
- Helper functions needed: `primitiveName :: Primitive -> String` and `primitiveFromName :: String -> Maybe Primitive`

**Decision**: Serialize primitives as Subject with:
- Label: "Primitive"
- Property: `name` (symbolic primitive name, e.g., "+", "string-append", "pattern-value")
- Deserialization: Look up primitive by name using `primitiveFromName` in registry

**Rationale**: Primitives are identified by name, not by implementation. Serializing by name allows primitives to be preserved across serialization boundaries while maintaining functionality through the registry lookup.

**Alternatives considered**:
- Serializing primitive implementations: Rejected because primitives are built-in and not user-defined
- Not serializing primitives: Rejected because complete serializability requires all value types

---

### 4. Variable Names: Properties vs Gram Identifiers

**Task**: Research representation of variable names in serialization (property-based vs Gram identifiers)

**Findings**:
- Gram identifiers (pattern variables like `x` in `(x:Node)`) are used for establishing identity relationships between elements
- Variable names in Lisp closures are scoped lexically (name-based scoping)
- Property-based representation: Variable names stored as property values (e.g., `{name: "x"}`)
- Gram identifiers: Global identifiers used for element identity, not variable scoping

**Decision**: Use property-based representation for variable names (Option B from TODO document):
- Variable references: Subject with Label "Var", property `{name: String}`
- Variable names are properties scoped to their elements, not Gram identifiers
- Gram identifiers only used when establishing identity relationships between elements
- Variable resolution handled by evaluator's scoping rules (not by identifier lookup)

**Rationale**: Property-based representation matches Lisp's name-based scoping. Simpler than identifier management. Avoids complexity of qualified identifiers or UUIDs. Gram identifiers serve a different purpose (element identity) than variable bindings.

**Alternatives considered**:
- Using Gram identifiers for variables: Rejected because it adds unnecessary complexity and doesn't match Lisp's scoping model
- Using UUIDs for variables: Rejected because name-based scoping is simpler and more natural for Lisp

---

### 5. Runtime State Management Patterns

**Task**: Research patterns for managing runtime state, tool validation, and execution tracing

**Findings**:
- Runtime state contains: current Pattern Subject state, tool definitions (Map String Value), execution trace
- Tool validation: Check that evaluated expression is `VClosure (Closure ["state"] _ _)`
- Tool execution: Bind "state" parameter, evaluate closure body, extract result as Pattern Subject
- Execution tracing: Record (toolName, inputState, outputState) for each execution
- State persistence: Serialize entire runtime to Subject, deserialize to resume execution

**Decision**: Implement `RuntimeState` type with:
- `currentState :: Pattern Subject`
- `toolDefinitions :: Map String Value` (serializable tools)
- `executionTrace :: [(String, Pattern Subject, Pattern Subject)]`
- Functions: `validateTool`, `executeTool`, `updateRuntime`, `serializeRuntime`, `deserializeRuntime`

**Rationale**: This structure provides the infrastructure needed for tool execution, state management, and traceability. Serialization enables persistence and code distribution.

**Alternatives considered**:
- Stateless tool execution: Rejected because state management and tracing are core requirements
- Separate tool registry: Rejected because tools are values and should be part of runtime state

---

### 6. Pattern Primitive Implementation Patterns

**Task**: Research implementation patterns for pattern construction, query, and predicate primitives

**Findings**:
- Pattern construction: Convert Lisp values to Subject, create Pattern with Subject decoration
- Pattern queries: Extract information from Pattern structure (decoration, elements, size, depth)
- Pattern predicates: Traverse Pattern structure, apply closure predicates, return results
- Error handling: Type checking (ensure VPattern where needed), arity checking

**Decision**: Implement pattern primitives in `PatternPrimitives.hs`:
- Construction: `evalPatternCreate`, `evalPatternWith`
- Queries: `evalPatternValue`, `evalPatternElements`, `evalPatternLength`, `evalPatternSize`, `evalPatternDepth`, `evalPatternValues`
- Predicates: `evalPatternFind`, `evalPatternAny`, `evalPatternAll`
- All functions use `EvalM` monad for error handling

**Rationale**: Separate module keeps pattern operations organized. `EvalM` provides consistent error handling. Functions can be tested independently.

**Alternatives considered**:
- Inline pattern operations in Eval.hs: Rejected because it would make Eval.hs too large
- Separate pattern library: Rejected because these are Lisp primitives, not standalone library

---

### 7. CLI State Initialization Approaches

**Task**: Research different approaches for initializing Pattern Subject state in the CLI/REPL

**Problem**: Pattern Lisp programs are pure functions `(lambda (state) ...)` that transform Pattern Subject state. Before tools can be executed, the initial state must be established. How should users initialize state in the CLI?

**Approaches Considered**:

#### Option A: Empty Pattern as Default

**Description**: Runtime starts with an empty Pattern Subject (no decoration, no elements). Users must explicitly initialize state before executing tools.

**Implementation**:
- `RuntimeState` initialized with `currentState = patternWith (subject "empty") []`
- REPL command `:init <expr>` to set initial state from Lisp expression
- Example: `:init (pattern "initial-state")`

**Pros**:
- Simple and explicit
- Forces users to think about initial state
- No magic or hidden state

**Cons**:
- Requires explicit initialization step
- Empty state may not be useful for all use cases
- Users must remember to initialize

---

#### Option B: Pattern from Expression

**Description**: REPL command to set state from a Lisp expression that evaluates to a Pattern.

**Implementation**:
- REPL command `:state <expr>` to set state (overloads display command)
- Or separate command: `:set-state <expr>`
- Expression must evaluate to `VPattern`
- Example: `:set-state (pattern-with "root" (list (pattern "a") (pattern "b")))`

**Pros**:
- Flexible - users can construct any Pattern structure
- Uses existing Lisp evaluation
- Can be used to update state at any time

**Cons**:
- Requires users to construct Pattern expressions
- May be verbose for simple cases
- State updates vs initialization not clearly distinguished

---

#### Option C: Pattern from File (Gram or Subject)

**Description**: Load initial state from a Gram file or Subject file.

**Implementation**:
- REPL command `:load-state <file>` to load Pattern Subject from file
- File can be Gram notation or serialized Subject
- Parses file and deserializes to Pattern Subject
- Example: `:load-state initial-state.gram`

**Pros**:
- Enables state persistence and reuse
- Can define complex state structures in files
- Supports version control of state

**Cons**:
- Requires file I/O
- Users must create state files separately
- Less interactive for quick testing

---

#### Option D: Pattern from Command-Line Argument

**Description**: Accept initial state as command-line argument when starting REPL.

**Implementation**:
- CLI flag: `pattern-lisp --state <file>` or `pattern-lisp --state-expr <expr>`
- Loads state from file or evaluates expression before starting REPL
- REPL starts with state already initialized

**Pros**:
- Convenient for scripting and automation
- State ready when REPL starts
- Supports both file and expression forms

**Cons**:
- Only works at startup, not during REPL session
- Less flexible for interactive exploration
- Requires CLI argument parsing

---

#### Option E: Hybrid Approach (Recommended)

**Description**: Combine multiple approaches - default empty state, with commands to initialize from expression, file, or CLI argument.

**Implementation**:
- Default: Empty Pattern Subject on REPL startup
- `:init <expr>` - Set state from Lisp expression
- `:load-state <file>` - Load state from Gram/Subject file
- CLI: `pattern-lisp --init-state <file>` - Initialize from file at startup
- CLI: `pattern-lisp --init-expr <expr>` - Initialize from expression at startup
- `:state` - Display current state (read-only)
- `:set-state <expr>` - Update state from expression (if needed)

**Pros**:
- Maximum flexibility
- Supports both interactive and scripted workflows
- Clear separation between initialization and updates
- Follows principle of least surprise (empty default, explicit initialization)

**Cons**:
- More commands to implement
- Users must learn multiple commands
- More complex than single approach

---

**Decision**: **Option F (Convention-Based Auto-Loading)** - SELECTED

**Selected Approach**: 
- All non-flag CLI arguments are files (`.plisp` → functions, `.gram` → state variables)
- No directory scanning - files provided explicitly
- Filenames become identifiers (remove extension)
- Functions and states are first-class in environment
- Function invocation: `(tool-name state-var)`
- Shell-friendly: `pattern-lisp *.plisp *.gram`

**Rationale**: 
- Convention over configuration (file extensions)
- Explicit file list (no hidden scanning, predictable)
- Shell-friendly (works with globbing)
- More Lisp-like (functions are functions, not special commands)
- Multiple states available simultaneously
- Works for both interactive and non-interactive use

---

## Summary of Decisions

1. **Pattern Subject as decoration type**: All Pattern values use `Pattern Subject` for complete serializability
2. **Closure serialization**: Serialize as Subject with params, environment bindings, and body expression
3. **Primitive serialization**: Serialize by name, lookup in registry during deserialization
4. **Variable names**: Use property-based representation (not Gram identifiers) for Lisp variable scoping
5. **Runtime state**: Manage state, tools, and execution trace in `RuntimeState` type
6. **Pattern primitives**: Implement in separate `PatternPrimitives.hs` module

All research tasks completed. **CLI State Initialization Approach selected**: Option F (Convention-Based Auto-Loading). Ready for Phase 1 design.

