# Data Model: Pattern State Functions

**Feature**: Pattern State Functions  
**Date**: 2025-01-27  
**Phase**: 1 - Design

## Core Entities

### Pattern Subject

**Type**: `Pattern Subject` (from Pattern.Core and Gram.Subject)

**Description**: The core data structure representing state. A Pattern decorated with a Subject value, where Subject provides serializable properties and structure. Patterns can contain other Patterns as elements, forming tree structures.

**Structure**:
- Decoration: `Subject` (provides labels, properties, elements)
- Elements: `[Pattern Subject]` (recursive tree structure)

**Operations**:
- Construction: `pattern :: Subject -> Pattern Subject` (atomic), `patternWith :: Subject -> [Pattern Subject] -> Pattern Subject` (with elements)
- Queries: Extract decoration, elements, size (total nodes), depth (max nesting), length (direct elements)
- Traversal: Flatten to list of all Subject values, find subpatterns matching predicates

**Validation Rules**:
- Patterns are immutable (transformations return new Patterns)
- Subject decoration must be valid Subject value
- Elements must be valid Pattern Subject values

**State Transitions**: N/A (immutable structures)

---

### Value (Extended)

**Type**: Extended `Value` type in `PatternLisp.Syntax`

**Description**: Runtime values that expressions evaluate to. Extended to include Pattern as a first-class value type.

**Structure**:
```haskell
data Value
  = VNumber Integer
  | VString Text
  | VBool Bool
  | VList [Value]
  | VPattern (Pattern Subject)  -- NEW
  | VClosure Closure
  | VPrimitive Primitive
```

**New Constructor**: `VPattern (Pattern Subject)`

**Validation Rules**:
- `VPattern` must contain valid `Pattern Subject`
- Pattern operations require `VPattern` values (type checking in evaluator)

**State Transitions**: N/A (immutable values)

---

### Closure (Serializable)

**Type**: `Closure` in `PatternLisp.Syntax`

**Description**: Function value that captures its lexical environment. Must be serializable to Subject representation.

**Structure**:
```haskell
data Closure = Closure
  { params :: [String]    -- Parameter names
  , body   :: Expr        -- Function body expression (AST)
  , env    :: Env         -- Captured lexical environment
  }
```

**Serialization Format** (as Subject):
- Label: "Closure"
- Property: `params` (list of parameter name strings)
- Elements: `[envSubject, bodySubject]`
  - `envSubject`: Label "Env", elements are bindings
  - Each binding: Label "Binding", properties `{name: String, value: Subject}`
  - `bodySubject`: Expression AST as Subject (via `exprToSubject`)

**Validation Rules**:
- Parameters must be non-empty list of valid identifiers
- Body must be valid Expr
- Environment must be valid Env (Map String Value)
- After serialization/deserialization, closure must remain executable

**State Transitions**: N/A (immutable closures)

---

### Primitive (Serializable)

**Type**: Extended `Primitive` type in `PatternLisp.Syntax`

**Description**: Built-in primitive functions. Extended with pattern operations. Must be serializable by name.

**Structure**:
```haskell
data Primitive
  = Add | Sub | Mul | Div           -- Arithmetic
  | Gt | Lt | Eq | Ne               -- Comparison
  | StringAppend | StringLength | Substring  -- String operations
  -- Pattern construction
  | PatternCreate      -- (pattern value)
  | PatternWith        -- (pattern-with value elements)
  -- Pattern queries
  | PatternValue       -- (pattern-value p)
  | PatternElements    -- (pattern-elements p)
  | PatternLength      -- (pattern-length p)
  | PatternSize        -- (pattern-size p)
  | PatternDepth        -- (pattern-depth p)
  | PatternValues      -- (pattern-values p)
  -- Pattern predicates
  | PatternFind        -- (pattern-find p pred)
  | PatternAny         -- (pattern-any? p pred)
  | PatternAll         -- (pattern-all? p pred)
```

**Serialization Format** (as Subject):
- Label: "Primitive"
- Property: `name` (symbolic primitive name, e.g., "+", "string-append", "pattern-value")

**Helper Functions**:
- `primitiveName :: Primitive -> String` - Convert primitive to name
- `primitiveFromName :: String -> Maybe Primitive` - Look up primitive by name

**Validation Rules**:
- Primitive names must be unique
- Deserialization must find primitive in registry (error if not found)

**State Transitions**: N/A (immutable primitives)

---

### RuntimeState

**Type**: `RuntimeState` in `PatternLisp.Runtime`

**Description**: Complete execution context including environment with functions and states (from `.plisp` and `.gram` files), and optional execution trace history.

**Structure**:
```haskell
data RuntimeState = RuntimeState
  { environment :: Env  -- Functions and states from files
  , executionTrace :: [(String, Pattern Subject, Pattern Subject)]  -- Optional trace
  }
```

**Fields**:
- `environment`: Environment containing functions (from `.plisp` files) and state variables (from `.gram` files)
- `executionTrace`: Optional list of (toolName, inputState, outputState) records (if tracing enabled)

**Operations**:
- `loadPlispFile :: FilePath -> Env -> IO (Either Error Env)` - Load `.plisp` file, evaluate, bind to filename-derived name
- `loadGramFile :: FilePath -> IO (Either Error (String, Pattern Subject))` - Load `.gram` file, parse to Pattern Subject, return (name, state)
- `processFiles :: [FilePath] -> Env -> IO (Either Error Env)` - Process all files (`.gram` first, then `.plisp`)
- `serializeRuntime :: RuntimeState -> Subject` - Serialize runtime to Subject
- `deserializeRuntime :: Subject -> Either Error RuntimeState` - Deserialize runtime from Subject

**Validation Rules**:
- `.plisp` files must evaluate to valid values (VClosure for tools, or any value for named bindings)
- `.gram` files must parse to valid Pattern Subject
- Filename-to-identifier conversion must produce valid Lisp identifiers
- File processing order: `.gram` files first, then `.plisp` files

**State Transitions**:
- File loading: `environment` updated with new bindings from files
- Function invocation: Direct function calls in environment (no special execution model needed)

---

### ExecutionTrace

**Type**: `[(String, Pattern Subject, Pattern Subject)]`

**Description**: Sequence of records, each containing a tool name, input Pattern Subject state, and output Pattern Subject state.

**Structure**: List of tuples:
- `String`: Tool name
- `Pattern Subject`: Input state (before tool execution)
- `Pattern Subject`: Output state (after tool execution)

**Operations**:
- Append: Add new trace record after tool execution
- Query: Find trace records by tool name or state pattern
- Serialize: Convert to Subject representation (part of RuntimeState serialization)

**Validation Rules**:
- Trace records must be in chronological order
- Input and output states must be valid Pattern Subject values
- Tool names must match registered tool definitions

**State Transitions**: Append-only (immutable history)

---

## Relationships

- **Pattern Subject** contains other **Pattern Subject** values as elements (tree structure)
- **Value** can be **VPattern (Pattern Subject)** (first-class Pattern values)
- **Closure** captures **Env** (Map String Value) which may contain **VPattern** values
- **RuntimeState** contains **Pattern Subject** (currentState) and **Value** (toolDefinitions)
- **ExecutionTrace** contains **Pattern Subject** values (input/output states)

## Serialization Relationships

- All **Value** types serialize to **Subject** representation
- **Closure** serializes to **Subject** with environment and body as Subjects
- **Primitive** serializes to **Subject** with name property
- **Pattern Subject** serializes recursively (decoration and elements as Subjects)
- **RuntimeState** serializes to **Subject** (all components as Subjects)

## Validation Summary

- Pattern operations require `VPattern` values (type checking)
- Tool definitions must be canonical form `(lambda (state) ...)` (validation)
- Serialization round-trips must preserve functionality (testing)
- Variable names use property-based representation (not Gram identifiers)
- Primitives must be found in registry during deserialization (error handling)

