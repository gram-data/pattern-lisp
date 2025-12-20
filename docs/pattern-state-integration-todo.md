# TODO: Pattern State Integration

## Overview

Integrate Pattern as a first-class value type in Pattern Lisp, enforcing that all state is serializable as Subject values. This establishes the foundation for the `Pattern Subject -> Pattern Subject` canonical form.

**Core Principle**: Pattern Lisp is 1:1 with Gram representation. ALL values (including closures and primitives) are serializable, enabling complete introspection, persistence, and code-as-data.

## Design Decisions

- **Value Type**: `Pattern Subject` - All pattern values use Subject for decoration
- **Complete Serializability**: Closures and primitives ARE serializable (code is data)
- **Gram Equivalence**: Every Pattern Lisp value has a canonical Gram representation
- **Deferring**: Host-calls and Graph Lens integration deferred to later phases
- **Module Structure**: Reorganize to `PatternLisp.*` namespace for clarity

## Phase 1A: Core Pattern Integration

### 1. Module Reorganization

**Rationale**: Clean namespace separation and better organization

- [ ] Create `src/PatternLisp/` directory
- [ ] Move and rename files:
  - [ ] `Lisp/Syntax.hs` → `PatternLisp/Syntax.hs`
  - [ ] `Lisp/Parser.hs` → `PatternLisp/Parser.hs`
  - [ ] `Lisp/Eval.hs` → `PatternLisp/Eval.hs`
  - [ ] `Lisp/Primitives.hs` → `PatternLisp/Primitives.hs`
  - [ ] `Lisp/Test.hs` → `PatternLisp/Test.hs` (or remove placeholder)
- [ ] Update module declarations in all files
- [ ] Update imports in all files
- [ ] Update `pattern-lisp.cabal` exposed-modules
- [ ] Update `app/Main.hs` imports
- [ ] Verify build: `cabal build all`
- [ ] Verify tests still pass: `cabal test`

### 2. Add Subject Type Dependency

**Rationale**: Pattern values must use Subject for serializable state

- [ ] Add gram-data dependency to `pattern-lisp.cabal`:
  ```yaml
  build-depends:
    , gram-data
  ```
- [ ] Import Subject and Pattern types in `PatternLisp/Syntax.hs`:
  ```haskell
  import Gram.Subject (Subject)
  import Pattern.Core (Pattern)
  ```
- [ ] Verify Subject type is available: `cabal build`

### 3. Extend Value Type with Pattern

**File**: `src/PatternLisp/Syntax.hs`

- [ ] Add `VPattern` constructor to `Value` type:
  ```haskell
  data Value
    = VNumber Integer
    | VString Text
    | VBool Bool
    | VList [Value]
    | VPattern (Pattern Subject)  -- NEW: Pattern with Subject decoration
    | VClosure Closure
    | VPrimitive Primitive
  ```
- [ ] Add pattern primitives to `Primitive` type:
  ```haskell
  data Primitive
    = Add | Sub | Mul | Div
    | Gt | Lt | Eq | Ne
    | StringAppend | StringLength | Substring
    -- Pattern construction
    | PatternCreate      -- (pattern value)
    | PatternWith        -- (pattern-with value elements)
    -- Pattern queries
    | PatternValue       -- (pattern-value p)
    | PatternElements    -- (pattern-elements p)
    | PatternLength      -- (pattern-length p)
    | PatternSize        -- (pattern-size p)
    | PatternDepth       -- (pattern-depth p)
    | PatternValues      -- (pattern-values p) - flatten
    -- Pattern predicates
    | PatternFind        -- (pattern-find p pred)
    | PatternAny         -- (pattern-any? p pred)
    | PatternAll         -- (pattern-all? p pred)
  ```
- [ ] Add helper for primitive name lookup:
  ```haskell
  primitiveName :: Primitive -> String
  primitiveFromName :: String -> Maybe Primitive
  ```
- [ ] Verify types compile: `cabal build`

### 4. Implement Complete Value ↔ Subject Serialization

**File**: `src/PatternLisp/Subject.hs` (NEW)

**Rationale**: Complete serializability enables code-as-data, persistence, and introspection

**Design Decision**: Use **Option B - Names as Properties**. Variable names are property values scoped to their elements, not Gram identifiers. This is simpler, matches Lisp's name-based scoping, and avoids identifier management complexity. Gram identifiers (pattern variables like `x` in `(x:Node)`) are only used when establishing identity relationships between elements, not for variable bindings.

Create **complete** conversion between Lisp Values and Subjects:

- [ ] Create file `src/PatternLisp/Subject.hs`
- [ ] Implement `valueToSubject :: Value -> Subject`:
  - [ ] `VNumber n` → Subject with label "Number" and numeric property
  - [ ] `VString s` → Subject with label "String" and text property
  - [ ] `VBool b` → Subject with label "Bool" and boolean property
  - [ ] `VList vs` → Subject with label "List" and elements (recursive on values)
  - [ ] `VPattern p` → Subject representing pattern structure (recursive)
  - [ ] `VClosure (Closure params body env)` → Subject with:
    - Label: `"Closure"`
    - Property: `params` as list of parameter name strings
    - Elements: `[envSubject, bodySubject]`
    - `envSubject` has label "Env" with binding elements
    - Each binding: label "Binding", properties `{name: String, value: Subject}`
    - `bodySubject` from `exprToSubject body`
  - [ ] `VPrimitive prim` → Subject with:
    - Label: `"Primitive"`
    - Property: `name` as symbolic primitive name (e.g., "+", "string-append")
- [ ] Implement `subjectToValue :: Subject -> Either Error Value`:
  - [ ] Match on label to determine value type
  - [ ] Deserialize all Value types including closures and primitives
  - [ ] Reconstruct captured environments recursively from binding elements
  - [ ] Look up primitives by name in registry (using `primitiveFromName`)
  - [ ] Handle missing primitives as errors with helpful message
  - [ ] Validate structure (correct properties/elements for each type)
- [ ] Implement `exprToSubject :: Expr -> Subject`:
  - [ ] `Atom (Symbol name)` → Subject with label "Var", property `{name: String}`
  - [ ] `Atom (Number n)` → Subject with label "Number", property `{value: Integer}`
  - [ ] `Atom (String s)` → Subject with label "String", property `{text: Text}`
  - [ ] `Atom (Bool b)` → Subject with label "Bool", property `{value: Bool}`
  - [ ] `List exprs` → Subject with label "List", elements from recursive exprToSubject
  - [ ] `Quote expr` → Subject with label "Quote", single element from exprToSubject
  - [ ] Variable references use name properties, not Gram identifiers
- [ ] Implement `subjectToExpr :: Subject -> Either Error Expr`:
  - [ ] Match on label to determine expression type
  - [ ] Reconstruct Atom, List, Quote forms from Subject structure
  - [ ] Variable names extracted from name properties
  - [ ] Enables closure body reconstruction
- [ ] Add comprehensive unit tests:
  - [ ] Round-trip all basic values (numbers, strings, bools, lists)
  - [ ] Round-trip patterns (nested structures)
  - [ ] Round-trip closures (including captured environment)
  - [ ] Round-trip primitives (by name lookup)
  - [ ] Shadowed variables (verify name scoping works)
  - [ ] Deeply nested closures (closures capturing closures)
  - [ ] Large patterns with mixed value types
- [ ] Document serialization format in module header
- [ ] Document that variable names are properties, not Gram identifiers
- [ ] Document Gram notation equivalents for each value type

### 5. Create Pattern Primitives Module

**File**: `src/PatternLisp/PatternPrimitives.hs` (NEW)

- [ ] Create file with module exports
- [ ] Implement pattern construction primitives:
  - [ ] `evalPatternCreate :: Value -> EvalM Value`
    - Accept a Value, convert to Subject, create atomic pattern
    - Return `VPattern (pattern subject)`
  - [ ] `evalPatternWith :: Value -> [Value] -> EvalM Value`
    - Convert value to Subject for decoration
    - Convert elements: expect VPattern values or convert Values to patterns
    - Create pattern with elements
    - Return `VPattern (patternWith subject elements)`
- [ ] Implement pattern query primitives:
  - [ ] `evalPatternValue :: Pattern Subject -> EvalM Value`
    - Extract Subject decoration
    - Convert Subject back to Value
  - [ ] `evalPatternElements :: Pattern Subject -> EvalM Value`
    - Extract elements list
    - Wrap each as VPattern
    - Return VList of VPattern elements
  - [ ] `evalPatternLength :: Pattern Subject -> EvalM Value`
    - Return VNumber of direct element count
  - [ ] `evalPatternSize :: Pattern Subject -> EvalM Value`
    - Return VNumber of total node count (recursive)
  - [ ] `evalPatternDepth :: Pattern Subject -> EvalM Value`
    - Return VNumber of max depth
  - [ ] `evalPatternValues :: Pattern Subject -> EvalM Value`
    - Flatten pattern to list of all Subject values
    - Convert each Subject to Value
    - Return VList of all values
- [ ] Implement pattern predicate primitives:
  - [ ] `evalPatternFind :: Pattern Subject -> Value -> EvalM Value`
    - Accept closure predicate: Pattern -> Bool
    - Find first subpattern matching predicate
    - Return VPattern if found, or appropriate "nothing" representation
  - [ ] `evalPatternAny :: Pattern Subject -> Value -> EvalM Value`
    - Accept closure predicate: Value -> Bool
    - Check if any Subject value (converted to Value) satisfies predicate
    - Return VBool
  - [ ] `evalPatternAll :: Pattern Subject -> Value -> EvalM Value`
    - Accept closure predicate: Value -> Bool
    - Check if all Subject values satisfy predicate
    - Return VBool
- [ ] Add comprehensive examples in module documentation
- [ ] Document expected value types and error conditions

### 6. Integrate Pattern Primitives into Evaluator

**File**: `src/PatternLisp/Eval.hs`

- [ ] Import `PatternLisp.PatternPrimitives`
- [ ] Import `PatternLisp.Subject` for conversion functions
- [ ] Extend `applyPrimitive` with pattern construction cases:
  ```haskell
  applyPrimitive PatternCreate [val] = 
    PatternPrimitives.evalPatternCreate val
  applyPrimitive PatternWith [val, VList elements] = 
    PatternPrimitives.evalPatternWith val elements
  applyPrimitive PatternWith args = 
    throwError $ ArityMismatch "pattern-with" 2 (length args)
  ```
- [ ] Extend `applyPrimitive` with pattern query cases:
  ```haskell
  applyPrimitive PatternValue [VPattern p] = 
    PatternPrimitives.evalPatternValue p
  applyPrimitive PatternElements [VPattern p] = 
    PatternPrimitives.evalPatternElements p
  applyPrimitive PatternLength [VPattern p] = 
    PatternPrimitives.evalPatternLength p
  -- ... etc for Size, Depth, Values
  ```
- [ ] Extend `applyPrimitive` with pattern predicate cases:
  ```haskell
  applyPrimitive PatternFind [VPattern p, predicate] = 
    PatternPrimitives.evalPatternFind p predicate
  applyPrimitive PatternAny [VPattern p, predicate] = 
    PatternPrimitives.evalPatternAny p predicate
  applyPrimitive PatternAll [VPattern p, predicate] = 
    PatternPrimitives.evalPatternAll p predicate
  ```
- [ ] Add helper function `expectPattern :: Value -> EvalM (Pattern Subject)`:
  ```haskell
  expectPattern :: Value -> EvalM (Pattern Subject)
  expectPattern (VPattern p) = return p
  expectPattern v = throwError $ TypeMismatch 
    ("Expected pattern, but got: " ++ show v) v
  ```
- [ ] Add arity checking for all pattern primitives
- [ ] Add type checking (ensure VPattern where needed)
- [ ] Verify evaluator compiles: `cabal build`

### 7. Update Initial Environment

**File**: `src/PatternLisp/Primitives.hs`

- [ ] Add pattern primitives to `initialEnv`:
  ```haskell
  initialEnv :: Env
  initialEnv = Map.fromList $
    -- Existing arithmetic primitives
    [ ("+", VPrimitive Add)
    , ("-", VPrimitive Sub)
    , ("*", VPrimitive Mul)
    , ("/", VPrimitive Div)
    -- Existing comparison primitives
    , (">", VPrimitive Gt)
    , ("<", VPrimitive Lt)
    , ("=", VPrimitive Eq)
    , ("/=", VPrimitive Ne)
    -- Existing string primitives
    , ("string-append", VPrimitive StringAppend)
    , ("string-length", VPrimitive StringLength)
    , ("substring", VPrimitive Substring)
    ] ++
    -- Pattern construction primitives
    [ ("pattern", VPrimitive PatternCreate)
    , ("pattern-with", VPrimitive PatternWith)
    -- Pattern query primitives
    , ("pattern-value", VPrimitive PatternValue)
    , ("pattern-elements", VPrimitive PatternElements)
    , ("pattern-length", VPrimitive PatternLength)
    , ("pattern-size", VPrimitive PatternSize)
    , ("pattern-depth", VPrimitive PatternDepth)
    , ("pattern-values", VPrimitive PatternValues)
    -- Pattern predicate primitives
    , ("pattern-find", VPrimitive PatternFind)
    , ("pattern-any?", VPrimitive PatternAny)
    , ("pattern-all?", VPrimitive PatternAll)
    ]
  ```
- [ ] Verify all primitives are registered correctly
- [ ] Test in REPL: `(pattern "hello")`
- [ ] Test pattern with elements: `(pattern-with "root" (list (pattern "a") (pattern "b")))`

## Phase 1B: Tool Validation and Runtime

### 8. Create Runtime Module

**File**: `src/PatternLisp/Runtime.hs` (NEW)

- [ ] Define `RuntimeState` type:
  ```haskell
  data RuntimeState = RuntimeState
    { currentState :: Pattern Subject
    , toolDefinitions :: Map String Value  -- Serializable tools
    , executionTrace :: [(String, Pattern Subject, Pattern Subject)]
    }
  ```
- [ ] Implement `validateTool :: Expr -> Env -> Either Error Value`:
  - [ ] Parse and evaluate expression
  - [ ] Check result is `VClosure (Closure ["state"] _ _)`
  - [ ] Return closure if valid
  - [ ] Return error with helpful message if not canonical form
- [ ] Implement `executeTool :: Value -> Pattern Subject -> Env -> Either Error (Pattern Subject)`:
  - [ ] Verify value is a valid closure (VClosure)
  - [ ] Bind "state" parameter to `VPattern state` in closure's captured env
  - [ ] Evaluate closure body in extended environment
  - [ ] Extract result, ensuring it's a VPattern
  - [ ] Return the resulting Pattern Subject
- [ ] Implement `updateRuntime :: String -> Pattern Subject -> RuntimeState -> RuntimeState`:
  - [ ] Update currentState with new state
  - [ ] Append (toolName, oldState, newState) to executionTrace
  - [ ] Return updated RuntimeState
- [ ] Implement `serializeRuntime :: RuntimeState -> Subject`:
  - [ ] Convert entire runtime state to Subject (tools, state, trace)
  - [ ] Enables persisting runtime to Gram
- [ ] Implement `deserializeRuntime :: Subject -> Either Error RuntimeState`:
  - [ ] Reconstruct runtime from Subject
  - [ ] Enables loading runtime from Gram
- [ ] Add comprehensive documentation and examples

### 9. Update REPL for Tool Execution

**File**: `app/Main.hs`

- [ ] Import `PatternLisp.Runtime`
- [ ] Import `PatternLisp.Subject` for display formatting
- [ ] Add REPL state to track RuntimeState
- [ ] Add REPL command `:load <file>` to load tool definitions:
  - [ ] Parse .plisp file
  - [ ] Validate as tool (canonical form check)
  - [ ] Register in toolDefinitions map
- [ ] Add REPL command `:exec <tool-name>` to execute a tool:
  - [ ] Look up tool in toolDefinitions
  - [ ] Execute with current state
  - [ ] Update runtime with result
  - [ ] Display resulting state
- [ ] Add REPL command `:state` to display current state:
  - [ ] Format Pattern Subject as readable output
  - [ ] Show structure (depth-limited for large patterns)
- [ ] Add REPL command `:trace` to show execution trace:
  - [ ] Display list of tool executions
  - [ ] Show input/output states for each
- [ ] Add REPL command `:save <file>` to persist runtime:
  - [ ] Serialize RuntimeState to Subject
  - [ ] Write as Gram to file
- [ ] Add REPL command `:restore <file>` to load runtime:
  - [ ] Parse Gram file to Subject
  - [ ] Deserialize to RuntimeState
  - [ ] Resume with loaded state and tools
- [ ] Implement pretty-printing for Pattern values
- [ ] Add error handling for tool validation failures
- [ ] Update help text (`:help`) with all new commands

### 10. Create Example Tools

**Files**: `examples/*.plisp`

- [ ] Create `examples/README.md` with overview and usage instructions
- [ ] Create `examples/basic-patterns.plisp`:
  ```scheme
  ;; Demonstrate pattern construction
  (define atomic (pattern "atom"))
  (define with-elements (pattern-with "root" 
    (list (pattern "child1") (pattern "child2"))))
  
  ;; Demonstrate pattern queries
  (pattern-value atomic)        ; => "atom"
  (pattern-elements with-elements) ; => list of patterns
  (pattern-length with-elements)   ; => 2
  ```
- [ ] Create `examples/hello-tool.plisp`:
  ```scheme
  ;; Simple tool that adds a greeting to state
  (lambda (state)
    (pattern-with 
      (pattern-value state)  ; Preserve decoration
      (cons 
        (pattern "Hello from Pattern Lisp!")
        (pattern-elements state))))
  ```
- [ ] Create `examples/state-reader.plisp`:
  ```scheme
  ;; Tool that reads from state and creates summary
  (lambda (state)
    (let ((count (pattern-size state))
          (depth (pattern-depth state)))
      (pattern-with
        (pattern-value state)
        (cons 
          (pattern-with "summary" 
            (list 
              (pattern count)
              (pattern depth)))
          (pattern-elements state)))))
  ```
- [ ] Create `examples/filter-tool.plisp`:
  ```scheme
  ;; Tool that filters state elements using pattern-find
  (lambda (state)
    (let ((filtered (pattern-find state 
                      (lambda (p) 
                        (> (pattern-size p) 1)))))
      (pattern-with "filtered" (list filtered))))
  ```
- [ ] Create `examples/composition.plisp`:
  ```scheme
  ;; Multiple tools demonstrating composition
  (define add-timestamp
    (lambda (state)
      (pattern-with (pattern-value state)
        (cons (pattern "timestamp") (pattern-elements state)))))
  
  (define add-metadata
    (lambda (state)
      (pattern-with (pattern-value state)
        (cons (pattern "metadata") (pattern-elements state)))))
  
  ;; Composed tool
  (lambda (state)
    (add-metadata (add-timestamp state)))
  ```
- [ ] Create `examples/closure-in-state.plisp`:
  ```scheme
  ;; Demonstrate code-as-data: storing closures in state
  (lambda (state)
    (let ((incrementer (lambda (x) (+ x 1)))
          (doubler (lambda (x) (* x 2))))
      (pattern-with "functions"
        (list (pattern incrementer)
              (pattern doubler)))))
  ```

## Phase 1C: Testing

### 11. Unit Tests for Pattern Integration

**File**: `test/PatternLisp/PatternSpec.hs` (NEW)

- [ ] Create test module with imports
- [ ] Test pattern construction primitives:
  - [ ] `(pattern "value")` creates atomic pattern with Subject decoration
  - [ ] `(pattern 42)` creates pattern with numeric Subject
  - [ ] `(pattern-with "root" (list ...))` creates pattern with elements
  - [ ] Elements can be mixed value types
- [ ] Test pattern query primitives:
  - [ ] `(pattern-value p)` extracts decoration correctly
  - [ ] `(pattern-elements p)` returns list of VPattern elements
  - [ ] `(pattern-length p)` returns correct count
  - [ ] `(pattern-size p)` counts all nodes recursively
  - [ ] `(pattern-depth p)` returns max depth correctly
  - [ ] `(pattern-values p)` flattens all values
- [ ] Test pattern predicate primitives:
  - [ ] `(pattern-find p pred)` finds matching pattern
  - [ ] `(pattern-any? p pred)` checks existence
  - [ ] `(pattern-all? p pred)` checks universal property
  - [ ] Predicates work with closures
- [ ] Test nested patterns:
  - [ ] Deeply nested structures (depth 5+)
  - [ ] Wide structures (many elements)
  - [ ] Mixed nesting patterns
- [ ] Test error cases:
  - [ ] Type mismatches (non-pattern to pattern-value)
  - [ ] Arity errors (wrong number of arguments)
  - [ ] Invalid predicates (non-closure to pattern-find)

### 12. Integration Tests for Tools

**File**: `test/PatternLisp/RuntimeSpec.hs` (NEW)

- [ ] Create test module
- [ ] Test tool validation:
  - [ ] Valid tool: `(lambda (state) ...)`
  - [ ] Invalid: `(lambda (x y) ...)` - wrong param count
  - [ ] Invalid: `(lambda () ...)` - no parameters
  - [ ] Invalid: `(lambda (x) ...)` - wrong param name
  - [ ] Invalid: not a lambda expression
- [ ] Test tool execution:
  - [ ] Identity tool: `(lambda (state) state)`
  - [ ] Tool that adds to state elements
  - [ ] Tool that transforms state decoration
  - [ ] Tool that queries and conditionally transforms
  - [ ] Tool that uses pattern predicates
  - [ ] Tool that returns non-pattern (should error)
- [ ] Test execution tracing:
  - [ ] Trace records input/output states correctly
  - [ ] Multiple tool executions build trace history
  - [ ] Trace preserves state snapshots
- [ ] Test state threading:
  - [ ] Execute multiple tools sequentially
  - [ ] Each tool sees previous tool's output
  - [ ] Final state accumulates all transformations
- [ ] Test error handling:
  - [ ] Tool evaluation errors propagate
  - [ ] Type errors in tool body caught
  - [ ] Runtime exceptions handled gracefully
- [ ] Test runtime serialization:
  - [ ] Serialize runtime to Subject
  - [ ] Deserialize runtime from Subject
  - [ ] Round-trip preserves tools and state

### 13. Complete Serialization Tests

**File**: `test/PatternLisp/SubjectSpec.hs` (NEW)

- [ ] Test `valueToSubject` conversion:
  - [ ] Numbers convert correctly
  - [ ] Strings convert correctly
  - [ ] Booleans convert correctly
  - [ ] Lists convert correctly (recursive)
  - [ ] Patterns convert correctly (recursive structure)
  - [ ] **Closures convert correctly:**
    - [ ] Simple closure: `(lambda (x) x)`
    - [ ] Closure with arithmetic: `(lambda (x) (+ x 1))`
    - [ ] Closure with captured environment
    - [ ] Nested closures (closure returning closure)
  - [ ] **Primitives convert correctly:**
    - [ ] All arithmetic primitives (+, -, *, /)
    - [ ] All comparison primitives (>, <, =, /=)
    - [ ] All string primitives
    - [ ] All pattern primitives
- [ ] Test `subjectToValue` conversion:
  - [ ] Round-trip all basic values
  - [ ] Round-trip lists with nested values
  - [ ] Round-trip patterns with complex structure
  - [ ] **Round-trip closures:**
    - [ ] Reconstruct lambda parameters
    - [ ] Reconstruct body expression
    - [ ] Reconstruct captured environment
    - [ ] Closures remain executable after round-trip
  - [ ] **Round-trip primitives:**
    - [ ] Primitive lookup by name succeeds
    - [ ] Primitives remain functional after round-trip
- [ ] Test Expr ↔ Subject conversion:
  - [ ] `exprToSubject` handles all Expr forms
  - [ ] `subjectToExpr` reconstructs AST correctly
  - [ ] Round-trip preserves expression structure
- [ ] Test edge cases:
  - [ ] Empty lists, empty patterns
  - [ ] Deeply nested closures (5+ levels)
  - [ ] Large captured environments
  - [ ] Closures capturing other closures
- [ ] Test code-as-data scenarios:
  - [ ] Pattern containing closures as elements
  - [ ] List of mixed closures and primitives
  - [ ] Closure capturing pattern containing closure

## Phase 1D: Documentation

### 14. Update README

**File**: `README.md`

- [ ] Update overview to emphasize Pattern State design
- [ ] Highlight 1:1 Gram representation property
- [ ] Add section on `Pattern Subject -> Pattern Subject` canonical form
- [ ] Add examples of pattern operations
- [ ] Add examples of tool definitions
- [ ] Show closure serialization example
- [ ] Show code-as-data example (closures in state)
- [ ] Update architecture diagram with serialization layer
- [ ] Add "Quick Start" section for tool development
- [ ] Add REPL command reference
- [ ] Link to detailed documentation

### 15. Document Pattern Primitives

**File**: `docs/pattern-primitives.md` (NEW)

- [ ] Create comprehensive reference for all pattern primitives
- [ ] For each primitive, document:
  - [ ] Signature with types
  - [ ] Detailed description
  - [ ] Multiple examples (simple and complex)
  - [ ] Error cases and error messages
  - [ ] Performance characteristics
- [ ] Pattern construction section:
  - [ ] `pattern` - atomic patterns
  - [ ] `pattern-with` - patterns with elements
- [ ] Pattern query section:
  - [ ] `pattern-value`, `pattern-elements`
  - [ ] `pattern-length`, `pattern-size`, `pattern-depth`
  - [ ] `pattern-values` - flattening
- [ ] Pattern predicate section:
  - [ ] `pattern-find`, `pattern-any?`, `pattern-all?`
  - [ ] Using closures as predicates
- [ ] Include idiomatic usage patterns
- [ ] Show common recipes (filtering, mapping, searching)

### 16. Document Tool Development

**File**: `docs/tool-development.md` (NEW)

- [ ] Explain canonical form: `(lambda (state) ...)`
- [ ] Why this form (purity, composability, traceability)
- [ ] Show how to read from state:
  - [ ] Extracting values
  - [ ] Querying patterns
  - [ ] Finding specific patterns
- [ ] Show how to transform state:
  - [ ] Adding elements
  - [ ] Removing elements
  - [ ] Transforming decoration
- [ ] Show how to compose tools:
  - [ ] Sequential composition
  - [ ] Conditional execution
  - [ ] Higher-order tools
- [ ] Document code-as-data patterns:
  - [ ] Storing closures in state
  - [ ] Dynamic tool selection
  - [ ] Self-modifying tools
- [ ] Include testing guidelines:
  - [ ] Unit testing tools
  - [ ] Testing with mock state
  - [ ] Property-based testing
- [ ] Include debugging tips:
  - [ ] Using `:trace` command
  - [ ] Inspecting state with `:state`
  - [ ] Common pitfalls
- [ ] Reference example tools in examples/

### 17. Document Serialization Format

**File**: `docs/serialization.md` (NEW)

- [ ] Explain 1:1 Gram correspondence principle
- [ ] **Clarify identity model:**
  - [ ] Gram identifiers (pattern variables) vs. property values
  - [ ] Variable names are properties scoped to elements, not global identifiers
  - [ ] Gram identifiers only used for establishing identity relationships between elements
  - [ ] Example: `(x:Node)` vs. `{name: "x"}` - when to use each
- [ ] Document Subject representation for each value type:
  - [ ] Numbers: label "Number", property `{value: Integer}`
  - [ ] Strings: label "String", property `{text: Text}`
  - [ ] Booleans: label "Bool", property `{value: Bool}`
  - [ ] Lists: label "List", elements are recursive Subjects
  - [ ] Patterns: recursive structure preserving decoration and elements
  - [ ] **Closures:**
    - Label: "Closure"
    - Property: `params` as list of parameter name strings
    - Elements: environment bindings and body expression
    - Each binding: label "Binding", properties `{name: String, value: Subject}`
    - Body: expression as Subject (via exprToSubject)
    - Example Gram representation
  - [ ] **Primitives:**
    - Label: "Primitive"
    - Property: `name` as symbolic primitive name
    - Lookup in primitive registry on deserialization
    - Example Gram representation
  - [ ] **Variable references:**
    - Label: "Var"
    - Property: `{name: String}` - just a name, not a Gram identifier
    - Resolved by evaluator's scoping rules
- [ ] Show complete examples:
  - [ ] Simple closure: `(lambda (x) (+ x 1))`
  - [ ] Closure with captured vars
  - [ ] Closure with shadowed variables (show scoping)
  - [ ] Pattern containing closures
  - [ ] Complete tool definition in Gram
- [ ] Document round-trip guarantees:
  - [ ] What survives serialization
  - [ ] When equality is preserved
  - [ ] When equivalence (not equality) is guaranteed
- [ ] Document persistence workflow:
  - [ ] Save runtime to file
  - [ ] Load runtime from file
  - [ ] Versioning considerations
- [ ] Include section on scoping:
  - [ ] How variable names in properties are resolved
  - [ ] Shadowing works naturally via evaluator's scoping
  - [ ] No need for qualified identifiers or UUIDs

### 18. Update Design Document

**File**: `design/pattern-state-lisp.md`

- [ ] Update with implementation details
- [ ] Add section on complete serializability
- [ ] Emphasize code-as-data properties
- [ ] Document module organization
- [ ] Add examples using actual implemented syntax
- [ ] Update benefits section with serialization advantages:
  - [ ] Complete introspection
  - [ ] Persistence and resume
  - [ ] Code distribution
  - [ ] Self-modification
- [ ] Link to primitive reference docs
- [ ] Link to serialization format docs
- [ ] Note deferred features (host-calls, Graph Lens)

## Verification Checklist

After completing all phases:

- [ ] All tests pass: `cabal test`
- [ ] REPL can load and execute example tools
- [ ] Pattern operations work as expected
- [ ] Tool validation enforces canonical form
- [ ] Execution trace records state transformations
- [ ] **Serialization round-trips work:**
  - [ ] Basic values serialize/deserialize
  - [ ] Closures serialize/deserialize and remain executable
  - [ ] Primitives serialize/deserialize and remain functional
  - [ ] Complex patterns with closures round-trip correctly
- [ ] **Runtime persistence works:**
  - [ ] Save runtime to Gram file
  - [ ] Load runtime from Gram file
  - [ ] Execution continues correctly after load
- [ ] Documentation is complete and accurate
- [ ] Examples demonstrate all key features
- [ ] Code is well-commented
- [ ] Module organization is clean
- [ ] No regressions in existing functionality

## Future Phases (Not in Scope)

These are explicitly deferred:

- Host-call mechanism for side effects
- Graph Lens integration for graph navigation
- Gram notation parsing for tool definitions (currently only S-expressions)
- Multi-agent coordination
- Optimized pattern storage backends

## Notes

- **Complete Serializability**: ALL values, including closures and primitives, must serialize to Subject. This is fundamental to the design.
- **Code-as-Data**: Closures can be stored in state, inspected, and even modified. This enables powerful metaprogramming patterns.
- **1:1 Gram Correspondence**: Every Pattern Lisp value has a canonical Gram representation. This is not optional.
- **Module Organization**: The `PatternLisp.*` namespace keeps the code organized and signals this is a specialized Lisp variant.
- **Test Coverage**: Comprehensive testing of serialization is critical, as it's the foundation for persistence and code-as-data.
- **Examples First**: Clear examples demonstrating code-as-data and closure serialization are essential for understanding the power of this design.
