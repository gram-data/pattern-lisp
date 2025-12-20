# Pattern State Lisp Design

## Overview

Tiny Lisp is specialized as a pure functional language where **all programs are lambdas that transform Pattern state**. This design unifies the language semantics with the Pattern data model, creating an elegant foundation for portable tool definitions.

## Core Design Principle

Every Lisp program has the canonical type:

```haskell
type LispProgram v = Pattern v -> Pattern v
```

In S-expression form:
```scheme
(lambda (state) <body>)
```

Where:
- **Input**: `state` is a Pattern containing the current context/knowledge
- **Output**: A new Pattern representing the transformed state
- **Purity**: No mutation, no global state, no hidden side effects

## Why This Design

### 1. Eliminates State Management Complexity

**Traditional Approach** (with global state):
```scheme
;; Requires IORef, state threading, mutation
(update-state! (lambda (s) ...))
(get-state)
(set-state! new-state)
```

**Pattern State Approach**:
```scheme
;; Pure function, runtime handles state threading
(lambda (state)
  (pattern-with 
    (pattern-value state)
    (cons new-item (pattern-elements state))))
```

The runtime manages state transitions. Lisp code is purely functional.

### 2. Perfect for Tool Definitions

Tools in Pattern Agents are naturally `Pattern v -> Pattern v` transformations:

```scheme
;; Tool: Extract and format user greeting
(lambda (state)
  (let* ((user (pattern-find state is-user?))
         (name (pattern-query user "$.name")))
    (pattern-with 
      {:greeting (string-append "Hello, " name)}
      (pattern-elements state))))
```

### 3. Composition is Natural

Tools compose as functions:

```scheme
;; Define tools
(define get-user 
  (lambda (state)
    (pattern-find state is-user?)))

(define format-greeting
  (lambda (state)
    (let ((user (get-user state)))
      (pattern {:greeting (string-append "Hello, " (pattern-value user))}))))

;; Compose: format-greeting ∘ get-user
(define greet-user 
  (lambda (state)
    (format-greeting (get-user state))))
```

No special coordination machinery needed. Function composition suffices.

### 4. Testing is Trivial

```haskell
-- Test a tool with input/output patterns
testTool :: Pattern v -> Pattern v -> Bool
testTool input expected = 
  runLisp toolProgram input == Right expected
```

Pure functions eliminate mocking, setup, and teardown. Just assert input → output.

### 5. Traceability and Time Travel

Every execution is a recorded state transformation:

```haskell
-- Execution trace
type Trace v = [(ToolName, Pattern v, Pattern v)]

trace :: Trace v
trace = 
  [ ("lookup_user", state0, state1)
  , ("format_greeting", state1, state2)
  , ("send_email", state2, state3)
  ]

-- Replay from any point
replayFrom :: Int -> Trace v -> Pattern v
replayFrom idx trace = 
  foldl' (\s (_, _, result) -> result) initialState (drop idx trace)
```

## Language Design

### Core Expression Types

```haskell
data Expr
  = EVar String                -- Variable reference
  | ELambda [String] Expr      -- Lambda (canonical: single "state" param)
  | EApp Expr [Expr]           -- Function application
  | ELet String Expr Expr      -- Local binding
  | EIf Expr Expr Expr         -- Conditional
  | EPattern (Pattern v)       -- Pattern literal
  | EHostCall String [Expr]    -- Host function call (for side effects)
```

### Value Types

```haskell
data Value v
  = VNum Double
  | VStr String
  | VBool Bool
  | VPattern (Pattern v)       -- Pattern is first-class
  | VClosure [String] Expr Env
  | VHostFn (HostFunction v)
```

### Canonical Program Form

Every valid Lisp program must evaluate to:

```haskell
VClosure ["state"] body env :: Value v
```

This is enforced at the tool definition boundary.

## Pattern as First-Class Value

Pattern is a **native Lisp value type**, not accessed through host-calls.

### Pattern Primitives (Read-Only)

```scheme
;; Pattern inspection
(pattern-value p)          ; Extract decoration value
(pattern-elements p)       ; Get elements list
(pattern-length p)         ; Number of direct elements
(pattern-size p)           ; Total nodes in structure
(pattern-depth p)          ; Maximum nesting depth
(pattern-values p)         ; Flatten all values

;; Pattern queries
(pattern-find p predicate) ; Find first matching subpattern
(pattern-filter p pred)    ; Filter subpatterns
(pattern-any? p pred)      ; Any value satisfies predicate
(pattern-all? p pred)      ; All values satisfy predicate
```

### Pattern Construction (Return New Values)

```scheme
;; Pattern construction
(pattern value)                    ; Atomic pattern
(pattern-with value elements)      ; Pattern with elements
(from-list value [v1 v2 v3])      ; Convenience constructor

;; Pattern transformation (returns new pattern)
(pattern-map fn p)                 ; Map over values
(pattern-extend p new-elements)    ; Add elements
(pattern-replace-at idx new-elem p); Replace element
```

### Graph Lens Integration

```scheme
;; Create a graph lens
(graph-lens scope-pattern node-predicate)

;; Graph operations (using lens)
(nodes lens)                       ; Extract all nodes
(relationships lens)               ; Extract all relationships
(neighbors lens node)              ; Find adjacent nodes
(degree lens node)                 ; Count incident relationships
(connected-components lens)        ; Find components
(find-path lens start end)         ; Path between nodes
```

The state Pattern becomes a **knowledge graph** that tools navigate and transform.

## Host-Call Boundary

Since programs are pure, **host-calls** are the only mechanism for side effects:

```scheme
(lambda (state)
  (let ((user-id (pattern-query state "$.user.id")))
    ;; Host call performs I/O, returns value
    (let ((db-result (host-call 'db-query 
                        "SELECT * FROM users WHERE id = ?" 
                        user-id)))
      ;; Incorporate result into new state pattern
      (pattern-with 
        {:query-result db-result}
        (pattern-elements state)))))
```

### Host-Call Semantics

- **Evaluation**: Host-calls are evaluated for their return value
- **Side Effects**: Effects occur during evaluation (DB query, HTTP request, file I/O)
- **Return Value**: Returned value is incorporated into the output Pattern
- **Purity Boundary**: Only place where side effects occur

### Standard Host Functions

```scheme
;; I/O operations
(host-call 'file-read path)
(host-call 'file-write path content)
(host-call 'http-get url)
(host-call 'http-post url body)

;; Database operations
(host-call 'db-query sql params)
(host-call 'db-execute sql params)

;; External process
(host-call 'exec command args)

;; Pattern persistence (future)
(host-call 'pattern-persist handle pattern)
(host-call 'pattern-load handle)
```

## Runtime Execution Model

### Agent Runtime

```haskell
data AgentRuntime v = AgentRuntime
  { currentState :: Pattern v
  , toolDefinitions :: Map String (LispProgram v)
  , hostFunctions :: Map String (HostFunction v)
  , executionTrace :: [(ToolName, Pattern v, Pattern v)]
  }

-- Execute a tool by name
executeTool :: String -> AgentRuntime v -> Either Error (Pattern v, AgentRuntime v)
executeTool toolName runtime = do
  program <- lookupTool toolName (toolDefinitions runtime)
  newState <- runLisp program (currentState runtime)
  let trace = (toolName, currentState runtime, newState) : executionTrace runtime
  return (newState, runtime { currentState = newState, executionTrace = trace })
```

### Execution Flow

```
┌─────────────────────────────────────┐
│   Initial Pattern State             │
│   (user context, conversation, etc) │
└─────────────────────────────────────┘
                 │
                 ▼
┌─────────────────────────────────────┐
│   Tool: (lambda (state) ...)        │
│   • Read state (Pattern operations) │
│   • Query/navigate (Graph Lens)     │
│   • Transform (return new Pattern)  │
│   • Side effects (host-calls)       │
└─────────────────────────────────────┘
                 │
                 ▼
┌─────────────────────────────────────┐
│   New Pattern State                 │
│   (updated with tool results)       │
└─────────────────────────────────────┘
                 │
                 ▼
         [Next Tool or Return]
```

## Example: Complete Tool Definition

### In Gram Notation

```gram
[:Tool {name: "lookup_order", params: ["order_id"]} |
  [:Type {
    signature: "Pattern<V> -> Pattern<V>"
  }],
  [:Implementation |
    (lambda (state)
      (let* ((order-id (pattern-query state "$.params.order_id"))
             (db-result (host-call 'db-query 
                          "SELECT * FROM orders WHERE id = ?" 
                          order-id))
             (order-pattern (pattern-with 
                             {:type "Order"
                              :id order-id
                              :data db-result}
                             [])))
        (pattern-with
          (pattern-value state)
          (cons order-pattern (pattern-elements state)))))
  ]
]
```

### In Pure S-Expression

```scheme
;; lookup_order tool
(lambda (state)
  (let* ((order-id (pattern-query state "$.params.order_id"))
         (db-result (host-call 'db-query 
                      "SELECT * FROM orders WHERE id = ?" 
                      order-id))
         (order-pattern (pattern-with 
                         {:type "Order"
                          :id order-id
                          :data db-result}
                         [])))
    (pattern-with
      (pattern-value state)
      (cons order-pattern (pattern-elements state)))))
```

## Benefits for Pattern Agents

### 1. Unified Data Model

State, knowledge, context, and results are all represented as Pattern structures. The language and data model are one.

### 2. Graph-Native Navigation

Graph Lens provides natural graph navigation over state:

```scheme
(lambda (state)
  (let* ((lens (graph-lens state is-atomic?))
         (user-nodes (filter (lambda (n) 
                               (equal? (pattern-query n "$.type") "User"))
                             (nodes lens)))
         (connections (relationships lens)))
    ;; Navigate state as a graph
    ...))
```

### 3. Factorizable Tools

Complex agents decompose into simple tool transformations:

```scheme
;; Complex agent = composition of tools
(define process-order
  (compose 
    validate-order
    calculate-total
    apply-discount
    create-invoice))

;; Each tool: Pattern v -> Pattern v
```

### 4. Portable Definitions

The same tool definition runs across all runtimes (browser, Python, JVM, Rust):

```gram
// Single .gram file contains complete agent
[:Agent {name: "OrderProcessor"} |
  [:Tool {name: "validate"} | (lambda (state) ...)],
  [:Tool {name: "process"} | (lambda (state) ...)],
  [:Tool {name: "notify"} | (lambda (state) ...)]
]
```

### 5. Inspectable Execution

Every tool execution produces a state transformation that can be:
- Traced (input → output)
- Replayed (rerun from any point)
- Analyzed (what changed?)
- Debugged (why did this fail?)

## Implementation Phases

### Phase 1: Pure Lisp with Pattern Values
- S-expression parser
- Evaluator with Pattern as native value type
- Pattern primitive operations
- Basic REPL

### Phase 2: Canonical Form Enforcement
- Validate programs are `(lambda (state) ...)`
- Tool definition parser from Gram
- Runtime execution with state threading

### Phase 3: Host-Call Boundary
- Host function registry
- Side effect evaluation
- Error handling for I/O failures

### Phase 4: Graph Lens Integration
- Graph Lens construction in Lisp
- Node/relationship queries
- Graph navigation primitives

### Phase 5: Agent Runtime
- Multi-tool execution
- Execution tracing
- State persistence (via host-calls)

## Design Constraints and Trade-offs

### Constraints

1. **Single Parameter**: All programs must accept exactly one `state` parameter
2. **Pattern Return**: All programs must return a Pattern value
3. **No Mutation**: State is immutable; transformations return new Patterns
4. **Explicit Effects**: Side effects only through host-calls

### Trade-offs

**Gains**:
- Simplicity (one type signature)
- Composability (functions compose naturally)
- Testability (pure functions)
- Traceability (state transformations)
- Portability (no runtime dependencies except Pattern)

**Costs**:
- Host-calls needed for I/O (explicit boundary)
- State threading managed by runtime (not visible in code)
- Pattern construction overhead (creates new structures)

The gains significantly outweigh the costs for the Pattern Agent use case.

## Future Extensions

### Parallel Execution

Tools operating on disjoint Pattern subtrees can execute in parallel:

```haskell
-- If tools don't conflict, run concurrently
parallelTools :: [LispProgram v] -> Pattern v -> IO (Pattern v)
```

### Incremental Updates

For large Pattern states, support incremental transformations:

```scheme
;; Update only a subtree
(pattern-update-at "$.users[0]" update-fn state)
```

### Optimistic Concurrency

Multiple tools propose state changes; runtime merges:

```haskell
-- Tool executions with conflict resolution
mergeStates :: Pattern v -> Pattern v -> Pattern v
```

### Database-Backed Patterns

Pattern interface can be backed by graph database:

```haskell
-- Pattern operations transparently query DB
class PatternStore m v where
  patternQuery :: Pattern v -> Query -> m [Pattern v]
```

## Conclusion

Specializing Tiny Lisp as a Pattern state transformation language creates an elegant, powerful foundation for portable tool definitions. The unification of language semantics with the Pattern data model enables:

- **Pure functional programs** that are easy to reason about
- **Graph-native state** that tools navigate naturally
- **Composable tools** that factor complex agents
- **Portable definitions** that run across all environments
- **Traceable execution** with complete state history

This design positions Pattern Agents as a unique approach to agentic systems: agents are declarative pattern transformations rather than imperative programs.
