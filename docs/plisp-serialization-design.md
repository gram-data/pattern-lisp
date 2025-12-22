# Pattern Lisp Serialization Design

## Overview

Pattern Lisp (`.plisp`) is a minimal Lisp designed for portable tool definitions with complete serializability. All Pattern Lisp programs serialize to Gram notation through an intermediate representation called Pattern Subject.

This document specifies the logical design of Pattern Lisp serialization, independent of any particular runtime implementation.

## Core Principles

### 1. S-expressions as Universal Syntax

Everything in Pattern Lisp is an s-expression:
- Atomic values: `42`, `"hello"`, `#t`
- Symbols: `x`, `+`, `lambda`
- Lists: `(+ 1 2)`, `(lambda (x) x)`

There is no syntactic distinction between "code" and "data" - all program elements are s-expressions.

### 2. Three-Layer Architecture

Serialization operates through three distinct layers:

```
S-expression ←→ Pattern Subject ←→ Gram notation
```

- **S-expression**: The syntax layer (what programmers write)
- **Pattern Subject**: The data model layer (structured patterns)
- **Gram notation**: The serialization format layer (text representation)

### 3. Pattern Subject as Data Model

Every s-expression maps to a Pattern Subject. A Pattern Subject is a decorated sequence where:
- The decoration (value) is a Subject (identifier, labels, properties)
- The elements are an ordered list of Pattern Subjects (potentially empty)

Atomic s-expressions become patterns with zero elements. Compound s-expressions become patterns with one or more elements.

### 4. Complete Serializability

All values, including closures and primitives, serialize completely. Code is data. A serialized Pattern Lisp program can be:
- Persisted to disk
- Transmitted over network
- Loaded and executed in any Pattern Lisp runtime
- Introspected and analyzed as structured data

## S-expression to Pattern Subject Mapping

### Atomic Values

Atomic s-expressions serialize to patterns with zero elements:

**Number**: `42`
```gram
[:Number {value: 42}]
```

**String**: `"hello"`
```gram
[:String {value: "hello"}]
```

**Boolean**: `#t` or `#f`
```gram
[:Bool {value: true}]
```
```gram
[:Bool {value: false}]
```

**Symbol**: `foo`
```gram
[:Symbol {name: "foo"}]
```

### Lists

List s-expressions serialize to patterns with elements:

**Simple list**: `(1 2 3)`
```gram
[:List |
  [:Number {value: 1}],
  [:Number {value: 2}],
  [:Number {value: 3}]
]
```

**Function call**: `(+ x 1)`
```gram
[:List |
  [:Symbol {name: "+"}],
  [:Symbol {name: "x"}],
  [:Number {value: 1}]
]
```

### Special Forms

Special forms in code use explicit labels to distinguish them from function calls:

**Lambda in code**: `(lambda (x) (+ x 1))` (when it appears in a closure body)
```gram
[:List |
  [:Symbol {name: "lambda"}],
  [:List |
    [:Symbol {name: "x"}]
  ],
  [:List |
    [:Symbol {name: "+"}],
    [:Symbol {name: "x"}],
    [:Number {value: 1}]
  ]
]
```
Note: When `lambda` is evaluated and becomes a closure value, it's serialized as `:Closure` (see Closure structure section).

**If**: `(if condition then else)`
```gram
[:If |
  condition_expr,
  then_expr,
  else_expr
]
```

**Let**: `(let ((var val)...) body)`
```gram
[:Let |
  [:List |
    [:List | [:Symbol {name: "var1"}], val1_expr],
    [:List | [:Symbol {name: "var2"}], val2_expr]
  ],
  body_expr
]
```

**Begin**: `(begin expr1 expr2 ...)`
```gram
[:Begin |
  expr1,
  expr2,
  ...
]
```

**Define**: `(define name value)`
```gram
[:Define |
  [:Symbol {name: "name"}],
  value_expr
]
```

**Quote**: `'foo` or `(quote foo)`
```gram
[:Quote |
  [:Symbol {name: "foo"}]
]
```

## Program Structure Model

When serializing a program (one or more s-expressions with closures), the result uses Gram's native file structure with file-level metadata.

### File Structure

Gram files are sequences of patterns - the file itself is the "outermost pattern" with file contents as elements. Pattern Lisp programs use file-level property records for metadata:

```gram
{
  kind: "Pattern Lisp",
  name: "example.gram",
  derivedFrom: "example.plisp",
  created: datet"2025-12-22"
}

[:Environment |
  ; Bindings (optional - can be omitted if empty)
]

; Expressions (whitespace delimited patterns)
expr1
expr2
...
```

**Purpose**: 
- File-level property record identifies the file as a Pattern Lisp program and provides metadata
- Environment section (optional) contains shared bindings
- Expressions are patterns in the file sequence

### File-Level Metadata

The file-level property record distinguishes Pattern Lisp programs from arbitrary Gram files:

```gram
{
  kind: "Pattern Lisp",
  name: "example.gram",
  derivedFrom: "example.plisp",
  created: datet"2025-12-22"
}
```

**Key properties**:
- `kind: "Pattern Lisp"` - identifies the file as a Pattern Lisp program
- `name` - optional filename
- `derivedFrom` - optional source file
- `created` - optional creation date
- Additional metadata fields can be added as needed

### Environment Section

The Environment (optional) contains bindings that are shared across multiple closures:

```gram
[:Environment |
  b1:[:Binding {name: "config"} |
    [:Record {threshold: 50, rate: 0.08}]
  ],
  b2:[:Binding {name: "helper"} |
    [:Closure | ...]
  ]
]
```

**Key properties**:
- Each binding is a pattern with an identifier (e.g., `b1`, `b2`)
- The binding annotation includes `{name: "..."}` as metadata
- The binding's single element is the value (itself a Pattern Subject)
- Bindings are deduplicated: same (name, value) pair = same identifier
- **Can be omitted if empty** - simple values don't need an Environment section

### Expressions

Expressions are patterns in the file sequence (whitespace delimited):

```gram
[:Closure | ...]

[:Closure | ...]

[:Number {value: 42}]
```

**Key properties**:
- Expressions are direct patterns in the file sequence
- No wrapper needed - Gram's file structure provides the container
- Multiple expressions are whitespace delimited

### What Creates Closures?

**Important**: Serialization operates on **runtime values**, not source code. A `:Closure` pattern appears in serialization when a runtime closure value (`VClosure`) is being serialized.

**Only `lambda` creates closures**:
- The `lambda` special form evaluates to a `VClosure` value
- This closure value captures its lexical environment at creation time
- When serializing, this runtime closure value becomes a `:Closure` pattern

**Other forms do NOT create closures**:
- `let` - Creates a new scope and evaluates the body, but returns whatever the body evaluates to (which might be a closure if the body contains a `lambda`)
- `if` - Returns a value (could be a closure if one branch evaluates to a `lambda`)
- `define` - Modifies the environment, doesn't create closures
- `begin` - Returns the last expression's value
- `quote` - Returns the quoted value

**Example**:
```scheme
(let ((multiplier 10))
  (lambda (x) (* x multiplier)))
```

- The `let` creates a new scope with `multiplier = 10`
- The `lambda` creates the closure and captures `multiplier` from that scope
- When serialized, the `:Closure` pattern represents the runtime closure value created by `lambda`, not the `let` form

**Closure structure**:

```gram
[:Closure |
  [:Env | b1, b2],
  [:Lambda |
    [:Parameters | x, y],
    [:Body |
      [:List |
        [:Symbol {name: "process"}],
        x,
        b1
      ]
    ]
  ]
]
```

**Key properties**:
- `[:Env | refs]` contains references to bindings in Environment section (captured bindings)
- `[:Lambda | ...]` contains the lambda definition
- `[:Parameters | ...]` lists parameter names (function arguments)
- `[:Body | expr]` contains the function body as a Pattern Subject
- In the body, **parameters are referenced directly** (e.g., `x`, `y`) - they're part of the lambda structure
- In the body, **bound values are referenced as identifiers** (e.g., `b1`) - they reference the Environment section
- In the body, **special forms use explicit labels** (`:If`, `:Let`, `:Begin`, `:Define`, `:Quote`) to distinguish them from function calls
- In the body, **function calls use `:List`** with the function symbol as the first element

### Example: Complete Program

S-expression:
```scheme
(define config {:threshold 50})

(lambda (state)
  (process state config))
```

Serialized:
```gram
{ kind: "Pattern Lisp" }

[:Environment |
  b1:[:Binding {name: "config"} |
    [:Record {threshold: 50}]
  ]
]

[:Closure |
  [:Env | b1],
  [:Lambda |
    [:Parameters | state],
    [:Body |
      [:List |
        [:Symbol {name: "process"}],
        state,
        b1
      ]
    ]
  ]
]
```

**Key observations**:
- `state` is a **parameter** (function argument) - listed in `[:Parameters | ...]` and referenced directly in the body
- `b1` is a **bound value** (captured from environment) - referenced as an identifier in both `[:Env | ...]` and the body

## Parameters vs Bound Values

Pattern Lisp distinguishes between two types of variable references in closures:

### Parameters (Function Arguments)

Parameters are function arguments, explicitly listed in the `[:Parameters | ...]` section:

```gram
[:Lambda |
  [:Parameters | x, y],
  [:Body | ...]
]
```

In the body, parameters are **referenced directly** (e.g., `x`, `y`) - they're part of the lambda structure and don't need lookup.

### Bound Values (Captured from Environment)

Bound values are captured from the lexical environment, referenced as **Gram identifiers**:

```gram
[:Closure |
  [:Env | b1, b2],
  [:Lambda | ...]
]
```

Here `b1`, `b2` are Gram identifiers that refer to specific binding patterns in the Environment section.

### Why This Distinction?

- **Parameters** are function arguments - part of the lambda definition, resolved by position
- **Bound values** are captured from environment - structural references that establish identity relationships between patterns

Example showing both:

```gram
b1:[:Binding {name: "num"} |
  [:Number {value: 10}]
]

[:Closure |
  [:Env | b1],                  ; b1 is a bound value (identifier)
  [:Lambda |
    [:Parameters | y],          ; y is a parameter
    [:Body |
      [:List |
        [:Symbol {name: "+"}],
        b1,                      ; bound value referenced as identifier
        y                        ; parameter referenced directly
      ]
    ]
  ]
]
```

During evaluation:
1. Parameter `y` is resolved by position when the closure is called
2. Bound value `b1` is resolved by identifier lookup in the Environment section
3. Binding `b1` has the value `[:Number {value: 10}]`

## Binding Deduplication

When multiple closures capture the same binding, they share a reference to the same binding pattern.

### Example: Shared Configuration

S-expressions:
```scheme
(define config {:threshold 50})

(define tool-a (lambda (state) (process-a state config)))
(define tool-b (lambda (state) (process-b state config)))
```

Serialized:
```gram
{ kind: "Pattern Lisp" }

[:Environment |
  cfg:[:Binding {name: "config"} |
    [:Record {threshold: 50}]
  ]
]

[:Closure |
  [:Env | cfg],
  [:Lambda |
    [:Parameters | state],
    [:Body | [:List | [:Symbol {name: "process-a"}], state, cfg]]
  ]
]

[:Closure |
  [:Env | cfg],
  [:Lambda |
    [:Parameters | state],
    [:Body | [:List | [:Symbol {name: "process-b"}], state, cfg]]
  ]
]
```

**Key observation**: The binding `cfg` is defined once in the Environment section, and both closures reference it. This:
- Eliminates redundancy (config serialized once, not twice)
- Preserves semantic sharing (both closures reference the same binding)
- Enables efficient storage and transmission

### Deduplication Algorithm (Logical)

When serializing a program:

1. **Collect bindings**: Extract all bindings from all closures
2. **Deduplicate**: For each unique (name, value) pair, create one binding pattern
3. **Assign identifiers**: Give each unique binding a Gram identifier
4. **Build Environment**: Create Environment section with identified bindings
5. **Replace references**: In closures, replace inline bindings with identifier references

### Equality for Deduplication

Two bindings are considered equal if:
- They have the same name
- They have structurally equal values

**Binding Identity Semantics**:
- Deduplication uses **value equality**, not binding identity
- Two bindings with the same name and structurally equal values are deduplicated, even if they originated from different scopes
- This is safe because closures capture name→value mappings, not binding identities
- For simple values (numbers, strings, booleans, records), this is always correct
- For closure values, equality is by structural equality (same code + same captured environment)

**Example: Same binding from same scope**:
```scheme
(let ((x 10))
  (list
    (lambda (a) (+ a x))
    (lambda (b) (* b x))))
```

Both closures capture `x` with value `10` from the same scope. These are the **same binding**, so:
- One binding pattern is created: `x_1:[:Binding {name: "x"} | [:Number {value: 10}]]`
- Both closures reference `x_1` in their `[:Env | ...]` sections

**Example: Different scopes, same name and value**:
```scheme
(let ((x 10))
  (let ((x 10))  ; Different binding, same name and value
    (list
      (lambda () x)  ; Captures inner x
      (lambda () x))))  ; Also captures inner x
```

Even though the outer `x` and inner `x` are different bindings in the source code, both closures capture the inner `x` with value `10`. Since they have the same name and value, they deduplicate to one binding pattern. This is correct because the closures' behavior is identical - they both resolve to `10`.

### Closure Equality for Deduplication

**Closure Equality Semantics**:
- Closures are compared by **structural equality**: same code (body) + same captured environment
- Two closures with identical code but different binding names are **not** deduplicated
- Deduplication is by (name, value) pairs, so different names = different bindings

**Example: Equivalent closures, different names**:
```scheme
(let ((f (lambda (x) (+ x 1)))
      (g (lambda (x) (+ x 1))))
  (list f g))
```

- `f` and `g` closures are **equivalent** (isomorphic - same code, same behavior)
- But they are **not equal** for deduplication because bindings are (name, value) pairs
- Binding 1: `name="f"`, `value=closure(...)`
- Binding 2: `name="g"`, `value=closure(...)`
- Since names differ, these bindings are **NOT deduplicated**, even though closure values are equivalent

**Example: Same closure, same name**:
```scheme
(let ((helper (lambda (x) (+ x 1))))
  (list
    (lambda (a) (helper a))
    (lambda (b) (helper b))))
```

Both closures capture `helper` with the same closure value. These are the **same binding**, so:
- One binding pattern is created for `helper`
- Both closures reference the same binding identifier

### Closure Materialization Rules

**When Closures Are Serialized**:
- Closures in captured environments are **immediately materialized** as values
- No deferral - the environment contains actual values, not deferred expressions
- This matches Common Lisp semantics: closures capture their lexical environment at creation time

**Example: Nested Closures**:
```scheme
(let ((helper (lambda (x) (+ x 1))))
  (lambda (y) (helper y)))
```

**Evaluation order**:
1. `(lambda (x) (+ x 1))` evaluates to a closure value
2. That closure value is bound to `helper` in the environment
3. `(lambda (y) (helper y))` captures `currentEnv`, which already contains `helper` as a closure value
4. The outer closure's environment has `helper` as a concrete, materialized closure

**Serialized**:
```gram
{ kind: "Pattern Lisp" }

[:Environment |
  helper_binding:[:Binding {name: "helper"} |
    [:Closure |
      [:Env |],
      [:Lambda |
        [:Parameters | x],
        [:Body |
          [:List |
            [:Symbol {name: "+"}],
            x,
            [:Number {value: 1}]
          ]
        ]
      ]
    ]
  ]
]

[:Closure |
  [:Env | helper_binding],
  [:Lambda |
    [:Parameters | y],
    [:Body |
      [:List |
        [:Symbol {name: "helper"}],
        y
      ]
    ]
  ]
]
```

**Key point**: The captured closure (`helper`) is serialized immediately as a binding in the Environment section. It's not deferred until the outer closure is called.

## Primitives

Primitive functions (built-in operations like `+`, `string-append`) serialize as symbolic references:

```gram
[:Primitive {name: "+"}]
```

**Deserialization**: The runtime looks up the primitive by name in its standard library registry. If the primitive doesn't exist, deserialization fails with an error.

**Rationale**: Primitives are provided by the runtime, not serialized as code. This keeps serialization compact and allows runtimes to optimize primitive implementations.

## Standard Library and Environment Filtering

### The Standard Environment

Every Pattern Lisp runtime provides a standard environment (`initialEnv`) containing:
- Arithmetic primitives: `+`, `-`, `*`, `/`
- Comparison primitives: `>`, `<`, `=`, `/=`
- String primitives: `string-append`, `string-length`, `substring`
- Pattern primitives: `pattern`, `pattern-with`, `pattern-value`, etc.

### Environment Filtering

When serializing, **exclude standard library bindings** from the Environment section.

**Rationale**: 
- Every runtime has the standard library
- Serializing it is redundant and wasteful
- Standard library is stable across sessions

**Process**:
1. When building Environment section, filter out any binding present in standard library
2. Only serialize bindings that are **not** in the standard library
3. On deserialization, merge serialized bindings with standard library

### Example

Closure captures: `{"+": Primitive, "config": {:threshold 50}}`

Serialized Environment:
```gram
[:Environment |
  cfg:[:Binding {name: "config"} |
    [:Record {threshold: 50}]
  ]
  ; "+" is NOT serialized - it's in standard library
]
```

Deserialized environment: `standard library ∪ {config: ...}` = full environment restored

### Standard Library Versioning

**Note**: Standard library versioning is a **runtime implementation concern**, not a serialization problem. The serialization design is sufficient as-is.

**Runtime Responsibilities**:
- Runtime should be generative and adaptive, synthesizing library functions as needed
- Runtime should infer primitive behavior from names and usage patterns when primitives are missing
- Runtime should maintain meta-information about available primitives
- Runtime should handle compatibility and version differences

**Serialization Format Flexibility**:
- The serialization format could support metadata fields (like `description` and `version`) in bindings as an extension
- This would be an optional enhancement, not a requirement
- Current format: `[:Binding {name: "..."} | value]`
- Future format (optional): `[:Binding {name: "...", description: "...", version: "..."} | value]`

**Current Behavior**:
- Primitives serialize as `[:Primitive {name: "..."}]` (symbolic reference)
- Runtime looks up primitives by name during deserialization
- If primitive doesn't exist, deserialization fails (runtime should handle this gracefully)

## Canonical Tool Form

Pattern Lisp tools follow a canonical form:

```scheme
(lambda (state) <body>)
```

Where:
- `state` is a Pattern Subject (the current state)
- `<body>` transforms the state and returns a new Pattern Subject

### Tool Serialization

A tool is just a closure, so it serializes using the program structure:

```gram
{ kind: "Pattern Lisp" }

[:Environment |
  ; Any captured bindings (optional - omit if empty)
]

[:Closure |
  [:Env | ...],
  [:Lambda |
    [:Parameters | state],
    [:Body | ...]
  ]
]
```

### RuntimeState Serialization

A complete runtime contains multiple tools and current state:

```scheme
; Conceptual RuntimeState structure
{
  currentState: Pattern Subject,
  toolDefinitions: {"tool-a": closure, "tool-b": closure},
  executionTrace: [...]
}
```

Serialized:
```gram
[:RuntimeState |
  [:Environment |
    ; Shared bindings across all tools
  ],
  [:CurrentState |
    ; The Pattern Subject representing current state
  ],
  [:ToolDefinitions |
    [:Tool {name: "tool-a"} |
      [:Closure | ...]
    ],
    [:Tool {name: "tool-b"} |
      [:Closure | ...]
    ]
  ],
  [:ExecutionTrace |
    ; Trace entries
  ]
]
```

**Key property**: All tools that share configuration/bindings reference the same bindings in the Environment section. This dramatically reduces serialization size for multi-tool agents.

## Recursive and Mutually Recursive Closures

Pattern Lisp fully supports recursive and mutually recursive closures. Cycles in the binding graph are handled naturally through forward references.

### Mutual Recursion

**Example: Mutually recursive closures**:
```scheme
(let ((f (lambda (x) (if (= x 0) 1 (g (- x 1)))))
      (g (lambda (x) (if (= x 0) 1 (f (- x 1))))))
  f)
```

**Serialized**:
```gram
{ kind: "Pattern Lisp" }

[:Environment |
  f_binding:[:Binding {name: "f"} |
    [:Closure |
      [:Env | g_binding],
      [:Lambda |
        [:Parameters | x],
        [:Body |
          [:If |
            [:List | [:Symbol {name: "="}], x, [:Number {value: 0}]],
            [:Number {value: 1}],
            [:List |
              [:Symbol {name: "g"}],
              [:List | [:Symbol {name: "-"}], x, [:Number {value: 1}]]
            ]
          ]
        ]
      ]
    ]
  ],
  g_binding:[:Binding {name: "g"} |
    [:Closure |
      [:Env | f_binding],
      [:Lambda |
        [:Parameters | x],
        [:Body |
          [:If |
            [:List | [:Symbol {name: "="}], x, [:Number {value: 0}]],
            [:Number {value: 1}],
            [:List |
              [:Symbol {name: "f"}],
              [:List | [:Symbol {name: "-"}], x, [:Number {value: 1}]]
            ]
          ]
        ]
      ]
    ]
  ]
]

f_binding
```

**Key observations**:
- Both `f` and `g` are closures, so they're bindings in the Environment section
- `f_binding` references `g_binding` in its `[:Env | ...]` section (forward reference)
- `g_binding` references `f_binding` in its `[:Env | ...]` section (forward reference)
- This creates a cycle: `f_binding` → `g_binding` → `f_binding`
- Forward references are valid in Gram, so this works
- In the body, `f` and `g` are referenced as symbols which are resolved via the environment at runtime

### Self-Recursion

**Example: Self-recursive closure**:
```scheme
(let ((fact (lambda (n) 
               (if (= n 0) 
                 1 
                 (* n (fact (- n 1)))))))
  fact)
```

**Serialized**:
```gram
{ kind: "Pattern Lisp" }

[:Environment |
  fact_binding:[:Binding {name: "fact"} |
    [:Closure |
      [:Env | fact_binding],  ; Self-reference!
      [:Lambda |
        [:Parameters | n],
        [:Body |
          [:If |
            [:List | [:Symbol {name: "="}], n, [:Number {value: 0}]],
            [:Number {value: 1}],
            [:List |
              [:Symbol {name: "*"}],
              n,
              [:List |
                [:Symbol {name: "fact"}],
                [:List | [:Symbol {name: "-"}], n, [:Number {value: 1}]]
              ]
            ]
          ]
        ]
      ]
    ]
  ]
]

fact_binding
```

**Key observation**: The closure references itself in its `[:Env | fact_binding]` section. This is a self-referential cycle, which is also fully supported.

### How Cycles Are Handled

Cycles in the binding graph are handled naturally because:
1. **Forward references are valid** in Gram notation
2. **Bindings are identified by identifiers**, not by position
3. **Environment lookup resolves cycles** during deserialization
4. **Symbol resolution** in bodies happens at runtime via the environment
5. **Binding deduplication works correctly** - each closure is a unique binding (different names), even if they reference each other

## Pattern References and Ordering

### Using References

Any pattern with an identifier can be referenced elsewhere:

```gram
x:[:Number {value: 42}]

[:List | x, x, x]  ; References x three times
```

**Benefits**:
- Eliminates duplication
- Makes sharing explicit
- Enables graph structure (cycles, DAGs)

### Ordering Considerations

**Ideal ordering**: Patterns with fewer elements before patterns with more elements.

```gram
; Good: atomic patterns first
x:[:Number {value: 10}]
y:[:String {value: "hello"}]

[:List | x, y]

; Also valid: forward references work
[:List | x, y]

x:[:Number {value: 10}]
y:[:String {value: "hello"}]
```

**Rationale**: 
- Dependencies-first ordering is more readable
- Simpler patterns (atoms, bindings) typically referenced by complex patterns (closures, lists)
- Forward references are legal, so ordering is a convention, not a requirement

### Canonical Ordering for Program

Recommended structure:

```gram
{ kind: "Pattern Lisp" }

[:Environment |
  ; Bindings ordered by complexity (simple values first)
  ; Optional - omit if empty
  b1:[:Binding {name: "threshold"} | [:Number {value: 50}]],
  b2:[:Binding {name: "config"} | [:Record | ...]],
  b3:[:Binding {name: "helper"} | [:Closure | ...]]
]

; Top-level expressions (whitespace delimited)
expr1
expr2
...
```

## Deserialization Process

Deserialization reverses the serialization process:

### Step 1: Parse Gram to Pattern Subject

Parse the Gram notation text into Pattern Subject structures.

**Input**: Gram text
**Output**: Pattern Subject tree with identifiers resolved

### Step 2: Extract Program Structure

Parse the file-level property record and extract:
- File-level metadata (identifies file as Pattern Lisp program)
- Environment section (optional, bindings with identifiers)
- Expressions (remaining patterns in file sequence)

### Step 3: Build Binding Lookup Table

Create a map from identifier to binding value:

```
b1 → [:Number {value: 10}]
b2 → [:Record {threshold: 50}]
```

### Step 4: Reconstruct Closures

For each closure:
1. Extract parameter names from `[:Parameters | ...]` section
2. Resolve environment references using binding lookup table
3. Merge with standard library: `fullEnv = standardLibrary ∪ resolvedBindings`
4. Reconstruct body from `[:Body | expr]`
5. Create closure value with params, body, and full environment

### Step 5: Convert Pattern Subjects to S-expressions

Recursively convert Pattern Subjects to runtime s-expression values using label-based pattern matching:

**Deserialization Pattern Matching**:

The label on each pattern explicitly indicates its interpretation. The deserializer uses a decision tree based on labels:

**Runtime Values** (when deserializing top-level expressions or binding values):
- `[:Closure | ...]` → closure value with reconstructed environment
- `[:Number {value: n}]` → number value
- `[:String {value: s}]` → string value
- `[:Bool {value: b}]` → boolean value
- `[:List | elems]` → list value (data)
- `[:Primitive {name: "..."}]` → primitive function value

**Code/Expressions** (when deserializing closure bodies):
- `[:If | cond, then, else]` → if special form expression
- `[:Let | bindings, body]` → let special form expression
- `[:Begin | exprs...]` → begin special form expression
- `[:Define | name, value]` → define special form expression
- `[:Quote | expr]` → quote special form expression
- `[:List | func, args...]` → function call expression (or lambda in code)
- `[:Symbol {name: "..."}]` → symbol expression
- `[:Number {value: n}]` → number literal expression
- `[:String {value: s}]` → string literal expression
- `[:Bool {value: b}]` → boolean literal expression

**Key Principles**:
1. **Labels are unambiguous**: Each label has a single interpretation
2. **Context determines value vs code**: Top-level expressions and binding values are runtime values; closure bodies are code
3. **Special forms always use explicit labels**: `:If`, `:Let`, `:Begin`, `:Define`, `:Quote` are never ambiguous
4. **Function calls use `:List`**: In code context, `:List` with a symbol as first element is a function call
5. **Closures are always marked**: When a closure is a value, it's always `:Closure`, never `:List`

**Decision Tree Example**:
```
Pattern Subject → Check Label:
  ├─ :Closure → Closure value (reconstruct with env + lambda)
  ├─ :If → If special form (code)
  ├─ :Let → Let special form (code)
  ├─ :Begin → Begin special form (code)
  ├─ :Define → Define special form (code)
  ├─ :Quote → Quote special form (code)
  ├─ :List → Check context:
  │   ├─ Value context → List value
  │   └─ Code context → Function call (or lambda in code)
  ├─ :Number → Number (value or literal, depending on context)
  ├─ :String → String (value or literal, depending on context)
  ├─ :Bool → Boolean (value or literal, depending on context)
  └─ :Symbol → Symbol expression (code)
```

**Output**: Runtime values ready for evaluation

## Examples

### Example 0: Simple Value

**S-expression**:
```scheme
42
```

**Serialized**:
```gram
{ kind: "Pattern Lisp" }
[:Number {value: 42}]
```

**Note**: For simple values, the Environment section is omitted since there are no bindings.

### Example 1: Simple Arithmetic Function

**S-expression**:
```scheme
(lambda (x) (+ x 1))
```

**Serialized**:
```gram
{ kind: "Pattern Lisp" }

[:Closure |
  [:Env |],
  [:Lambda |
    [:Parameters | x],
    [:Body |
      [:List |
        [:Symbol {name: "+"}],
        x,
        [:Number {value: 1}]
      ]
    ]
  ]
]
```

### Example 2: Closure with Captured Variable

**S-expression**:
```scheme
(let ((multiplier 10))
  (lambda (x) (* x multiplier)))
```

**Serialized**:
```gram
{ kind: "Pattern Lisp" }

[:Environment |
  m:[:Binding {name: "multiplier"} |
    [:Number {value: 10}]
  ]
]

[:Closure |
  [:Env | m],
  [:Lambda |
    [:Parameters | x],
    [:Body |
      [:List |
        [:Symbol {name: "*"}],
        x,
        m
      ]
    ]
  ]
]
```

### Example 3: Multiple Closures Sharing Configuration

**S-expressions**:
```scheme
(define config {:threshold 50, :rate 0.08})

(define tool-a (lambda (state) (process-a state config)))
(define tool-b (lambda (state) (process-b state config)))
```

**Serialized**:
```gram
{ kind: "Pattern Lisp" }

[:Environment |
  cfg:[:Binding {name: "config"} |
    [:Record {threshold: 50, rate: 0.08}]
  ]
]

[:Closure |
  [:Env | cfg],
  [:Lambda |
    [:Parameters | state],
    [:Body |
      [:List |
        [:Symbol {name: "process-a"}],
        state,
        cfg
      ]
    ]
  ]
]

[:Closure |
  [:Env | cfg],
  [:Lambda |
    [:Parameters | state],
    [:Body |
      [:List |
        [:Symbol {name: "process-b"}],
        state,
        cfg
      ]
    ]
  ]
]
```

**Observation**: Both closures reference the same binding `cfg`. The configuration is serialized once, not duplicated.

### Example 4: Closure Containing Closure

**S-expression**:
```scheme
(lambda (x)
  (lambda (y)
    (+ x y)))
```

**Serialized**:
```gram
{ kind: "Pattern Lisp" }

[:Closure |
  [:Env |],
  [:Lambda |
    [:Parameters | x],
    [:Body |
      [:List |
        [:Symbol {name: "lambda"}],
        [:List |
          [:Symbol {name: "y"}]
        ],
        [:List |
          [:Symbol {name: "+"}],
          [:Var {name: "x"}],
          [:Var {name: "y"}]
        ]
      ]
    ]
  ]
]
```

**Note**: The inner lambda is represented as an s-expression (List) in the body. When the outer closure is called, the inner lambda will be evaluated, creating a new closure that captures `x`. The `[:Var {name: "x"}]` in the inner lambda code is a free variable reference that will be resolved when the inner lambda is created.

### Example 5: Closure Body with Special Forms

**S-expression**:
```scheme
(lambda (x)
  (if (= x 0)
    1
    (let ((y (- x 1)))
      (* x y))))
```

**Serialized**:
```gram
{ kind: "Pattern Lisp" }

[:Closure |
  [:Env |],
  [:Lambda |
    [:Parameters | x],
    [:Body |
      [:If |
        [:List | [:Symbol {name: "="}], x, [:Number {value: 0}]],
        [:Number {value: 1}],
        [:Let |
          [:List |
            [:List | [:Symbol {name: "y"}], [:List | [:Symbol {name: "-"}], x, [:Number {value: 1}]]]
          ],
          [:List | [:Symbol {name: "*"}], x, [:Symbol {name: "y"}]]
        ]
      ]
    ]
  ]
]
```

**Key observations**:
- `:If` label explicitly marks the if special form
- `:Let` label explicitly marks the let special form
- Function calls (like `=`, `-`, `*`) use `:List` with the function symbol as first element
- This makes the distinction between special forms and function calls clear

### Example 6: Tool with Pattern State

**S-expression**:
```scheme
(lambda (state)
  (pattern-with
    (pattern-value state)
    (cons
      (pattern "new-item")
      (pattern-elements state))))
```

**Serialized** (simplified):
```gram
{ kind: "Pattern Lisp" }

[:Closure |
  [:Env |],
  [:Lambda |
    [:Parameters | state],
    [:Body |
      [:List |
        [:Symbol {name: "pattern-with"}],
        [:List | [:Symbol {name: "pattern-value"}], state],
        [:List |
          [:Symbol {name: "cons"}],
          [:List | [:Symbol {name: "pattern"}], [:String {value: "new-item"}]],
          [:List | [:Symbol {name: "pattern-elements"}], state]
        ]
      ]
    ]
  ]
]
```

## Design Rationale

### Why Three Layers?

**S-expression**: Programmer-friendly syntax, homoiconic (code is data)
**Pattern Subject**: Structured data model with identity and relationships
**Gram notation**: Human-readable serialization format

This separation allows:
- Different syntactic surfaces (could add other syntaxes)
- Uniform data model (everything is patterns)
- Portable serialization (Gram is language-agnostic)

### Why File-Level Structure?

Using file-level property records and optional Environment:
- **Efficiency**: Shared bindings serialized once (in optional Environment section)
- **Clarity**: File-level metadata identifies Pattern Lisp programs
- **Simplicity**: No unnecessary wrapper - leverages Gram's native file structure
- **Flexibility**: Environment section optional (omit if empty)
- **Portability**: Self-contained programs with all dependencies

### Why Filter Standard Library?

- **Compact**: Don't serialize what every runtime has
- **Stable**: Standard library is consistent across sessions
- **Correct**: Primitives resolve to runtime-optimized implementations

### Why Gram Identifiers for Bindings?

Gram identifiers establish **identity relationships**:
- Multiple closures sharing a binding = semantic fact
- Identity preserved through serialization
- Enables queries: "which closures use this binding?"

### Why Properties for Variable Names?

Variable names are **symbolic references** resolved by scoping:
- Not global identifiers
- Scoped to their lexical context
- Resolved at evaluation time, not serialization time

## Implementation Considerations

### For Runtime Implementers

1. **Standard Library**: Define your standard environment consistently
2. **Binding Equality**: Implement structural equality for deduplication
3. **Identifier Generation**: Use consistent, unique identifiers (e.g., `b1`, `b2`, ...)
4. **Error Handling**: Handle missing primitives, malformed patterns gracefully
5. **Optimization**: Consider caching parsed/deserialized programs

### Testing Serialization

Essential test cases:
- Round-trip: serialize then deserialize should equal original
- Sharing: multiple closures with shared bindings
- Nesting: closures containing closures
- Large programs: performance with 100+ tools
- Edge cases: empty environment, no captures, primitives only

### Versioning

Consider including version metadata in file-level property record:

```gram
{
  kind: "Pattern Lisp",
  version: "1.0"
}

[:Environment | ...]

; Expressions
...
```

This enables:
- Compatibility checking
- Migration between versions
- Runtime requirements specification

## Conclusion

Pattern Lisp's serialization design provides:
- **Complete serializability**: All values, including closures, serialize
- **Efficiency**: Deduplication via shared bindings
- **Portability**: Gram notation works across runtimes
- **Clarity**: Three-layer architecture with clear responsibilities
- **Extensibility**: Pattern Subject model supports rich data structures

This design enables Pattern Lisp to serve as a portable intermediate representation for tool definitions in agentic systems, with full code-as-data capabilities and cross-runtime compatibility.
