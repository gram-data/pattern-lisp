# Nested Scope Serialization Proposal

**Date**: 2025-01-27  
**Status**: ✅ **IMPLEMENTED** - This proposal describes the current nested scope serialization implementation using inline `:Scope` patterns.  
**Related**: `docs/plisp-serialization-design.md`, `src/PatternLisp/Codec.hs`

## Problem Statement

The current serialization design uses a flat `:Environment` section that doesn't preserve scope hierarchy. This loses important information about:

1. **Scope inheritance**: Inner closures should inherit from outer closures
2. **Shadowing**: Inner scopes can shadow outer bindings
3. **Scope chain**: The lexical scope chain (Program → Outer → Inner) should be preserved
4. **First-class environments**: Environments should be patterns, not special sections

## Design Principles

1. **First-class environments**: Environments are patterns with identifiers, inlined at each scope
2. **CSS-like inheritance**: Inner environments inherit from outer environments via identifier references
3. **Program-level is outermost**: Environment with empty parent `[]` is program-level
4. **Shadowing is explicit**: Inner bindings shadow outer bindings by name (lookup order)
5. **Scope chain preserved**: Serialization maintains the lexical scope hierarchy explicitly
6. **No special sections**: Eliminates need for separate `:Environment` section

## Proposed Format: First-Class Inline Environments

### Core Principle

**Environments are first-class patterns** that are **inlined at each scope level**. This approach:
- Eliminates the need for a separate `:Environment` section
- Makes environments first-class patterns with identifiers
- Makes scope hierarchy explicit in the pattern structure
- Is consistent with Gram's pattern model (everything is a pattern)

### Scope Structure

Each `:Scope` pattern contains:
- **Parent environment reference**: An identifier reference to the parent environment (or `[]` for program-level)
- **Binding patterns**: `[ id:Binding {name: "..."} | value ]` patterns for bindings in this scope

```gram
[e1:Scope |
  e0,                              ; Parent environment (identifier reference)
  [ x_binding:Binding {name: "x"} |
    [:Number {value: 10}]
  ],
  [ y_binding:Binding {name: "y"} |
    [:Number {value: 20}]
  ]
]
```

### Program-Level Environment

The program-level environment has an empty parent (`[]`):

```gram
[e0:Scope |
  [],                              ; Empty parent = program-level (outermost scope)
  [ global_binding:Binding {name: "global"} |
    [:Number {value: 100}]
  ]
]
```

### Closure Structure

Closures inline their environment pattern as the first element:

```gram
[:Closure |
  [e1:Scope |
    e0,                            ; Parent environment reference
    [ x_binding:Binding {name: "x"} | ... ]
  ],
  [:Lambda |
    [:Parameters | x],
    [:Body | ...]
  ]
]
```

### Top-Level Closures

For closures at program level (no outer `let`), they reference the program-level environment:

```gram
[:Closure |
  [e0:Scope | []],                   ; Just program-level, no additional bindings
  [:Lambda |
    [:Parameters | x],
    [:Body | ...]
  ]
]
```

Or if they have program-level bindings:

```gram
[e_program:Scope |
  [],
  [ config_binding:Binding {name: "config"} |
    [:Record {threshold: 50}]
  ]
]

[:Closure |
  [e_tool:Scope |
    e_program,                     ; Reference to shared program environment
    []                             ; No additional bindings
  ],
  [:Lambda | ...]
]
```

## Minimal Viable Examples

### Example 1: Simple Nested Scopes

**Lisp:**
```scheme
(define global 100)              ; Program-level

(let ((outer 10))                ; Outer scope
  (lambda (x) 
    (+ x outer global)))
```

**Serialized:**
```gram
{ kind: "Pattern Lisp" }

[:Closure |
  [e1:Scope |
    [e0:Scope |
      [],                         ; Program-level (empty parent)
      [global_binding:Binding {name: "global"} |
        [:Number {value: 100}]
      ]
    ],
    [outer_binding:Binding {name: "outer"} |
      [:Number {value: 10}]
    ]
  ],
  [:Lambda |
    [:Parameters | x],
    [:Body |
      [:List |
        [:Symbol {name: "+"}],
        x,                        ; Parameter
        outer_binding,            ; From e1 (direct)
        global_binding             ; From e0 (via e1's parent)
      ]
    ]
  ]
]
```

**Deserialization:**
1. Build environment from `e1`:
   - Direct bindings: `outer = 10` (from `outer_binding`)
   - Parent `e0`: `global = 100` (from `global_binding`)
2. Result: `{outer: 10, global: 100}`
3. Lookup order: Check `e1` first, then `e0` (parent)

---

### Example 2: Shadowing

**Lisp:**
```scheme
(define x 1)                     ; Program-level

(let ((x 10))                   ; Outer scope shadows program-level
  (lambda () x))                 ; Resolves to 10 (outer)
```

**Serialized (New Design - Inline Scopes):**
```gram
{ kind: "Pattern Lisp" }

[:Closure |
  [e1:Scope |
    [e0:Scope |
      [],                         ; Program-level (empty parent)
      [x_program:Binding {name: "x"} |
        [:Number {value: 1}]
      ]
    ],
    [x_outer:Binding {name: "x"} |
      [:Number {value: 10}]
    ]
  ],
  [:Lambda |
    [:Parameters],
    [:Body |
      x_outer                     ; Resolves to 10 (shadows program-level)
    ]
  ]
]
```

**Deserialization:**
1. Look up `x` in closure's `:Scope` → find `x_outer` binding → `10` ✅ (found, stops here)
2. If not found, check parent scope `e0` → `x_program` → `1`
3. Result: `x = 10` (outer shadows program-level)

---

### Example 3: Closure Capturing Another Closure

**Lisp:**
```scheme
(define config {:threshold 50})  ; Program-level

(let ((helper (lambda (x) (+ x 1))))  ; Outer closure captures helper
  (lambda (y) (helper y)))             ; Inner closure captures helper + config
```

**Serialized:**
```gram
{ kind: "Pattern Lisp" }

[:Closure |
  [e1:Scope |
    [e0:Scope |
      [],                         ; Program-level
      [config_binding:Binding {name: "config"} |
        [:Record {threshold: 50}]
      ]
    ],
    [helper_binding:Binding {name: "helper"} |
      [:Closure |
        [e_helper:Scope |
          e0,                     ; Inherit from program-level
          []                      ; No additional bindings
        ],
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
  ],
  [:Lambda |
    [:Parameters | y],
    [:Body |
      [:List |
        helper_binding,           ; Use captured closure
        y
      ]
    ]
  ]
]
```

**Deserialization:**
1. Outer closure's environment (`e1`):
   - Direct: `helper = <closure>` (from `helper_binding`)
   - Parent `e0`: `config = {:threshold: 50}` (from `config_binding`)
2. Result: `{helper: <closure>, config: {:threshold: 50}}`
3. The `helper` closure itself has environment `e_helper` with parent `e0`

---

### Example 3b: Multiple Bindings at Same Scope

**Lisp:**
```scheme
(define global 100)              ; Program-level

(let ((x 10) (y 20) (z 30))     ; Multiple bindings at same scope
  (lambda () (+ x y z global)))
```

**Serialized:**
```gram
{ kind: "Pattern Lisp" }

[:Closure |
  [e1:Scope |
    [e0:Scope |
      [],                         ; Program-level
      [global_binding:Binding {name: "global"} |
        [:Number {value: 100}]
      ]
    ],
    [x_binding:Binding {name: "x"} |
      [:Number {value: 10}]
    ],
    [y_binding:Binding {name: "y"} |
      [:Number {value: 20}]
    ],
    [z_binding:Binding {name: "z"} |
      [:Number {value: 30}]
    ]
  ],
  [:Lambda |
    [:Parameters],
    [:Body |
      [:List |
        [:Symbol {name: "+"}],
        x_binding,                ; All available from e1
        y_binding,
        z_binding,
        global_binding             ; From e0 (parent)
      ]
    ]
  ]
]
```

**Note**: All bindings at the same scope level (`x`, `y`, `z`) are in the same `:Scope` pattern. They're all available to the closure.

---

### Example 4: Multiple Levels of Shadowing

**Lisp:**
```scheme
(define x 1)                     ; Program-level

(let ((x 10))                   ; Level 1: shadows program
  (let ((x 20))                 ; Level 2: shadows level 1
    (lambda () x)))              ; Resolves to 20 (innermost)
```

**Serialized:**
```gram
{ kind: "Pattern Lisp" }

[:Closure |
  [e1:Scope |
    [e0:Scope |
      [],                         ; Program-level
      [x_program:Binding {name: "x"} |
        [:Number {value: 1}]
      ]
    ],
    [x_level1:Binding {name: "x"} |
      [:Number {value: 10}]
    ]
  ],
  [:Lambda |
    [:Parameters],
    [:Body |
      [:Let |
        [:Bindings |
          [x_level2:Binding {name: "x"} |
            [:Number {value: 20}]
          ]
        ],
        [:Body |
          [:Closure |
            [e2:Scope |
              e1,                 ; Parent is e1 (level 1)
              [x_level2:Binding {name: "x"} |
                [:Number {value: 20}]
              ]
            ],
            [:Lambda |
              [:Parameters],
              [:Body |
                x_level2          ; Resolves to 20 (innermost)
              ]
            ]
          ]
        ]
      ]
    ]
  ]
]
```

**Deserialization:**
1. Look up `x` in inner closure's environment (`e2`):
   - Check `e2` (direct) → find `x_level2` → `20` ✅ (found, stops here)
   - If not found, check `e1` (parent) → `x_level1` → `10`
   - If not found, check `e0` (parent's parent) → `x_program` → `1`
2. Result: `x = 20` (innermost shadows all outer)
3. Shadowing works because lookup checks environments in order: `e2` → `e1` → `e0`

---

### Example 5: Shared Program-Level Environment

**Lisp:**
```scheme
(define config {:threshold 50})  ; Program-level

(define tool-a (lambda (state) (process-a state config)))
(define tool-b (lambda (state) (process-b state config)))
```

**Serialized:**
```gram
{ kind: "Pattern Lisp" }

[e_program:Scope |
  [],
  [config_binding:Binding {name: "config"} |
    [:Record {threshold: 50}]
  ]
]

[:Closure |
  [e_tool_a:Scope |
    e_program,                    ; Reference to shared program environment
    []                            ; No additional bindings
  ],
  [:Lambda |
    [:Parameters | state],
    [:Body |
      [:List |
        [:Symbol {name: "process-a"}],
        state,
        config_binding
      ]
    ]
  ]
]

[:Closure |
  [e_tool_b:Scope |
    e_program,                    ; Same shared parent
    []
  ],
  [:Lambda |
    [:Parameters | state],
    [:Body |
      [:List |
        [:Symbol {name: "process-b"}],
        state,
        config_binding
      ]
    ]
  ]
]
```

**Note**: Both closures share the same program-level environment (`e_program`). The `config` binding is serialized once and referenced by both closures.

## Implementation Considerations

### Serialization

1. **Inline environments**: Create `:Scope` patterns inline at each scope level
2. **Assign environment IDs**: Each `:Scope` pattern gets a globally unique identifier
3. **Reference parent**: Parent environment is referenced by identifier (or `[]` for program-level)
4. **Deduplicate bindings**: Bindings are deduplicated separately (by name, value pairs)
5. **Program-level**: Environment with empty parent `[]` is program-level

### Deserialization

1. **Build environment tree**: Parse `:Scope` patterns and build environment tree structure
2. **Resolve scope chain**: For each closure, resolve bindings by:
   - Check bindings in closure's `:Scope` pattern first
   - If not found, check parent environment (follow identifier reference)
   - Continue up the chain until found or reach program-level
3. **Handle shadowing**: First binding found wins (innermost shadows outer)

### Binding Resolution Algorithm

```
resolveBinding(name, env_pattern):
  1. Check bindings in env_pattern → if found, return
  2. Get parent_env_ref from env_pattern (identifier or [])
  3. If parent_env_ref is identifier:
     - Lookup parent_env_pattern by identifier
     - Recursively call resolveBinding(name, parent_env_pattern)
  4. If parent_env_ref is []:
     - Stop (reached program-level)
  5. If not found in any parent:
     - Return error (undefined variable)
```

### Key Differences from Previous Approach

1. **No separate `:Environment` section**: Environments are inline patterns
2. **First-class environments**: `:Scope` patterns are patterns with identifiers
3. **Explicit nesting**: Scope hierarchy is visible in the pattern structure
4. **Parent as identifier**: Parent is just an identifier reference, not special syntax

## Answers to Key Questions

### Q1: What's the difference between `:Environment` and `:Scope`?

**Answer**: There is no `:Environment` section in this approach! The old design used a separate `:Environment` section, but that has been eliminated. 

- **`:Scope`** patterns are **first-class patterns** that are **inlined** at each scope level
- Each `:Scope` pattern contains:
  - Parent environment reference (identifier or `[]`)
  - Binding patterns for bindings in this scope
- Environments are nested via identifier references (e.g., `e1` references `e0`)
- Program-level environment has empty parent `[]`

### Q2: Is nesting ever not obvious?

**Answer**: Nesting is **always obvious** from the pattern structure:

- **Environments are nested** via identifier references
- When a closure is nested inside another closure's body, the nesting is obvious from the structure
- The **scope relationship** is explicit: parent environment is referenced by identifier
- Example: `[e1:Scope | e0, ...]` clearly shows `e1` inherits from `e0`

**Example**: 
```gram
[:Closure |
  [e1:Scope |
    e0,                           ; ← Parent relationship is explicit
    [x_binding:Binding | ...]
  ],
  [:Lambda |
    [:Body |
      [:Closure |                 ; ← Nesting is obvious from structure
        [e2:Scope |
          e1,                      ; ← Parent relationship is explicit
          [y_binding:Binding | ...]
        ],
        [:Lambda | ...]
      ]
    ]
  ]
]
```

Both the structural nesting and the scope relationship are explicit and obvious.

## Questions for Review

1. **Parent Reference Format**: ✅ **Resolved** - Parent is an identifier reference (or `[]` for program-level)
2. **Program-Level Reference**: ✅ **Resolved** - Program-level is `e0:Env | []` (empty parent)
3. **Empty Environments**: ✅ **Resolved** - Empty environments still reference parent (e.g., `[e1:Scope | e0, []]`)
4. **Scope IDs**: ✅ **Globally unique** - all pattern identifiers are globally unique
5. **Backward Compatibility**: ✅ **Never worry about backwards compatibility**
6. **Binding Deduplication**: ✅ **Separate** - Bindings are deduplicated separately from environments
7. **Scope Location**: ✅ **Inlined** - Scopes are inlined at each scope level, no separate `:Environment` section

## Next Steps

1. Review this proposal
2. Decide on parent reference format (explicit vs implicit)
3. Update `docs/plisp-serialization-design.md` with nested scope support
4. Implement serialization changes
5. Implement deserialization changes
6. Add tests for nested scopes and shadowing

