# Pattern Lisp Serialization Design Review - TODO

This document tracks open questions and design decisions from the conceptual review of `plisp-serialization-design.md`.

## Status Legend
- ⏳ Pending - Not yet addressed
- 🔄 In Progress - Currently being worked on
- ✅ Resolved - Decision made and documented
- ❌ Rejected - Decision made to not pursue

---

## Critical Design Decisions

**Note**: Items are ordered to respect dependencies. Work through sequentially.

### 1. Binding Identity vs Value Equality ⏳
**Dependencies**: None (foundational)
**Issue**: Binding deduplication uses (name, value) equality, which may collapse distinct bindings that happen to have the same name and value but exist in different scopes.

**Questions**:
- Should deduplication preserve binding identity (distinct bindings even if same value)?
- Or is value equality sufficient (same name + value = same binding)?
- What are the implications for closure equality?

**Example Case**:
```scheme
(let ((x 10))
  (let ((x 10))  ; Different binding, same name and value
    (list (lambda () x) (lambda () x))))
```

**Analysis**:
- For **simple values** (numbers, strings, booleans, records): Deduplication by (name, value) is safe. Closures capture name→value mappings, so if two closures have the same mapping, deduplication is semantically correct regardless of which scope the binding came from.
- For **closure values**: Closure equality is by structural equality (code + captured environment). See #2 for details.

**Decision**: ✅ **Resolved** - Value equality is sufficient for deduplication. Two bindings are deduplicated if they have the same name AND structurally equal values. For closures, this means same code and same captured environment (by structural equality of bindings). Different binding names are never deduplicated, even if values are equivalent.

---

### 2. Closure Equality for Deduplication ⏳
**Dependencies**: #1 (Binding Identity vs Value Equality)
**Issue**: Structural equality for closures is ambiguous - same code vs same closure identity.

**Questions**:
- Are closures with identical code but different identities considered equal?
- Should closure equality be by code structure or by closure identity?
- How does this affect binding deduplication?

**Example Case**:
```scheme
(let ((f (lambda (x) (+ x 1)))
      (g (lambda (x) (+ x 1))))
  (list f g))
```

**Analysis**:
- `f` and `g` closures are **equivalent** (isomorphic - same code, same behavior)
- But they are **not equal** for deduplication purposes because bindings are (name, value) pairs
- Binding 1: `name="f"`, `value=closure(...)`
- Binding 2: `name="g"`, `value=closure(...)`
- Since names differ, these bindings are NOT deduplicated, even though closure values are equivalent

**Decision**: ✅ **Resolved** - Closures with equivalent code but different binding names are NOT deduplicated. Deduplication is by (name, value) pairs, so different names = different bindings, regardless of value equivalence.

---

### 3. Closure Environment Capture Timing ⏳
**Dependencies**: #1 (Binding Identity), #2 (Closure Equality)
**Issue**: Unclear when nested closures are materialized vs deferred during serialization.

**Questions**:
- When a closure captures another closure, is the captured closure serialized immediately?
- Or is it deferred until the outer closure is called?
- How does this interact with the Environment section?

**Example Case**:
```scheme
(let ((helper (lambda (x) (+ x 1))))
  (lambda (y) (helper y)))
```

**Analysis**:
- **Common Lisp semantics**: Closures capture their lexical environment **at creation time** (immediate, not deferred). The environment contains actual values, not deferred expressions.
- **Pattern Lisp implementation**: `evalLambda` captures `currentEnv` which is a `Map String Value` - it stores actual values. When a closure is created, any closures already in the environment are concrete values.
- **Evaluation order for example**:
  1. `(lambda (x) (+ x 1))` evaluates to a closure value
  2. That closure value is bound to `helper` in the environment
  3. `(lambda (y) (helper y))` captures `currentEnv`, which already contains `helper` as a closure value
  4. The outer closure's environment has `helper` as a concrete, materialized closure

**Decision**: ✅ **Resolved** - Closures in captured environments are **immediately materialized** as values. They should be serialized immediately as bindings in the Environment section. No deferral is needed - the environment contains actual values, not deferred expressions. This matches Common Lisp semantics and the Pattern Lisp implementation.

---

### 4. Var vs Binding Reference Mixing ⏳
**Dependencies**: #1 (Binding Identity) - benefits from understanding binding semantics
**Issue**: Bodies can mix structural references (binding identifiers like `cfg`) with symbolic references (Var patterns like `[:Var {name: "state"}]`).

**Questions**:
- When should binding identifiers appear directly in bodies vs wrapped in Var patterns?
- How do these two reference mechanisms interact during evaluation?
- Is the distinction clear enough for implementers?

**Example Case**: See Example 3 in design doc - body uses both `cfg` (identifier) and `[:Var {name: "state"}]` (property).

**Proposed Solution**:
The distinction should be clearer:
- **Parameters**: Function arguments (part of lambda structure) - explicitly listed in `[:Parameters | ...]`
- **Bound values**: Captured from environment - referenced as identifiers (like `n`, `cfg`)

**Example**:
```scheme
(let ((num 1))
  (lambda (x) (+ num x)))
```

**Proposed Gram representation**:
```gram
[:Program |
  [:Environment |
    n:[:Binding {name: "num"} |
      [:Number {value: 1}]
    ]
  ],
  [:Expressions |
    [:Closure |
      [:Env | n],
      [:Lambda |
        [:Parameters | x],
        [:Body |
          [:List |
            [:Symbol {name: "+"}],
            n,
            x
          ]
        ]
      ]
    ]
  ]
]
```

**Key improvements**:
- Parameters explicitly listed in `[:Parameters | ...]` section
- In body, parameters referenced directly (like `x`) - they're part of the lambda structure
- Bound values referenced as identifiers (like `n`) - they reference Environment section
- Clear separation: parameters vs captured bindings

**Decision**: ✅ **Resolved** - Use explicit `[:Parameters | ...]` section for lambda parameters. In the body, parameters are referenced directly (they're part of the lambda structure), while bound values from the environment are referenced as identifiers. This makes the distinction between "function arguments" and "captured bindings" much clearer.

---

### 5. Recursive/Mutually Recursive Closures ⏳
**Dependencies**: #1 (Binding Identity), #2 (Closure Equality), #3 (Closure Capture Timing)
**Issue**: Design doesn't address how recursive closures serialize.

**Questions**:
- Can closures capture themselves (directly or indirectly)?
- Does binding deduplication handle cycles in the binding graph?
- Are cycles in the binding graph supported?

**Example Case**:
```scheme
(let ((f (lambda (x) (if (= x 0) 1 (g (- x 1)))))
      (g (lambda (x) (if (= x 0) 1 (f (- x 1))))))
  f)
```

**Analysis**:

When this is evaluated:
1. `let` creates a new scope and evaluates all bindings
2. `f` is created as a closure: `(lambda (x) (if (= x 0) 1 (g (- x 1))))`
3. `g` is created as a closure: `(lambda (x) (if (= x 0) 1 (f (- x 1))))`
4. Both closures capture the same environment, which includes both `f` and `g`
5. So `f`'s closure captures `g`, and `g`'s closure captures `f` (mutual recursion)

**Serialization Option 1: Both closures in Environment, forward references**

```gram
[:Program |
  [:Environment |
    f_binding:[:Binding {name: "f"} |
      [:Closure |
        [:Env | g_binding],
        [:Lambda |
          [:Parameters | x],
          [:Body |
            [:List |
              [:Symbol {name: "if"}],
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
            [:List |
              [:Symbol {name: "if"}],
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
  ],
  [:Expressions |
    f_binding
  ]
]
```

**Key observations**:
- Both `f` and `g` are closures, so they're bindings in the Environment section
- `f_binding` references `g_binding` in its `[:Env | ...]` section (forward reference)
- `g_binding` references `f_binding` in its `[:Env | ...]` section (forward reference)
- This creates a cycle: `f_binding` → `g_binding` → `f_binding`
- Forward references are valid in Gram, so this works
- In the body, `f` and `g` are referenced as symbols (like `[:Symbol {name: "f"}]`, `[:Symbol {name: "g"}]`)
- During evaluation, these symbols are resolved via the closure's captured environment
- The environment contains references to the bindings, so `"f"` → `f_binding` → closure value, `"g"` → `g_binding` → closure value

**Serialization Option 2: Using identifiers in body (alternative)**

If we wanted to be more explicit, we could reference the bindings directly in the body:

```gram
[:Closure |
  [:Env | g_binding],
  [:Lambda |
    [:Parameters | x],
    [:Body |
      [:List |
        [:Symbol {name: "if"}],
        [:List | [:Symbol {name: "="}], x, [:Number {value: 0}]],
        [:Number {value: 1}],
        [:List |
          g_binding,  ; Direct reference instead of symbol lookup
          [:List | [:Symbol {name: "-"}], x, [:Number {value: 1}]]
        ]
      ]
    ]
  ]
]
```

However, this changes semantics - the body would need to know about binding identifiers, which mixes levels. The symbol-based approach (Option 1) is cleaner.

**Additional Case: Self-Recursive Closure**

A closure can also capture itself:

```scheme
(let ((fact (lambda (n) 
               (if (= n 0) 
                 1 
                 (* n (fact (- n 1)))))))
  fact)
```

**Serialized**:
```gram
[:Program |
  [:Environment |
    fact_binding:[:Binding {name: "fact"} |
      [:Closure |
        [:Env | fact_binding],  ; Self-reference!
        [:Lambda |
          [:Parameters | n],
          [:Body |
            [:List |
              [:Symbol {name: "if"}],
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
  ],
  [:Expressions |
    fact_binding
  ]
]
```

**Key observation**: The closure references itself in its `[:Env | fact_binding]` section. This is a self-referential cycle, which is also fully supported.

**Decision**: ✅ **Resolved** - Recursive/mutually recursive closures are fully supported, including:
- **Mutual recursion**: Closures that capture each other (like `f` and `g`)
- **Self-recursion**: Closures that capture themselves (like `fact`)

Closures that capture each other or themselves are serialized as bindings in the Environment section, with forward/self references. The cycle is handled naturally because:
1. Forward references are valid in Gram notation
2. Bindings are identified by their identifiers, not by their position
3. The environment lookup resolves the cycle during deserialization
4. The body uses symbol names (like `f`, `g`, `fact`) which are resolved via the environment at runtime
5. Binding deduplication works correctly - each closure is a unique binding (different names), even if they reference each other

---

### 6. Pattern Subject → Runtime Value Mapping ⏳
**Dependencies**: #1-4 (understanding serialization structure)
**Issue**: Unclear how runtime distinguishes different interpretations of Pattern Subjects during deserialization.

**Questions**:
- How does runtime know when `[:List | ...]` is a list value vs a closure form?
- Are closures always explicitly marked with `[:Closure | ...]`?
- How are special forms identified during deserialization?

**Analysis**:

**Key insight**: Labels indicate whether a list is "special". Since all patterns ↔ s-expressions, the label always indicates the interpretation.

**Pattern Lisp Special Forms**:
1. `lambda` - Creates closures (serialized as `:Closure` when it's a value)
2. `if` - Conditional evaluation
3. `let` - Local bindings
4. `quote` - Prevents evaluation
5. `begin` - Sequential evaluation
6. `define` - Global bindings

**Two contexts for serialization**:

1. **Runtime Values** (what gets evaluated to):
   - `:Closure` - Closure values (always marked)
   - `:Number` - Number values
   - `:String` - String values
   - `:Bool` - Boolean values
   - `:List` - List values (data)
   - `:Primitive` - Primitive function values

2. **Code/Expressions** (in closure bodies, AST):
   - `:List` - Function calls or lambda expressions in code
   - `:If` - If special form
   - `:Let` - Let special form
   - `:Begin` - Begin special form
   - `:Define` - Define special form
   - `:Quote` - Quote special form (already exists)
   - `:Symbol`, `:Number`, `:String`, `:Bool` - Atomic expressions

**Proposed Label Taxonomy**:

**For Runtime Values**:
- `:Closure` - Closure values (already exists)
- `:Number` - Number values
- `:String` - String values
- `:Bool` - Boolean values
- `:List` - List values (when it's data, not code)
- `:Primitive` - Primitive function values

**For Code/Expressions** (in closure bodies):
- `:List` - Function calls: `(func arg1 arg2)` or lambda in code: `(lambda (x) ...)`
- `:If` - If special form: `(if cond then else)`
- `:Let` - Let special form: `(let ((var val)...) body)`
- `:Begin` - Begin special form: `(begin expr1 expr2 ...)`
- `:Define` - Define special form: `(define name value)`
- `:Quote` - Quote special form: `(quote expr)` or `'expr`
- `:Symbol`, `:Number`, `:String`, `:Bool` - Atomic expressions

**Example: Closure body with special forms**

S-expression:
```scheme
(lambda (x)
  (if (= x 0)
    1
    (let ((y (- x 1)))
      (* x y))))
```

Serialized body:
```gram
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
```

**Benefits**:
- Clear distinction: labels explicitly indicate special forms vs function calls
- No ambiguity: `:If` is always an if special form, `:List` in code is always a function call
- Consistent: `:Quote` already exists, extends the pattern
- Deserialization is straightforward: label → special form handler

**Decision**: ✅ **Resolved** - Adopt label taxonomy for special forms:
- Runtime values use value labels (`:Closure`, `:Number`, `:String`, `:Bool`, `:List` for data)
- Code/expressions use special form labels (`:If`, `:Let`, `:Begin`, `:Define`, `:Quote`) or `:List` for function calls
- Closures are always marked with `:Closure` when they're values
- Labels make the interpretation explicit and unambiguous

---

### 7. Standard Library Versioning ⏳
**Dependencies**: None (independent design choice)
**Issue**: Standard library filtering assumes stable standard library, but runtimes may differ.

**Questions**:
- How should standard library changes be handled across versions?
- Should serialized programs explicitly list required primitives?
- How to handle missing/extra primitives gracefully?
- Should there be a standard library version identifier?

**Analysis**:

**Key insight**: This is primarily a **runtime implementation concern**, not a serialization problem. The serialization design should be flexible enough to support metadata, but the detailed behavior belongs to the runtime environment.

**Observations**:
1. **Runtime is generative and adaptive**: The runtime should synthesize library functions as needed, inferring behavior from names and usage patterns
2. **Primitive inference**: When a runtime receives an arbitrary plisp program, it should:
   - Scan for undefined primitives
   - Infer from name and usage what the primitive probably does
   - Generate or adapt implementations as needed
3. **Development vs deployment**: During development, implementations may exist in the development runtime. The runtime should maintain meta-information about the available library
4. **Serialization flexibility**: The serialization format could support metadata fields (like `description` and `version`) in bindings, but this is an extension, not a requirement

**Current design**:
- Standard library bindings are filtered out during serialization (correct approach)
- Primitives serialize as `[:Primitive {name: "..."}]` (symbolic reference)
- Runtime looks up primitives by name during deserialization
- If primitive doesn't exist, deserialization fails (current behavior)

**Future considerations** (for runtime environment work):
- Runtime could maintain a registry of available primitives with metadata
- Bindings could include optional metadata fields: `{name: "...", description: "...", version: "..."}`
- Runtime could implement fallback/generation strategies for missing primitives
- Runtime could track compatibility requirements

**Decision**: ✅ **Deferred to Runtime Environment Work** - The serialization design is sufficient as-is. Standard library versioning, primitive inference, and metadata are runtime concerns that should be addressed in runtime environment implementation. The current serialization format is flexible enough to support metadata extensions later (e.g., adding `description` and `version` fields to bindings) without breaking changes.

---

### 8. Program Structure Overhead ⏳
**Dependencies**: None (independent design choice)
**Issue**: Every serialization produces a `[:Program | ...]` wrapper, even for simple values.

**Questions**:
- Is the overhead acceptable for consistency?
- Should simple values be allowed to serialize without Program wrapper?
- What defines a "simple value" vs a "program"?

**Analysis**:

**Key insight**: Gram files are already sequences of patterns - the file itself is the "outermost pattern" with file contents as elements. An outer `[:Program ...]` wrapper is not needed.

**Better approach**: Use file-level property record for metadata instead of a wrapper pattern.

**Proposed structure**:

For Pattern Lisp programs:
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

For simple values:
```gram
{ kind: "Pattern Lisp" }
[:Number {value: 1}]
```

**Benefits**:
- No unnecessary nesting - leverages Gram's file-level structure
- File-level metadata distinguishes Pattern Lisp programs from arbitrary Gram files
- `[:Environment ...]` can be omitted if empty (simpler for simple values)
- Simple values serialize directly without wrapper
- More flexible - metadata can include version, source, creation date, etc.

**Current design implications**:
- The `[:Program | ...]` wrapper in the current design is unnecessary overhead
- File-level property records provide better metadata capabilities
- Environment section is optional (can be omitted if empty)
- Expressions are just patterns in the file sequence

**Decision**: ✅ **Resolved** - Remove `[:Program | ...]` wrapper. Use file-level property record for metadata. Environment section is optional (omit if empty). Expressions are patterns in the file sequence. This leverages Gram's native file structure and eliminates unnecessary nesting.

---

## Documentation Updates Needed

**Note**: These follow the same dependency order as Critical Design Decisions.

### 9. Clarify Binding Identity Semantics ⏳
**Dependencies**: #1
- Document whether deduplication preserves binding identity or only value equality
- Add examples showing the distinction

### 10. Document Closure Equality Semantics ⏳
**Dependencies**: #2
- Document closure equality rules for deduplication
- Add examples showing closure equality scenarios

### 11. Specify Closure Materialization Rules ⏳
**Dependencies**: #3
- Document when nested closures are serialized vs deferred
- Add examples of closure-in-closure scenarios

### 12. Clarify Var/Binding Reference Usage ⏳
**Dependencies**: #4
- Document when to use binding identifiers vs Var patterns in bodies
- Add clear examples showing the distinction
- Explain evaluation semantics for each

### 13. Address Cycle Handling ⏳
**Dependencies**: #5
- Document support (or lack thereof) for recursive closures
- Specify how cycles in binding graph are handled

### 14. Document Deserialization Pattern Matching ⏳
**Dependencies**: #6
- Clarify how runtime identifies closures vs lists vs other forms
- Document the deserialization decision tree

### 15. Standard Library Versioning Strategy ⏳
**Dependencies**: #7
- Define versioning approach for standard library
- Specify how runtime compatibility is determined
- Document error handling for missing primitives

---

## Implementation Considerations

**Note**: These follow the same dependency order as Critical Design Decisions.

### 16. Binding Equality Implementation ⏳
**Dependencies**: #1, #2
- Define precise equality semantics for all value types
- Specify closure equality algorithm
- Document edge cases

### 17. Cycle Detection Algorithm ⏳
**Dependencies**: #5
- If cycles are supported, specify detection/handling algorithm
- If not supported, specify error conditions

### 18. Standard Library Registry Design ⏳
**Dependencies**: #7
- Define standard library structure
- Specify how primitives are registered/looked up
- Design version checking mechanism

---

## Testing Requirements

**Note**: These follow the same dependency order as Critical Design Decisions.

### 19. Binding Identity Tests ⏳
**Dependencies**: #1
- Test cases for same-name, same-value, different-scope bindings

### 20. Closure Equality Tests ⏳
**Dependencies**: #2
- Test closure equality scenarios
- Test deduplication of identical closures

### 21. Closure Capture Timing Tests ⏳
**Dependencies**: #3
- Test nested closure serialization
- Test closure-in-closure scenarios

### 22. Var/Binding Reference Tests ⏳
**Dependencies**: #4
- Test mixed usage in bodies
- Test evaluation semantics
- Test edge cases

### 23. Recursive Closure Tests ⏳
**Dependencies**: #5
- Test self-referential closures
- Test mutually recursive closures
- Test cycles in binding graph

### 24. Deserialization Pattern Matching Tests ⏳
**Dependencies**: #6
- Test runtime identification of closures vs lists
- Test special form recognition

### 25. Standard Library Versioning Tests ⏳
**Dependencies**: #7
- Test deserialization with different standard library versions
- Test missing primitive handling
- Test extra primitive handling

---

## Notes

- Work through items methodically, one at a time
- Document decisions in this file as they're made
- Update main design doc once decisions are finalized
- Consider implementation implications when making decisions

