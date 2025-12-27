# TODO - Pattern Lisp Development Roadmap

## Completed Work

✅ **Phase 0: Project Setup** - Complete
✅ **Phase 1: Core Lisp Evaluator** - Complete
✅ **Phase 2: Gram Serialization** - Complete
✅ **Pattern as First-Class Value** - Complete (Pattern primitives, canonical form enforcement)

---

## Remaining Work (Ordered by Dependencies)

### Tier 1: Foundational Language Features

**Priority**: High - Core language features needed before higher-level features.

#### Keywords, Maps, and Sets

**From `docs/pattern-lisp-syntax-conventions.md` (Design Document v0.1)**

**Keywords with postfix colon syntax**: `name:`, `age:`, etc.
- Keywords are self-evaluating symbols (no lookup occurs)
- Used for map keys, option/configuration maps, tagged unions, keyword arguments
- **Implementation tasks**:
  - Add `Keyword String` variant to `Atom` type in `Syntax.hs`
  - Add `VKeyword String` variant to `Value` type in `Syntax.hs`
  - Add keyword parser to `Parser.hs` (recognizes `symbol:` pattern)
  - Add keyword evaluation to `Eval.hs` (keywords evaluate to themselves)
  - Update serialization in `Codec.hs` to handle keywords

**Sets with hash set syntax**: `#{:Person :Employee}`, `#{1 2 3}`
- Sets are unordered collections of unique elements
- Used for Subject labels (which are `Set String`), unique value collections
- **Implementation tasks**:
  - Add `VSet (Set Value)` variant to `Value` type in `Syntax.hs`
  - Add set literal parser to `Parser.hs` (recognizes `#{ ... }` syntax)
  - Add set literal evaluation to `Eval.hs`
  - Add set operation primitives: `contains?`, `set-union`, `set-intersection`, `set-difference`, `set-symmetric-difference`, `set-subset?`, `set-equal?`, `empty?`
  - Add `hash-set` constructor function
  - Update serialization in `Codec.hs` to handle sets

**Map literals with curly brace syntax**: `{name: "Alice" age: 30 active: true}`
- Maps use keywords as keys with postfix colon syntax
- Support nested maps
- **Implementation tasks**:
  - Add `VMap (Map Keyword Value)` variant to `Value` type in `Syntax.hs` (requires keywords first)
  - Add map literal parser to `Parser.hs` (recognizes `{ key: value ... }` syntax)
  - Add map literal evaluation to `Eval.hs`
  - Update serialization in `Codec.hs` to handle maps

**Map operations**:
- `(get m key:)` - get value with default nil
- `(get m key: default)` - get value with explicit default
- `(get-in data [user: name:])` - nested access (path is list of keywords)
- `(assoc m key: value)` - add/update key
- `(dissoc m key:)` - remove key
- `(update m key: f)` - apply function to value at key
- `(contains? m key:)` - check if key present
- `(empty? m)` - check if map is empty
- `(hash-map key1: val1 key2: val2 ...)` - construct map from key-value pairs
- **Implementation tasks**:
  - Add map operation primitives to `Primitive` type in `Syntax.hs`
  - Add all map operation primitives to `Primitives.hs` initialEnv
  - Implement map operations in `Eval.hs`
  - Add tests for all map operations

**Label syntax** (prefix colon): `:Person`, `:Employee`
- Labels are used for gram notation interop and type annotations
- Support label sets: `#{:Person :Employee}` (Subject labels are `Set String`)
- **Implementation tasks**:
  - Add `Label String` variant to `Atom` type in `Syntax.hs` (optional - may be handled via gram interop only)
  - Add label parser to `Parser.hs` (recognizes `:symbol` pattern)
  - Add label evaluation to `Eval.hs`
  - Update serialization in `Codec.hs` to handle labels

**Namespaced symbols**: `effect/succeed`, `string/split`
- ✅ Already supported as regular symbols (parser allows `/` in symbol names)
- Core semantics treat namespaced symbols as ordinary symbols (naming convention only)
- Future: Enhanced namespace resolution for module system (deferred)

**Rationale**: These features are essential for Pattern Lisp's design goal of clean interoperability with gram notation and support for structured data. Maps and keywords are fundamental data structures that enable configuration, tagged unions, and structured state representation.

**Dependencies**: None - foundational language features

**Implementation Order**:
1. Keywords (needed for maps)
2. Sets (can be parallel with keywords)
3. Maps (requires keywords)
4. Labels (optional, can be deferred if handled via gram interop)

---

#### Recursive Definitions

**Issue**: `define` form doesn't support recursive references
- Current implementation evaluates value expression before adding name to environment
- Prevents recursive function definitions (e.g., `factorial.plisp` fails)
- **Implementation tasks**:
  - Modify `evalDefine` in `Eval.hs` to support forward references
  - Options: lazy evaluation, two-pass approach, or deferred binding
  - Update `examples/factorial.plisp` to work correctly
  - Add tests for recursive definitions

**Dependencies**: None - can be implemented independently

---

### Tier 2: Language Extensions

**Priority**: Medium - Builds on foundational language features.

#### Host-Call Boundary

**From `docs/pattern-state-lisp-design.md` (Phase 3)**

Add `(host-call 'name args...)` for platform-specific capabilities.

**Implementation tasks**:
- Add `host-call` special form to parser
- Add host function registry to evaluation environment
- Implement host-call evaluation in `Eval.hs`
- Add error handling for undefined host calls
- Support multiple "host profiles" (mock, testing, production)
- Standard host functions: `file-read`, `file-write`, `http-get`, `http-post`, `db-query`, `db-execute`, `exec`
- Pattern persistence via host-calls
- Examples demonstrating host-call usage patterns
- Documentation of host call contract and conventions

**Dependencies**: Keywords and Maps (for configuration maps passed to host functions)

---

#### Graph Lens Integration

**From `docs/pattern-state-lisp-design.md` (Phase 4)**

Enable state Pattern to function as a knowledge graph.

**Implementation tasks**:
- Graph Lens construction: `(graph-lens scope-pattern node-predicate)`
- Node/relationship queries: `(nodes lens)`, `(relationships lens)`
- Graph navigation primitives: `(neighbors lens node)`, `(degree lens node)`, `(connected-components lens)`, `(find-path lens start end)`
- Add graph lens primitives to `Primitive` type
- Implement graph operations in `Eval.hs`
- Add tests for graph lens operations

**Dependencies**: Maps and Sets (for graph data structures)

---

### Tier 3: Runtime & Tooling

**Priority**: Medium - Builds on language features.

#### Extended Examples & Tooling

**Demonstrate practical usage and provide development aids**

**Implementation tasks**:
- Rich example library showing idiomatic patterns
- REPL enhancements: multi-line input, history, better error messages
- Debugging aids: expression tracing, environment inspection
- Performance benchmarks establishing baseline behavior
- Documentation of design decisions and tradeoffs
- Guide for embedding Pattern Lisp in applications

**Dependencies**: All foundational language features (Keywords, Maps, Sets)

---

#### Specification Document

**Formalize language semantics for reimplementation**

**Implementation tasks**:
- Complete specification document covering:
  - Syntax (S-expression and Gram forms, including keywords, maps, sets)
  - Evaluation rules for all core forms
  - Primitive function semantics
  - Host call interface contract
  - Environment and closure behavior
  - Error conditions and handling
- Porting guide referencing Haskell implementation
- Conformance test suite (runnable from other implementations)
- Example: skeleton Rust/JavaScript interpreter outline

**Dependencies**: All language features should be implemented first

---

### Tier 4: Advanced Features

**Priority**: Low - Future enhancements beyond core language.

#### Effect System

**From `docs/pattern-lisp-effect-system.md` (Design Document v0.1)**

Minimal effect system for describing I/O and side effects.

**Implementation tasks**:
- Effects as lazy descriptions (separation of description from execution)
- Explicit error tracking: `Effect<Success, Error, Requirements>`
- Tier 1 operations: succeed, fail, sync, map, flatMap, catchAll, service
- Tier 2 operations: async, try, die, pipe, do, either, catchTag, orElse, orDie, match, matchCause
- Tier 3+ host-managed operations: with-timeout, with-delay, with-retry, with-resource, with-concurrency, race
- Service architecture for dependency injection
- Host implementation guide for effect interpretation

**Dependencies**: Host-Call Boundary, Maps (for effect descriptions)

---

#### Language Extensions

- Pattern matching on structured data
- Module/namespace system (enhanced namespace resolution)
- Gradual typing or contracts

**Dependencies**: All foundational language features

---

#### Advanced Tooling

- Static analyzer for common errors
- Optimizer/compiler targeting specific runtimes
- Visual debugger with step-through evaluation

**Dependencies**: Specification Document, Extended Examples & Tooling

---

#### Runtime Ports

- Rust implementation with WASM compilation
- JavaScript implementation for browser/Node.js
- Python implementation for Jupyter/data science
- JVM implementation (Clojure interop)

**Dependencies**: Specification Document, Conformance Test Suite

---

#### Integration with Pattern Agents

- Agent tool definitions using Pattern Lisp
- Multi-agent coordination primitives
- Tool composition and higher-order patterns

**Note**: Agent runtime features will be implemented in a separate project that builds on Pattern Lisp.

**Dependencies**: Effect System

---

#### Future Extensions

**From `docs/pattern-state-lisp-design.md`**

- Parallel execution of tools operating on disjoint Pattern subtrees
- Incremental updates: `(pattern-update-at "$.users[0]" update-fn state)`
- Optimistic concurrency with state merging
- Database-backed Patterns with transparent DB queries

**Note**: These features may be implemented in a separate agent runtime project.

**Dependencies**: Graph Lens Integration

---

## Implementation Order Summary

**Recommended sequence** (by dependencies):

1. **Keywords** (foundational - needed for maps)
2. **Sets** (foundational - can be parallel with keywords)
3. **Maps** (requires keywords)
4. **Recursive Definitions** (language feature, independent)
5. **Host-Call Boundary** (requires maps for configuration)
6. **Graph Lens Integration** (requires maps and sets)
7. **Extended Examples & Tooling** (requires all foundational features)
8. **Specification Document** (requires all language features)
9. **Effect System** (requires host-call and maps)
10. **Advanced Features** (future work)
