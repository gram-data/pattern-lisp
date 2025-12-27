# TODO - Pattern Lisp Development Roadmap

## Phase 0: Project Setup

**Initialize Cabal project with library and CLI components**

1. Create basic directory structure:
   ```
   pattern-lisp/
   ├── src/              # Library code
   ├── app/              # CLI executable
   ├── test/             # Test suite
   ├── examples/         # Example Lisp programs
   ├── cabal.project     # Multi-package configuration
   └── pattern-lisp.cabal
   ```

2. Configure `cabal.project` with source repository dependency for gram-hs:
   ```
   packages: .
   
   source-repository-package
     type: git
     location: https://github.com/gram-data/gram-hs
     tag: <appropriate-commit-or-tag>
   ```

3. Set up `pattern-lisp.cabal`:
   - Library exposing core modules (`Lisp.*`)
   - Executable for CLI/REPL
   - Test suite with dependencies (hspec, QuickCheck)
   - Common dependencies: text, containers, megaparsec, mtl

4. Verify build:
   ```bash
   cabal update
   cabal build all
   cabal test
   ```

5. Initialize git repository and push to GitHub

**Expected capabilities after Phase 0:**
- Clean Cabal project that builds successfully
- Can import gram-hs modules in library code
- Basic CLI executable runs (even if minimal)
- Test suite framework in place

---

## Phase 1: Core Lisp Evaluator

**Build a minimal Lisp interpreter with S-expression syntax**

Core modules:
- `Lisp.Syntax` - AST data types (Expr, Value)
- `Lisp.Parser` - S-expression parser
- `Lisp.Eval` - Environment-based evaluator
- `Lisp.Primitives` - Built-in functions

**Expected capabilities after Phase 1:**
- Parse and evaluate S-expressions from strings
- Support core forms: `lambda`, `if`, `let`, `quote`, `begin`, `define`
- String primitives: `string-append`, `string-length`, `substring`
- Arithmetic and comparison operators
- Interactive REPL for experimentation
- Property-based tests verifying key evaluation laws (substitution, closure semantics)
- Example programs demonstrating language capabilities

---

## Phase 2: Gram Serialization

**Represent Lisp code as Gram subject patterns**

**Expected capabilities after Phase 2:**
- Convert Lisp AST to/from Gram subject pattern representation
- Parser can accept both S-expression and Gram notation input
- Pretty-printer outputs Gram subject patterns
- Demonstrate structural queries over Lisp code represented as patterns
- Examples showing equivalent S-expression and Gram forms
- CLI can load and evaluate `.gram` files containing Lisp code

---

## Phase 3: Host Call Boundary

**Add `(host-call 'name args...)` for platform-specific capabilities**

**Expected capabilities after Phase 3:**
- Primitive form `(host-call symbol args...)` invokes registered host functions
- Host functions registered in evaluation environment
- Support multiple "host profiles" (mock, testing, production)
- Clear error handling for undefined host calls
- Examples demonstrating host-call usage patterns
- Documentation of host call contract and conventions

---

## Phase 4: Specification Document

**Formalize language semantics for reimplementation**

**Expected capabilities after Phase 4:**
- Complete specification document covering:
  - Syntax (S-expression and Gram forms)
  - Evaluation rules for all core forms
  - Primitive function semantics
  - Host call interface contract
  - Environment and closure behavior
  - Error conditions and handling
- Porting guide referencing Haskell implementation
- Conformance test suite (runnable from other implementations)
- Example: skeleton Rust/JavaScript interpreter outline

---

## Phase 5: Extended Examples & Tooling

**Demonstrate practical usage and provide development aids**

**Expected capabilities after Phase 5:**
- Rich example library showing idiomatic patterns
- REPL enhancements: multi-line input, history, better error messages
- Debugging aids: expression tracing, environment inspection
- Performance benchmarks establishing baseline behavior
- Documentation of design decisions and tradeoffs
- Guide for embedding Pattern Lisp in applications

---

## Future Directions (Beyond Initial Scope)

### Core Language Features from Syntax Conventions

**From `docs/pattern-lisp-syntax-conventions.md`:**

**Keywords and Maps** (Not Yet Implemented - Design Document v0.1)

This feature implements the core syntax conventions described in `docs/pattern-lisp-syntax-conventions.md`, enabling keywords, maps, and label syntax for clean interoperability with gram notation.

**Keywords with postfix colon syntax**: `name:`, `age:`, etc.
- Keywords are self-evaluating symbols (no lookup occurs)
- Used for map keys, option/configuration maps, tagged unions, keyword arguments
- **Implementation tasks**:
  - Add `Keyword String` variant to `Atom` type in `Syntax.hs`
  - Add `VKeyword String` variant to `Value` type in `Syntax.hs`
  - Add keyword parser to `Parser.hs` (recognizes `symbol:` pattern)
  - Add keyword evaluation to `Eval.hs` (keywords evaluate to themselves)
  - Update serialization in `Codec.hs` to handle keywords

**Map literals with curly brace syntax**: `{name: "Alice" age: 30 active: true}`
- Maps use keywords as keys with postfix colon syntax
- Support nested maps
- **Implementation tasks**:
  - Add `VMap (Map String Value)` variant to `Value` type in `Syntax.hs` (using String keys for now, or `Map Keyword Value` if keywords implemented first)
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
- Support label lists: `[:Person :Employee]`
- **Implementation tasks**:
  - Add `Label String` variant to `Atom` type in `Syntax.hs` (optional - may be handled via gram interop only)
  - Add label parser to `Parser.hs` (recognizes `:symbol` pattern)
  - Add label evaluation to `Eval.hs`
  - Update serialization in `Codec.hs` to handle labels

**Namespaced symbols**: `effect/succeed`, `string/split`
- Currently supported as regular symbols (parser allows `/` in symbol names at line 71)
- Core semantics treat namespaced symbols as ordinary symbols (naming convention only)
- **Implementation tasks**:
  - Verify parser correctly handles `/` in symbol names ✅ (already supported)
  - Future: Enhanced namespace resolution for module system (deferred)

**Rationale**: These features are essential for Pattern Lisp's design goal of clean interoperability with gram notation and support for structured data. Maps and keywords are fundamental data structures that enable configuration, tagged unions, and structured state representation. The postfix keyword syntax (`name:`) distinguishes keys from labels (`:Person`), enabling unambiguous parsing while maintaining gram notation compatibility.

**Priority**: High - These are core language features described in the syntax conventions document and are needed for complete language support.

### Planned Features from Design Documents

**From `docs/pattern-state-lisp-design.md`:**

**Phase 3: Host-Call Boundary** (Not Yet Implemented)
- Host function registry
- Side effect evaluation via `(host-call 'name args...)` form
- Error handling for I/O failures
- Standard host functions: file-read, file-write, http-get, http-post, db-query, db-execute, exec
- Pattern persistence via host-calls

**Phase 4: Graph Lens Integration** (Not Yet Implemented)
- Graph Lens construction in Lisp: `(graph-lens scope-pattern node-predicate)`
- Node/relationship queries: `(nodes lens)`, `(relationships lens)`
- Graph navigation primitives: `(neighbors lens node)`, `(degree lens node)`, `(connected-components lens)`, `(find-path lens start end)`
- Enable state Pattern to function as a knowledge graph

**Phase 5: Agent Runtime** (Partially Implemented)
- Multi-tool execution (not yet implemented)
- Execution tracing (RuntimeState has trace field, but not actively used)
- State persistence via host-calls (not yet implemented)

**Future Extensions** (Not Yet Implemented)
- Parallel execution of tools operating on disjoint Pattern subtrees
- Incremental updates: `(pattern-update-at "$.users[0]" update-fn state)`
- Optimistic concurrency with state merging
- Database-backed Patterns with transparent DB queries

**From `docs/pattern-lisp-effect-system.md`:**

**Effect System** (Not Yet Implemented - Design Document v0.1)
- Minimal effect system for describing I/O and side effects
- Effects as lazy descriptions (separation of description from execution)
- Explicit error tracking in type system: `Effect<Success, Error, Requirements>`
- Tier 1 operations: succeed, fail, sync, map, flatMap, catchAll, service
- Tier 2 operations: async, try, die, pipe, do, either, catchTag, orElse, orDie, match, matchCause
- Tier 3+ host-managed operations: with-timeout, with-delay, with-retry, with-resource, with-concurrency, race
- Service architecture for dependency injection
- Host implementation guide for effect interpretation

**Language extensions**
- Pattern matching on structured data
- Module/namespace system
- Gradual typing or contracts

**Advanced tooling**
- Static analyzer for common errors
- Optimizer/compiler targeting specific runtimes
- Visual debugger with step-through evaluation

**Runtime ports**
- Rust implementation with WASM compilation
- JavaScript implementation for browser/Node.js
- Python implementation for Jupyter/data science
- JVM implementation (Clojure interop)

**Integration with Pattern Agents**
- Agent tool definitions using Pattern Lisp
- Multi-agent coordination primitives
- Tool composition and higher-order patterns
