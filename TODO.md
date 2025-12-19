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
