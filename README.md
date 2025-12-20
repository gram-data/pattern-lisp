# Pattern Lisp

A minimal Lisp interpreter designed as a portable intermediate representation (IR) with clear semantics that can be reimplemented across multiple host environments.

## Overview

This project provides a tiny, well-specified Lisp evaluator that serves as a reference implementation for portable computation. By keeping the core language extremely small and precisely defined, the same Lisp programs can run identically across different host environments (browser, Python, JVM, Rust, native) with straightforward reimplementation.

## Design Philosophy

**Minimal Core Language**
- Small expression set: `lambda`, `if`, `let`, `define`, `quote`, `begin`
- Basic primitives: arithmetic, comparisons, string operations
- No I/O in core - host capabilities exposed through explicit boundary
- Tail-recursive, expression-only evaluation (Scheme-ish)
- Purely functional semantics with immutable environments

**Portability Through Simplicity**
- Reference implementation establishes canonical behavior
- Clear semantic specification enables mechanical consistency across runtimes
- Host-specific capabilities accessed via `(host-call 'name args...)`
- Same source runs in browser/Python/JVM/Rust with simple interpreter ports

**Gram as Serialization Format**
- Lisp code serialized as Gram subject patterns
- Implementation-agnostic: Gram is just structured data
- Enables querying and introspection of code structure
- Natural fit for embedding executable definitions in documents

## Example: "Hello World" Function

Using Gram subject pattern notation to represent a Lisp function:

```gram
[:Function {name: "greet", params: ["name"]} |
  [:Call {fn: "string-append"} |
    [:String {value: "Hello, "}],
    [:Symbol {name: "name"}],
    [:String {value: "!"}]
  ]
]
```

This is equivalent to the S-expression: `(lambda (name) (string-append "Hello, " name "!"))`

Both representations describe the same computation - the Gram form makes structure explicit and queryable.

## Architecture

```
┌─────────────────────────────────────┐
│    Lisp Code (S-expressions)        │
│    (lambda (x) (+ x 1))             │
└─────────────────────────────────────┘
                 │
                 ▼
┌─────────────────────────────────────┐
│   Gram Serialization (optional)     │
│   [:Lambda | [:Symbol "x"],         │
│              [:Call "+" ...]]       │
└─────────────────────────────────────┘
                 │
                 ▼
┌─────────────────────────────────────┐
│      Pattern Lisp Evaluator            │
│  • Parse expressions                │
│  • Environment-based evaluation     │
│  • Host call boundary               │
└─────────────────────────────────────┘
                 │
                 ▼
┌─────────────────────────────────────┐
│        Host Environment             │
│  • Haskell (reference impl)         │
│  • Rust / JavaScript / Python       │
│  • JVM (Clojure, Java, Kotlin)      │
└─────────────────────────────────────┘
```

## Project Goals

1. **Define a minimal Lisp core** with clear, unambiguous semantics
2. **Implement a reference evaluator** in Haskell demonstrating correct behavior
3. **Provide comprehensive test suite** verifying evaluation rules
4. **Document the specification** enabling faithful reimplementation in other languages
5. **Explore Gram serialization** of Lisp code as structured patterns
6. **Establish host call boundary** for platform-specific capabilities

## Non-Goals

- Rich standard library (intentionally minimal)
- Macros or compile-time metaprogramming
- Performance optimization (clarity over speed)
- Advanced type system or static analysis
- Built-in concurrency or async primitives

## Use Cases

While this is a general-purpose portable Lisp, it was designed with specific use cases in mind:

- **Embedded scripting**: Lightweight extension language for applications
- **Configuration with computation**: Beyond static config files, allow calculated values
- **Cross-platform tools**: Define behavior once, run everywhere
- **Agent tool definitions**: Express executable capabilities in a portable format (see Pattern Agents)
- **Educational**: Clear reference for understanding evaluation semantics

## Current Status

**Phase 1**: Core Lisp Evaluator ✅ **COMPLETE**

The core interpreter is fully functional with:
- ✅ S-expression parser with position-aware error messages
- ✅ Environment-based evaluator with lexical scoping
- ✅ Core language forms: `lambda`, `if`, `let`, `quote`, `begin`, `define`
- ✅ Primitive functions: arithmetic, comparison, string operations
- ✅ Interactive REPL with file loading support
- ✅ Comprehensive test suite (unit, property-based, integration)
- ✅ Example programs demonstrating language features

See [Quickstart Guide](specs/002-core-lisp-evaluator/quickstart.md) for detailed usage instructions.

## Setup & Build

### Prerequisites

- **Cabal**: 3.10 or later
- **GHC**: 9.6.3 (or compatible version)
- **Git**: For dependency resolution (gram-hs from GitHub)

### Initial Setup

1. **Update Cabal index and resolve dependencies**:
   ```bash
   cabal update
   ```

2. **Build all components**:
   ```bash
   cabal build all
   ```

3. **Run tests**:
   ```bash
   cabal test
   ```

4. **Run the CLI executable**:
   ```bash
   # Start interactive REPL
   cabal run pattern-lisp
   
   # Or execute a file directly
   cabal run pattern-lisp -- examples/functions.plisp
   ```

## Quick Start

### Interactive REPL

Start the REPL and try some expressions:

```bash
$ cabal run pattern-lisp
> (+ 1 2)
3
> (define square (lambda (x) (* x x)))
square
> (square 4)
16
> :quit
```

### Execute Example Programs

Run example programs from the `examples/` directory:

```bash
# Run a function example
cabal run pattern-lisp -- examples/functions.plisp

# Run a list example
cabal run pattern-lisp -- examples/lists.plisp
```

### Using as a Library

```haskell
import Lisp.Parser
import Lisp.Eval
import Lisp.Primitives

main = do
  case parseExpr "(+ 1 2)" of
    Left err -> print err
    Right expr -> case evalExpr expr initialEnv of
      Left err -> print err
      Right val -> print val  -- Output: VNumber 3
```

For more examples and detailed usage, see the [Quickstart Guide](specs/002-core-lisp-evaluator/quickstart.md).

### Project Structure

```
pattern-lisp/
├── src/              # Library code (Lisp.* modules)
├── app/              # CLI executable (Main.hs)
├── test/             # Test suite
├── examples/         # Example Lisp programs
├── cabal.project     # Multi-package configuration
└── pattern-lisp.cabal # Package definition
```

### Dependencies

- **gram-hs**: Source repository dependency from GitHub (https://github.com/gram-data/gram-hs)
- **Core libraries**: text, containers, megaparsec, mtl
- **Testing**: hspec, QuickCheck

## Documentation

- **[Quickstart Guide](specs/002-core-lisp-evaluator/quickstart.md)**: Get started with Pattern Lisp
- **[Feature Specification](specs/002-core-lisp-evaluator/spec.md)**: Complete feature requirements
- **[Implementation Plan](specs/002-core-lisp-evaluator/plan.md)**: Technical implementation details
- **[Examples](examples/README.md)**: Example programs demonstrating language features
- **[Development Roadmap](TODO.md)**: Future development phases

See [TODO.md](TODO.md) for the complete development roadmap.

## Related Work

- **gram-hs**: Haskell implementation of Gram notation parser
- **Scheme R5RS/R7RS-small**: Inspiration for minimal, well-specified semantics
- **Lua**: Model for lightweight embedded scripting language
- **WebAssembly**: Model for host function boundaries and portable execution
- **Pattern Agents**: Framework using this Lisp for portable tool definitions

## License

TBD
