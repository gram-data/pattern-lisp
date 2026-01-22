# Examples

This directory contains example Pattern Lisp programs (`.plisp` files) demonstrating language capabilities and serving as learning resources.

## Purpose

Example programs demonstrate:
- Basic Lisp syntax and evaluation
- Common programming patterns
- Function definitions and closures
- Conditional expressions
- Scoping and environment handling

## Running Examples

### Direct Execution

Run example programs directly:

```bash
# Using cabal run
cabal run pattern-lisp -- examples/arithmetic.plisp

# Or if installed
pattern-lisp examples/arithmetic.plisp
```

### Using the REPL

You can load and run example programs using the REPL:

```bash
cat examples/factorial.plisp | cabal run pattern-lisp
```

### Programmatically

Example programs can be loaded and evaluated programmatically using the library API. See the test suite (`test/ExamplesSpec.hs`) for examples of how to evaluate multi-line programs.

## Example Status

**Working Examples** (8/10):
- ✓ `arithmetic.plisp` - Basic arithmetic operations
- ✓ `conditionals.plisp` - Conditional expressions
- ✓ `functions.plisp` - Function definitions
- ✓ `lists.plisp` - List operations
- ✓ `scoping.plisp` - Scoping and closures
- ✓ `pattern-basics.plisp` - Pattern construction
- ✓ `pattern-predicates.plisp` - Pattern predicates
- ✓ `keywords-maps-sets.plisp` - Keywords, maps, and sets operations

**Examples with Known Issues** (2/10):
- ✗ `factorial.plisp` - Recursive definitions not yet supported (documented limitation)
- ✗ `records.plisp` - Parse error with multi-expression file containing empty records (`{}`) in `begin` expressions. Individual record expressions work correctly when tested separately.

## Example Files

### `factorial.plisp`

Demonstrates recursive function definition and function calls. Defines a factorial function and computes `(factorial 5)`.

**⚠️ Known Limitation**: This example currently fails because recursive definitions are not yet supported in the current implementation. The `define` form evaluates the value expression before adding the name to the environment, so recursive references aren't available. This is a known limitation that will be addressed in a future update.

### `lists.plisp`

Demonstrates working with quoted lists. Defines list values and shows how to reference them.

### `arithmetic.plisp`

Demonstrates basic arithmetic operations: addition, multiplication, subtraction, and division.

### `functions.plisp`

Demonstrates function definitions using `lambda` and function calls. Shows how to define and use simple functions.

### `conditionals.plisp`

Demonstrates conditional expressions using `if`. Shows how to use comparison operators with conditionals.

### `scoping.plisp`

Demonstrates local bindings with `let` and closure scoping. Shows how closures capture their lexical environment.

### `pattern-basics.plisp`

Demonstrates basic pattern construction and querying operations. Shows how to create patterns, extract values, and query structural properties.

### `pattern-predicates.plisp`

Demonstrates pattern predicate primitives (`pattern-find`, `pattern-any?`, `pattern-all?`). Shows how to search and filter patterns using predicate closures that recursively traverse pattern structures.

### `records.plisp`

Demonstrates record creation, manipulation, and operations using the inline record notation `{key: value, ...}`. Shows:
- Basic record creation with various value types
- Record operations: `get`, `has?`, `keys`, `values`, `assoc`, `dissoc`, `merge`
- Record transformations: `map`, `filter`
- Record conversion: `record->alist`, `alist->record`
- Nested records
- Quasiquotation with records (unquoting and splicing)
- Real-world examples (user profiles, configuration objects, pattern subjects)

Records are gram-compatible and use comma-separated key-value pairs. Keys can be identifiers or strings, and values can be any pattern-lisp value type.

**⚠️ Known Issue**: This example file currently fails to parse when run as a multi-expression file due to parser limitations with empty records (`{}`) in `begin` expressions. Individual record expressions work correctly. This is a known limitation that will be addressed in a future update.

### `keywords-maps-sets.plisp`

Demonstrates keywords, maps (records), and sets. Shows:
- Keyword syntax and self-evaluation
- Map/record creation and operations
- Set creation and operations (union, intersection, difference)
- Set predicates (subset, equality, empty checks)
- Combined examples with nested structures

**Note**: Uses `define` (not `def`) for variable bindings. Nested access uses `(get (get data "user") "name")` instead of `get-in` (which is not yet implemented).

## Notes

- All examples use `.plisp` extension to distinguish them from other Lisp implementations
- Examples are designed to be self-contained and runnable
- Comments use `;;` syntax
- Examples assume the interpreter has all core forms and primitives implemented

