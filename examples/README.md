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

### Using the REPL

You can load and run example programs using the REPL:

```bash
cat examples/factorial.plisp | cabal run pattern-lisp
```

### Programmatically

Example programs can be loaded and evaluated programmatically using the library API. See the test suite (`test/ExamplesSpec.hs`) for examples of how to evaluate multi-line programs.

## Example Files

### `factorial.plisp`

Demonstrates recursive function definition and function calls. Defines a factorial function and computes `(factorial 5)`.

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

## Notes

- All examples use `.plisp` extension to distinguish them from other Lisp implementations
- Examples are designed to be self-contained and runnable
- Comments use `;;` syntax
- Examples assume the interpreter has all core forms and primitives implemented

