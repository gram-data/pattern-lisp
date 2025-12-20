# Quickstart: Core Lisp Evaluator

**Date**: 2025-01-27  
**Feature**: Core Lisp Evaluator

## Prerequisites

- Haskell toolchain (GHC 9.10.3, Cabal 3.10+)
- Project dependencies resolved (`cabal update` completed)
- Project builds successfully (`cabal build all`)

## Building the Project

```bash
# Update dependencies
cabal update

# Build library and executable
cabal build all

# Run tests
cabal test
```

## Using the REPL

### Starting the REPL

```bash
# Run the executable
cabal run pattern-lisp
```

You should see a prompt:
```
>
```

### Basic Usage

**Arithmetic**:
```lisp
> (+ 1 2)
3
> (* 4 5)
20
> (- 10 3)
7
> (/ 15 3)
5
```

**Variables and Functions**:
```lisp
> (define x 10)
x
> (+ x 5)
15
> (define square (lambda (x) (* x x)))
square
> (square 4)
16
```

**Conditionals**:
```lisp
> (if (> 5 3) 'yes 'no)
yes
> (if (< 5 3) 'yes 'no)
no
```

**Let Bindings**:
```lisp
> (let ((x 10) (y 20)) (+ x y))
30
```

**Quotes**:
```lisp
> (quote (a b c))
(a b c)
> '(1 2 3)
(1 2 3)
```

**Sequencing**:
```lisp
> (begin (define x 5) (+ x 1))
6
```

**String Operations**:
```lisp
> (string-append "hello" " " "world")
"hello world"
> (string-length "hello")
5
> (substring "hello" 1 3)
"el"
```

### Exiting the REPL

```lisp
> :quit
```

Or press Ctrl+D (EOF).

## Using as a Library

### Parse Expressions

```haskell
import Lisp.Parser

main = do
  case parseExpr "(+ 1 2)" of
    Left err -> putStrLn $ "Parse error: " ++ show err
    Right expr -> print expr
```

### Evaluate Expressions

```haskell
import Lisp.Parser
import Lisp.Eval
import Lisp.Primitives

main = do
  case parseExpr "(+ 1 2)" of
    Left err -> putStrLn $ "Parse error: " ++ show err
    Right expr -> case evalExpr expr initialEnv of
      Left err -> putStrLn $ "Error: " ++ show err
      Right val -> print val
```

### Define and Use Functions

```haskell
import Lisp.Parser
import Lisp.Eval
import Lisp.Primitives

main = do
  -- Define a function
  let defineExpr = parseExpr "(define square (lambda (x) (* x x)))"
  case defineExpr of
    Left err -> putStrLn $ "Parse error: " ++ show err
    Right expr -> do
      case evalExprWithEnv expr initialEnv of
        Left err -> putStrLn $ "Error: " ++ show err
        Right (val, env) -> do
          -- Use the function
          let callExpr = parseExpr "(square 4)"
          case callExpr of
            Left err -> putStrLn $ "Parse error: " ++ show err
            Right expr2 -> case evalExpr expr2 env of
              Left err -> putStrLn $ "Error: " ++ show err
              Right val2 -> print val2  -- Should print 16
```

## Running Tests

### Unit Tests

```bash
# Run all tests
cabal test

# Run with verbose output
cabal test --test-show-details=direct
```

### Property-Based Tests

Property-based tests verify evaluation laws:

```bash
# Run property tests
cabal test --test-option="--match=Properties"
```

Expected output:
```
Properties
  Substitution semantics
    ✓ substitution preserves evaluation
    ✓ substitution respects scoping
  Closure semantics
    ✓ closures capture lexical environment
    ✓ closures use captured environment
  Evaluation order
    ✓ expressions evaluate in correct order
```

## Example Programs

### Factorial

Create `examples/factorial.plisp`:
```lisp
(define factorial
  (lambda (n)
    (if (= n 0)
        1
        (* n (factorial (- n 1))))))

(factorial 5)
```

Load and evaluate:
```bash
cabal run pattern-lisp -- examples/factorial.plisp
```

Expected output:
```
120
```

Alternatively, you can use the REPL `:load` command:
```bash
cabal run pattern-lisp
> :load examples/factorial.plisp
120
```

### List Operations

Create `examples/lists.plisp`:
```lisp
(define list1 '(1 2 3))
(define list2 '(4 5 6))

;; Note: List operations will be added in future phases
;; For now, we can work with quoted lists
list1
```

## Troubleshooting

### Build Errors

**Issue**: `cabal build` fails with dependency errors.

**Solution**: 
```bash
cabal update
cabal build --dependencies-only
cabal build all
```

### Parse Errors

**Issue**: REPL shows parse errors for valid-looking expressions.

**Solution**: 
- Check parentheses are balanced
- Check string quotes are matched
- Check symbols are valid identifiers

### Evaluation Errors

**Issue**: "Undefined variable" errors.

**Solution**:
- Ensure variable is defined before use
- Check variable name spelling
- Use `define` for global bindings, `let` for local bindings

### Test Failures

**Issue**: Property-based tests fail.

**Solution**:
- Check that evaluation semantics are correctly implemented
- Verify environment handling (lexical scoping)
- Verify closure capture behavior

## Next Steps

After verifying the quickstart works:

1. **Explore the Language**: Try writing your own Lisp programs
2. **Read the Spec**: Review `spec.md` for complete feature requirements
3. **Check the Plan**: Review `plan.md` for implementation details
4. **Run Tests**: Ensure all tests pass before making changes
5. **Contribute**: Follow test-first development (write tests, then implement)

## Verification Checklist

- [ ] Project builds successfully (`cabal build all`)
- [ ] Tests pass (`cabal test`)
- [ ] REPL starts and accepts input
- [ ] Can evaluate arithmetic expressions
- [ ] Can define and call functions
- [ ] Can use conditionals (`if`)
- [ ] Can use local bindings (`let`)
- [ ] Can quote expressions
- [ ] Can sequence expressions (`begin`)
- [ ] Can use string primitives
- [ ] Property-based tests pass
- [ ] Example programs run successfully

## Getting Help

- **Specification**: See `spec.md` for feature requirements
- **Implementation Plan**: See `plan.md` for technical details
- **Data Model**: See `data-model.md` for entity definitions
- **Research**: See `research.md` for implementation patterns

