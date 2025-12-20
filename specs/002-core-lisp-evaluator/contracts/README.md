# Contracts: Core Lisp Evaluator

**Date**: 2025-01-27  
**Feature**: Core Lisp Evaluator

## Overview

This directory contains interface contracts for the Lisp interpreter. The main contracts are:

1. **Library API Contract**: Public interface for using the interpreter as a library
2. **REPL Interface Contract**: Command-line interface specification

## Library API Contract

### Module: `Lisp.Parser`

**Purpose**: Parse S-expression strings into AST representation.

**Interface**:
```haskell
parseExpr :: String -> Either ParseError Expr
```

**Input**: S-expression string  
**Output**: Either parse error or AST expression  
**Errors**: Invalid syntax, unmatched parentheses, invalid tokens

**Contract**:
- Valid S-expressions parse successfully
- Invalid syntax produces clear error messages
- Error messages include position information

### Module: `Lisp.Eval`

**Purpose**: Evaluate expressions in an environment.

**Interface**:
```haskell
evalExpr :: Expr -> Env -> Either Error Value
evalExprWithEnv :: Expr -> Env -> Either Error (Value, Env)
```

**Input**: Expression and environment  
**Output**: Either error or evaluated value (and possibly updated environment)  
**Errors**: Undefined variable, type mismatch, arity mismatch, division by zero

**Contract**:
- Expressions evaluate to values according to Lisp semantics
- Environment is extended for `let` and `lambda` bindings
- `define` updates environment with global bindings
- Errors are clear and include context

### Module: `Lisp.Primitives`

**Purpose**: Built-in primitive functions.

**Interface**:
```haskell
initialEnv :: Env
```

**Output**: Initial environment with primitive functions registered

**Contract**:
- All required primitives are registered (arithmetic, comparison, string operations)
- Primitives have correct arity and type checking
- Primitive errors are clear and informative

## REPL Interface Contract

### Command-Line Interface

**Purpose**: Interactive read-eval-print loop for Lisp expressions.

**Protocol**: Text I/O
- **Input**: stdin (S-expression strings, one per line)
- **Output**: stdout (evaluated results)
- **Errors**: stderr (error messages)

**Commands**:
- Standard input: S-expression to evaluate
- `:quit` or `:q`: Exit REPL
- `:help` or `:h`: Show help message

**Contract**:
- Each line is parsed and evaluated independently
- Results are printed to stdout
- Errors are printed to stderr
- REPL continues after errors (doesn't crash)
- Exit command terminates gracefully

**Example Session**:
```
> (+ 1 2)
3
> (define x 10)
x
> (+ x 5)
15
> undefined-var
Error: Undefined variable: undefined-var
> :quit
```

## Error Contract

All errors must:
1. Be clear and actionable
2. Include context (expression, position, variable name, etc.)
3. Be printed to stderr (for REPL) or returned in Either (for library)
4. Not crash the interpreter (REPL continues, library returns error)

**Error Types**:
- Parse errors: Invalid syntax, position information
- Evaluation errors: Undefined variable, type mismatch, arity mismatch
- Runtime errors: Division by zero, out of memory (if applicable)

## Integration Points

### Library Usage
```haskell
import Lisp.Parser
import Lisp.Eval
import Lisp.Primitives

-- Parse and evaluate
case parseExpr "(+ 1 2)" of
  Left err -> print err
  Right expr -> case evalExpr expr initialEnv of
    Left err -> print err
    Right val -> print val
```

### REPL Usage
```bash
# Start REPL
pattern-lisp

# Or evaluate expression from command line
echo "(+ 1 2)" | pattern-lisp
```

## Notes

- Contracts are minimal for Phase 1 (core functionality)
- Future phases may add more contracts (Gram serialization, host calls, etc.)
- All interfaces follow Haskell best practices (pure functions, Either for errors)

