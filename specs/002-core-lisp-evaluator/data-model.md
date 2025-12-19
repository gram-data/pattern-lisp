# Data Model: Core Lisp Evaluator

**Date**: 2025-01-27  
**Feature**: Core Lisp Evaluator

## Overview

The data model for the Lisp interpreter consists of the abstract syntax tree (AST) representation of expressions, runtime values, evaluation environments, and closures. These entities work together to enable parsing, evaluation, and execution of Lisp programs.

## Core Entities

### Expression (Expr)

**Purpose**: Represents the abstract syntax tree of Lisp code, including atoms, lists, and special forms.

**Structure**:
```haskell
data Expr
  = Atom Atom          -- Symbols, numbers, strings, booleans
  | List [Expr]        -- S-expressions (function calls, special forms)
  | Quote Expr         -- Quoted expressions (prevent evaluation)
```

**Properties**:
- Recursive structure (lists contain expressions)
- Immutable (expressions don't change after parsing)
- Represents both code and data (quoted expressions)

**Validation Rules**:
- Lists must have valid structure (special forms have specific arities)
- Atoms must be valid (symbols are identifiers, numbers are numeric, strings are quoted)
- Quote expressions contain exactly one sub-expression

**Relationships**:
- Parsed from S-expression strings by `Lisp.Parser`
- Evaluated by `Lisp.Eval` to produce `Value`
- Can contain nested expressions (recursive)

### Atom

**Purpose**: Represents atomic values in the AST (symbols, numbers, strings, booleans).

**Structure**:
```haskell
data Atom
  = Symbol String      -- Variable names, function names
  | Number Integer     -- Integer literals
  | String Text        -- String literals
  | Bool Bool          -- Boolean literals (#t, #f)
```

**Properties**:
- Leaf nodes in AST (no sub-expressions)
- Symbols are used for variables and function names
- Numbers, strings, and booleans are self-evaluating

**Validation Rules**:
- Symbols must be valid identifiers (non-empty, no special characters except allowed ones)
- Numbers must be valid integer literals
- Strings must be properly quoted and escaped

### Value

**Purpose**: Represents runtime values that expressions evaluate to.

**Structure**:
```haskell
data Value
  = VNumber Integer           -- Numeric values
  | VString Text              -- String values
  | VBool Bool                -- Boolean values
  | VList [Value]             -- List values
  | VClosure Closure          -- Function closures
  | VPrimitive Primitive      -- Built-in primitive functions
```

**Properties**:
- Runtime representation of evaluated expressions
- Immutable (values don't change)
- Includes both data values and function values

**Validation Rules**:
- Values must match their types (numbers are numeric, strings are text, etc.)
- Closures must have valid parameter lists and bodies
- Primitives must be registered in the environment

**Relationships**:
- Produced by evaluating `Expr` in `Lisp.Eval`
- Stored in `Environment` for variable bindings
- Returned by function applications

### Closure

**Purpose**: Represents a function value that captures its lexical environment.

**Structure**:
```haskell
data Closure = Closure
  { params :: [String]    -- Function parameter names
  , body   :: Expr        -- Function body expression
  , env    :: Env         -- Captured lexical environment
  }
```

**Properties**:
- Captures environment at definition time (lexical scoping)
- Immutable (closure doesn't change after creation)
- Enables proper variable resolution in nested functions

**Validation Rules**:
- Parameter list must be non-empty and contain valid symbols
- Body must be a valid expression
- Environment must be valid (Map structure)

**Relationships**:
- Created when evaluating `lambda` expressions
- Stored as `VClosure` in `Value`
- Applied with arguments to produce new values

### Environment (Env)

**Purpose**: Maps variable names to their bound values, supporting lexical scoping.

**Structure**:
```haskell
type Env = Map String Value
```

**Properties**:
- Immutable (new bindings create new environment, don't modify existing)
- Supports nested scopes (let bindings extend environment)
- Efficient lookup using Map data structure

**Validation Rules**:
- Variable names must be valid symbols
- Values must be valid `Value` instances
- Environment lookup must handle undefined variables gracefully

**State Transitions**:
1. **Empty** → Initial environment (no bindings)
2. **Extended** → New bindings added (let, lambda parameters, define)
3. **Nested** → Environment extended for local scope (let, closure application)

**Relationships**:
- Threaded through evaluation via ReaderT monad
- Extended by `let` bindings and `lambda` parameters
- Captured by closures for lexical scoping
- Used by `define` for global bindings

### Primitive Function

**Purpose**: Represents built-in functions provided by the interpreter.

**Structure**:
```haskell
data Primitive
  = Add | Sub | Mul | Div           -- Arithmetic
  | Gt | Lt | Eq | Ne               -- Comparison
  | StringAppend | StringLength | Substring  -- String operations
```

**Properties**:
- Built-in functions (not user-defined)
- Have fixed arity and semantics
- Evaluated directly (not through closure application)

**Validation Rules**:
- Must have correct number of arguments (arity checking)
- Arguments must be correct types (type checking)
- Operations must handle edge cases (division by zero, etc.)

**Relationships**:
- Registered in initial environment
- Called during expression evaluation
- Represented as `VPrimitive` in `Value`

## Relationships Summary

```
S-expression String
    │
    ▼ (parse)
Expr (AST)
    │
    ▼ (eval with Env)
Value
    │
    ├──→ Stored in Env (variable bindings)
    ├──→ Returned as result
    └──→ Applied as function (Closure or Primitive)
```

**Evaluation Flow**:
1. Parse S-expression string → `Expr`
2. Evaluate `Expr` in `Env` → `Value`
3. Store `Value` in `Env` for variable bindings
4. Apply `Value` (if function) with arguments → new `Value`

**Scope Flow**:
1. Start with empty `Env`
2. `define` adds global bindings to `Env`
3. `let` creates new `Env` extended with local bindings
4. `lambda` creates `Closure` capturing current `Env`
5. Closure application uses captured `Env` extended with arguments

## Validation Rules

### Expression Validation
1. **List Structure**: Special forms must have correct arity:
   - `lambda`: 2 arguments (parameter list, body)
   - `if`: 3 arguments (condition, then, else)
   - `let`: 2 arguments (bindings, body)
   - `quote`: 1 argument (expression to quote)
   - `begin`: 1+ arguments (expressions to sequence)
   - `define`: 2 arguments (name, value)

2. **Atom Validation**:
   - Symbols: Valid identifier format
   - Numbers: Valid integer format
   - Strings: Properly quoted and escaped

### Evaluation Validation
1. **Variable Lookup**: Undefined variables must produce clear error
2. **Type Checking**: Operations must receive correct value types
3. **Arity Checking**: Functions must receive correct number of arguments
4. **Division by Zero**: Must be caught and reported

### Environment Validation
1. **Scope Rules**: Variable lookup respects lexical scoping
2. **Shadowing**: Inner bindings shadow outer bindings
3. **Closure Capture**: Closures capture environment at definition time

## State Transitions

### Evaluation State
1. **Parsed** → Expression parsed from string
2. **Evaluating** → Expression being evaluated in environment
3. **Evaluated** → Expression evaluated to value
4. **Error** → Evaluation failed with error

### Environment State
1. **Empty** → No bindings
2. **Global Bindings** → `define` statements add global bindings
3. **Extended** → `let` or `lambda` extends environment
4. **Nested** → Multiple nested scopes

## Notes

- All entities are immutable to ensure correct semantics
- Environment threading via ReaderT monad ensures lexical scoping
- Closure capture ensures functions see variables from definition site
- Property-based tests verify semantic correctness (substitution, closure behavior)

