# Research: Core Lisp Evaluator

**Date**: 2025-01-27  
**Feature**: Core Lisp Evaluator  
**Purpose**: Resolve technical unknowns and establish implementation patterns

## Research Questions

### 1. S-Expression Parsing with Megaparsec

**Question**: What is the best pattern for parsing S-expressions using megaparsec in Haskell?

**Decision**: Use a recursive parser that handles atoms (symbols, numbers, strings) and lists. Leverage megaparsec's built-in combinators for whitespace handling and error reporting.

**Rationale**: 
- Megaparsec provides excellent error messages and position tracking
- Recursive structure of S-expressions maps naturally to recursive parsers
- Built-in combinators handle common patterns (whitespace, brackets, etc.)
- Good performance for typical expression sizes

**Alternatives Considered**:
- attoparsec: Faster but worse error messages
- parsec: Older, less maintained
- Custom parser: Unnecessary complexity

**Implementation Pattern**:
```haskell
-- Pseudo-code structure
atom :: Parser Expr
atom = symbol <|> number <|> string

list :: Parser Expr
list = between (char '(') (char ')') (many expr)

expr :: Parser Expr
expr = whitespace *> (atom <|> list) <* whitespace
```

**Verification**: Test with various S-expression formats including nested structures, quoted expressions, and edge cases (empty lists, single-element lists).

### 2. Environment-Based Evaluation Pattern

**Question**: What is the best pattern for implementing environment-based evaluation with lexical scoping in Haskell?

**Decision**: Use a Reader monad transformer (ReaderT) to thread the environment through evaluation. Environment represented as a Map from variable names to values. Support nested environments for let bindings and closures.

**Rationale**:
- Reader monad pattern is idiomatic for environment threading
- Map provides efficient variable lookup
- Supports nested scopes naturally through environment extension
- Immutable environments ensure correct closure semantics

**Alternatives Considered**:
- State monad: Unnecessary mutation, complicates reasoning
- Explicit environment passing: Verbose, error-prone
- Global mutable state: Violates functional principles

**Implementation Pattern**:
```haskell
-- Pseudo-code structure
type Env = Map String Value
type EvalM = ReaderT Env (Either Error)

eval :: Expr -> EvalM Value
eval (Var name) = asks (Map.lookup name) >>= maybe (throwError ...) return
eval (Let bindings body) = do
  env <- ask
  newEnv <- extendEnv bindings env
  local (const newEnv) (eval body)
```

**Verification**: Property-based tests verify that variable lookup respects lexical scoping and closures capture correct environments.

### 3. Closure Implementation

**Question**: How should closures be represented and evaluated to correctly capture lexical environments?

**Decision**: Closures store both the function body (lambda expression) and the environment at definition time. When applying a closure, extend the captured environment with function arguments rather than using the current environment.

**Rationale**:
- Captures lexical scope correctly (variables from definition site, not call site)
- Immutable environment ensures closure behavior is predictable
- Standard pattern in functional language implementations

**Alternatives Considered**:
- Dynamic scoping: Incorrect semantics for Lisp
- Global environment only: Doesn't support lexical scoping
- Mutable closures: Complicates reasoning and violates immutability

**Implementation Pattern**:
```haskell
-- Pseudo-code structure
data Value = ...
           | Closure { params :: [String]
                     , body :: Expr
                     , env :: Env }

applyClosure :: Closure -> [Value] -> EvalM Value
applyClosure (Closure params body env) args = do
  let extendedEnv = extendEnv (zip params args) env
  runReaderT (eval body) extendedEnv
```

**Verification**: Property-based tests verify that closures capture and use their lexical environment correctly, even with nested functions and shadowed variables.

### 4. Property-Based Testing Patterns for Interpreters

**Question**: What property-based testing patterns are effective for verifying interpreter correctness?

**Decision**: Test evaluation laws (substitution semantics, closure semantics, evaluation order) using QuickCheck. Generate random expressions and verify properties hold across many cases. Test that equivalent expressions produce equivalent results.

**Rationale**:
- Property-based tests catch edge cases that unit tests might miss
- Verify semantic properties rather than specific examples
- QuickCheck is standard for Haskell property testing
- Can generate thousands of test cases automatically

**Alternatives Considered**:
- Unit tests only: Miss edge cases, require manual test case creation
- Fuzzing: Less structured, harder to verify specific properties
- Formal verification: Overkill for this phase

**Implementation Pattern**:
```haskell
-- Pseudo-code structure
prop_substitution :: Expr -> String -> Value -> Property
prop_substitution expr var val =
  let substituted = substitute expr var val
      env = Map.singleton var val
  in eval substituted env === eval expr env

prop_closure_capture :: Expr -> Property
prop_closure_capture expr =
  -- Verify closure captures environment at definition, not application
  ...
```

**Verification**: Run property tests with 1000+ randomly generated cases. Verify all properties pass consistently.

### 5. Error Handling and Reporting

**Question**: How should evaluation errors be represented and reported to users?

**Decision**: Use Either Error type for error handling. Error type includes error kind (undefined variable, type mismatch, arity mismatch, etc.) and context (expression, position, environment snapshot). Format errors as clear, informative messages.

**Rationale**:
- Either type is idiomatic Haskell error handling
- Structured errors enable better error messages
- Context information helps debugging
- Clear error messages improve developer experience

**Alternatives Considered**:
- Exceptions: Less composable, harder to reason about
- Maybe type: Loses error information
- Custom error monad: Unnecessary complexity for this phase

**Implementation Pattern**:
```haskell
-- Pseudo-code structure
data Error = UndefinedVar String Expr
           | TypeMismatch String Value
           | ArityMismatch String Int Int
           | DivisionByZero Expr
           | ...

instance Show Error where
  show (UndefinedVar name expr) = 
    "Undefined variable: " ++ name ++ " in expression: " ++ show expr
```

**Verification**: Test that common error cases produce clear, actionable error messages.

### 6. REPL Implementation Pattern

**Question**: What is the best pattern for implementing an interactive REPL in Haskell?

**Decision**: Use a simple read-eval-print loop with line-based input. Read from stdin, parse, evaluate, print result (or error). Support special commands (exit, help). Use hGetLine for line input, handle EOF gracefully.

**Rationale**:
- Simple pattern is sufficient for Phase 1
- Line-based input is standard for REPLs
- Can be enhanced later (history, multi-line, etc.)
- Text I/O aligns with constitution requirements

**Alternatives Considered**:
- Full terminal library (haskeline): More features but unnecessary complexity for Phase 1
- Block-based input: Less interactive
- GUI interface: Out of scope for Phase 1

**Implementation Pattern**:
```haskell
-- Pseudo-code structure
repl :: IO ()
repl = do
  putStr "> "
  hFlush stdout
  input <- getLine
  case input of
    ":quit" -> return ()
    _ -> case parseExpr input of
      Left err -> putStrLn ("Parse error: " ++ show err) >> repl
      Right expr -> case evalExpr expr of
        Left err -> putStrLn ("Error: " ++ show err) >> repl
        Right val -> print val >> repl
```

**Verification**: Test REPL with various inputs including valid expressions, parse errors, evaluation errors, and exit command.

## Summary

All technical unknowns have been resolved with patterns based on Haskell best practices and functional programming principles. The decisions prioritize:

1. **Correctness**: Lexical scoping, closure semantics, proper error handling
2. **Simplicity**: Standard patterns, minimal complexity, clear structure
3. **Testability**: Property-based testing, clear interfaces, composable components
4. **Developer Experience**: Clear error messages, interactive REPL, good documentation

**Key Implementation Patterns**:
- Megaparsec for parsing with recursive structure
- ReaderT monad transformer for environment threading
- Immutable environments with Map for variable storage
- Closures that capture lexical environment
- QuickCheck for property-based testing
- Either Error for structured error handling
- Simple line-based REPL

**Next Steps**: 
- Implement modules following these patterns
- Write property-based tests for evaluation laws
- Create example programs demonstrating language capabilities
- Verify all success criteria are met

