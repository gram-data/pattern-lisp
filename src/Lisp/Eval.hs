-- | Core Lisp evaluator implementing environment-based evaluation with lexical scoping.
--
-- This module provides the main evaluation functions for Pattern Lisp expressions.
-- Evaluation uses a Reader monad transformer to thread the environment through
-- computations, and an Except monad for error handling.
--
-- Key features:
-- * Environment-based evaluation with lexical scoping
-- * Support for closures that capture their lexical environment
-- * Special form handling (lambda, if, let, quote, begin, define)
-- * Primitive function application with type and arity checking
--
-- Example usage:
--
-- > import Lisp.Parser
-- > import Lisp.Eval
-- > import Lisp.Primitives
-- >
-- > case parseExpr "(+ 1 2)" of
-- >   Left err -> print err
-- >   Right expr -> case evalExpr expr initialEnv of
-- >     Left err -> print err
-- >     Right val -> print val
module Lisp.Eval
  ( evalExpr
  , evalExprWithEnv
  ) where

import Lisp.Syntax
import qualified Data.Map as Map
import Control.Monad.Reader
import Control.Monad.Except
import qualified Data.Text as T

-- | Evaluation monad: ReaderT for environment, Except for errors
-- Note: For define, we need to track environment changes, so we use a custom approach
type EvalM = ReaderT Env (Except Error)

-- | Result of evaluation that may include environment updates
data EvalResult = EvalResult
  { evalValue :: Value
  , evalEnv :: Env
  }

-- | Evaluate an expression in an environment, returning the value.
--
-- This function evaluates a single expression and returns its value.
-- The environment is not modified (use 'evalExprWithEnv' if you need
-- environment updates from 'define' or 'begin').
--
-- @since 0.1.0.0
evalExpr :: Expr -> Env -> Either Error Value
evalExpr expr environment = runExcept $ runReaderT (eval expr) environment

-- | Evaluate an expression in an environment, returning both the value and updated environment.
--
-- This function is similar to 'evalExpr', but also returns the updated environment.
-- This is necessary for handling 'define' and 'begin' forms that modify the environment
-- for subsequent evaluations.
--
-- @since 0.1.0.0
evalExprWithEnv :: Expr -> Env -> Either Error (Value, Env)
evalExprWithEnv expr environment = runExcept $ do
  result <- runReaderT (evalWithEnv expr) environment
  return (evalValue result, evalEnv result)

-- | Evaluate expression and return updated environment (for define and begin)
evalWithEnv :: Expr -> EvalM EvalResult
evalWithEnv expr = do
  currentEnv <- ask
  -- Check if this is a define expression
  case expr of
    List (Atom (Symbol "define"):Atom (Symbol name):valueExpr:[]) -> do
      val <- eval valueExpr
      let newEnv = Map.insert name val currentEnv
      local (const newEnv) $ do
        return $ EvalResult (VString (T.pack name)) newEnv
    List (Atom (Symbol "begin"):exprs) -> do
      -- Evaluate all expressions in begin, threading environment through
      evalBeginWithEnv exprs currentEnv
    _ -> do
      val <- eval expr
      -- Environment unchanged for non-define/non-begin expressions
      return $ EvalResult val currentEnv

-- | Main evaluation function
eval :: Expr -> EvalM Value
eval (Atom atom) = evalAtom atom
eval (List []) = return $ VList []
eval (List (Atom (Symbol "lambda"):rest)) = evalLambda rest
eval (List (Atom (Symbol "if"):rest)) = evalIf rest
eval (List (Atom (Symbol "let"):rest)) = evalLet rest
eval (List (Atom (Symbol "quote"):rest)) = evalQuoteForm rest
eval (List (Atom (Symbol "begin"):rest)) = evalBegin rest
eval (List (Atom (Symbol "define"):rest)) = evalDefine rest
eval (List (func:args)) = do
  funcVal <- eval func
  argVals <- mapM eval args
  applyFunction funcVal argVals
eval (Quote expr) = evalQuote expr

-- | Evaluate an atom (self-evaluating)
evalAtom :: Atom -> EvalM Value
evalAtom (Number n) = return $ VNumber n
evalAtom (String s) = return $ VString s
evalAtom (Bool b) = return $ VBool b
evalAtom (Symbol name) = do
  currentEnv <- ask
  case Map.lookup name currentEnv of
    Just val -> return val
    Nothing -> throwError $ UndefinedVar name (Atom (Symbol name))
      -- Error message includes variable name and expression context

-- | Apply a function value to arguments
applyFunction :: Value -> [Value] -> EvalM Value
applyFunction (VPrimitive prim) args = applyPrimitive prim args
applyFunction (VClosure closure) args = applyClosure closure args
applyFunction val _ = throwError $ TypeMismatch 
  ("Cannot apply non-function value: " ++ show val) val

-- | Apply a primitive function
applyPrimitive :: Primitive -> [Value] -> EvalM Value
applyPrimitive Add args
  | length args < 2 = throwError $ ArityMismatch "+" 2 (length args)
  | otherwise = do
      nums <- mapM expectNumber args
      return $ VNumber $ sum nums
applyPrimitive Sub args = case args of
  [] -> throwError $ ArityMismatch "-" 1 0
  [x] -> do
    n <- expectNumber x
    return $ VNumber (-n)
  (x:xs) -> do
    n <- expectNumber x
    ns <- mapM expectNumber xs
    return $ VNumber $ n - sum ns
applyPrimitive Mul args = do
  nums <- mapM expectNumber args
  return $ VNumber $ product nums
applyPrimitive Div args = case args of
  [] -> throwError $ ArityMismatch "/" 1 0
  [x] -> do
    n <- expectNumber x
    if n == 0
      then throwError $ DivisionByZero (Atom (Number 1))
        -- Error: "Division by zero in unary division: (/ " ++ show x ++ ")"
      else return $ VNumber (1 `div` n)
  (x:xs) -> do
    n <- expectNumber x
    ns <- mapM expectNumber xs
    if any (== 0) ns
      then throwError $ DivisionByZero (Atom (Number 1))
        -- Error: "Division by zero in division: (/ " ++ show x ++ " " ++ unwords (map show xs) ++ ")"
      else return $ VNumber $ foldl div n ns
applyPrimitive Gt args = case args of
  [x, y] -> do
    nx <- expectNumber x
    ny <- expectNumber y
    return $ VBool (nx > ny)
  _ -> throwError $ ArityMismatch ">" 2 (length args)
applyPrimitive Lt args = case args of
  [x, y] -> do
    nx <- expectNumber x
    ny <- expectNumber y
    return $ VBool (nx < ny)
  _ -> throwError $ ArityMismatch "<" 2 (length args)
applyPrimitive Eq args = case args of
  [x, y] -> do
    nx <- expectNumber x
    ny <- expectNumber y
    return $ VBool (nx == ny)
  _ -> throwError $ ArityMismatch "=" 2 (length args)
applyPrimitive Ne args = case args of
  [x, y] -> do
    nx <- expectNumber x
    ny <- expectNumber y
    return $ VBool (nx /= ny)
  _ -> throwError $ ArityMismatch "/=" 2 (length args)
applyPrimitive StringAppend args = do
  strs <- mapM expectString args
  return $ VString $ T.concat strs
applyPrimitive StringLength args = case args of
  [s] -> do
    str <- expectString s
    return $ VNumber $ fromIntegral $ T.length str
  _ -> throwError $ ArityMismatch "string-length" 1 (length args)
applyPrimitive Substring args = case args of
  [s, start, end] -> do
    str <- expectString s
    startNum <- expectNumber start
    endNum <- expectNumber end
    let startIdx = fromIntegral startNum
        endIdx = fromIntegral endNum
    if startIdx < 0 || endIdx > T.length str || startIdx > endIdx
      then throwError $ TypeMismatch "Invalid substring indices" (VList [])
      else return $ VString $ T.take (endIdx - startIdx) $ T.drop startIdx str
  _ -> throwError $ ArityMismatch "substring" 3 (length args)

-- | Apply a closure (extend captured environment with arguments)
applyClosure :: Closure -> [Value] -> EvalM Value
applyClosure (Closure paramNames bodyExpr capturedEnv) args = do
  if length paramNames /= length args
    then throwError $ ArityMismatch 
      ("lambda with " ++ show (length paramNames) ++ " parameter(s)")
      (length paramNames) 
      (length args)
      -- Error includes parameter count for clarity
    else do
      let bindings = Map.fromList $ zip paramNames args
          extendedEnv = Map.union bindings capturedEnv
      local (const extendedEnv) (eval bodyExpr)

-- | Evaluate a quoted expression (convert Expr to Value)
evalQuote :: Expr -> EvalM Value
evalQuote expr = exprToValue expr

-- | Convert an Expr to a Value (for quote evaluation)
exprToValue :: Expr -> EvalM Value
exprToValue (Atom (Number n)) = return $ VNumber n
exprToValue (Atom (String s)) = return $ VString s
exprToValue (Atom (Bool b)) = return $ VBool b
exprToValue (Atom (Symbol name)) = return $ VString (T.pack name)
exprToValue (List exprs) = do
  vals <- mapM exprToValue exprs
  return $ VList vals
exprToValue (Quote expr) = exprToValue expr

-- | Evaluate lambda form: (lambda (params...) body)
evalLambda :: [Expr] -> EvalM Value
evalLambda [List paramExprs, bodyExpr] = do
  paramNames <- mapM extractSymbol paramExprs
  currentEnv <- ask
  return $ VClosure (Closure paramNames bodyExpr currentEnv)
evalLambda args = throwError $ ParseError 
  ("lambda requires parameter list and body (2 arguments), but got " ++ 
   show (length args) ++ " argument(s)")

-- | Extract symbol name from Atom (Symbol ...)
extractSymbol :: Expr -> EvalM String
extractSymbol (Atom (Symbol name)) = return name
extractSymbol expr = throwError $ ParseError 
  ("lambda parameters must be symbols, but got: " ++ show expr)

-- | Evaluate if form: (if condition then else)
evalIf :: [Expr] -> EvalM Value
evalIf [condition, thenExpr, elseExpr] = do
  condVal <- eval condition
  case condVal of
    VBool True -> eval thenExpr
    VBool False -> eval elseExpr
    _ -> throwError $ TypeMismatch 
      ("if condition must be boolean, but got: " ++ show condVal) condVal
evalIf args = throwError $ ParseError 
  ("if requires exactly 3 arguments (condition then else), but got " ++ 
   show (length args) ++ " argument(s)")

-- | Evaluate let form: (let ((var val)...) body)
evalLet :: [Expr] -> EvalM Value
evalLet [List bindings, bodyExpr] = do
  currentEnv <- ask
  newBindings <- mapM evalBinding bindings
  let extendedEnv = Map.union (Map.fromList newBindings) currentEnv
  local (const extendedEnv) (eval bodyExpr)
evalLet args = throwError $ ParseError 
  ("let requires bindings list and body (2 arguments), but got " ++ 
   show (length args) ++ " argument(s)")

-- | Evaluate a single binding: (var val)
evalBinding :: Expr -> EvalM (String, Value)
evalBinding (List [Atom (Symbol name), valExpr]) = do
  val <- eval valExpr
  return (name, val)
evalBinding expr = throwError $ ParseError 
  ("let binding must be (name value), but got: " ++ show expr)

-- | Evaluate quote form: (quote expr)
evalQuoteForm :: [Expr] -> EvalM Value
evalQuoteForm [expr] = evalQuote expr
evalQuoteForm args = throwError $ ParseError 
  ("quote requires exactly one expression, but got " ++ show (length args) ++ " argument(s)")

-- | Evaluate begin form with environment tracking
evalBeginWithEnv :: [Expr] -> Env -> EvalM EvalResult
evalBeginWithEnv [] _ = throwError $ ParseError "begin requires at least one expression"
evalBeginWithEnv [expr] currentEnv = do
  local (const currentEnv) $ evalWithEnv expr
evalBeginWithEnv (expr:rest) currentEnv = do
  -- Evaluate first expression
  result <- local (const currentEnv) $ evalWithEnv expr
  -- Continue with remaining expressions using updated environment
  finalResult <- evalBeginWithEnv rest (evalEnv result)
  return finalResult

-- | Evaluate begin form: (begin expr1 expr2 ...)
-- Note: Each expression may modify the environment (via define), so we thread it through
evalBegin :: [Expr] -> EvalM Value
evalBegin [] = throwError $ ParseError "begin requires at least one expression"
evalBegin [expr] = eval expr
evalBegin (expr:rest) = do
  -- Evaluate first expression, which may modify environment
  result <- evalWithEnv expr
  -- Update environment for remaining expressions
  local (const (evalEnv result)) $
    -- Continue with remaining expressions
    evalBegin rest

-- | Evaluate define form: (define name value)
-- Note: This modifies the environment using local, which affects subsequent evaluations
evalDefine :: [Expr] -> EvalM Value
evalDefine [Atom (Symbol name), valueExpr] = do
  val <- eval valueExpr
  currentEnv <- ask
  let newEnv = Map.insert name val currentEnv
  -- Update environment for subsequent expressions
  local (const newEnv) $ do
    -- Return the name as a string value
    return $ VString (T.pack name)
evalDefine args = throwError $ ParseError 
  ("define requires name and value (2 arguments), but got " ++ 
   show (length args) ++ " argument(s)")

-- | Helper: expect a number value, providing context in error message
expectNumber :: Value -> EvalM Integer
expectNumber (VNumber n) = return n
expectNumber v = throwError $ TypeMismatch 
  ("Expected number, but got: " ++ show v) v

-- | Helper: expect a string value, providing context in error message
expectString :: Value -> EvalM T.Text
expectString (VString s) = return s
expectString v = throwError $ TypeMismatch 
  ("Expected string, but got: " ++ show v) v

