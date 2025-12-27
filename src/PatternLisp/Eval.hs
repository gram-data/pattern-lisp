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
-- > import PatternLisp.Parser
-- > import PatternLisp.Eval
-- > import PatternLisp.Primitives
-- >
-- > case parseExpr "(+ 1 2)" of
-- >   Left err -> print err
-- >   Right expr -> case evalExpr expr initialEnv of
-- >     Left err -> print err
-- >     Right val -> print val
module PatternLisp.Eval
  ( evalExpr
  , evalExprWithEnv
  ) where

import PatternLisp.Syntax
import PatternLisp.PatternPrimitives
import Pattern (Pattern)
import qualified Pattern.Core as PatternCore
import Subject.Core (Subject)
import qualified Data.Map as Map
import qualified Data.Set as Set
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
eval (SetLiteral exprs) = do
  vals <- mapM eval exprs
  return $ VSet (Set.fromList vals)  -- Remove duplicates automatically
eval (MapLiteral pairs) = do
  -- Pairs is a list of alternating [key, value, key, value, ...]
  -- We need to process them in pairs and handle duplicate keys (last wins)
  let processPairs :: [Expr] -> EvalM (Map.Map KeywordKey Value)
      processPairs [] = return Map.empty
      processPairs (k:v:rest) = do
        keyVal <- eval k
        valVal <- eval v
        restMap <- processPairs rest
        case keyVal of
          VKeyword name -> return $ Map.insert (KeywordKey name) valVal restMap
          _ -> throwError $ TypeMismatch ("Map keys must be keywords, got: " ++ show keyVal) keyVal
      processPairs _ = throwError $ ParseError "Map literal must have even number of elements (key-value pairs)"
  m <- processPairs pairs
  return $ VMap m
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
evalAtom (Keyword name) = return $ VKeyword name  -- Keywords are self-evaluating, no environment lookup
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
  [x, y] -> return $ VBool (x == y)  -- Use Eq instance for Value (handles all types including keywords)
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
applyPrimitive PatternCreate args = case args of
  [val] -> evalPatternCreate val
  _ -> throwError $ ArityMismatch "pattern" 1 (length args)
applyPrimitive PatternWith args = case args of
  [decoration, VList elements] -> evalPatternWith decoration elements
  [_, _] -> throwError $ TypeMismatch "pattern-with expects list of elements as second argument" (VList [])
  _ -> throwError $ ArityMismatch "pattern-with" 2 (length args)
-- Pattern query primitives
applyPrimitive PatternValue args = case args of
  [VPattern pat] -> evalPatternValue pat
  [v] -> throwError $ TypeMismatch ("pattern-value expects pattern, but got: " ++ show v) v
  _ -> throwError $ ArityMismatch "pattern-value" 1 (length args)
applyPrimitive PatternElements args = case args of
  [VPattern pat] -> evalPatternElements pat
  [v] -> throwError $ TypeMismatch ("pattern-elements expects pattern, but got: " ++ show v) v
  _ -> throwError $ ArityMismatch "pattern-elements" 1 (length args)
applyPrimitive PatternLength args = case args of
  [VPattern pat] -> evalPatternLength pat
  [v] -> throwError $ TypeMismatch ("pattern-length expects pattern, but got: " ++ show v) v
  _ -> throwError $ ArityMismatch "pattern-length" 1 (length args)
applyPrimitive PatternSize args = case args of
  [VPattern pat] -> evalPatternSize pat
  [v] -> throwError $ TypeMismatch ("pattern-size expects pattern, but got: " ++ show v) v
  _ -> throwError $ ArityMismatch "pattern-size" 1 (length args)
applyPrimitive PatternDepth args = case args of
  [VPattern pat] -> evalPatternDepth pat
  [v] -> throwError $ TypeMismatch ("pattern-depth expects pattern, but got: " ++ show v) v
  _ -> throwError $ ArityMismatch "pattern-depth" 1 (length args)
applyPrimitive PatternValues args = case args of
  [VPattern pat] -> evalPatternValues pat
  [v] -> throwError $ TypeMismatch ("pattern-values expects pattern, but got: " ++ show v) v
  _ -> throwError $ ArityMismatch "pattern-values" 1 (length args)
-- Pattern predicate primitives
applyPrimitive PatternFind args = case args of
  [VPattern pat, VClosure _] -> evalPatternFind pat (args !! 1)
  [VPattern _, predVal] -> throwError $ TypeMismatch ("pattern-find expects closure as predicate, but got: " ++ show predVal) predVal
  [v, _] -> throwError $ TypeMismatch ("pattern-find expects pattern as first argument, but got: " ++ show v) v
  _ -> throwError $ ArityMismatch "pattern-find" 2 (length args)
applyPrimitive PatternAny args = case args of
  [VPattern pat, VClosure _] -> evalPatternAny pat (args !! 1)
  [VPattern _, predVal] -> throwError $ TypeMismatch ("pattern-any? expects closure as predicate, but got: " ++ show predVal) predVal
  [v, _] -> throwError $ TypeMismatch ("pattern-any? expects pattern as first argument, but got: " ++ show v) v
  _ -> throwError $ ArityMismatch "pattern-any?" 2 (length args)
applyPrimitive PatternAll args = case args of
  [VPattern pat, VClosure _] -> evalPatternAll pat (args !! 1)
  [VPattern _, predVal] -> throwError $ TypeMismatch ("pattern-all? expects closure as predicate, but got: " ++ show predVal) predVal
  [v, _] -> throwError $ TypeMismatch ("pattern-all? expects pattern as first argument, but got: " ++ show v) v
  _ -> throwError $ ArityMismatch "pattern-all?" 2 (length args)
-- Pattern conversion primitives
applyPrimitive ValueToPattern args = case args of
  [val] -> evalValueToPattern val
  _ -> throwError $ ArityMismatch "value-to-pattern" 1 (length args)
applyPrimitive PatternToValue args = case args of
  [VPattern pat] -> evalPatternToValue pat
  [v] -> throwError $ TypeMismatch ("pattern-to-value expects pattern, but got: " ++ show v) v
  _ -> throwError $ ArityMismatch "pattern-to-value" 1 (length args)
-- Set operation primitives
applyPrimitive SetContains args = case args of
  [VSet s, val] -> return $ VBool (Set.member val s)
  [VMap m, VKeyword key] -> return $ VBool (Map.member (KeywordKey key) m)  -- Also handle maps
  [VMap _, v] -> throwError $ TypeMismatch ("contains? expects keyword as second argument for maps, but got: " ++ show v) v
  [v, _] -> throwError $ TypeMismatch ("contains? expects set or map as first argument, but got: " ++ show v) v
  _ -> throwError $ ArityMismatch "contains?" 2 (length args)
applyPrimitive SetUnion args = case args of
  [VSet s1, VSet s2] -> return $ VSet (Set.union s1 s2)
  [VSet _, v] -> throwError $ TypeMismatch ("set-union expects set as second argument, but got: " ++ show v) v
  [v, _] -> throwError $ TypeMismatch ("set-union expects set as first argument, but got: " ++ show v) v
  _ -> throwError $ ArityMismatch "set-union" 2 (length args)
applyPrimitive SetIntersection args = case args of
  [VSet s1, VSet s2] -> return $ VSet (Set.intersection s1 s2)
  [VSet _, v] -> throwError $ TypeMismatch ("set-intersection expects set as second argument, but got: " ++ show v) v
  [v, _] -> throwError $ TypeMismatch ("set-intersection expects set as first argument, but got: " ++ show v) v
  _ -> throwError $ ArityMismatch "set-intersection" 2 (length args)
applyPrimitive SetDifference args = case args of
  [VSet s1, VSet s2] -> return $ VSet (Set.difference s1 s2)
  [VSet _, v] -> throwError $ TypeMismatch ("set-difference expects set as second argument, but got: " ++ show v) v
  [v, _] -> throwError $ TypeMismatch ("set-difference expects set as first argument, but got: " ++ show v) v
  _ -> throwError $ ArityMismatch "set-difference" 2 (length args)
applyPrimitive SetSymmetricDifference args = case args of
  [VSet s1, VSet s2] -> return $ VSet (Set.union (Set.difference s1 s2) (Set.difference s2 s1))
  [VSet _, v] -> throwError $ TypeMismatch ("set-symmetric-difference expects set as second argument, but got: " ++ show v) v
  [v, _] -> throwError $ TypeMismatch ("set-symmetric-difference expects set as first argument, but got: " ++ show v) v
  _ -> throwError $ ArityMismatch "set-symmetric-difference" 2 (length args)
applyPrimitive SetSubset args = case args of
  [VSet s1, VSet s2] -> return $ VBool (Set.isSubsetOf s1 s2)
  [VSet _, v] -> throwError $ TypeMismatch ("set-subset? expects set as second argument, but got: " ++ show v) v
  [v, _] -> throwError $ TypeMismatch ("set-subset? expects set as first argument, but got: " ++ show v) v
  _ -> throwError $ ArityMismatch "set-subset?" 2 (length args)
applyPrimitive SetEqual args = case args of
  [VSet s1, VSet s2] -> return $ VBool (s1 == s2)
  [VSet _, v] -> throwError $ TypeMismatch ("set-equal? expects set as second argument, but got: " ++ show v) v
  [v, _] -> throwError $ TypeMismatch ("set-equal? expects set as first argument, but got: " ++ show v) v
  _ -> throwError $ ArityMismatch "set-equal?" 2 (length args)
applyPrimitive SetEmpty args = case args of
  [VSet s] -> return $ VBool (Set.null s)
  [VMap m] -> return $ VBool (Map.null m)  -- Also handle maps
  [v] -> throwError $ TypeMismatch ("empty? expects set or map, but got: " ++ show v) v
  _ -> throwError $ ArityMismatch "empty?" 1 (length args)
applyPrimitive HashSet args = return $ VSet (Set.fromList args)
-- Map operation primitives
applyPrimitive MapGet args = case args of
  [VMap m, VKeyword key] -> return $ case Map.lookup (KeywordKey key) m of
    Just val -> val
    Nothing -> VList []  -- Return empty list as nil
  [VMap m, VKeyword key, defaultVal] -> return $ Map.findWithDefault defaultVal (KeywordKey key) m
  [VMap _, v, _] -> throwError $ TypeMismatch ("get expects keyword as second argument, but got: " ++ show v) v
  [VMap _, v] -> throwError $ TypeMismatch ("get expects keyword as second argument, but got: " ++ show v) v
  [v, _] -> throwError $ TypeMismatch ("get expects map as first argument, but got: " ++ show v) v
  _ -> throwError $ ArityMismatch "get" 2 (length args)
applyPrimitive MapGetIn args = case args of
  [VMap m, VList keys] -> do
    -- keys is a list of keywords: [key1, key2, ...]
    let getInPath :: Map.Map KeywordKey Value -> [Value] -> EvalM Value
        getInPath _ [] = return $ VList []  -- Return nil if path exhausted
        getInPath currentMap (VKeyword key:rest) = do
          case Map.lookup (KeywordKey key) currentMap of
            Just (VMap nestedMap) -> getInPath nestedMap rest
            Just val | null rest -> return val
            Just _ -> return $ VList []  -- Path doesn't lead to map, return nil
            Nothing -> return $ VList []  -- Key not found, return nil
        getInPath _ (v:_) = throwError $ TypeMismatch ("get-in path must contain keywords, got: " ++ show v) v
    getInPath m keys
  [VMap _, v] -> throwError $ TypeMismatch ("get-in expects list of keywords as second argument, but got: " ++ show v) v
  [v, _] -> throwError $ TypeMismatch ("get-in expects map as first argument, but got: " ++ show v) v
  _ -> throwError $ ArityMismatch "get-in" 2 (length args)
applyPrimitive MapAssoc args = case args of
  [VMap m, VKeyword key, val] -> return $ VMap (Map.insert (KeywordKey key) val m)
  [VMap _, v, _] -> throwError $ TypeMismatch ("assoc expects keyword as second argument, but got: " ++ show v) v
  [v, _, _] -> throwError $ TypeMismatch ("assoc expects map as first argument, but got: " ++ show v) v
  _ -> throwError $ ArityMismatch "assoc" 3 (length args)
applyPrimitive MapDissoc args = case args of
  [VMap m, VKeyword key] -> return $ VMap (Map.delete (KeywordKey key) m)
  [VMap _, v] -> throwError $ TypeMismatch ("dissoc expects keyword as second argument, but got: " ++ show v) v
  [v, _] -> throwError $ TypeMismatch ("dissoc expects map as first argument, but got: " ++ show v) v
  _ -> throwError $ ArityMismatch "dissoc" 2 (length args)
applyPrimitive MapUpdate args = case args of
  [VMap m, VKeyword key, VClosure closure] -> do
    -- Get current value or nil (empty list)
    let currentVal = Map.findWithDefault (VList []) (KeywordKey key) m
    -- Apply function to current value
    updatedVal <- applyClosure closure [currentVal]
    return $ VMap (Map.insert (KeywordKey key) updatedVal m)
  [VMap _, VKeyword _, v] -> throwError $ TypeMismatch ("update expects closure as third argument, but got: " ++ show v) v
  [VMap _, v, _] -> throwError $ TypeMismatch ("update expects keyword as second argument, but got: " ++ show v) v
  [v, _, _] -> throwError $ TypeMismatch ("update expects map as first argument, but got: " ++ show v) v
  _ -> throwError $ ArityMismatch "update" 3 (length args)
applyPrimitive MapContains args = case args of
  [VMap m, VKeyword key] -> return $ VBool (Map.member (KeywordKey key) m)
  [VMap _, v] -> throwError $ TypeMismatch ("contains? expects keyword as second argument, but got: " ++ show v) v
  [v, _] -> throwError $ TypeMismatch ("contains? expects map as first argument, but got: " ++ show v) v
  _ -> throwError $ ArityMismatch "contains?" 2 (length args)
applyPrimitive MapEmpty args = case args of
  [VMap m] -> return $ VBool (Map.null m)
  [v] -> throwError $ TypeMismatch ("empty? expects map, but got: " ++ show v) v
  _ -> throwError $ ArityMismatch "empty?" 1 (length args)
applyPrimitive HashMap args
  | even (length args) = do
      -- Process alternating keyword-value pairs
      let processPairs :: [Value] -> EvalM (Map.Map KeywordKey Value)
          processPairs [] = return Map.empty
          processPairs (VKeyword key:val:rest) = do
            restMap <- processPairs rest
            return $ Map.insert (KeywordKey key) val restMap
          processPairs (v:_) = throwError $ TypeMismatch ("hash-map keys must be keywords, got: " ++ show v) v
      m <- processPairs args
      return $ VMap m
  | otherwise = throwError $ ParseError "hash-map requires even number of arguments (key-value pairs)"

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

-- | Pattern predicate primitives (implemented in Eval to avoid circular dependency)
-- These functions evaluate predicate closures on patterns

-- | Finds the first subpattern that matches a predicate closure
evalPatternFind :: Pattern Subject -> Value -> EvalM Value
evalPatternFind pat predVal = do
  case predVal of
    VClosure closure -> do
      matches <- applyPredicate closure pat
      if matches
        then return $ VPattern pat
        else findInElements (PatternCore.elements pat) closure
    _ -> throwError $ TypeMismatch 
           "pattern-find expects closure as predicate" predVal
  where
    applyPredicate :: Closure -> Pattern Subject -> EvalM Bool
    applyPredicate closure p = do
      let patternVal = VPattern p
          (Closure paramNames bodyExpr capturedEnv) = closure
      case paramNames of
        [paramName] -> do
          let bindings = Map.fromList [(paramName, patternVal)]
              extendedEnv = Map.union bindings capturedEnv
          result <- local (const extendedEnv) (eval bodyExpr)
          case result of
            VBool b -> return b
            _ -> throwError $ TypeMismatch 
                   "predicate must return boolean" result
        _ -> throwError $ ArityMismatch "predicate" 1 (length paramNames)
    
    findInElements :: [Pattern Subject] -> Closure -> EvalM Value
    findInElements [] _ = return $ VList []
    findInElements (p:ps) closure = do
      matches <- applyPredicate closure p
      if matches
        then return $ VPattern p
        else do
          nestedResult <- findInElements (PatternCore.elements p) closure
          case nestedResult of
            VPattern _ -> return nestedResult
            _ -> findInElements ps closure

-- | Checks if any subpattern matches a predicate closure
evalPatternAny :: Pattern Subject -> Value -> EvalM Value
evalPatternAny pat predVal = do
  case predVal of
    VClosure closure -> do
      matches <- applyPredicate closure pat
      if matches
        then return $ VBool True
        else anyInElements (PatternCore.elements pat) closure
    _ -> throwError $ TypeMismatch 
           "pattern-any? expects closure as predicate" predVal
  where
    applyPredicate :: Closure -> Pattern Subject -> EvalM Bool
    applyPredicate closure p = do
      let patternVal = VPattern p
          (Closure paramNames bodyExpr capturedEnv) = closure
      case paramNames of
        [paramName] -> do
          let bindings = Map.fromList [(paramName, patternVal)]
              extendedEnv = Map.union bindings capturedEnv
          result <- local (const extendedEnv) (eval bodyExpr)
          case result of
            VBool b -> return b
            _ -> throwError $ TypeMismatch 
                   "predicate must return boolean" result
        _ -> throwError $ ArityMismatch "predicate" 1 (length paramNames)
    
    anyInElements :: [Pattern Subject] -> Closure -> EvalM Value
    anyInElements [] _ = return $ VBool False
    anyInElements (p:ps) closure = do
      matches <- applyPredicate closure p
      if matches
        then return $ VBool True
        else do
          nestedResult <- anyInElements (PatternCore.elements p) closure
          case nestedResult of
            VBool True -> return $ VBool True
            _ -> anyInElements ps closure

-- | Checks if all subpatterns match a predicate closure
evalPatternAll :: Pattern Subject -> Value -> EvalM Value
evalPatternAll pat predVal = do
  case predVal of
    VClosure closure -> do
      matches <- applyPredicate closure pat
      if not matches
        then return $ VBool False
        else allInElements (PatternCore.elements pat) closure
    _ -> throwError $ TypeMismatch 
           "pattern-all? expects closure as predicate" predVal
  where
    applyPredicate :: Closure -> Pattern Subject -> EvalM Bool
    applyPredicate closure p = do
      let patternVal = VPattern p
          (Closure paramNames bodyExpr capturedEnv) = closure
      case paramNames of
        [paramName] -> do
          let bindings = Map.fromList [(paramName, patternVal)]
              extendedEnv = Map.union bindings capturedEnv
          result <- local (const extendedEnv) (eval bodyExpr)
          case result of
            VBool b -> return b
            _ -> throwError $ TypeMismatch 
                   "predicate must return boolean" result
        _ -> throwError $ ArityMismatch "predicate" 1 (length paramNames)
    
    allInElements :: [Pattern Subject] -> Closure -> EvalM Value
    allInElements [] _ = return $ VBool True
    allInElements (p:ps) closure = do
      matches <- applyPredicate closure p
      if not matches
        then return $ VBool False
        else do
          nestedResult <- allInElements (PatternCore.elements p) closure
          case nestedResult of
            VBool False -> return $ VBool False
            _ -> allInElements ps closure

-- | Evaluate a quoted expression (convert Expr to Value)
evalQuote :: Expr -> EvalM Value
evalQuote expr = exprToValue expr

-- | Convert an Expr to a Value (for quote evaluation)
exprToValue :: Expr -> EvalM Value
exprToValue (Atom (Number n)) = return $ VNumber n
exprToValue (Atom (String s)) = return $ VString s
exprToValue (Atom (Bool b)) = return $ VBool b
exprToValue (Atom (Keyword name)) = return $ VKeyword name
exprToValue (Atom (Symbol name)) = return $ VString (T.pack name)
exprToValue (List exprs) = do
  vals <- mapM exprToValue exprs
  return $ VList vals
exprToValue (SetLiteral exprs) = do
  vals <- mapM exprToValue exprs
  return $ VSet (Set.fromList vals)
exprToValue (MapLiteral pairs) = do
  -- Process pairs: [key, value, key, value, ...]
  let processPairs [] = return Map.empty
      processPairs (k:v:rest) = do
        keyVal <- exprToValue k
        valVal <- exprToValue v
        restMap <- processPairs rest
        case keyVal of
          VKeyword name -> return $ Map.insert (KeywordKey name) valVal restMap
          _ -> throwError $ TypeMismatch ("Map keys must be keywords, got: " ++ show keyVal) keyVal
      processPairs _ = throwError $ ParseError "Map literal must have even number of elements (key-value pairs)"
  m <- processPairs pairs
  return $ VMap m
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

