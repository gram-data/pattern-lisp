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
import Data.Maybe (catMaybes)
import Control.Monad.Reader
import Control.Monad.Except

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
        return $ EvalResult (VString name) newEnv
    List (Atom (Symbol "begin"):exprs) -> do
      -- Evaluate all expressions in begin, threading environment through
      evalBeginWithEnv exprs currentEnv
    _ -> do
      val <- eval expr
      -- Environment unchanged for non-define/non-begin expressions
      return $ EvalResult val currentEnv

-- | Main evaluation function
eval :: Expr -> EvalM Value

-- | Convert a Value to a String key for record operations.
--
-- Accepts keywords and strings as valid keys.
-- Returns an error for other value types.
--
-- Examples:
--
-- > valueToStringKey (VKeyword "name")  -- Right "name"
-- > valueToStringKey (VString "age")    -- Right "age"
-- > valueToStringKey (VInteger 42)       -- Left (TypeMismatch ...)
valueToStringKey :: Value -> Either Error String
valueToStringKey (VKeyword name) = Right name
valueToStringKey (VString s) = Right s
valueToStringKey v = Left $ TypeMismatch ("Record keys must be keywords or strings, got: " ++ show v) v

eval (Atom atom) = evalAtom atom
eval (ArrayLiteral exprs) = do
  vals <- mapM eval exprs
  return $ VArray vals
eval (SetLiteral exprs) = do
  vals <- mapM eval exprs
  return $ VSet (Set.fromList vals)  -- Remove duplicates automatically
-- | Evaluate a record literal to a VMap value.
--
-- Processes key-value pairs from left to right, with duplicate keys
-- handled by last-wins semantics.
--
-- Supports quasiquotation:
-- * @Unquote expr@: Evaluates expression and uses as value
-- * @UnquoteSplice expr@: Evaluates to record and merges entries (key is ignored)
--
-- Returns @VMap (Map String Value)@ representing the record.
eval (RecordLiteral pairs) = do
  let processPairs :: Map.Map String Value -> [(String, Expr)] -> EvalM (Map.Map String Value)
      processPairs acc [] = return acc
      processPairs acc ((keyStr, valExpr):rest) = do
        case valExpr of
          UnquoteSplice expr -> do
            -- Splice: evaluate to record and merge its entries (ignore keyStr)
            recordVal <- eval expr
            case recordVal of
              VMap spliceMap -> processPairs (Map.union spliceMap acc) rest  -- Merge entries
              _ -> throwError $ TypeMismatch ("record splice expects record, but got: " ++ show recordVal) recordVal
          Unquote expr -> do
            -- Unquote: evaluate expression and use as value
            valVal <- eval expr
            processPairs (Map.insert keyStr valVal acc) rest
          _ -> do
            -- Normal evaluation
            valVal <- eval valExpr
            processPairs (Map.insert keyStr valVal acc) rest
  m <- processPairs Map.empty pairs
  return $ VMap m
eval (List []) = return $ VArray []
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
eval (Unquote _) = throwError $ ParseError "Unquote (`,expr) can only appear inside quasiquoted expressions"
eval (UnquoteSplice _) = throwError $ ParseError "Unquote-splice (`,@expr) can only appear inside quasiquoted expressions"

-- | Evaluate an atom (self-evaluating)
evalAtom :: Atom -> EvalM Value
evalAtom (Number n) = return $ VInteger n
evalAtom (String s) = return $ VString s
evalAtom (Bool b) = return $ VBoolean b
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
      return $ VInteger $ sum nums
applyPrimitive Sub args = case args of
  [] -> throwError $ ArityMismatch "-" 1 0
  [x] -> do
    n <- expectNumber x
    return $ VInteger (-n)
  (x:xs) -> do
    n <- expectNumber x
    ns <- mapM expectNumber xs
    return $ VInteger $ n - sum ns
applyPrimitive Mul args = do
  nums <- mapM expectNumber args
  return $ VInteger $ product nums
applyPrimitive Div args = case args of
  [] -> throwError $ ArityMismatch "/" 1 0
  [x] -> do
    n <- expectNumber x
    if n == 0
      then throwError $ DivisionByZero (Atom (Number 1))
        -- Error: "Division by zero in unary division: (/ " ++ show x ++ ")"
      else return $ VInteger (1 `div` n)
  (x:xs) -> do
    n <- expectNumber x
    ns <- mapM expectNumber xs
    if any (== 0) ns
      then throwError $ DivisionByZero (Atom (Number 1))
        -- Error: "Division by zero in division: (/ " ++ show x ++ " " ++ unwords (map show xs) ++ ")"
      else return $ VInteger $ foldl div n ns
applyPrimitive Gt args = case args of
  [x, y] -> do
    nx <- expectNumber x
    ny <- expectNumber y
    return $ VBoolean (nx > ny)
  _ -> throwError $ ArityMismatch ">" 2 (length args)
applyPrimitive Lt args = case args of
  [x, y] -> do
    nx <- expectNumber x
    ny <- expectNumber y
    return $ VBoolean (nx < ny)
  _ -> throwError $ ArityMismatch "<" 2 (length args)
applyPrimitive Eq args = case args of
  [x, y] -> return $ VBoolean (x == y)  -- Use Eq instance for Value (handles all types including keywords)
  _ -> throwError $ ArityMismatch "=" 2 (length args)
applyPrimitive Ne args = case args of
  [x, y] -> do
    nx <- expectNumber x
    ny <- expectNumber y
    return $ VBoolean (nx /= ny)
  _ -> throwError $ ArityMismatch "/=" 2 (length args)
applyPrimitive StringAppend args = do
  strs <- mapM expectString args
  return $ VString $ concat strs
applyPrimitive StringLength args = case args of
  [s] -> do
    str <- expectString s
    return $ VInteger $ fromIntegral $ length str
  _ -> throwError $ ArityMismatch "string-length" 1 (length args)
applyPrimitive Substring args = case args of
  [s, start, end] -> do
    str <- expectString s
    startNum <- expectNumber start
    endNum <- expectNumber end
    let startIdx = fromIntegral startNum
        endIdx = fromIntegral endNum
    if startIdx < 0 || endIdx > length str || startIdx > endIdx
      then throwError $ TypeMismatch "Invalid substring indices" (VArray [])
      else return $ VString $ take (endIdx - startIdx) $ drop startIdx str
  _ -> throwError $ ArityMismatch "substring" 3 (length args)
applyPrimitive Pure args = case args of
  [val] -> evalPatternCreate val
  _ -> throwError $ ArityMismatch "pure" 1 (length args)
applyPrimitive PatternCreate args = case args of
  [decoration, VArray elements] -> evalPatternWith decoration elements
  [_] -> throwError $ ArityMismatch "pattern" 2 (length args)
  [_, _] -> throwError $ TypeMismatch "pattern expects list of elements as second argument" (VArray [])
  _ -> throwError $ ArityMismatch "pattern" 2 (length args)
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
  [VSet s, val] -> return $ VBoolean (Set.member val s)
  [VMap m, keyVal] -> case valueToStringKey keyVal of
    Right keyStr -> return $ VBoolean (Map.member keyStr m)  -- Also handle maps
    Left err -> throwError err
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
  [VSet s1, VSet s2] -> return $ VBoolean (Set.isSubsetOf s1 s2)
  [VSet _, v] -> throwError $ TypeMismatch ("set-subset? expects set as second argument, but got: " ++ show v) v
  [v, _] -> throwError $ TypeMismatch ("set-subset? expects set as first argument, but got: " ++ show v) v
  _ -> throwError $ ArityMismatch "set-subset?" 2 (length args)
applyPrimitive SetEqual args = case args of
  [VSet s1, VSet s2] -> return $ VBoolean (s1 == s2)
  [VSet _, v] -> throwError $ TypeMismatch ("set-equal? expects set as second argument, but got: " ++ show v) v
  [v, _] -> throwError $ TypeMismatch ("set-equal? expects set as first argument, but got: " ++ show v) v
  _ -> throwError $ ArityMismatch "set-equal?" 2 (length args)
applyPrimitive SetEmpty args = case args of
  [VSet s] -> return $ VBoolean (Set.null s)
  [VMap m] -> return $ VBoolean (Map.null m)  -- Also handle maps
  [v] -> throwError $ TypeMismatch ("empty? expects set or map, but got: " ++ show v) v
  _ -> throwError $ ArityMismatch "empty?" 1 (length args)
applyPrimitive HashSet args = return $ VSet (Set.fromList args)
-- | Record constructor: @(record key1 val1 key2 val2 ...)@
--
-- Creates a record from alternating key-value pairs.
-- Requires an even number of arguments.
-- Keys must be strings or keywords.
--
-- Examples:
--
-- > (record "name" "Alice" "age" 30)
-- > -- Returns: {name: "Alice", age: 30}
applyPrimitive Record args
  | even (length args) = do
      -- Process alternating keyword-value or string-value pairs
      -- Process left-to-right so that later keys overwrite earlier ones
      let processPairs :: Map.Map String Value -> [Value] -> EvalM (Map.Map String Value)
          processPairs acc [] = return acc
          processPairs acc (keyVal:val:rest) = do
            case valueToStringKey keyVal of
              Right keyStr -> processPairs (Map.insert keyStr val acc) rest
              Left err -> throwError err
          processPairs _ _ = throwError $ ParseError "record requires even number of arguments (key-value pairs)"
      m <- processPairs Map.empty args
      return $ VMap m
  | otherwise = throwError $ ParseError "record requires even number of arguments (key-value pairs)"
-- | Type predicate: @(record? value)@
--
-- Returns @true@ if value is a record, @false@ otherwise.
applyPrimitive RecordType args = case args of
  [VMap _] -> return $ VBoolean True
  [_] -> return $ VBoolean False
  _ -> throwError $ ArityMismatch "record?" 1 (length args)
-- | Get value from record: @(get record key [default])@
--
-- Returns the value associated with @key@ in @record@.
-- If key is missing and @default@ is provided, returns @default@.
-- If key is missing and no default, returns @nil@ (empty array).
--
-- Examples:
--
-- > (get {name: "Alice"} "name")        -- "Alice"
-- > (get {name: "Alice"} "age" 0)       -- 0
-- > (get {name: "Alice"} "email")       -- ()
applyPrimitive RecordGet args = case args of
  [VMap m, keyVal] -> case valueToStringKey keyVal of
    Right keyStr -> return $ case Map.lookup keyStr m of
      Just val -> val
      Nothing -> VArray []  -- Return empty array as nil
    Left err -> throwError err
  [VMap m, keyVal, defaultVal] -> case valueToStringKey keyVal of
    Right keyStr -> return $ Map.findWithDefault defaultVal keyStr m
    Left err -> throwError err
  [v, _] -> throwError $ TypeMismatch ("get expects record as first argument, but got: " ++ show v) v
  _ -> throwError $ ArityMismatch "get" 2 (length args)
-- | Check if record has key: @(has? record key)@
--
-- Returns @true@ if @record@ contains @key@, @false@ otherwise.
applyPrimitive RecordHas args = case args of
  [VMap m, keyVal] -> case valueToStringKey keyVal of
    Right keyStr -> return $ VBoolean (Map.member keyStr m)
    Left err -> throwError err
  [v, _] -> throwError $ TypeMismatch ("has? expects record as first argument, but got: " ++ show v) v
  _ -> throwError $ ArityMismatch "has?" 2 (length args)
-- | Get all keys from record: @(keys record)@
--
-- Returns an array of all keys in @record@.
-- Key order is preserved from record creation.
applyPrimitive RecordKeys args = case args of
  [VMap m] -> return $ VArray (map VString (Map.keys m))
  [v] -> throwError $ TypeMismatch ("keys expects record, but got: " ++ show v) v
  _ -> throwError $ ArityMismatch "keys" 1 (length args)
-- | Get all values from record: @(values record)@
--
-- Returns an array of all values in @record@.
-- Value order matches key order.
applyPrimitive RecordValues args = case args of
  [VMap m] -> return $ VArray (Map.elems m)
  [v] -> throwError $ TypeMismatch ("values expects record, but got: " ++ show v) v
  _ -> throwError $ ArityMismatch "values" 1 (length args)
-- | Convert record to association list: @(record->alist record)@
--
-- Returns a list of @[key value]@ pairs.
-- Useful for iteration and transformation.
applyPrimitive RecordToAlist args = case args of
  [VMap m] -> return $ VArray (map (\(k, v) -> VArray [VString k, v]) (Map.toList m))
  [v] -> throwError $ TypeMismatch ("record->alist expects record, but got: " ++ show v) v
  _ -> throwError $ ArityMismatch "record->alist" 1 (length args)
-- | Convert association list to record: @(alist->record pairs)@
--
-- Takes a list of @[key value]@ pairs and creates a record.
-- Duplicate keys use last-wins semantics.
applyPrimitive AlistToRecord args = case args of
  [VArray pairs] -> do
    let processPairs :: Map.Map String Value -> [Value] -> EvalM (Map.Map String Value)
        processPairs acc [] = return acc
        processPairs acc (VArray [VString key, val]:rest) = processPairs (Map.insert key val acc) rest
        processPairs acc (VArray [keyVal, val]:rest) = do
          case valueToStringKey keyVal of
            Right keyStr -> processPairs (Map.insert keyStr val acc) rest
            Left err -> throwError err
        processPairs _ (badPair:_) = throwError $ TypeMismatch ("alist->record expects list of [key value] pairs, but got: " ++ show badPair) badPair
    m <- processPairs Map.empty pairs
    return $ VMap m
  [v] -> throwError $ TypeMismatch ("alist->record expects list, but got: " ++ show v) v
  _ -> throwError $ ArityMismatch "alist->record" 1 (length args)
-- | Associate key with value: @(assoc record key value)@
--
-- Returns a new record with @key@ set to @value@.
-- If @key@ already exists, its value is replaced.
-- Original record is unchanged (immutable).
applyPrimitive RecordSet args = case args of
  [VMap m, keyVal, val] -> case valueToStringKey keyVal of
    Right keyStr -> return $ VMap (Map.insert keyStr val m)
    Left err -> throwError err
  [v, _, _] -> throwError $ TypeMismatch ("assoc expects record as first argument, but got: " ++ show v) v
  _ -> throwError $ ArityMismatch "assoc" 3 (length args)
-- | Dissociate key from record: @(dissoc record key)@
--
-- Returns a new record with @key@ removed.
-- If @key@ doesn't exist, returns original record unchanged.
-- Original record is unchanged (immutable).
applyPrimitive RecordRemove args = case args of
  [VMap m, keyVal] -> case valueToStringKey keyVal of
    Right keyStr -> return $ VMap (Map.delete keyStr m)
    Left err -> throwError err
  [v, _] -> throwError $ TypeMismatch ("dissoc expects record as first argument, but got: " ++ show v) v
  _ -> throwError $ ArityMismatch "dissoc" 2 (length args)
-- | Merge two records: @(merge record1 record2)@
--
-- Returns a new record containing all keys from both records.
-- Keys from @record2@ take precedence over keys from @record1@.
-- Original records are unchanged (immutable).
applyPrimitive RecordMerge args = case args of
  [VMap m1, VMap m2] -> return $ VMap (Map.union m2 m1)  -- m2 takes precedence (right merge)
  [VMap _, v] -> throwError $ TypeMismatch ("merge expects record as second argument, but got: " ++ show v) v
  [v, _] -> throwError $ TypeMismatch ("merge expects record as first argument, but got: " ++ show v) v
  _ -> throwError $ ArityMismatch "merge" 2 (length args)
-- | Map over record entries: @(map fn record)@
--
-- Applies @fn@ to each @(key, value)@ pair in @record@.
-- @fn@ must be a closure that takes two arguments: key and value.
-- Returns a new record with transformed values.
-- Original record is unchanged (immutable).
applyPrimitive RecordMap args = case args of
  [VClosure closure, VMap m] -> do
    let mapOverRecord :: Map.Map String Value -> EvalM (Map.Map String Value)
        mapOverRecord = Map.traverseWithKey (\k v -> do
          result <- applyClosure closure [VString k, v]
          return result)
    mapped <- mapOverRecord m
    return $ VMap mapped
  [VClosure _, v] -> throwError $ TypeMismatch ("map expects record as second argument, but got: " ++ show v) v
  [v, _] -> throwError $ TypeMismatch ("map expects closure as first argument, but got: " ++ show v) v
  _ -> throwError $ ArityMismatch "map" 2 (length args)
-- | Filter record entries: @(filter pred record)@
--
-- Applies @pred@ to each @(key, value)@ pair in @record@.
-- @pred@ must be a closure that takes two arguments (key, value) and returns a boolean.
-- Returns a new record containing only entries where @pred@ returns @true@.
-- Original record is unchanged (immutable).
applyPrimitive RecordFilter args = case args of
  [VClosure closure, VMap m] -> do
    let filterEntry :: String -> Value -> EvalM (Maybe (String, Value))
        filterEntry k v = do
          result <- applyClosure closure [VString k, v]
          case result of
            VBoolean True -> return $ Just (k, v)
            VBoolean False -> return Nothing
            _ -> throwError $ TypeMismatch ("filter predicate must return boolean, but got: " ++ show result) result
    filteredPairs <- mapM (uncurry filterEntry) (Map.toList m)
    return $ VMap (Map.fromList (catMaybes filteredPairs))
  [VClosure _, v] -> throwError $ TypeMismatch ("filter expects record as second argument, but got: " ++ show v) v
  [v, _] -> throwError $ TypeMismatch ("filter expects closure as first argument, but got: " ++ show v) v
  _ -> throwError $ ArityMismatch "filter" 2 (length args)

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
            VBoolean b -> return b
            _ -> throwError $ TypeMismatch 
                   "predicate must return boolean" result
        _ -> throwError $ ArityMismatch "predicate" 1 (length paramNames)
    
    findInElements :: [Pattern Subject] -> Closure -> EvalM Value
    findInElements [] _ = return $ VArray []
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
        then return $ VBoolean True
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
            VBoolean b -> return b
            _ -> throwError $ TypeMismatch 
                   "predicate must return boolean" result
        _ -> throwError $ ArityMismatch "predicate" 1 (length paramNames)
    
    anyInElements :: [Pattern Subject] -> Closure -> EvalM Value
    anyInElements [] _ = return $ VBoolean False
    anyInElements (p:ps) closure = do
      matches <- applyPredicate closure p
      if matches
        then return $ VBoolean True
        else do
          nestedResult <- anyInElements (PatternCore.elements p) closure
          case nestedResult of
            VBoolean True -> return $ VBoolean True
            _ -> anyInElements ps closure

-- | Checks if all subpatterns match a predicate closure
evalPatternAll :: Pattern Subject -> Value -> EvalM Value
evalPatternAll pat predVal = do
  case predVal of
    VClosure closure -> do
      matches <- applyPredicate closure pat
      if not matches
        then return $ VBoolean False
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
            VBoolean b -> return b
            _ -> throwError $ TypeMismatch 
                   "predicate must return boolean" result
        _ -> throwError $ ArityMismatch "predicate" 1 (length paramNames)
    
    allInElements :: [Pattern Subject] -> Closure -> EvalM Value
    allInElements [] _ = return $ VBoolean True
    allInElements (p:ps) closure = do
      matches <- applyPredicate closure p
      if not matches
        then return $ VBoolean False
        else do
          nestedResult <- allInElements (PatternCore.elements p) closure
          case nestedResult of
            VBoolean False -> return $ VBoolean False
            _ -> allInElements ps closure

-- | Evaluate a quoted expression (convert Expr to Value)
evalQuote :: Expr -> EvalM Value
evalQuote expr = exprToValue expr

-- | Convert an Expr to a Value (for quote evaluation)
exprToValue :: Expr -> EvalM Value
exprToValue (Atom (Number n)) = return $ VInteger n
exprToValue (Atom (String s)) = return $ VString s
exprToValue (Atom (Bool b)) = return $ VBoolean b
exprToValue (Atom (Keyword name)) = return $ VKeyword name
exprToValue (Atom (Symbol name)) = return $ VString name
exprToValue (ArrayLiteral exprs) = do
  vals <- mapM exprToValue exprs
  return $ VArray vals
exprToValue (List exprs) = do
  vals <- mapM exprToValue exprs
  return $ VArray vals
exprToValue (SetLiteral exprs) = do
  vals <- mapM exprToValue exprs
  return $ VSet (Set.fromList vals)
exprToValue (RecordLiteral pairs) = do
  -- Process pairs: [(String, Expr), ...]
  -- Process left-to-right so that later keys overwrite earlier ones
  -- Support Unquote (evaluate expression) and UnquoteSplice (evaluate to record and merge)
  let processPairs :: Map.Map String Value -> [(String, Expr)] -> EvalM (Map.Map String Value)
      processPairs acc [] = return acc
      processPairs acc ((keyStr, valExpr):rest) = do
        case valExpr of
          UnquoteSplice expr -> do
            -- Splice: evaluate to record and merge its entries (ignore keyStr)
            recordVal <- eval expr  -- Use eval, not exprToValue, to evaluate the expression
            case recordVal of
              VMap spliceMap -> processPairs (Map.union spliceMap acc) rest  -- Merge entries (spliceMap takes precedence)
              _ -> throwError $ TypeMismatch ("record splice expects record, but got: " ++ show recordVal) recordVal
          Unquote expr -> do
            -- Unquote: evaluate expression and use as value
            valVal <- eval expr  -- Use eval, not exprToValue, to evaluate the expression
            processPairs (Map.insert keyStr valVal acc) rest
          _ -> do
            -- Normal: convert to value (recursive quote evaluation)
            valVal <- exprToValue valExpr
            processPairs (Map.insert keyStr valVal acc) rest
  m <- processPairs Map.empty pairs
  return $ VMap m
exprToValue (Quote expr) = exprToValue expr
exprToValue (Unquote expr) = eval expr  -- In quasiquotation context, evaluate unquotes
exprToValue (UnquoteSplice expr) = eval expr  -- In quasiquotation context, evaluate splices (but should be handled in RecordLiteral)

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
    VBoolean True -> eval thenExpr
    VBoolean False -> eval elseExpr
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
    return $ VString name
evalDefine args = throwError $ ParseError 
  ("define requires name and value (2 arguments), but got " ++ 
   show (length args) ++ " argument(s)")

-- | Helper: expect a number value, providing context in error message
expectNumber :: Value -> EvalM Integer
expectNumber (VInteger n) = return n
expectNumber v = throwError $ TypeMismatch 
  ("Expected number, but got: " ++ show v) v

-- | Helper: expect a string value, providing context in error message
expectString :: Value -> EvalM String
expectString (VString s) = return s
expectString v = throwError $ TypeMismatch 
  ("Expected string, but got: " ++ show v) v

