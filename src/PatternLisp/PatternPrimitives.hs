-- | Pattern primitive operations for Pattern Lisp.
--
-- This module provides functions for constructing, querying, and transforming
-- Pattern values. Patterns are decorated with Subject types to enable complete
-- serialization.
--
-- Example usage:
--
-- > import PatternLisp.PatternPrimitives
-- > import PatternLisp.Eval
-- >
-- > evalPatternCreate (VString "hello")  -- Creates atomic pattern
-- > evalPatternWith (VString "root") [pattern1, pattern2]  -- Creates pattern with elements
module PatternLisp.PatternPrimitives
  ( evalPatternCreate
  , evalPatternWith
  , evalPatternValue
  , evalPatternElements
  , evalPatternLength
  , evalPatternSize
  , evalPatternDepth
  , evalPatternValues
  , valueToPatternSubject
  ) where

import PatternLisp.Syntax
import PatternLisp.Subject
import Pattern (Pattern)
import Pattern.Core (pattern, patternWith)
import qualified Pattern.Core as PatternCore
import Subject.Core (Subject(..))
import qualified Subject.Core as SubjectCore
import qualified Data.Set as Set
import qualified Data.Map.Strict as Map
import Control.Monad.Reader
import Control.Monad.Except

-- | Re-export EvalM type for pattern primitives
-- This avoids circular dependency with PatternLisp.Eval
type EvalM = ReaderT Env (Except Error)

-- | Creates an atomic Pattern from a Value.
--
-- Converts the Value to a Subject and creates an atomic Pattern with that
-- Subject as decoration.
evalPatternCreate :: Value -> EvalM Value
evalPatternCreate val = do
  -- Convert Value to Subject using full serialization
  let subject = valueToSubject val
      pat = pattern subject
  return $ VPattern pat

-- | Creates a Pattern with elements.
--
-- Takes a decoration Value and a list of element Values (which should be
-- VPattern values or convertible to patterns), and creates a Pattern with
-- those elements.
evalPatternWith :: Value -> [Value] -> EvalM Value
evalPatternWith decorationVal elementVals = do
  -- Convert decoration to Subject using full serialization
  let decoration = valueToSubject decorationVal
  -- Convert elements: expect VPattern values
  patternElements <- mapM expectPattern elementVals
  -- Create pattern with elements
  let pat = patternWith decoration patternElements
  return $ VPattern pat

-- | Helper to extract Pattern from Value, or convert Value to Pattern
expectPattern :: Value -> EvalM (Pattern Subject)
expectPattern (VPattern p) = return p
expectPattern v = throwError $ TypeMismatch 
  ("Expected pattern value, but got: " ++ show v) v

-- | Extracts the Subject decoration from a Pattern and converts it to Value.
evalPatternValue :: Pattern Subject -> EvalM Value
evalPatternValue pat = do
  let subj = PatternCore.value pat
  case subjectToValue subj of
    Left err -> throwError err
    Right val -> return val

-- | Extracts the elements list from a Pattern.
evalPatternElements :: Pattern Subject -> EvalM Value
evalPatternElements pat = do
  let elems = PatternCore.elements pat
  patternVals <- mapM (\p -> return $ VPattern p) elems
  return $ VList patternVals

-- | Returns the number of direct elements (not recursive).
evalPatternLength :: Pattern Subject -> EvalM Value
evalPatternLength pat = do
  let elems = PatternCore.elements pat
  return $ VNumber (fromIntegral $ length elems)

-- | Returns the total node count (recursive).
evalPatternSize :: Pattern Subject -> EvalM Value
evalPatternSize pat = do
  let size = patternSize pat
  return $ VNumber size
  where
    patternSize :: Pattern Subject -> Integer
    patternSize p = 1 + sum (map patternSize (PatternCore.elements p))

-- | Returns the maximum nesting depth.
evalPatternDepth :: Pattern Subject -> EvalM Value
evalPatternDepth pat = do
  let depth = patternDepth pat
  return $ VNumber depth
  where
    patternDepth :: Pattern Subject -> Integer
    patternDepth p
      | null (PatternCore.elements p) = 0
      | otherwise = 1 + maximum (map patternDepth (PatternCore.elements p))

-- | Flattens Pattern to list of all Subject values.
evalPatternValues :: Pattern Subject -> EvalM Value
evalPatternValues pat = do
  let values = patternValues pat
  return $ VList values
  where
    patternValues :: Pattern Subject -> [Value]
    patternValues p = 
      case subjectToValue (PatternCore.value p) of
        Right val -> val : concatMap patternValues (PatternCore.elements p)
        Left _ -> concatMap patternValues (PatternCore.elements p)  -- Skip on error

-- | Maps any Value to its Pattern Subject representation.
-- This enables all s-expressions to be represented as Pattern Subject.
-- 
-- * VPattern: Returns the pattern directly
-- * VList: Converts to pattern-with with elements (empty list becomes atomic pattern)
-- * Other values: Converts to Subject and wraps in atomic pattern
valueToPatternSubject :: Value -> EvalM (Pattern Subject)
valueToPatternSubject (VPattern pat) = return pat
valueToPatternSubject (VList []) = do
  -- Empty list becomes atomic pattern with empty Subject decoration
  let emptySubject = SubjectCore.Subject
        { identity = SubjectCore.Symbol ""
        , labels = Set.fromList ["List"]
        , properties = Map.empty
        }
  return $ pattern emptySubject
valueToPatternSubject (VList (v:vs)) = do
  -- Non-empty list: convert to pattern-with
  -- Decoration is empty Subject, elements are recursively converted
  let emptySubject = SubjectCore.Subject
        { identity = SubjectCore.Symbol ""
        , labels = Set.fromList ["List"]
        , properties = Map.empty
        }
  -- Convert each element to Pattern Subject recursively
  elementPatterns <- mapM valueToPatternSubject (v:vs)
  return $ patternWith emptySubject elementPatterns
valueToPatternSubject val = do
  -- For atoms and other values: convert to Subject, then wrap in atomic pattern
  let subject = valueToSubject val
      pat = pattern subject
  return pat

