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
  ) where

import PatternLisp.Syntax
import Pattern (Pattern)
import Pattern.Core (pattern, patternWith)
import Subject.Core (Subject(..))
import qualified Subject.Core as SubjectCore
import qualified Subject.Value as SubjectValue
import qualified Data.Text as T
import qualified Data.Map.Strict as Map
import qualified Data.Set as Set
import Control.Monad.Reader
import Control.Monad.Except

-- | Re-export EvalM type for pattern primitives
-- This avoids circular dependency with PatternLisp.Eval
type EvalM = ReaderT Env (Except Error)

-- | Temporary helper to convert basic Value to Subject for Phase 2
-- Full serialization will be implemented in Phase 5 (PatternLisp.Subject)
-- This creates minimal Subject values with appropriate labels and properties
valueToSubjectBasic :: Value -> Subject
valueToSubjectBasic (VNumber n) = Subject
  { identity = SubjectCore.Symbol ""
  , labels = Set.fromList ["Number"]
  , properties = Map.fromList [("value", SubjectValue.VInteger n)]
  }
valueToSubjectBasic (VString s) = Subject
  { identity = SubjectCore.Symbol ""
  , labels = Set.fromList ["String"]
  , properties = Map.fromList [("text", SubjectValue.VString (T.unpack s))]
  }
valueToSubjectBasic (VBool b) = Subject
  { identity = SubjectCore.Symbol ""
  , labels = Set.fromList ["Bool"]
  , properties = Map.fromList [("value", SubjectValue.VBoolean b)]
  }
valueToSubjectBasic (VList _) = error "List to Subject conversion will be implemented in Phase 5"
valueToSubjectBasic (VPattern _) = error "Pattern to Subject conversion will be implemented in Phase 5"
valueToSubjectBasic (VClosure _) = error "Closure to Subject conversion will be implemented in Phase 5"
valueToSubjectBasic (VPrimitive _) = error "Primitive to Subject conversion will be implemented in Phase 5"

-- | Creates an atomic Pattern from a Value.
--
-- Converts the Value to a Subject and creates an atomic Pattern with that
-- Subject as decoration.
evalPatternCreate :: Value -> EvalM Value
evalPatternCreate val = do
  -- For Phase 2, we'll use a placeholder implementation
  -- Full Subject conversion will be in Phase 5
  let subject = valueToSubjectBasic val
      pat = pattern subject
  return $ VPattern pat

-- | Creates a Pattern with elements.
--
-- Takes a decoration Value and a list of element Values (which should be
-- VPattern values or convertible to patterns), and creates a Pattern with
-- those elements.
evalPatternWith :: Value -> [Value] -> EvalM Value
evalPatternWith decorationVal elementVals = do
  -- Convert decoration to Subject
  let decoration = valueToSubjectBasic decorationVal
  -- Convert elements: expect VPattern values
  elements <- mapM expectPattern elementVals
  -- Create pattern with elements
  let pat = patternWith decoration elements
  return $ VPattern pat

-- | Helper to extract Pattern from Value, or convert Value to Pattern
expectPattern :: Value -> EvalM (Pattern Subject)
expectPattern (VPattern p) = return p
expectPattern v = throwError $ TypeMismatch 
  ("Expected pattern value, but got: " ++ show v) v

