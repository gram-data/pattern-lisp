-- | Serialization between Pattern Lisp s-expressions and Pattern Subject representation.
--
-- This module provides functions for converting Pattern Lisp expressions and values
-- to/from Pattern Subject (the gram-serializable format) and Subject (for property storage).
--
-- **Primary Serialization Path: Pattern Subject**
--
-- The primary target for serialization is `Pattern Subject`, which can be directly
-- serialized to gram notation. All s-expressions can be represented as Pattern Subject:
--
-- * List expressions: Pattern Subject with decoration `:List` and elements as Pattern Subjects
-- * Atom expressions: Atomic Pattern Subject with decoration as Subject (Var, Number, String, Bool)
-- * Closures: Pattern Subject with decoration `:Closure {params: [...]}` and body as Pattern Subject element
--
-- See `PatternLisp.PatternPrimitives.valueToPatternSubject` for the primary conversion function.
--
-- **Secondary Path: Subject (for property storage)**
--
-- `valueToSubject` and `subjectToValue` are lower-level helpers for storing values
-- in Subject properties. They convert to `Subject` (not `Pattern Subject`), which
-- requires Patterns to be stored in properties as VMap structures. This is useful
-- for property storage but not for gram serialization.
--
-- **Expression Serialization**
--
-- `exprToSubject` and `subjectToExpr` convert expressions to/from Subject representation
-- (for property storage). For Pattern Subject conversion, see `PatternLisp.PatternPrimitives.exprToPatternSubject`.
--
-- Variable names in expressions are stored as properties (not Gram identifiers),
-- matching Lisp's name-based scoping.
--
-- Example usage:
--
-- > import PatternLisp.Codec
-- > import PatternLisp.PatternPrimitives
-- >
-- > -- Primary path: Expression to Pattern Subject (gram-serializable)
-- > expr <- parseExpr "(+ 1 2)"
-- > pat <- exprToPatternSubject expr  -- Returns Pattern Subject
-- >
-- > -- Secondary path: Value to Subject (for property storage)
-- > let val = VNumber 42
-- > let subj = valueToSubject val
-- > case subjectToValue subj of
-- >   Right val' -> val' == val  -- True
module PatternLisp.Codec
  ( valueToSubject
  , subjectToValue
  , exprToSubject
  , subjectToExpr
  , envToSubject
  , subjectToEnv
  , subjectToBinding
  , patternSubjectToExpr
  ) where

import PatternLisp.Syntax
import PatternLisp.Primitives (initialEnv)
import Pattern (Pattern)
import Pattern.Core (pattern, patternWith)
import qualified Pattern.Core as PatternCore
import Subject.Core (Subject(..))
import qualified Subject.Core as SubjectCore
import qualified Subject.Value as SubjectValue
import qualified Data.Text as T
import qualified Data.Map.Strict as Map
import qualified Data.Set as Set

-- ============================================================================
-- Helper Functions (for property storage)
-- ============================================================================

-- | Helper to convert Subject to SubjectValue (for storing in properties)
subjectToSubjectValue :: Subject -> SubjectValue.Value
subjectToSubjectValue subj = SubjectValue.VMap (Map.fromList 
  [ ("identity", subjectIdentityToValue (identity subj))
  , ("labels", SubjectValue.VArray (map SubjectValue.VString (Set.toList (labels subj))))
  , ("properties", SubjectValue.VMap (properties subj))
  ])
  where
    subjectIdentityToValue :: SubjectCore.Symbol -> SubjectValue.Value
    subjectIdentityToValue (SubjectCore.Symbol s) = SubjectValue.VSymbol s

-- | Helper to convert SubjectValue back to Subject
subjectValueToSubject :: SubjectValue.Value -> Either Error Subject
subjectValueToSubject (SubjectValue.VMap m) = do
  identVal <- case Map.lookup "identity" m of
    Just v -> Right v
    Nothing -> Left $ TypeMismatch "Subject map missing identity" (VList [])
  ident <- case identVal of
    SubjectValue.VSymbol s -> Right (SubjectCore.Symbol s)
    _ -> Left $ TypeMismatch "Subject identity must be VSymbol" (VList [])
  
  labelsVal <- case Map.lookup "labels" m of
    Just (SubjectValue.VArray vs) -> Right vs
    _ -> Left $ TypeMismatch "Subject map missing labels array" (VList [])
  labelStrs <- mapM extractStringFromValue labelsVal
  let lbls = Set.fromList labelStrs
  
  propsVal <- case Map.lookup "properties" m of
    Just (SubjectValue.VMap p) -> Right p
    _ -> Left $ TypeMismatch "Subject map missing properties map" (VList [])
  
  Right $ Subject
    { identity = ident
    , labels = lbls
    , properties = propsVal
    }
subjectValueToSubject _ = Left $ TypeMismatch "Subject value must be VMap" (VList [])

extractStringFromValue :: SubjectValue.Value -> Either Error String
extractStringFromValue (SubjectValue.VString s) = Right s
extractStringFromValue _ = Left $ TypeMismatch "Expected VString in labels array" (VList [])

-- ============================================================================
-- Primary Path: Expression <-> Pattern Subject
-- ============================================================================
-- These functions convert s-expressions to/from Pattern Subject (gram-serializable).
-- For the main conversion function, see PatternLisp.PatternPrimitives.exprToPatternSubject.

-- | Converts a Pattern Subject back to expression AST.
-- This handles the case where List Exprs are represented as Pattern Subjects with elements.
patternSubjectToExpr :: Pattern Subject -> Either Error Expr
patternSubjectToExpr pat
  | "List" `Set.member` labels (PatternCore.value pat) = do
      -- List pattern: extract elements and convert recursively
      elementPatterns <- return $ PatternCore.elements pat
      exprs <- mapM patternSubjectToExpr elementPatterns
      Right $ List exprs
  | otherwise = do
      -- Atomic pattern: convert decoration Subject to Expr
      subjectToExpr (PatternCore.value pat)

-- ============================================================================
-- Secondary Path: Expression <-> Subject (for property storage)
-- ============================================================================
-- These functions convert expressions to/from Subject representation.
-- This is used for storing expressions in Subject properties, not for gram serialization.

-- | Converts an expression AST to Subject representation.
-- Note: For gram serialization, use PatternLisp.PatternPrimitives.exprToPatternSubject instead.
exprToSubject :: Expr -> Subject
exprToSubject (Atom (Symbol name)) = Subject
  { identity = SubjectCore.Symbol ""
  , labels = Set.fromList ["Var"]
  , properties = Map.fromList [("name", SubjectValue.VString name)]
  }
exprToSubject (Atom (Number n)) = Subject
  { identity = SubjectCore.Symbol ""
  , labels = Set.fromList ["Number"]
  , properties = Map.fromList [("value", SubjectValue.VInteger n)]
  }
exprToSubject (Atom (String s)) = Subject
  { identity = SubjectCore.Symbol ""
  , labels = Set.fromList ["String"]
  , properties = Map.fromList [("text", SubjectValue.VString (T.unpack s))]
  }
exprToSubject (Atom (Bool b)) = Subject
  { identity = SubjectCore.Symbol ""
  , labels = Set.fromList ["Bool"]
  , properties = Map.fromList [("value", SubjectValue.VBoolean b)]
  }
exprToSubject (List exprs) = Subject
  { identity = SubjectCore.Symbol ""
  , labels = Set.fromList ["List"]
  , properties = Map.fromList [("elements", SubjectValue.VArray (map (subjectToSubjectValue . exprToSubject) exprs))]
  }
exprToSubject (Quote expr) = Subject
  { identity = SubjectCore.Symbol ""
  , labels = Set.fromList ["Quote"]
  , properties = Map.fromList [("expr", subjectToSubjectValue (exprToSubject expr))]
  }

-- | Converts a Subject representation back to expression AST.
-- Note: For Pattern Subject conversion, use patternSubjectToExpr instead.
subjectToExpr :: Subject -> Either Error Expr
subjectToExpr subj
  | "Var" `Set.member` labels subj =
      case Map.lookup "name" (properties subj) of
        Just (SubjectValue.VString name) -> Right $ Atom (Symbol name)
        _ -> Left $ TypeMismatch "Var Subject missing name property" (VList [])
  | "Number" `Set.member` labels subj =
      case Map.lookup "value" (properties subj) of
        Just (SubjectValue.VInteger n) -> Right $ Atom (Number n)
        _ -> Left $ TypeMismatch "Number Subject missing value property" (VList [])
  | "String" `Set.member` labels subj =
      case Map.lookup "text" (properties subj) of
        Just (SubjectValue.VString s) -> Right $ Atom (String (T.pack s))
        _ -> Left $ TypeMismatch "String Subject missing text property" (VList [])
  | "Bool" `Set.member` labels subj =
      case Map.lookup "value" (properties subj) of
        Just (SubjectValue.VBoolean b) -> Right $ Atom (Bool b)
        _ -> Left $ TypeMismatch "Bool Subject missing value property" (VList [])
  | "List" `Set.member` labels subj = do
      elementsVal <- case Map.lookup "elements" (properties subj) of
        Just (SubjectValue.VArray vs) -> Right vs
        _ -> Left $ TypeMismatch "List Subject missing elements property" (VList [])
      elementSubjects <- mapM subjectValueToSubject elementsVal
      exprs <- mapM subjectToExpr elementSubjects
      Right $ List exprs
  | "Quote" `Set.member` labels subj = do
      exprVal <- case Map.lookup "expr" (properties subj) of
        Just v -> subjectValueToSubject v
        Nothing -> Left $ TypeMismatch "Quote Subject missing expr property" (VList [])
      expr <- subjectToExpr exprVal
      Right $ Quote expr
  | otherwise = Left $ TypeMismatch ("Unknown expression Subject label: " ++ show (Set.toList (labels subj))) (VList [])

-- ============================================================================
-- Secondary Path: Value <-> Subject (for property storage)
-- ============================================================================
-- These functions convert runtime values to/from Subject representation.
-- This is used for storing values in Subject properties, not for gram serialization.
-- For gram serialization, use PatternLisp.PatternPrimitives.valueToPatternSubject instead.

-- | Converts a Pattern Lisp Value to Subject representation.
-- Note: This is a helper for property storage. For gram serialization, use
-- PatternLisp.PatternPrimitives.valueToPatternSubject instead.
valueToSubject :: Value -> Subject
valueToSubject (VNumber n) = Subject
  { identity = SubjectCore.Symbol ""
  , labels = Set.fromList ["Number"]
  , properties = Map.fromList [("value", SubjectValue.VInteger n)]
  }
valueToSubject (VString s) = Subject
  { identity = SubjectCore.Symbol ""
  , labels = Set.fromList ["String"]
  , properties = Map.fromList [("text", SubjectValue.VString (T.unpack s))]
  }
valueToSubject (VBool b) = Subject
  { identity = SubjectCore.Symbol ""
  , labels = Set.fromList ["Bool"]
  , properties = Map.fromList [("value", SubjectValue.VBoolean b)]
  }
valueToSubject (VList vs) = Subject
  { identity = SubjectCore.Symbol ""
  , labels = Set.fromList ["List"]
  , properties = Map.fromList [("elements", SubjectValue.VArray (map (subjectToSubjectValue . valueToSubject) vs))]
  }
valueToSubject (VPattern pat) = patternToSubject pat
  where
    -- Convert Pattern to Subject: store decoration and elements in properties
    -- Since Subject doesn't have elements, we store Pattern structure in properties
    patternToSubject :: Pattern Subject -> Subject
    patternToSubject p = Subject
      { identity = SubjectCore.Symbol ""
      , labels = Set.fromList ["Pattern"]
      , properties = Map.fromList 
          [ ("decoration", subjectToSubjectValue (PatternCore.value p))
          , ("elements", SubjectValue.VArray (map (subjectToSubjectValue . patternToSubject) (PatternCore.elements p)))
          ]
      }
valueToSubject (VClosure closure) = 
  -- Closures are serialized as Pattern Subject, then stored in a Subject
  -- using the Pattern serialization format (decoration + elements in properties)
  -- The body Expr must be converted to Pattern Subject format (not just Subject)
  let bodyPattern = exprToPatternSubjectForCodec (body closure)
      patternPat = patternWith 
        (SubjectCore.Subject
          { identity = SubjectCore.Symbol ""
          , labels = Set.fromList ["Closure"]
          , properties = Map.fromList 
              [ ("params", SubjectValue.VArray (map SubjectValue.VString (params closure)))
              ]
          })
        [bodyPattern]
  in patternToSubject patternPat
  where
    -- Helper to convert Expr to Pattern Subject (for use in valueToSubject)
    -- This handles List Exprs correctly by creating Pattern Subjects with elements
    exprToPatternSubjectForCodec :: Expr -> Pattern Subject
    exprToPatternSubjectForCodec (List exprs) =
      -- List Expr: represent as Pattern Subject with elements
      let decoration = SubjectCore.Subject
            { identity = SubjectCore.Symbol ""
            , labels = Set.fromList ["List"]
            , properties = Map.empty
            }
          elementPatterns = map exprToPatternSubjectForCodec exprs
      in patternWith decoration elementPatterns
    exprToPatternSubjectForCodec expr =
      -- For atoms and other Exprs: convert to Subject, then wrap in atomic pattern
      pattern (exprToSubject expr)
    
    patternToSubject :: Pattern Subject -> Subject
    patternToSubject p = Subject
      { identity = SubjectCore.Symbol ""
      , labels = Set.fromList ["Pattern"]
      , properties = Map.fromList 
          [ ("decoration", subjectToSubjectValue (PatternCore.value p))
          , ("elements", SubjectValue.VArray (map (subjectToSubjectValue . patternToSubject) (PatternCore.elements p)))
          ]
      }
valueToSubject (VPrimitive prim) = Subject
  { identity = SubjectCore.Symbol ""
  , labels = Set.fromList ["Primitive"]
  , properties = Map.fromList [("name", SubjectValue.VString (primitiveName prim))]
  }

-- ============================================================================
-- Environment and Binding Helpers (for property storage)
-- ============================================================================
-- These functions are helpers for serializing environments and bindings.
-- Note: Not currently used for Closure serialization to avoid deeply nested VMap structures.

-- | Converts an environment to Subject representation.
-- Note: Not currently used for Closure serialization to avoid deeply nested VMap structures.
{-# WARNING envToSubject "Not currently used for Closure serialization" #-}
{-# NOINLINE envToSubject #-}
envToSubject :: Env -> Subject
envToSubject runtimeEnv = Subject
  { identity = SubjectCore.Symbol ""
  , labels = Set.fromList ["Env"]
  , properties = Map.fromList [("bindings", SubjectValue.VArray (map (subjectToSubjectValue . bindingToSubject) (Map.toList runtimeEnv)))]
  }
  where
    bindingToSubject :: (String, Value) -> Subject
    bindingToSubject (name, val) = Subject
      { identity = SubjectCore.Symbol ""
      , labels = Set.fromList ["Binding"]
      , properties = Map.fromList 
          [ ("name", SubjectValue.VString name)
          , ("value", subjectToSubjectValue (valueToSubject val))
          ]
      }

-- | Converts a Subject representation back to Pattern Lisp Value.
-- Note: This is a helper for property storage. For Pattern Subject conversion,
-- use PatternLisp.PatternPrimitives.evalPatternToValue instead.
subjectToValue :: Subject -> Either Error Value
subjectToValue subj
  | "Number" `Set.member` labels subj =
      case Map.lookup "value" (properties subj) of
        Just (SubjectValue.VInteger n) -> Right $ VNumber n
        _ -> Left $ TypeMismatch "Number Subject missing value property" (VList [])
  | "String" `Set.member` labels subj =
      case Map.lookup "text" (properties subj) of
        Just (SubjectValue.VString s) -> Right $ VString (T.pack s)
        _ -> Left $ TypeMismatch "String Subject missing text property" (VList [])
  | "Bool" `Set.member` labels subj =
      case Map.lookup "value" (properties subj) of
        Just (SubjectValue.VBoolean b) -> Right $ VBool b
        _ -> Left $ TypeMismatch "Bool Subject missing value property" (VList [])
  | "List" `Set.member` labels subj = do
      elementsVal <- case Map.lookup "elements" (properties subj) of
        Just (SubjectValue.VArray vs) -> Right vs
        _ -> Left $ TypeMismatch "List Subject missing elements property" (VList [])
      elementSubjects <- mapM subjectValueToSubject elementsVal
      vals <- mapM subjectToValue elementSubjects
      Right $ VList vals
  | "Pattern" `Set.member` labels subj = do
      -- Pattern stores decoration and elements in properties
      decorationVal <- case Map.lookup "decoration" (properties subj) of
        Just v -> subjectValueToSubject v
        Nothing -> Left $ TypeMismatch "Pattern Subject missing decoration property" (VList [])
      elementsVal <- case Map.lookup "elements" (properties subj) of
        Just (SubjectValue.VArray vs) -> Right vs
        _ -> Left $ TypeMismatch "Pattern Subject missing elements property" (VList [])
      elementSubjects <- mapM subjectValueToSubject elementsVal
      -- Check if this Pattern represents a Closure (decoration has "Closure" label)
      if "Closure" `Set.member` labels decorationVal then do
        -- This is a Closure serialized as Pattern Subject
        -- Extract params from decoration properties
        paramsList <- case Map.lookup "params" (properties decorationVal) of
          Just (SubjectValue.VArray paramStrs) -> do
            paramNames <- mapM extractString paramStrs
            Right paramNames
          _ -> Left $ TypeMismatch "Closure Pattern decoration missing params property" (VList [])
        -- Extract body from first element (should be Pattern Subject representing body Expr)
        case elementSubjects of
          [bodyPatternSubject] -> do
            -- The body element is a Pattern Subject (stored as Subject with "Pattern" label)
            -- Convert it back to Pattern Subject, then to Expr
            bodyPattern <- subjectToPatternFromSubject bodyPatternSubject
            bodyExpr <- patternSubjectToExpr bodyPattern
            Right $ VClosure (Closure paramsList bodyExpr initialEnv)
          _ -> Left $ TypeMismatch "Closure Pattern must have exactly one element (body)" (VList [])
      else do
        -- Regular Pattern: create pattern with decoration and elements
        elementPatterns <- mapM subjectToPatternFromSubject elementSubjects
        let pat = patternWith decorationVal elementPatterns
        Right $ VPattern pat
  | "Primitive" `Set.member` labels subj =
      case Map.lookup "name" (properties subj) of
        Just (SubjectValue.VString name) ->
          case primitiveFromName name of
            Just prim -> Right $ VPrimitive prim
            Nothing -> Left $ TypeMismatch ("Unknown primitive name: " ++ name) (VList [])
        _ -> Left $ TypeMismatch "Primitive Subject missing name property" (VList [])
  | otherwise = Left $ TypeMismatch ("Unknown Subject label: " ++ show (Set.toList (labels subj))) (VList [])

-- | Helper to extract string from SubjectValue
extractString :: SubjectValue.Value -> Either Error String
extractString (SubjectValue.VString s) = Right s
extractString _ = Left $ TypeMismatch "Expected string in params list" (VList [])

-- | Converts a Subject representation back to an environment.
-- Note: Not currently used for Closure deserialization since env is not serialized.
{-# WARNING subjectToEnv "Not currently used for Closure deserialization" #-}
subjectToEnv :: Subject -> Either Error Env
subjectToEnv subj
  | "Env" `Set.member` labels subj = do
      bindingsVal <- case Map.lookup "bindings" (properties subj) of
        Just (SubjectValue.VArray vs) -> Right vs
        _ -> Left $ TypeMismatch "Env Subject missing bindings property" (VList [])
      bindingSubjects <- mapM subjectValueToSubject bindingsVal
      bindings <- mapM subjectToBinding bindingSubjects
      Right $ Map.fromList bindings
  | otherwise = Left $ TypeMismatch "Expected Env Subject" (VList [])

-- | Converts a Subject representation back to a binding.
-- Note: Not currently used for Closure deserialization since env is not serialized.
{-# WARNING subjectToBinding "Not currently used for Closure deserialization" #-}
subjectToBinding :: Subject -> Either Error (String, Value)
subjectToBinding subj
  | "Binding" `Set.member` labels subj = do
      name <- case Map.lookup "name" (properties subj) of
        Just (SubjectValue.VString n) -> Right n
        _ -> Left $ TypeMismatch "Binding Subject missing name property" (VList [])
      valueVal <- case Map.lookup "value" (properties subj) of
        Just v -> subjectValueToSubject v
        Nothing -> Left $ TypeMismatch "Binding Subject missing value property" (VList [])
      val <- subjectToValue valueVal
      Right (name, val)
  | otherwise = Left $ TypeMismatch "Expected Binding Subject" (VList [])

-- ============================================================================
-- Pattern Subject Helpers (for deserialization)
-- ============================================================================

-- | Converts a Subject back to a Pattern (recursive helper).
-- Used when deserializing Pattern Subject from Subject properties.
subjectToPatternFromSubject :: Subject -> Either Error (Pattern Subject)
subjectToPatternFromSubject subj
  | "Pattern" `Set.member` labels subj = do
      -- Pattern stores decoration and elements in properties
      decorationVal <- case Map.lookup "decoration" (properties subj) of
        Just v -> subjectValueToSubject v
        Nothing -> Left $ TypeMismatch "Pattern Subject missing decoration property" (VList [])
      elementsVal <- case Map.lookup "elements" (properties subj) of
        Just (SubjectValue.VArray vs) -> Right vs
        _ -> Left $ TypeMismatch "Pattern Subject missing elements property" (VList [])
      elementSubjects <- mapM subjectValueToSubject elementsVal
      elementPatterns <- mapM subjectToPatternFromSubject elementSubjects
      -- Create pattern with decoration and elements
      Right $ patternWith decorationVal elementPatterns
  | otherwise = Left $ TypeMismatch "Expected Pattern Subject" (VList [])


