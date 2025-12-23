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
  -- Gram serialization functions (Phase 5)
  , valueToSubjectForGram
  , valueToPatternSubjectForGram
  , patternSubjectToValue
  , programToGram
  , gramToProgram
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
import Gram.Serialize (toGram)
import Gram.Parse (fromGram)

-- ============================================================================
-- Helper Functions (for property storage)
-- ============================================================================

-- NOTE: patternToSubject removed - it was incorrect. A Pattern already has a Subject
-- (its decoration). Converting Pattern -> Subject by storing decoration/elements in
-- properties is redundant and lossy. For storing Patterns in Subject properties, we
-- should just use the Pattern's decoration Subject directly, or use Pattern Subject
-- serialization instead.

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
-- Handles special form labels: :If, :Let, :Begin, :Define, :Quote
patternSubjectToExpr :: Pattern Subject -> Either Error Expr
patternSubjectToExpr pat = do
  let subj = PatternCore.value pat
      lbls = labels subj
      elements = PatternCore.elements pat
  if "If" `Set.member` lbls then do
    -- If special form: [:If | cond, then, else] -> (if cond then else)
    case elements of
      [cond, thenExpr, elseExpr] -> do
        condExpr <- patternSubjectToExpr cond
        thenExpr' <- patternSubjectToExpr thenExpr
        elseExpr' <- patternSubjectToExpr elseExpr
        Right $ List [Atom (Symbol "if"), condExpr, thenExpr', elseExpr']
      _ -> Left $ TypeMismatch "If pattern must have 3 elements" (VList [])
  else if "Let" `Set.member` lbls then do
    -- Let special form: [:Let | bindings, body] -> (let bindings body)
    case elements of
      [bindingsExpr, bodyExpr] -> do
        bindings <- patternSubjectToExpr bindingsExpr
        bodyExpr' <- patternSubjectToExpr bodyExpr
        Right $ List [Atom (Symbol "let"), bindings, bodyExpr']
      _ -> Left $ TypeMismatch "Let pattern must have 2 elements" (VList [])
  else if "Begin" `Set.member` lbls then do
    -- Begin special form: [:Begin | expr1, expr2, ...] -> (begin expr1 expr2 ...)
    exprs <- mapM patternSubjectToExpr elements
    Right $ List (Atom (Symbol "begin") : exprs)
  else if "Define" `Set.member` lbls then do
    -- Define special form: [:Define | name, value] -> (define name value)
    case elements of
      [nameExpr, valueExpr] -> do
        name <- patternSubjectToExpr nameExpr
        value <- patternSubjectToExpr valueExpr
        Right $ List [Atom (Symbol "define"), name, value]
      _ -> Left $ TypeMismatch "Define pattern must have 2 elements" (VList [])
  else if "Quote" `Set.member` lbls then do
    -- Quote special form: [:Quote | expr] -> (quote expr) or 'expr
    case elements of
      [exprPat] -> do
        expr <- patternSubjectToExpr exprPat
        Right $ Quote expr
      _ -> Left $ TypeMismatch "Quote pattern must have 1 element" (VList [])
  else if "List" `Set.member` lbls then do
    -- List pattern: extract elements and convert recursively
    exprs <- mapM patternSubjectToExpr elements
    Right $ List exprs
  else do
    -- Atomic pattern: convert decoration Subject to Expr
    subjectToExpr subj

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
valueToSubject (VPattern pat) = 
  -- A Pattern already has a Subject (its decoration). For property storage,
  -- we just return the decoration Subject. Note: This loses the elements,
  -- but Subject properties can't store Patterns directly anyway.
  -- For proper Pattern serialization, use valueToPatternSubjectForGram instead.
  PatternCore.value pat
valueToSubject (VClosure closure) = 
  -- For property storage, we just return the decoration Subject with closure metadata.
  -- Note: This loses the body expression. For proper Closure serialization,
  -- use valueToPatternSubjectForGram or closureToPatternSubject instead.
  Subject
    { identity = SubjectCore.Symbol ""
    , labels = Set.fromList ["Closure"]
    , properties = Map.fromList 
        [ ("params", SubjectValue.VArray (map SubjectValue.VString (params closure)))
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
      -- Support both "value" (Gram serialization) and "text" (legacy property storage)
      case Map.lookup "value" (properties subj) of
        Just (SubjectValue.VString s) -> Right $ VString (T.pack s)
        Nothing -> case Map.lookup "text" (properties subj) of
          Just (SubjectValue.VString s) -> Right $ VString (T.pack s)
          _ -> Left $ TypeMismatch "String Subject missing value or text property" (VList [])
        _ -> Left $ TypeMismatch "String Subject missing value or text property" (VList [])
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
      -- NOTE: Pattern reconstruction from Subject properties is not supported.
      -- The patternToSubject function was incorrect and has been removed.
      -- For Pattern serialization, use patternSubjectToValue instead.
      Left $ TypeMismatch "Pattern Subject reconstruction from properties is not supported. Use patternSubjectToValue for Pattern deserialization." (VList [])
  | "Primitive" `Set.member` labels subj =
      case Map.lookup "name" (properties subj) of
        Just (SubjectValue.VString name) ->
          case primitiveFromName name of
            Just prim -> Right $ VPrimitive prim
            Nothing -> Left $ TypeMismatch ("Unknown primitive name: " ++ name) (VList [])
        _ -> Left $ TypeMismatch "Primitive Subject missing name property" (VList [])
  | otherwise = Left $ TypeMismatch ("Unknown Subject label: " ++ show (Set.toList (labels subj))) (VList [])

-- NOTE: subjectToPatternFromSubject removed - it was used for the incorrect patternToSubject
-- conversion. Patterns should be serialized as Pattern Subject, not converted to Subject.

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
-- NOTE: subjectToPatternFromSubject removed - it was used for the incorrect
-- patternToSubject conversion. Patterns should be serialized as Pattern Subject
-- directly, not converted to Subject for property storage.

-- ============================================================================
-- Gram Serialization Functions (Phase 5)
-- ============================================================================
-- These functions implement the Gram serialization design from
-- docs/plisp-serialization-design.md
--
-- Key design principles:
-- * File-level property records for metadata
-- * Optional Environment section for shared bindings
-- * Expressions as patterns in file sequence
-- * Closure structure: [:Closure | [:Env | ...], [:Lambda | [:Parameters | ...], [:Body | ...]]]
-- * Special form labels: :If, :Let, :Begin, :Define, :Quote
-- * Parameters vs bound values distinction
-- * Binding deduplication by (name, value) pairs
-- * Recursive closure support via forward references
-- * Standard library filtering

-- | Converts a Value to Subject decoration for Gram serialization.
-- This extracts just the Subject decoration (not the full Pattern).
-- For runtime pattern operations that need to create pattern decorations.
valueToSubjectForGram :: Value -> Subject
valueToSubjectForGram (VNumber n) = Subject
  { identity = SubjectCore.Symbol ""
  , labels = Set.fromList ["Number"]
  , properties = Map.fromList [("value", SubjectValue.VInteger n)]
  }
valueToSubjectForGram (VString s) = Subject
  { identity = SubjectCore.Symbol ""
  , labels = Set.fromList ["String"]
  , properties = Map.fromList [("value", SubjectValue.VString (T.unpack s))]
  }
valueToSubjectForGram (VBool b) = Subject
  { identity = SubjectCore.Symbol ""
  , labels = Set.fromList ["Bool"]
  , properties = Map.fromList [("value", SubjectValue.VBoolean b)]
  }
valueToSubjectForGram (VList _) = Subject
  { identity = SubjectCore.Symbol ""
  , labels = Set.fromList ["List"]
  , properties = Map.empty
  }
valueToSubjectForGram (VPattern pat) = PatternCore.value pat
valueToSubjectForGram (VPrimitive prim) = Subject
  { identity = SubjectCore.Symbol ""
  , labels = Set.fromList ["Primitive"]
  , properties = Map.fromList [("name", SubjectValue.VString (primitiveName prim))]
  }
valueToSubjectForGram (VClosure _) = Subject
  { identity = SubjectCore.Symbol ""
  , labels = Set.fromList ["Closure"]
  , properties = Map.empty
  }

-- | Converts a Value to Pattern Subject for Gram serialization.
-- This follows the design in docs/plisp-serialization-design.md
-- This is a pure function for serialization (unlike PatternPrimitives.valueToPatternSubject which is monadic)
valueToPatternSubjectForGram :: Value -> Pattern Subject
valueToPatternSubjectForGram (VNumber n) = pattern $ valueToSubjectForGram (VNumber n)
valueToPatternSubjectForGram (VString s) = pattern $ valueToSubjectForGram (VString s)
valueToPatternSubjectForGram (VBool b) = pattern $ valueToSubjectForGram (VBool b)
valueToPatternSubjectForGram (VList vs) = patternWith
  (valueToSubjectForGram (VList []))
  (map valueToPatternSubjectForGram vs)
valueToPatternSubjectForGram (VPattern pat) = pat
valueToPatternSubjectForGram (VPrimitive prim) = pattern $ valueToSubjectForGram (VPrimitive prim)
valueToPatternSubjectForGram (VClosure closure) = closureToPatternSubject closure

-- | Converts a Pattern Subject back to a Value.
-- This is the inverse of valueToPatternSubject.
patternSubjectToValue :: Pattern Subject -> Either Error Value
patternSubjectToValue pat = do
  let subj = PatternCore.value pat
  case Set.toList (labels subj) of
    ["Number"] -> do
      val <- case Map.lookup "value" (properties subj) of
        Just (SubjectValue.VInteger n) -> Right n
        _ -> Left $ TypeMismatch "Number pattern missing value property" (VList [])
      Right $ VNumber val
    ["String"] -> do
      val <- case Map.lookup "value" (properties subj) of
        Just (SubjectValue.VString s) -> Right s
        _ -> Left $ TypeMismatch "String pattern missing value property" (VList [])
      Right $ VString (T.pack val)
    ["Bool"] -> do
      val <- case Map.lookup "value" (properties subj) of
        Just (SubjectValue.VBoolean b) -> Right b
        _ -> Left $ TypeMismatch "Bool pattern missing value property" (VList [])
      Right $ VBool val
    ["List"] -> do
      let elements = PatternCore.elements pat
      vals <- mapM patternSubjectToValue elements
      Right $ VList vals
    ["Primitive"] -> do
      name <- case Map.lookup "name" (properties subj) of
        Just (SubjectValue.VString n) -> Right n
        _ -> Left $ TypeMismatch "Primitive pattern missing name property" (VList [])
      case primitiveFromName name of
        Just prim -> Right $ VPrimitive prim
        Nothing -> Left $ TypeMismatch ("Unknown primitive name: " ++ name) (VList [])
    ["Closure"] -> do
      closure <- patternSubjectToClosure pat
      Right $ VClosure closure
    _ -> 
      -- Generic pattern: if it doesn't match any specific value type,
      -- it's already a Pattern, so return it as VPattern
      -- This handles patterns that are patterns (like VPattern values)
      Right $ VPattern pat

-- | Pure version of exprToPatternSubject for use in Codec (avoids circular dependency)
-- Handles special forms with explicit labels: :If, :Let, :Begin, :Define, :Quote
exprToPatternSubjectPure :: Expr -> Pattern Subject
exprToPatternSubjectPure (List exprs) =
  -- Check if this is a special form
  case exprs of
    (Atom (Symbol "if")):cond:thenExpr:elseExpr:[] ->
      -- If special form: [:If | cond, then, else]
      let decoration = Subject
            { identity = SubjectCore.Symbol ""
            , labels = Set.fromList ["If"]
            , properties = Map.empty
            }
          elementPatterns = map exprToPatternSubjectPure [cond, thenExpr, elseExpr]
      in patternWith decoration elementPatterns
    (Atom (Symbol "let")):bindingsExpr:bodyExpr:[] ->
      -- Let special form: [:Let | bindings, body]
      let decoration = Subject
            { identity = SubjectCore.Symbol ""
            , labels = Set.fromList ["Let"]
            , properties = Map.empty
            }
          elementPatterns = map exprToPatternSubjectPure [bindingsExpr, bodyExpr]
      in patternWith decoration elementPatterns
    (Atom (Symbol "begin")):rest ->
      -- Begin special form: [:Begin | expr1, expr2, ...]
      let decoration = Subject
            { identity = SubjectCore.Symbol ""
            , labels = Set.fromList ["Begin"]
            , properties = Map.empty
            }
          elementPatterns = map exprToPatternSubjectPure rest
      in patternWith decoration elementPatterns
    (Atom (Symbol "define")):nameExpr:valueExpr:[] ->
      -- Define special form: [:Define | name, value]
      let decoration = Subject
            { identity = SubjectCore.Symbol ""
            , labels = Set.fromList ["Define"]
            , properties = Map.empty
            }
          elementPatterns = map exprToPatternSubjectPure [nameExpr, valueExpr]
      in patternWith decoration elementPatterns
    (Atom (Symbol "quote")):expr:[] ->
      -- Quote special form: [:Quote | expr]
      let decoration = Subject
            { identity = SubjectCore.Symbol ""
            , labels = Set.fromList ["Quote"]
            , properties = Map.empty
            }
          elementPatterns = [exprToPatternSubjectPure expr]
      in patternWith decoration elementPatterns
    _ ->
      -- Regular function call or list: [:List | ...]
      let decoration = Subject
            { identity = SubjectCore.Symbol ""
            , labels = Set.fromList ["List"]
            , properties = Map.empty
            }
          elementPatterns = map exprToPatternSubjectPure exprs
      in patternWith decoration elementPatterns
exprToPatternSubjectPure (Quote expr) =
  -- Quote expression: [:Quote | expr]
  let decoration = Subject
        { identity = SubjectCore.Symbol ""
        , labels = Set.fromList ["Quote"]
        , properties = Map.empty
        }
      elementPatterns = [exprToPatternSubjectPure expr]
  in patternWith decoration elementPatterns
exprToPatternSubjectPure expr =
  -- For atoms: convert to Subject, then wrap in atomic pattern
  let subject = exprToSubject expr
      pat = pattern subject
  in pat

-- | Converts a Closure to Pattern Subject using the design format:
-- [:Closure | [:Env | ...], [:Lambda | [:Parameters | ...], [:Body | ...]]]
-- TODO: Implement binding collection, deduplication, and environment section
closureToPatternSubject :: Closure -> Pattern Subject
closureToPatternSubject (Closure paramNames bodyExpr _capturedEnv) =
  -- Convert body expression to Pattern Subject
  let bodyPattern = exprToPatternSubjectPure bodyExpr
      -- Create parameters pattern with parameter names as elements
      -- Each parameter is a Symbol pattern
      paramPatterns = map (\name -> pattern $ Subject
        { identity = SubjectCore.Symbol ""
        , labels = Set.fromList ["Symbol"]
        , properties = Map.fromList [("name", SubjectValue.VString name)]
        }) paramNames
      paramsPattern = if null paramPatterns
        then pattern $ Subject
          { identity = SubjectCore.Symbol ""
          , labels = Set.fromList ["Parameters"]
          , properties = Map.empty
          }
        else patternWith
          (Subject
            { identity = SubjectCore.Symbol ""
            , labels = Set.fromList ["Parameters"]
            , properties = Map.empty
            })
          paramPatterns
      -- Create environment pattern (empty for now, will be populated with bindings)
      envPattern = pattern $ Subject
        { identity = SubjectCore.Symbol ""
        , labels = Set.fromList ["Env"]
        , properties = Map.empty
        }
      -- Create lambda pattern with parameters and body
      lambdaPattern = patternWith
        (Subject
          { identity = SubjectCore.Symbol ""
          , labels = Set.fromList ["Lambda"]
          , properties = Map.empty
          })
        [paramsPattern, bodyPattern]
  in patternWith
    (Subject
      { identity = SubjectCore.Symbol ""
      , labels = Set.fromList ["Closure"]
      , properties = Map.empty
      })
    [envPattern, lambdaPattern]

-- | Converts a Pattern Subject back to a Closure.
-- Extracts [:Env | ...], [:Lambda | [:Parameters | ...], [:Body | ...]] structure
-- TODO: Implement full binding resolution from environment section
patternSubjectToClosure :: Pattern Subject -> Either Error Closure
patternSubjectToClosure pat = do
  let subj = PatternCore.value pat
  if "Closure" `Set.member` labels subj
    then do
      let elements = PatternCore.elements pat
      -- Closure should have 2 elements: [:Env | ...] and [:Lambda | ...]
      if length elements /= 2
        then Left $ TypeMismatch "Closure pattern must have 2 elements (Env and Lambda)" (VList [])
        else do
          let _envPat = elements !! 0  -- Will be used for binding resolution
              lambdaPat = elements !! 1
          -- Extract environment (for now, use empty - will be resolved from environment section)
          let capturedEnv = initialEnv
          -- Extract lambda structure
          let lambdaSubj = PatternCore.value lambdaPat
          if "Lambda" `Set.member` labels lambdaSubj
            then do
              let lambdaElements = PatternCore.elements lambdaPat
              -- Lambda should have 2 elements: [:Parameters | ...] and [:Body | ...]
              if length lambdaElements /= 2
                then Left $ TypeMismatch "Lambda pattern must have 2 elements (Parameters and Body)" (VList [])
                else do
                  let paramsPat = lambdaElements !! 0
                      bodyPat = lambdaElements !! 1
                  -- Extract parameters
                  let paramsSubj = PatternCore.value paramsPat
                  if "Parameters" `Set.member` labels paramsSubj
                    then do
                      let paramElements = PatternCore.elements paramsPat
                      paramNames <- mapM extractParamName paramElements
                      -- Extract body
                      bodyExpr <- patternSubjectToExpr bodyPat
                      Right $ Closure paramNames bodyExpr capturedEnv
                    else Left $ TypeMismatch "Expected Parameters pattern" (VList [])
            else Left $ TypeMismatch "Expected Lambda pattern" (VList [])
    else Left $ TypeMismatch "Expected Closure pattern" (VList [])

-- | Helper to extract parameter name from a Symbol pattern
extractParamName :: Pattern Subject -> Either Error String
extractParamName pat = do
  let subj = PatternCore.value pat
  if "Symbol" `Set.member` labels subj
    then case Map.lookup "name" (properties subj) of
      Just (SubjectValue.VString name) -> Right name
      _ -> Left $ TypeMismatch "Symbol pattern missing name property" (VList [])
    else Left $ TypeMismatch "Expected Symbol pattern for parameter" (VList [])

-- | Serializes a program (list of values) to Gram notation with file-level structure.
-- TODO: Implement file-level property records, environment section, expressions
programToGram :: [Value] -> Env -> String
programToGram values _runtimeEnv = 
  -- For now, serialize each value separately
  -- Full implementation will:
  -- 1. Create file-level property record {kind: "Pattern Lisp", ...}
  -- 2. Collect all bindings from closures, deduplicate, create Environment section
  -- 3. Serialize expressions as patterns in file sequence
  unlines $ map (\v -> toGram (valueToPatternSubjectForGram v)) values

-- | Deserializes Gram notation to a program (list of values and environment).
-- TODO: Implement file-level parsing, environment section parsing, expressions parsing
gramToProgram :: String -> Either Error ([Value], Env)
gramToProgram gramText = do
  -- For now, parse as single pattern
  -- Full implementation will:
  -- 1. Parse file-level property record
  -- 2. Parse optional Environment section
  -- 3. Parse remaining patterns as expressions
  -- 4. Merge environment with standard library
  pat <- case fromGram gramText of
    Left parseErr -> Left $ ParseError (show parseErr)
    Right p -> Right p
  val <- patternSubjectToValue pat
  Right ([val], initialEnv)

