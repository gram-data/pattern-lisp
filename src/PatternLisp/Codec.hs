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
import Data.List (nubBy)
import Data.Maybe (mapMaybe)
import Control.Monad.State (State, runState, get, modify)
import Control.Monad (foldM)
import Debug.Trace (trace)
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
-- This is a wrapper that calls the version with bindings (empty map)
patternSubjectToExpr :: Pattern Subject -> Either Error Expr
patternSubjectToExpr = patternSubjectToExprWithBindings Map.empty

-- | Converts a Pattern Subject back to expression AST, with support for identifier references
-- Identifier references (patterns with empty labels and non-empty identity) are replaced
-- with variable names from the binding map
patternSubjectToExprWithBindings :: Map.Map SubjectCore.Symbol String -> Pattern Subject -> Either Error Expr
patternSubjectToExprWithBindings bindingMap pat = do
  let subj = PatternCore.value pat
      lbls = labels subj
      elements = PatternCore.elements pat
      -- Check if this is an identifier reference (empty labels, non-empty identity)
      isIdentifierRef = Set.null lbls && identity subj /= SubjectCore.Symbol ""
  if isIdentifierRef
    then do
      -- Identifier reference: look up in binding map to get variable name
      case Map.lookup (identity subj) bindingMap of
        Just varName -> Right $ Atom (Symbol varName)
        Nothing -> Left $ TypeMismatch ("Unknown identifier reference: " ++ show (identity subj)) (VList [])
    else if "If" `Set.member` lbls then do
      -- If special form: [:If | cond, then, else] -> (if cond then else)
      case elements of
        [cond, thenExpr, elseExpr] -> do
          condExpr <- patternSubjectToExprWithBindings bindingMap cond
          thenExpr' <- patternSubjectToExprWithBindings bindingMap thenExpr
          elseExpr' <- patternSubjectToExprWithBindings bindingMap elseExpr
          Right $ List [Atom (Symbol "if"), condExpr, thenExpr', elseExpr']
        _ -> Left $ TypeMismatch "If pattern must have 3 elements" (VList [])
    else if "Let" `Set.member` lbls then do
      -- Let special form: [:Let | bindings, body] -> (let bindings body)
      case elements of
        [bindingsExpr, bodyExpr] -> do
          bindings <- patternSubjectToExprWithBindings bindingMap bindingsExpr
          bodyExpr' <- patternSubjectToExprWithBindings bindingMap bodyExpr
          Right $ List [Atom (Symbol "let"), bindings, bodyExpr']
        _ -> Left $ TypeMismatch "Let pattern must have 2 elements" (VList [])
    else if "Begin" `Set.member` lbls then do
      -- Begin special form: [:Begin | expr1, expr2, ...] -> (begin expr1 expr2 ...)
      exprs <- mapM (patternSubjectToExprWithBindings bindingMap) elements
      Right $ List (Atom (Symbol "begin") : exprs)
    else if "Define" `Set.member` lbls then do
      -- Define special form: [:Define | name, value] -> (define name value)
      case elements of
        [nameExpr, valueExpr] -> do
          name <- patternSubjectToExprWithBindings bindingMap nameExpr
          value <- patternSubjectToExprWithBindings bindingMap valueExpr
          Right $ List [Atom (Symbol "define"), name, value]
        _ -> Left $ TypeMismatch "Define pattern must have 2 elements" (VList [])
    else if "Quote" `Set.member` lbls then do
      -- Quote special form: [:Quote | expr] -> (quote expr) or 'expr
      case elements of
        [exprPat] -> do
          expr <- patternSubjectToExprWithBindings bindingMap exprPat
          Right $ Quote expr
        _ -> Left $ TypeMismatch "Quote pattern must have 1 element" (VList [])
    else if "List" `Set.member` lbls then do
      -- List pattern: extract elements and convert recursively
      exprs <- mapM (patternSubjectToExprWithBindings bindingMap) elements
      Right $ List exprs
    else if "Symbol" `Set.member` lbls then do
      -- Symbol pattern: extract name property (used for parameters)
      case Map.lookup "name" (properties subj) of
        Just (SubjectValue.VString name) -> Right $ Atom (Symbol name)
        _ -> Left $ TypeMismatch "Symbol pattern missing name property" (VList [])
    else do
      -- Atomic pattern: convert decoration Subject to Expr
      -- Try subjectToExpr for other patterns (Var, Number, String, Bool, etc.)
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
  , labels = Set.fromList ["Scope"]
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
  | "Scope" `Set.member` labels subj = do
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
-- * Inline :Scope patterns for lexical scopes (no separate Environment section)
-- * Expressions as patterns in file sequence
-- * Closure structure: [:Closure | [:Scope | ...], [:Lambda | [:Parameters | ...], [:Body | ...]]]
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
valueToPatternSubjectForGram (VPattern pat) = 
  -- A VPattern value is semantically a Pattern Subject with label "Pattern"
  -- containing the inner pattern as an element.
  -- Example: (pattern 42) → [:Pattern | [:Number {value: 42}]]
  patternWith (Subject { identity = SubjectCore.Symbol "", labels = Set.fromList ["Pattern"], properties = Map.empty }) [pat]
valueToPatternSubjectForGram (VPrimitive prim) = pattern $ valueToSubjectForGram (VPrimitive prim)
valueToPatternSubjectForGram (VClosure closure) = closureToPatternSubject closure

-- | Internal version that uses State monad for scope ID generation
-- This allows nested closures to share the same counter for unique IDs
valueToPatternSubjectForGramWithState :: Value -> ScopeIdState (Pattern Subject)
valueToPatternSubjectForGramWithState (VNumber n) = return $ pattern $ valueToSubjectForGram (VNumber n)
valueToPatternSubjectForGramWithState (VString s) = return $ pattern $ valueToSubjectForGram (VString s)
valueToPatternSubjectForGramWithState (VBool b) = return $ pattern $ valueToSubjectForGram (VBool b)
valueToPatternSubjectForGramWithState (VList vs) = do
  elementPatterns <- mapM valueToPatternSubjectForGramWithState vs
  return $ patternWith
    (Subject
      { identity = SubjectCore.Symbol ""
      , labels = Set.fromList ["List"]
      , properties = Map.empty
      })
    elementPatterns
valueToPatternSubjectForGramWithState (VPattern pat) = 
  -- VPattern wraps a Pattern Subject - we need to wrap it with :Pattern label
  return $ patternWith
    (Subject
      { identity = SubjectCore.Symbol ""
      , labels = Set.fromList ["Pattern"]
      , properties = Map.empty
      })
    [pat]
valueToPatternSubjectForGramWithState (VClosure closure) = closureToPatternSubjectWithState closure
valueToPatternSubjectForGramWithState (VPrimitive prim) = return $ pattern $ valueToSubjectForGram (VPrimitive prim)

-- | Converts a Pattern Subject back to a Value.
-- This is the inverse of valueToPatternSubject.
patternSubjectToValue :: Pattern Subject -> Either Error Value
patternSubjectToValue = patternSubjectToValueWithScopeMap Map.empty Set.empty

-- | Converts a Pattern Subject back to a Value, with optional scope map and resolving scopes for nested closures
patternSubjectToValueWithScopeMap :: Map.Map SubjectCore.Symbol (Pattern Subject) -> Set.Set SubjectCore.Symbol -> Pattern Subject -> Either Error Value
patternSubjectToValueWithScopeMap scopeMap resolvingScopes pat = do
  let subj = PatternCore.value pat
  case Set.toList (labels subj) of
    ["Number"] -> do
      val <- case Map.lookup "value" (properties subj) of
        Just (SubjectValue.VInteger n) -> Right n
        _ -> Left $ TypeMismatch "Number pattern missing value property" (VList [])
      Right $ VNumber val
    ["String"] -> do
      -- Support both "value" (Gram serialization) and "text" (legacy property storage)
      val <- case Map.lookup "value" (properties subj) of
        Just (SubjectValue.VString s) -> Right s
        Nothing -> case Map.lookup "text" (properties subj) of
          Just (SubjectValue.VString s) -> Right s
          _ -> Left $ TypeMismatch "String pattern missing value or text property" (VList [])
        _ -> Left $ TypeMismatch "String pattern missing value or text property" (VList [])
      Right $ VString (T.pack val)
    ["Bool"] -> do
      val <- case Map.lookup "value" (properties subj) of
        Just (SubjectValue.VBoolean b) -> Right b
        _ -> Left $ TypeMismatch "Bool pattern missing value property" (VList [])
      Right $ VBool val
    ["List"] -> do
      let elements = PatternCore.elements pat
      vals <- mapM (patternSubjectToValueWithScopeMap scopeMap resolvingScopes) elements
      Right $ VList vals
    ["Primitive"] -> do
      name <- case Map.lookup "name" (properties subj) of
        Just (SubjectValue.VString n) -> Right n
        _ -> Left $ TypeMismatch "Primitive pattern missing name property" (VList [])
      case primitiveFromName name of
        Just prim -> Right $ VPrimitive prim
        Nothing -> Left $ TypeMismatch ("Unknown primitive name: " ++ name) (VList [])
    ["Closure"] -> do
      closure <- patternSubjectToClosure pat scopeMap resolvingScopes
      Right $ VClosure closure
    ["Pattern"] -> do
      -- This is a VPattern value: [:Pattern | innerPattern]
      -- Extract the inner pattern and return it as VPattern
      case PatternCore.elements pat of
        [innerPat] -> Right $ VPattern innerPat
        _ -> Left $ TypeMismatch "Pattern label must have exactly one element" (VList [])
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

-- | Binding information for serialization
data BindingInfo = BindingInfo
  { bindingName :: String
  , bindingValue :: Value  -- Used for deduplication and creating binding patterns
  , bindingIdentifier :: SubjectCore.Symbol  -- Gram identifier for this binding
  }

-- | Collect bindings from environment, filter standard library, deduplicate, and assign identifiers
collectBindings :: Env -> Env -> [BindingInfo]
collectBindings standardLib capturedEnv =
  -- Extract bindings from captured environment
  let bindings = Map.toList capturedEnv
      -- Filter out standard library bindings
      nonStandardBindings = filter (\(name, _) -> not (Map.member name standardLib)) bindings
      -- Deduplicate by (name, value) pairs
      uniqueBindings = nubBy (\(n1, v1) (n2, v2) -> n1 == n2 && valueEqual v1 v2) nonStandardBindings
      -- Assign identifiers (variableName@1, variableName@2, ...)
      -- Using @ as separator since it's less common in variable names than _
      indexed = zip uniqueBindings [1..]
  in map (\((name, val), idx) -> BindingInfo name val (SubjectCore.Symbol (name ++ "@" ++ show (idx :: Int)))) indexed
  where
    -- Value equality for deduplication (structural equality)
    valueEqual :: Value -> Value -> Bool
    valueEqual (VNumber n1) (VNumber n2) = n1 == n2
    valueEqual (VString s1) (VString s2) = s1 == s2
    valueEqual (VBool b1) (VBool b2) = b1 == b2
    valueEqual (VList vs1) (VList vs2) = length vs1 == length vs2 && all (uncurry valueEqual) (zip vs1 vs2)
    valueEqual (VPattern p1) (VPattern p2) = patternEqual p1 p2
    valueEqual (VClosure c1) (VClosure c2) = closureEqual c1 c2
    valueEqual (VPrimitive p1) (VPrimitive p2) = p1 == p2
    valueEqual _ _ = False
    
    -- Pattern equality (by structure, ignoring identity)
    patternEqual :: Pattern Subject -> Pattern Subject -> Bool
    patternEqual p1 p2 =
      let subj1 = PatternCore.value p1
          subj2 = PatternCore.value p2
      in labels subj1 == labels subj2 &&
         properties subj1 == properties subj2 &&
         length (PatternCore.elements p1) == length (PatternCore.elements p2) &&
         all (uncurry patternEqual) (zip (PatternCore.elements p1) (PatternCore.elements p2))
    
    -- Closure equality (by code structure and captured environment)
    closureEqual :: Closure -> Closure -> Bool
    closureEqual (Closure params1 body1 env1) (Closure params2 body2 env2) =
      params1 == params2 &&
      body1 == body2 &&
      -- Compare environments by (name, value) pairs
      Map.toList env1 == Map.toList env2

-- | Converts an expression to Pattern Subject, with special handling for identifier references
-- Identifier references are patterns with just the identifier (no labels, identity = identifier)
-- If bindingMap is empty, falls back to exprToPatternSubjectPure for efficiency
exprToPatternSubjectWithBindings :: Expr -> Map.Map String SubjectCore.Symbol -> [String] -> Pattern Subject
exprToPatternSubjectWithBindings expr bindingMap paramNames
  | Map.null bindingMap = exprToPatternSubjectPure expr  -- No bindings to transform, use pure version
  | otherwise = transformExprWithBindings expr bindingMap paramNames
  where
    transformExprWithBindings :: Expr -> Map.Map String SubjectCore.Symbol -> [String] -> Pattern Subject
    transformExprWithBindings expr' bindingMap' paramNames' = case expr' of
      -- Symbol: check if it's a bound variable or parameter
      Atom (Symbol name) ->
        if name `elem` paramNames'
          then 
            -- Parameter: create Symbol pattern
            let subject = Subject
                  { identity = SubjectCore.Symbol ""
                  , labels = Set.fromList ["Symbol"]
                  , properties = Map.fromList [("name", SubjectValue.VString name)]
                  }
            in pattern subject
          else case Map.lookup name bindingMap' of
            Just identifier ->
              -- Bound variable: create identifier reference pattern
              -- Identifier reference is a pattern with just the identifier (empty labels)
              pattern $ Subject
                { identity = identifier
                , labels = Set.empty
                , properties = Map.empty
                }
            Nothing ->
              -- Regular symbol (not bound, not parameter): create Symbol pattern
              let subject = Subject
                    { identity = SubjectCore.Symbol ""
                    , labels = Set.fromList ["Symbol"]
                    , properties = Map.fromList [("name", SubjectValue.VString name)]
                    }
              in pattern subject
      -- Lists: handle special forms and function calls
      List exprs' ->
        case exprs' of
          (Atom (Symbol "if")):cond:thenExpr:elseExpr:[] ->
            let decoration = Subject
                  { identity = SubjectCore.Symbol ""
                  , labels = Set.fromList ["If"]
                  , properties = Map.empty
                  }
                elementPatterns = map (\e -> transformExprWithBindings e bindingMap' paramNames') [cond, thenExpr, elseExpr]
            in patternWith decoration elementPatterns
          (Atom (Symbol "let")):bindingsExpr:bodyExpr:[] ->
            let decoration = Subject
                  { identity = SubjectCore.Symbol ""
                  , labels = Set.fromList ["Let"]
                  , properties = Map.empty
                  }
                elementPatterns = map (\e -> transformExprWithBindings e bindingMap' paramNames') [bindingsExpr, bodyExpr]
            in patternWith decoration elementPatterns
          (Atom (Symbol "begin")):rest ->
            let decoration = Subject
                  { identity = SubjectCore.Symbol ""
                  , labels = Set.fromList ["Begin"]
                  , properties = Map.empty
                  }
                elementPatterns = map (\e -> transformExprWithBindings e bindingMap' paramNames') rest
            in patternWith decoration elementPatterns
          (Atom (Symbol "define")):nameExpr:valueExpr:[] ->
            let decoration = Subject
                  { identity = SubjectCore.Symbol ""
                  , labels = Set.fromList ["Define"]
                  , properties = Map.empty
                  }
                elementPatterns = map (\e -> transformExprWithBindings e bindingMap' paramNames') [nameExpr, valueExpr]
            in patternWith decoration elementPatterns
          (Atom (Symbol "quote")):expr'':[] ->
            let decoration = Subject
                  { identity = SubjectCore.Symbol ""
                  , labels = Set.fromList ["Quote"]
                  , properties = Map.empty
                  }
                elementPatterns = [transformExprWithBindings expr'' bindingMap' paramNames']
            in patternWith decoration elementPatterns
          _ ->
            -- Regular function call or list
            let decoration = Subject
                  { identity = SubjectCore.Symbol ""
                  , labels = Set.fromList ["List"]
                  , properties = Map.empty
                  }
                elementPatterns = map (\e -> transformExprWithBindings e bindingMap' paramNames') exprs'
            in patternWith decoration elementPatterns
      -- Quote: transform the inner expression
      Quote expr'' ->
        let decoration = Subject
              { identity = SubjectCore.Symbol ""
              , labels = Set.fromList ["Quote"]
              , properties = Map.empty
              }
            elementPatterns = [transformExprWithBindings expr'' bindingMap' paramNames']
        in patternWith decoration elementPatterns
      -- Other atoms: convert normally
      _ ->
        let subject = exprToSubject expr'
        in pattern subject

-- | State for tracking scope ID generation
type ScopeIdState = State Int

-- | Generate next unique scope ID (e1, e2, e3, ...)
nextScopeId :: ScopeIdState SubjectCore.Symbol
nextScopeId = do
  counter <- get
  modify (+1)
  return $ SubjectCore.Symbol ("e" ++ show counter)

-- | Converts a Closure to Pattern Subject using inline :Scope pattern format:
-- [:Closure | [e1:Scope | e0, [bindings...]], [:Lambda | [:Parameters | ...], [:Body | ...]]]
-- 
-- The :Scope pattern is inlined with:
-- - Parent scope reference (identifier or empty pattern for program-level)
-- - Binding patterns directly in the :Scope pattern
-- 
-- Uses State monad to generate unique scope IDs
closureToPatternSubjectWithState :: Closure -> ScopeIdState (Pattern Subject)
closureToPatternSubjectWithState (Closure paramNames bodyExpr capturedEnv) = do
  -- Collect and process bindings from captured environment
  -- DEBUG: Log original environment
  let bindingInfos = collectBindings initialEnv capturedEnv
      _ = trace ("[SERIALIZE] closureToPatternSubjectWithState: originalEnvSize=" ++ show (Map.size capturedEnv) ++
                 ", originalKeys=" ++ show (Map.keys capturedEnv) ++
                 ", collectedBindings=" ++ show (map bindingName bindingInfos)) ()
      -- Create a map from binding names to identifiers for body transformation
      bindingMap = Map.fromList $ map (\bi -> (bindingName bi, bindingIdentifier bi)) bindingInfos
      -- Convert body expression to Pattern Subject, replacing bound variables with identifier references
      bodyPattern = exprToPatternSubjectWithBindings bodyExpr bindingMap paramNames
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
      -- Create inline :Scope pattern with parent reference and binding patterns
      -- Format: [e1:Scope | parent_ref, [binding1...], [binding2...], ...]
      -- Parent reference can be:
      --   - Empty pattern ([]) for program-level
      --   - Identifier reference (e0) for shared parent scopes
      --   - Inlined parent scope pattern for nested closures (round-trip support)
      -- 
      -- For nested closures: When a closure captures another closure, the captured closure's
      -- scope should reference the capturing closure's scope as its parent to preserve
      -- the lexical scope hierarchy.
      parentScopeRef = pattern $ Subject
        { identity = SubjectCore.Symbol ""  -- Empty identity = program-level
        , labels = Set.empty
        , properties = Map.empty
        }
  -- Generate unique scope ID for this closure's scope (needed for nested closure parent references)
  scopeId <- nextScopeId
  -- Create binding patterns directly in the :Scope pattern
  -- Note: Serializing binding values may create nested closures, which also need scope IDs
  -- If a binding value is a closure, its scope should reference this closure's scope as parent
  bindingPatterns <- mapM (\bi -> do
    valuePat <- case bindingValue bi of
      VClosure nestedClosure -> do
        -- This is a nested closure - serialize it first
        nestedClosurePat <- closureToPatternSubjectWithState nestedClosure
        -- Extract the nested closure's scope pattern and update its parent reference
        let nestedElements = PatternCore.elements nestedClosurePat
        case nestedElements of
          [] -> return nestedClosurePat  -- Shouldn't happen, but handle gracefully
          (nestedScopePat : restNestedElements) -> do
            let nestedScopeSubj = PatternCore.value nestedScopePat
                nestedScopeElements = PatternCore.elements nestedScopePat
            -- Replace the nested scope's parent reference with this scope's ID
            -- Format: [e2:Scope | e1, ...] where e1 is this scope's ID
            case nestedScopeElements of
              [] -> return nestedClosurePat  -- Shouldn't happen
              (_oldParentRef : nestedBindings) -> do
                let newParentRef = pattern $ Subject
                      { identity = scopeId  -- Reference to this closure's scope
                      , labels = Set.empty
                      , properties = Map.empty
                      }
                    newNestedScopeElements = newParentRef : nestedBindings
                    newNestedScopePat = patternWith nestedScopeSubj newNestedScopeElements
                    newNestedElements = newNestedScopePat : restNestedElements
                return $ patternWith (PatternCore.value nestedClosurePat) newNestedElements
      _ -> do
        -- Not a closure - serialize normally
        valueToPatternSubjectForGramWithState (bindingValue bi)
    return $ patternWith
      (Subject
        { identity = bindingIdentifier bi
        , labels = Set.fromList ["Binding"]
        , properties = Map.fromList [("name", SubjectValue.VString (bindingName bi))]
        })
      [valuePat]
    ) bindingInfos
  -- Build :Scope pattern elements: [parent_ref, binding1, binding2, ...]
  -- Always include parent reference (even if empty for program-level)
  let scopeElements = parentScopeRef : bindingPatterns
      scopePattern = patternWith
        (Subject
          { identity = scopeId
          , labels = Set.fromList ["Scope"]
          , properties = Map.empty
          })
        scopeElements
      -- Create lambda pattern with parameters and body
      lambdaPattern = patternWith
        (Subject
          { identity = SubjectCore.Symbol ""
          , labels = Set.fromList ["Lambda"]
          , properties = Map.empty
          })
        [paramsPattern, bodyPattern]
  return $ patternWith
    (Subject
      { identity = SubjectCore.Symbol ""
      , labels = Set.fromList ["Closure"]
      , properties = Map.empty
      })
    [scopePattern, lambdaPattern]

-- | Wrapper that uses State monad to generate unique scope IDs
closureToPatternSubject :: Closure -> Pattern Subject
closureToPatternSubject closure = 
  -- Use State monad to generate unique scope IDs, starting from counter 1
  let (result, _) = runState (closureToPatternSubjectWithState closure) 1
  in result

-- | Extracts parent scope reference and bindings from inline :Scope pattern
-- Format: [e1:Scope | parent_ref, [binding1...], [binding2...], ...]
-- Returns: (parent_scope_id_or_pattern, bindings)
-- parent_scope_id_or_pattern is:
--   - Nothing for program-level (empty pattern)
--   - Just (Left identifier) for identifier reference to parent
--   - Just (Right pattern) for inlined parent scope pattern
extractScopeStructure :: Pattern Subject -> Either Error (Maybe (Either SubjectCore.Symbol (Pattern Subject)), [Pattern Subject])
extractScopeStructure scopePat = do
  let scopeSubj = PatternCore.value scopePat
      scopeId = identity scopeSubj
  if "Scope" `Set.member` labels scopeSubj
    then do
      let scopeElements = PatternCore.elements scopePat
          _ = trace ("[DESERIALIZE] extractScopeStructure: scopeId=" ++ show scopeId ++
                     ", scopeElementsCount=" ++ show (length scopeElements) ++
                     ", scopeElementLabels=" ++ show (map (\e -> Set.toList (labels (PatternCore.value e))) scopeElements)) ()
      case scopeElements of
        [] -> Right (Nothing, [])  -- Empty scope (program-level)
        parentRef : bindings -> do
          -- First element is parent scope reference
          let parentSubj = PatternCore.value parentRef
              _ = trace ("[DESERIALIZE] extractScopeStructure: parentRefLabels=" ++ show (Set.toList (labels parentSubj)) ++
                         ", parentRefIdentity=" ++ show (identity parentSubj) ++
                         ", bindingsCount=" ++ show (length bindings) ++
                         ", bindingLabels=" ++ show (map (\b -> Set.toList (labels (PatternCore.value b))) bindings)) ()
          parentScope <- if Set.null (labels parentSubj) && identity parentSubj == SubjectCore.Symbol ""
            then Right Nothing  -- Empty = program-level
            else do
              -- Check if it's an identifier reference or an inlined scope pattern
              if "Scope" `Set.member` labels parentSubj
                then Right (Just (Right parentRef))  -- Inlined parent scope pattern
              else if Set.null (labels parentSubj) && identity parentSubj /= SubjectCore.Symbol ""
                then Right (Just (Left (identity parentSubj)))  -- Identifier reference to parent
                else Left $ TypeMismatch "Parent scope reference must be identifier, empty pattern, or :Scope pattern" (VList [])
          Right (parentScope, bindings)
    else Left $ TypeMismatch "Expected Scope pattern" (VList [])

-- | Extracts binding from a Binding pattern
extractBindingFromPattern :: Map.Map SubjectCore.Symbol (Pattern Subject) -> Set.Set SubjectCore.Symbol -> Pattern Subject -> Either Error (String, Value)
extractBindingFromPattern scopeMap resolvingScopes bindingPat = do
  let bindingSubj = PatternCore.value bindingPat
      bindingLabels = Set.toList (labels bindingSubj)
      bindingId = identity bindingSubj
      _ = trace ("[DESERIALIZE] extractBindingFromPattern: bindingId=" ++ show bindingId ++
                 ", bindingLabels=" ++ show bindingLabels ++
                 ", hasBindingLabel=" ++ show ("Binding" `Set.member` labels bindingSubj)) ()
  if "Binding" `Set.member` labels bindingSubj
    then do
      -- Extract name from properties
      name <- case Map.lookup "name" (properties bindingSubj) of
        Just (SubjectValue.VString n) -> 
          let _ = trace ("[DESERIALIZE] extractBindingFromPattern: extracted name=" ++ n) ()
          in Right n
        _ -> Left $ TypeMismatch ("Binding pattern missing name property. Binding labels: " ++ show bindingLabels ++ ", id: " ++ show bindingId) (VList [])
      -- Extract value from single element
      let bindingElements = PatternCore.elements bindingPat
          _ = trace ("[DESERIALIZE] extractBindingFromPattern: bindingElementsCount=" ++ show (length bindingElements)) ()
      case bindingElements of
        [valuePat] -> do
          let _ = trace ("[DESERIALIZE] extractBindingFromPattern: valuePattern labels=" ++ 
                         show (Set.toList (labels (PatternCore.value valuePat))) ++ 
                         ", valueId=" ++ show (identity (PatternCore.value valuePat))) ()
          value <- case patternSubjectToValueWithScopeMap scopeMap resolvingScopes valuePat of
            Left err -> 
              let _ = trace ("[DESERIALIZE] extractBindingFromPattern: patternSubjectToValueWithScopeMap failed: " ++ show err) ()
              in Left err
            Right v -> 
              let _ = trace ("[DESERIALIZE] extractBindingFromPattern: extracted value=" ++ show v) ()
              in Right v
          let _ = trace ("[DESERIALIZE] extractBindingFromPattern: returning (" ++ name ++ ", " ++ show value ++ ")") ()
          Right (name, value)
        _ -> Left $ TypeMismatch ("Binding pattern must have exactly one element (the value). Found " ++ show (length bindingElements) ++ " elements. Binding labels: " ++ show bindingLabels) (VList [])
    else Left $ TypeMismatch ("Expected Binding pattern, got labels: " ++ show bindingLabels ++ ", id: " ++ show bindingId) (VList [])

-- | Resolves bindings from a scope pattern by following parent chain
-- For round-trip tests, we need to resolve parent scopes from the serialized structure
-- This is a helper that extracts all bindings from a :Scope pattern and its parents
-- Takes a set of scope IDs currently being resolved to detect cycles
resolveScopeBindings :: Pattern Subject -> Map.Map SubjectCore.Symbol (Pattern Subject) -> Set.Set SubjectCore.Symbol -> Either Error [(String, Value)]
resolveScopeBindings scopePat scopeMap resolvingScopes = do
  let scopeSubj = PatternCore.value scopePat
      scopeId = identity scopeSubj
  -- DEBUG: Log scope resolution
  let debugMsg = "resolveScopeBindings: scopeId=" ++ show scopeId ++ ", resolvingScopes=" ++ show (Set.toList resolvingScopes)
  -- Check for cycles: if we're already resolving this exact scope, return empty to break recursion
  if scopeId /= SubjectCore.Symbol "" && scopeId `Set.member` resolvingScopes
    then do
      -- DEBUG: Log cycle detection
      let _ = trace ("[DESERIALIZE] CYCLE DETECTED: " ++ debugMsg) ()
      Right []  -- Cycle detected - we're already resolving this scope, return empty to break recursion
    else do
      let newResolvingScopes = if scopeId /= SubjectCore.Symbol ""
            then Set.insert scopeId resolvingScopes
            else resolvingScopes
      (parentScopeRef, bindingPatterns) <- extractScopeStructure scopePat
      -- DEBUG: Log what extractScopeStructure returned
      let bindingPatternsCount = length bindingPatterns
          _ = trace ("[DESERIALIZE] resolveScopeBindings: extractScopeStructure returned " ++ show bindingPatternsCount ++ 
                     " binding patterns. Labels: " ++ show (map (\bp -> Set.toList (labels (PatternCore.value bp))) bindingPatterns)) ()
      -- FORCE ERROR if bindingPatterns is unexpectedly empty when scope has elements
      let _ = if bindingPatternsCount == 0 && length (PatternCore.elements scopePat) > 1
            then error $ "CRITICAL: resolveScopeBindings: extractScopeStructure returned 0 bindingPatterns but scope has " ++ 
                        show (length (PatternCore.elements scopePat)) ++ " elements! Scope elements: " ++ 
                        show (map (\e -> (Set.toList (labels (PatternCore.value e)), identity (PatternCore.value e))) (PatternCore.elements scopePat))
            else ()
      -- Resolve parent bindings FIRST, before extracting direct bindings
      -- This ensures parent bindings are available when extracting nested closures that reference them
      parentBindings <- case parentScopeRef of
        Nothing -> Right []  -- Program-level, no parent
        Just (Right parentScopePat) -> do
          -- Inlined parent scope pattern - resolve it directly
          resolveScopeBindings parentScopePat scopeMap newResolvingScopes
        Just (Left parentId) -> do
          -- Identifier reference - lookup parent scope in map
          case Map.lookup parentId scopeMap of
            Just parentScopePat -> resolveScopeBindings parentScopePat scopeMap newResolvingScopes
            Nothing -> Left $ TypeMismatch ("Parent scope not found: " ++ show parentId) (VList [])
      -- Extract direct bindings AFTER resolving parent bindings
      -- This allows nested closures to use parent bindings that are already resolved
      -- Pass parent bindings in scope map so nested closures can access them
      -- DEBUG: Verify bindingPatterns before extraction
      let bindingCount = length bindingPatterns
          bindingDebug = map (\bp ->
            let subj = PatternCore.value bp
            in (Set.toList (labels subj), identity subj, Map.keys (properties subj))
            ) bindingPatterns
          -- FORCE ERROR if we have patterns but count is wrong
          _ = if bindingCount > 0 && null bindingPatterns
            then error "CRITICAL BUG: bindingCount > 0 but bindingPatterns is null!"
            else if bindingCount == 0 && not (null bindingPatterns)
            then error "CRITICAL BUG: bindingCount == 0 but bindingPatterns is not null!"
            else ()
      -- Try to extract each binding individually to see which one fails
      -- Extract bindings one by one to get detailed error info
      directBindings <- if bindingCount == 0
        then Right []  -- No bindings to extract
        else do
          -- CRITICAL: Verify bindingPatterns is not empty before foldM
          if null bindingPatterns
            then Left $ TypeMismatch ("resolveScopeBindings: bindingPatterns is empty but bindingCount=" ++ show bindingCount ++ 
                                      ". This should not happen!") (VList [])
            else do
              -- Force evaluation and add error context - this WILL be called if bindingCount > 0
              bindings <- case foldM (\acc bp -> do
                    let bindingSubj = PatternCore.value bp
                        bindingLabels = Set.toList (labels bindingSubj)
                        bindingId = identity bindingSubj
                        bindingElements = PatternCore.elements bp
                        bindingElementCount = length bindingElements
                    -- Call extractBindingFromPattern and wrap any error with context
                    case extractBindingFromPattern scopeMap newResolvingScopes bp of
                      Left err -> Left $ TypeMismatch ("Failed to extract binding #" ++ show (length acc + 1) ++ 
                                                       ". Binding labels: " ++ show bindingLabels ++ 
                                                       ", id: " ++ show bindingId ++ 
                                                       ", elementCount: " ++ show bindingElementCount ++
                                                       ". Error: " ++ show err) (VList [])
                      Right binding -> Right (acc ++ [binding])
                  ) [] bindingPatterns of
                Left err -> 
                  let _ = error $ "CRITICAL: foldM returned Left error: " ++ show err
                  in Left err
                Right bs -> 
                  -- ALWAYS check if we got empty results when we expected bindings
                  let _ = if null bs && bindingCount > 0
                        then error $ "CRITICAL: foldM returned empty list! bindingCount=" ++ show bindingCount ++ 
                                    ", bindingPatterns length=" ++ show (length bindingPatterns) ++
                                    ", bs=" ++ show bs ++
                                    ". Binding pattern details: " ++ show bindingDebug
                        else ()
                  in if null bs
                    then Left $ TypeMismatch ("resolveScopeBindings: foldM returned empty list. bindingCount=" ++ show bindingCount ++ 
                                              ", bindingPatterns length=" ++ show (length bindingPatterns) ++
                                              ". Binding pattern details: " ++ show bindingDebug) (VList [])
                    else Right bs
              -- Double-check: if we have patterns but got no bindings, that's definitely an error
              if null bindings && bindingCount > 0
                then Left $ TypeMismatch ("resolveScopeBindings: FINAL CHECK - extracted 0 bindings from " ++ show bindingCount ++ 
                                          " binding patterns after foldM. Binding pattern details: " ++ show bindingDebug) (VList [])
                else Right bindings
      -- DEBUG: Log binding extraction
      let extractedCount = length directBindings
          bindingNames = map fst directBindings
          debugInfo = "bindingPatternsCount=" ++ show bindingCount ++
                     ", extractedCount=" ++ show extractedCount ++
                     ", bindingNames=" ++ show bindingNames ++
                     ", parentBindings=" ++ show (map fst parentBindings) ++
                     ", bindingPatternDetails=" ++ show bindingDebug
      let _ = trace ("[DESERIALIZE] " ++ debugMsg ++ ", " ++ debugInfo) ()
      -- Merge: parent bindings first, then direct (direct shadows parent)
      -- Use Map.fromList with later values overriding earlier ones
      let allBindings = Map.toList $ Map.fromList (parentBindings ++ directBindings)
      Right allBindings

-- | Builds a map of scope identifiers to scope patterns from a closure pattern
-- This extracts all :Scope patterns that are referenced (for parent resolution)
-- Recursively collects scopes from nested closures
buildScopeMap :: Pattern Subject -> Map.Map SubjectCore.Symbol (Pattern Subject)
buildScopeMap pat = 
  let subj = PatternCore.value pat
  in if "Closure" `Set.member` labels subj
    then
      let elements = PatternCore.elements pat
      in if length elements >= 1
        then
          let scopePat = elements !! 0
              scopeSubj = PatternCore.value scopePat
              scopeId = identity scopeSubj
              -- Add this scope to map
              scopeMap = if "Scope" `Set.member` labels scopeSubj && scopeId /= SubjectCore.Symbol ""
                then Map.singleton scopeId scopePat
                else Map.empty
              -- Recursively collect scopes from bindings (if any are closures)
              bindingPatterns = case PatternCore.elements scopePat of
                _parentRef : bindings -> bindings
                [] -> []
              closureScopes = Map.unions $ map (\bindingPat ->
                let bindingElements = PatternCore.elements bindingPat
                in case bindingElements of
                  [valuePat] -> buildScopeMap valuePat  -- Recursively check if value is a closure
                  _ -> Map.empty
                ) bindingPatterns
          in Map.union scopeMap closureScopes
        else Map.empty
    else Map.empty

-- | Converts a Pattern Subject back to a Closure.
-- Extracts inline :Scope pattern: [e1:Scope | parent_ref, [bindings...]]
-- Then extracts [:Lambda | [:Parameters | ...], [:Body | ...]] structure
-- Resolves parent scope chain for round-trip support
-- Takes an optional outer scope map and resolving scopes to resolve parent references in nested closures
patternSubjectToClosure :: Pattern Subject -> Map.Map SubjectCore.Symbol (Pattern Subject) -> Set.Set SubjectCore.Symbol -> Either Error Closure
patternSubjectToClosure pat outerScopeMap resolvingScopes = do
  let subj = PatternCore.value pat
  if "Closure" `Set.member` labels subj
    then do
      let elements = PatternCore.elements pat
      -- Closure should have 2 elements: [e1:Scope | ...] and [:Lambda | ...]
      if length elements /= 2
        then Left $ TypeMismatch "Closure pattern must have 2 elements (Scope and Lambda)" (VList [])
        else do
          let scopePat = elements !! 0
              lambdaPat = elements !! 1
          -- Build scope map from this closure and any nested closures
          -- Merge with outer scope map to resolve parent references
          let localScopeMap = buildScopeMap pat
              scopeMap = Map.union localScopeMap outerScopeMap
          -- Resolve all bindings (including from parent chain)
          -- First, check what extractScopeStructure returns
          case extractScopeStructure scopePat of
            Left err -> Left err
            Right (_, bindingPatterns) -> do
              -- DEBUG: Log what we found
              let bindingPatternDetails = map (\bp ->
                    let bindingSubj = PatternCore.value bp
                    in (Set.toList (labels bindingSubj), identity bindingSubj, Map.keys (properties bindingSubj))
                    ) bindingPatterns
              -- Now resolve bindings
              -- NOTE: Don't add scopeId to resolvingScopes here - resolveScopeBindings will do it internally
              allBindings <- resolveScopeBindings scopePat scopeMap resolvingScopes
              -- Build captured environment: merge with standard library
              let serializedBindings = Map.fromList allBindings
                  capturedEnv = Map.union serializedBindings initialEnv
                  scopeElements = PatternCore.elements scopePat
                  scopeElementsCount = length scopeElements
                  debugInfo = "extractScopeStructure found " ++ show (length bindingPatterns) ++ " binding patterns: " ++ show bindingPatternDetails ++
                             ". resolveScopeBindings returned " ++ show (length allBindings) ++ " bindings: " ++ show (map fst allBindings) ++
                             ". Scope has " ++ show scopeElementsCount ++ " elements: " ++ show (map (\e -> (Set.toList (labels (PatternCore.value e)), identity (PatternCore.value e))) scopeElements)
              -- If we got no bindings but extractScopeStructure found binding patterns, this is an error
              if null allBindings && not (null bindingPatterns)
                then Left $ TypeMismatch ("No bindings resolved from scope. " ++ debugInfo) (VList [])
                else do
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
                              -- Build identifier-to-name map from binding patterns for body transformation
                              -- Extract identifiers from binding patterns (they have the identifier as their identity)
                              let identifierToName = Map.fromList $ 
                                    mapMaybe (\bp -> do
                                      let bindingSubj = PatternCore.value bp
                                      if "Binding" `Set.member` labels bindingSubj
                                        then do
                                          name <- case Map.lookup "name" (properties bindingSubj) of
                                            Just (SubjectValue.VString n) -> Just n
                                            _ -> Nothing
                                          let identifier = identity bindingSubj
                                          if identifier /= SubjectCore.Symbol ""
                                            then Just (identifier, name)
                                            else Nothing
                                        else Nothing
                                      ) bindingPatterns
                              -- Extract body with identifier resolution
                              bodyExpr <- patternSubjectToExprWithBindings identifierToName bodyPat
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
-- Format:
--   { kind: "Pattern Lisp" }
--   expr1
--   expr2
--   ...
-- Note: No separate Environment section - scopes are inlined in :Scope patterns
programToGram :: [Value] -> Env -> String
programToGram values _runtimeEnv = 
  -- Create file-level property record pattern
  let fileMetadata = patternWith
        (Subject
          { identity = SubjectCore.Symbol ""
          , labels = Set.empty
          , properties = Map.fromList [("kind", SubjectValue.VString "Pattern Lisp")]
          })
        []
      -- Serialize each value as a pattern
      valuePatterns = map valueToPatternSubjectForGram values
      -- Combine file metadata with value patterns
      -- Gram files are sequences of patterns, so we serialize them separately
      metadataGram = toGram fileMetadata
      valueGrams = map toGram valuePatterns
  in unlines (metadataGram : valueGrams)

-- | Deserializes Gram notation to a program (list of values and environment).
-- Expects format:
--   { kind: "Pattern Lisp" }
--   expr1
--   expr2
--   ...
-- Note: No separate Environment section - scopes are inlined in :Scope patterns
gramToProgram :: String -> Either Error ([Value], Env)
gramToProgram gramText = do
  -- Split by lines and parse each pattern
  let lines' = filter (not . null) $ map (dropWhile (== ' ')) $ lines gramText
  case lines' of
    [] -> Left $ TypeMismatch "Empty Gram file" (VList [])
    (metadataLine : valueLines) -> do
      -- Parse first pattern as file metadata
      metadataPat <- case fromGram metadataLine of
        Left parseErr -> Left $ ParseError (show parseErr)
        Right p -> Right p
      -- Verify it's the file metadata (has kind property)
      let metadataSubj = PatternCore.value metadataPat
          kindProp = Map.lookup "kind" (properties metadataSubj)
      case kindProp of
        Just (SubjectValue.VString "Pattern Lisp") -> do
          -- Parse remaining patterns as expressions
          valuePatterns <- mapM (\line -> case fromGram line of
            Left parseErr -> Left $ ParseError (show parseErr)
            Right p -> Right p
            ) valueLines
          -- Convert each pattern to a value
          values <- mapM patternSubjectToValue valuePatterns
          -- Return values with standard library environment
          Right (values, initialEnv)
        _ -> Left $ TypeMismatch "File missing 'kind: Pattern Lisp' property record" (VList [])

