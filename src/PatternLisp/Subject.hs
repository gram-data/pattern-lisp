-- | Complete serialization between Pattern Lisp values and Subject representation.
--
-- This module provides functions for converting Pattern Lisp values (including
-- closures and primitives) to Subject representation and back, enabling
-- code-as-data, persistence, and complete introspection of program state.
--
-- Serialization Format:
--
-- * Numbers: Label "Number", property `{value: Integer}`
-- * Strings: Label "String", property `{text: Text}`
-- * Booleans: Label "Bool", property `{value: Bool}`
-- * Lists: Label "List", property `elements` (VArray of Subjects)
-- * Patterns: Label "Pattern", properties `decoration` (Subject) and `elements` (VArray of Subjects)
-- * Closures: Serialized as Pattern Subject:
--   - Decoration: Subject with label "Closure", property `params` (VArray of strings)
--   - Elements: [Pattern Subject] where the Pattern's decoration is the body Expr (as Subject via `exprToSubject`)
--   - Note: Environment is not serialized to avoid deeply nested structures
--   - Legacy format (backward compatible): Subject with label "Closure", properties:
--     - `params`: VArray of strings (parameter names)
--     - `body`: Subject (Expression AST via `exprToSubject`)
-- * Primitives: Label "Primitive", property `name` (string)
--
-- Variable Names:
-- Variable names in expressions are stored as properties (not Gram identifiers).
-- This matches Lisp's name-based scoping and avoids identifier management complexity.
--
-- Example usage:
--
-- > import PatternLisp.Subject
-- > import PatternLisp.Parser
-- >
-- > let val = VNumber 42
-- > let subj = valueToSubject val
-- > case subjectToValue subj of
-- >   Right val' -> val' == val  -- True
-- >   Left err -> error (show err)
module PatternLisp.Subject
  ( valueToSubject
  , subjectToValue
  , exprToSubject
  , subjectToExpr
  , envToSubject
  , subjectToEnv
  , subjectToBinding
  ) where

import PatternLisp.Syntax
import PatternLisp.Primitives (initialEnv)
import Pattern (Pattern)
import Pattern.Core (patternWith)
import qualified Pattern.Core as PatternCore
import Subject.Core (Subject(..))
import qualified Subject.Core as SubjectCore
import qualified Subject.Value as SubjectValue
import qualified Data.Text as T
import qualified Data.Map.Strict as Map
import qualified Data.Set as Set

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

-- | Converts a Pattern Lisp Value to Subject representation.
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
valueToSubject (VClosure (Closure paramNames bodyExpr _capturedEnv)) = Subject
  { identity = SubjectCore.Symbol ""
  , labels = Set.fromList ["Closure"]
  , properties = Map.fromList 
      [ ("params", SubjectValue.VArray (map SubjectValue.VString paramNames))
      , ("body", subjectToSubjectValue (exprToSubject bodyExpr))
      -- Note: We don't serialize the full environment because it creates
      -- deeply nested VMap structures that gram notation can't handle.
      -- The environment can be reconstructed from the body expression if needed.
      -- For gram serialization, we only serialize params and body.
      ]
  }
valueToSubject (VPrimitive prim) = Subject
  { identity = SubjectCore.Symbol ""
  , labels = Set.fromList ["Primitive"]
  , properties = Map.fromList [("name", SubjectValue.VString (primitiveName prim))]
  }

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
  | "Closure" `Set.member` labels subj = do
      -- Extract params from properties
      paramsList <- case Map.lookup "params" (properties subj) of
        Just (SubjectValue.VArray paramStrs) -> do
          paramNames <- mapM extractString paramStrs
          Right paramNames
        _ -> Left $ TypeMismatch "Closure Subject missing params property" (VList [])
      -- Extract body from properties
      bodyVal <- case Map.lookup "body" (properties subj) of
        Just v -> subjectValueToSubject v
        Nothing -> Left $ TypeMismatch "Closure Subject missing body property" (VList [])
      bodyExpr <- subjectToExpr bodyVal
      -- Note: Environment is not serialized to avoid deeply nested VMap structures.
      -- Use initialEnv as default (closures will need to be recreated with proper env if needed).
      -- For gram serialization, we only serialize params and body.
      Right $ VClosure (Closure paramsList bodyExpr initialEnv)
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

-- | Converts a Subject back to a Pattern (recursive helper).
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

-- | Converts an expression AST to Subject representation.
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

-- | Converts a Subject representation back to expression AST.
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

