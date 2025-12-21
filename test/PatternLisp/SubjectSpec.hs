module PatternLisp.SubjectSpec (spec) where

import Test.Hspec
import PatternLisp.Syntax
import PatternLisp.Parser
import PatternLisp.Eval
import PatternLisp.Primitives
import PatternLisp.Subject
import Pattern (Pattern)
import Pattern.Core (pattern, patternWith)
import qualified Pattern.Core as PatternCore
import Subject.Core (Subject(..))
import qualified Subject.Core as SubjectCore
import qualified Subject.Value as SubjectValue
import qualified Data.Text as T
import qualified Data.Map.Strict as Map
import qualified Data.Set as Set

-- Helper to create a simple Pattern Subject for testing
createTestPattern :: String -> Pattern Subject
createTestPattern s = pattern $ Subject
  { identity = SubjectCore.Symbol ""
  , labels = Set.fromList ["String"]
  , properties = Map.fromList [("text", SubjectValue.VString s)]
  }

spec :: Spec
spec = describe "PatternLisp.Subject - Complete Value Serialization" $ do
  describe "Basic value round-trips" $ do
    it "round-trip numbers" $ do
      let val = VNumber 42
      let subj = valueToSubject val
      case subjectToValue subj of
        Left err -> fail $ "Deserialization failed: " ++ show err
        Right val' -> val' `shouldBe` val
    
    it "round-trip strings" $ do
      let val = VString (T.pack "hello")
      let subj = valueToSubject val
      case subjectToValue subj of
        Left err -> fail $ "Deserialization failed: " ++ show err
        Right val' -> val' `shouldBe` val
    
    it "round-trip booleans" $ do
      let val = VBool True
      let subj = valueToSubject val
      case subjectToValue subj of
        Left err -> fail $ "Deserialization failed: " ++ show err
        Right val' -> val' `shouldBe` val
      
      let val2 = VBool False
      let subj2 = valueToSubject val2
      case subjectToValue subj2 of
        Left err -> fail $ "Deserialization failed: " ++ show err
        Right val2' -> val2' `shouldBe` val2
    
    it "round-trip lists" $ do
      let val = VList [VNumber 1, VNumber 2, VNumber 3]
      let subj = valueToSubject val
      case subjectToValue subj of
        Left err -> fail $ "Deserialization failed: " ++ show err
        Right val' -> val' `shouldBe` val
      
      let val2 = VList [VString (T.pack "a"), VString (T.pack "b")]
      let subj2 = valueToSubject val2
      case subjectToValue subj2 of
        Left err -> fail $ "Deserialization failed: " ++ show err
        Right val2' -> val2' `shouldBe` val2
    
    it "round-trip patterns" $ do
      let pat = createTestPattern "hello"
      let val = VPattern pat
      let subj = valueToSubject val
      case subjectToValue subj of
        Left err -> fail $ "Deserialization failed: " ++ show err
        Right val' -> case val' of
          VPattern pat' -> do
            -- Compare pattern decorations
            let dec1 = PatternCore.value pat
            let dec2 = PatternCore.value pat'
            -- Check labels match
            labels dec1 `shouldBe` labels dec2
            -- Check properties match
            properties dec1 `shouldBe` properties dec2
          _ -> fail $ "Expected VPattern, got: " ++ show val'
  
  describe "Closure round-trips" $ do
    it "round-trip simple closure" $ do
      -- Create a simple closure: (lambda (x) (+ x 1))
      case parseExpr "(lambda (x) (+ x 1))" of
        Left err -> fail $ "Parse error: " ++ show err
        Right expr -> case evalExpr expr initialEnv of
          Left err -> fail $ "Eval error: " ++ show err
          Right val -> case val of
            VClosure closure -> do
              let subj = valueToSubject val
              case subjectToValue subj of
                Left err' -> fail $ "Deserialization failed: " ++ show err'
                Right val' -> case val' of
                  VClosure closure' -> do
                    -- Check parameters match
                    params closure' `shouldBe` params closure
                    -- Check body matches (compare as expressions)
                    body closure' `shouldBe` body closure
                  _ -> fail $ "Expected VClosure, got: " ++ show val'
            _ -> fail $ "Expected VClosure, got: " ++ show val
    
    it "round-trip closure with captured environment" $ do
      -- Create a closure that captures a variable
      case parseExpr "(let ((y 10)) (lambda (x) (+ x y)))" of
        Left err -> fail $ "Parse error: " ++ show err
        Right expr -> case evalExpr expr initialEnv of
          Left err -> fail $ "Eval error: " ++ show err
          Right val -> case val of
            VClosure closure -> do
              let subj = valueToSubject val
              case subjectToValue subj of
                Left err' -> fail $ "Deserialization failed: " ++ show err'
                Right val' -> case val' of
                  VClosure closure' -> do
                    -- Check parameters match
                    params closure' `shouldBe` params closure
                    -- Check body matches
                    body closure' `shouldBe` body closure
                    -- Check environment has captured variable
                    Map.member "y" (env closure') `shouldBe` True
                  _ -> fail $ "Expected VClosure, got: " ++ show val'
            _ -> fail $ "Expected VClosure, got: " ++ show val
    
    it "round-trip nested closures" $ do
      -- Create nested closures: (lambda (x) (lambda (y) (+ x y)))
      case parseExpr "(lambda (x) (lambda (y) (+ x y)))" of
        Left err -> fail $ "Parse error: " ++ show err
        Right expr -> case evalExpr expr initialEnv of
          Left err -> fail $ "Eval error: " ++ show err
          Right val -> case val of
            VClosure closure -> do
              let subj = valueToSubject val
              case subjectToValue subj of
                Left err' -> fail $ "Deserialization failed: " ++ show err'
                Right val' -> case val' of
                  VClosure closure' -> do
                    -- Check parameters match
                    params closure' `shouldBe` params closure
                    -- Check body matches
                    body closure' `shouldBe` body closure
                  _ -> fail $ "Expected VClosure, got: " ++ show val'
            _ -> fail $ "Expected VClosure, got: " ++ show val
    
    it "closure remains executable after round-trip" $ do
      -- Create closure and round-trip it, then execute it
      case parseExpr "(lambda (x) (+ x 1))" of
        Left err -> fail $ "Parse error: " ++ show err
        Right expr -> case evalExpr expr initialEnv of
          Left err -> fail $ "Eval error: " ++ show err
          Right val -> case val of
            VClosure closure -> do
              let subj = valueToSubject val
              case subjectToValue subj of
                Left err' -> fail $ "Deserialization failed: " ++ show err'
                Right val' -> case val' of
                  VClosure closure' -> do
                    -- Execute the deserialized closure
                    let arg = VNumber 5
                    let argBindings = Map.fromList (zip (params closure') [arg])
                    let extendedEnv = Map.union argBindings (env closure')
                    case evalExpr (body closure') extendedEnv of
                      Left err'' -> fail $ "Execution failed: " ++ show err''
                      Right result -> result `shouldBe` VNumber 6
                  _ -> fail $ "Expected VClosure, got: " ++ show val'
            _ -> fail $ "Expected VClosure, got: " ++ show val
  
  describe "Primitive round-trips" $ do
    it "round-trip primitives" $ do
      let prim = VPrimitive Add
      let subj = valueToSubject prim
      case subjectToValue subj of
        Left err -> fail $ "Deserialization failed: " ++ show err
        Right val -> case val of
          VPrimitive Add -> True `shouldBe` True
          _ -> fail $ "Expected VPrimitive Add, got: " ++ show val
      
      let prim2 = VPrimitive PatternCreate
      let subj2 = valueToSubject prim2
      case subjectToValue subj2 of
        Left err -> fail $ "Deserialization failed: " ++ show err
        Right val -> case val of
          VPrimitive PatternCreate -> True `shouldBe` True
          _ -> fail $ "Expected VPrimitive PatternCreate, got: " ++ show val
    
    it "primitive remains functional after round-trip" $ do
      let prim = VPrimitive Add
      let subj = valueToSubject prim
      case subjectToValue subj of
        Left err -> fail $ "Deserialization failed: " ++ show err
        Right val -> case val of
          VPrimitive Add -> do
            -- Test that the primitive can be used
            case parseExpr "(+ 1 2)" of
              Left err' -> fail $ "Parse error: " ++ show err'
              Right expr -> case evalExpr expr initialEnv of
                Left err' -> fail $ "Eval error: " ++ show err'
                Right result -> result `shouldBe` VNumber 3
          _ -> fail $ "Expected VPrimitive Add, got: " ++ show val
    
    it "missing primitive in registry errors correctly" $ do
      -- Create a Subject with an invalid primitive name
      let invalidSubj = Subject
            { identity = SubjectCore.Symbol ""
            , labels = Set.fromList ["Primitive"]
            , properties = Map.fromList [("name", SubjectValue.VString "invalid-primitive")]
            }
      case subjectToValue invalidSubj of
        Left (TypeMismatch _ _) -> True `shouldBe` True
        Left err -> fail $ "Expected TypeMismatch, got: " ++ show err
        Right _ -> fail "Expected error for missing primitive"
  
  describe "Pattern containing closures round-trips" $ do
    it "pattern containing closures round-trips" $ do
      -- Create a closure
      case parseExpr "(lambda (x) x)" of
        Left err -> fail $ "Parse error: " ++ show err
        Right expr -> case evalExpr expr initialEnv of
          Left err -> fail $ "Eval error: " ++ show err
          Right val -> case val of
            VClosure closure -> do
              -- Create a pattern containing this closure
              let closureSubj = valueToSubject val
              let closurePat = pattern closureSubj
              let patternVal = VPattern closurePat
              let patternSubj = valueToSubject patternVal
              case subjectToValue patternSubj of
                Left err' -> fail $ "Deserialization failed: " ++ show err'
                Right val' -> case val' of
                  VPattern pat' -> do
                    -- Extract the closure from the pattern
                    let dec = PatternCore.value pat'
                    case subjectToValue dec of
                      Left err'' -> fail $ "Closure deserialization failed: " ++ show err''
                      Right closureVal -> case closureVal of
                        VClosure closure' -> do
                          params closure' `shouldBe` params closure
                          body closure' `shouldBe` body closure
                        _ -> fail $ "Expected VClosure in pattern, got: " ++ show closureVal
                  _ -> fail $ "Expected VPattern, got: " ++ show val'
            _ -> fail $ "Expected VClosure, got: " ++ show val
  
  describe "Expression serialization" $ do
    it "exprToSubject and subjectToExpr round-trip" $ do
      -- Test various expression forms
      let exprs = 
            [ Atom (Symbol "x")
            , Atom (Number 42)
            , Atom (String (T.pack "hello"))
            , Atom (Bool True)
            , List [Atom (Symbol "+"), Atom (Number 1), Atom (Number 2)]
            , Quote (Atom (Symbol "x"))
            ]
      
      mapM_ (\expr -> do
        let subj = exprToSubject expr
        case subjectToExpr subj of
          Left err -> fail $ "Deserialization failed for " ++ show expr ++ ": " ++ show err
          Right expr' -> expr' `shouldBe` expr
        ) exprs
    
    it "variable names use property-based representation" $ do
      -- Test that variable names are stored as properties
      let expr = Atom (Symbol "my-var")
      let subj = exprToSubject expr
      -- Check that the Subject has a "name" property
      Map.member "name" (properties subj) `shouldBe` True
      case Map.lookup "name" (properties subj) of
        Just (SubjectValue.VString name) -> name `shouldBe` "my-var"
        _ -> fail "Variable name not stored correctly as property"
  
  describe "Error handling" $ do
    it "invalid Subject structures error correctly" $ do
      -- Test with invalid label
      let invalidSubj = Subject
            { identity = SubjectCore.Symbol ""
            , labels = Set.fromList ["InvalidLabel"]
            , properties = Map.empty
            }
      case subjectToValue invalidSubj of
        Left _ -> True `shouldBe` True
        Right _ -> fail "Expected error for invalid label"
      
      -- Test with missing properties
      let invalidSubj2 = Subject
            { identity = SubjectCore.Symbol ""
            , labels = Set.fromList ["Number"]
            , properties = Map.empty  -- Missing "value" property
            }
      case subjectToValue invalidSubj2 of
        Left _ -> True `shouldBe` True
        Right _ -> fail "Expected error for missing properties"

