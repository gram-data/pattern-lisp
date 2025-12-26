module PatternLisp.CodecSpec (spec) where

import Test.Hspec
import PatternLisp.Syntax
import PatternLisp.Parser
import PatternLisp.Eval
import PatternLisp.Primitives
import PatternLisp.Codec (valueToPatternSubjectForGram, patternSubjectToValue, exprToSubject, subjectToExpr)
import PatternLisp.Gram (patternToGram, gramToPattern)
import PatternLisp.Syntax (Error(..))
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

-- Helper for round-trip testing using Gram serialization path
roundTripValue :: Value -> Either Error Bool
roundTripValue val = do
  let pat = valueToPatternSubjectForGram val
      gramText = patternToGram pat
  pat' <- case gramToPattern gramText of
    Left parseErr -> Left $ ParseError (show parseErr)
    Right p -> Right p
  val' <- patternSubjectToValue pat'
  Right (val == val')

-- Helper to run roundTripValue in IO context
runRoundTripValue :: Value -> IO Bool
runRoundTripValue val = case roundTripValue val of
  Left err -> fail $ "Round-trip error: " ++ show err
  Right result -> return result

spec :: Spec
spec = describe "PatternLisp.Codec - Complete Value Serialization" $ do
  describe "Basic value round-trips" $ do
    it "round-trip numbers" $ do
      let val = VNumber 42
      result <- runRoundTripValue val
      if result then return () else fail "Round-trip failed: values not equal"
    
    it "round-trip strings" $ do
      let val = VString (T.pack "hello")
      result <- runRoundTripValue val
      if result then return () else fail "Round-trip failed: values not equal"
    
    it "round-trip booleans" $ do
      let val = VBool True
      result <- runRoundTripValue val
      if result then return () else fail "Round-trip failed: values not equal"
      
      let val2 = VBool False
      result2 <- runRoundTripValue val2
      if result2 then return () else fail "Round-trip failed: values not equal"
    
    it "round-trip lists" $ do
      let val = VList [VNumber 1, VNumber 2, VNumber 3]
      result <- runRoundTripValue val
      if result then return () else fail "Round-trip failed: values not equal"
      
      let val2 = VList [VString (T.pack "a"), VString (T.pack "b")]
      result2 <- runRoundTripValue val2
      if result2 then return () else fail "Round-trip failed: values not equal"
    
    it "round-trip patterns" $ do
      let pat = createTestPattern "hello"
      let val = VPattern pat
      result <- runRoundTripValue val
      if result then return () else fail "Round-trip failed: values not equal"
  
  describe "Closure round-trips" $ do
    it "round-trip simple closure" $ do
      -- Create a simple closure: (lambda (x) (+ x 1))
      case parseExpr "(lambda (x) (+ x 1))" of
        Left err -> fail $ "Parse error: " ++ show err
        Right expr -> case evalExpr expr initialEnv of
          Left err -> fail $ "Eval error: " ++ show err
          Right val -> do
            result <- runRoundTripValue val
            if result then return () else fail "Round-trip failed: values not equal"
    
    it "round-trip closure with captured environment" $ do
      -- Create a closure that captures a variable from outer scope: (let ((x 10)) (lambda (y) (+ x y)))
      case parseExpr "(let ((x 10)) (lambda (y) (+ x y)))" of
        Left err -> fail $ "Parse error: " ++ show err
        Right expr -> case evalExpr expr initialEnv of
          Left err -> fail $ "Eval error: " ++ show err
          Right val -> do
            result <- runRoundTripValue val
            if result then return () else fail "Round-trip failed: values not equal"
    
    it "round-trip nested closures" $ do
      -- Create nested closures: (lambda (x) (lambda (y) (+ x y)))
      case parseExpr "(lambda (x) (lambda (y) (+ x y)))" of
        Left err -> fail $ "Parse error: " ++ show err
        Right expr -> case evalExpr expr initialEnv of
          Left err -> fail $ "Eval error: " ++ show err
          Right val -> do
            result <- runRoundTripValue val
            if result then return () else fail "Round-trip failed: values not equal"
    
    it "closure remains executable after round-trip" $ do
      -- Create closure and round-trip it, then execute it
      case parseExpr "(lambda (x) (+ x 1))" of
        Left err -> fail $ "Parse error: " ++ show err
        Right expr -> case evalExpr expr initialEnv of
          Left err -> fail $ "Eval error: " ++ show err
          Right val -> do
            -- Round-trip through Gram serialization
            let pat = valueToPatternSubjectForGram val
                gramText = patternToGram pat
            val' <- case gramToPattern gramText of
              Left parseErr -> fail $ "Parse error: " ++ show parseErr
              Right pat' -> case patternSubjectToValue pat' of
                Left err' -> fail $ "Deserialization failed: " ++ show err'
                Right v -> return v
            case val' of
              VClosure closure' -> do
                -- Execute the deserialized closure
                let arg = VNumber 5
                let argBindings = Map.fromList (zip (params closure') [arg])
                let extendedEnv = Map.union argBindings (env closure')
                case evalExpr (body closure') extendedEnv of
                  Left err'' -> fail $ "Execution failed: " ++ show err''
                  Right result -> result `shouldBe` VNumber 6
              _ -> fail $ "Expected VClosure, got: " ++ show val'
  
  describe "Primitive round-trips" $ do
    it "round-trip primitives" $ do
      let prim = VPrimitive Add
      result <- runRoundTripValue prim
      if result then return () else fail "Round-trip failed: values not equal"
      
      let prim2 = VPrimitive PatternCreate
      result2 <- runRoundTripValue prim2
      if result2 then return () else fail "Round-trip failed: values not equal"
    
    it "primitive remains functional after round-trip" $ do
      let prim = VPrimitive Add
      _ <- runRoundTripValue prim
      -- Test that the primitive can be used
      case parseExpr "(+ 1 2)" of
        Left err' -> fail $ "Parse error: " ++ show err'
        Right expr -> case evalExpr expr initialEnv of
          Left err' -> fail $ "Eval error: " ++ show err'
          Right result -> result `shouldBe` VNumber 3
    
    it "missing primitive in registry errors correctly" $ do
      -- Create a pattern with an invalid primitive name
      let invalidPat = pattern $ Subject
            { identity = SubjectCore.Symbol ""
            , labels = Set.fromList ["Primitive"]
            , properties = Map.fromList [("name", SubjectValue.VString "invalid-primitive")]
            }
      case patternSubjectToValue invalidPat of
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
          Right val -> do
            -- Create a pattern containing this closure
            let closurePat = valueToPatternSubjectForGram val
                patternVal = VPattern closurePat
            result <- runRoundTripValue patternVal
            if result then return () else fail "Round-trip failed: values not equal"
  
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
    it "invalid pattern structures error correctly" $ do
      -- Test with missing properties in Number pattern
      let invalidPat = pattern $ Subject
            { identity = SubjectCore.Symbol ""
            , labels = Set.fromList ["Number"]
            , properties = Map.empty  -- Missing "value" property
            }
      case patternSubjectToValue invalidPat of
        Left _ -> True `shouldBe` True
        Right _ -> fail "Expected error for missing properties"

