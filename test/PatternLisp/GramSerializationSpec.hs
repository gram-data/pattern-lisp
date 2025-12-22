module PatternLisp.GramSerializationSpec (spec) where

import Test.Hspec
import PatternLisp.Syntax
import PatternLisp.Parser
import PatternLisp.Eval
import PatternLisp.Primitives
import PatternLisp.Gram
import PatternLisp.PatternPrimitives
import PatternLisp.Codec (patternSubjectToValue, valueToPatternSubjectForGram)
import Pattern (Pattern)
import Subject.Core (Subject)
import qualified Data.Text as T
import qualified Data.Map as Map
import Control.Monad.Reader
import Control.Monad.Except
import Data.Either

-- Helper to evaluate an expression and convert to pattern
evalToPattern :: String -> Env -> Either Error (Pattern Subject)
evalToPattern exprStr env = do
  expr <- case parseExpr exprStr of
    Left err -> Left err
    Right e -> Right e
  val <- evalExpr expr env
  case runExcept $ runReaderT (valueToPatternSubject val) env of
    Left err -> Left err
    Right pat -> Right pat

-- Helper to check if gram output is valid
isValidGram :: Pattern Subject -> Bool
isValidGram pat = 
  case gramToPattern (patternToGram pat) of
    Left _ -> False
    Right _ -> True

-- Helper for round-trip testing: serialize value to gram, deserialize back, compare
-- Note: patternSubjectToValue will be implemented in Codec.hs as part of Phase 5
roundTripValue :: Value -> Env -> Either Error Bool
roundTripValue val env = do
  -- Serialize to gram
  let pat = valueToPatternSubjectForGram val
      gramText = patternToGram pat
  -- Deserialize from gram
  pat' <- case gramToPattern gramText of
    Left parseErr -> Left $ ParseError (show parseErr)
    Right p -> Right p
  -- Convert back to value using patternSubjectToValue
  val' <- patternSubjectToValue pat'
  -- Compare (values should be equal)
  Right (val == val')

-- Helper to run roundTripValue in IO context
runRoundTripValue :: Value -> Env -> IO Bool
runRoundTripValue val env = case roundTripValue val env of
  Left err -> fail $ "Round-trip error: " ++ show err
  Right result -> return result

-- Helper to apply a closure (reimplementing logic since applyClosure is not exported)
applyClosureHelper :: Closure -> [Value] -> Env -> Either Error Value
applyClosureHelper (Closure paramNames bodyExpr capturedEnv) args env = do
  if length paramNames /= length args
    then Left $ ArityMismatch 
      ("lambda with " ++ show (length paramNames) ++ " parameter(s)")
      (length paramNames) 
      (length args)
    else do
      let bindings = Map.fromList $ zip paramNames args
          extendedEnv = Map.union bindings capturedEnv
      case evalExpr bodyExpr extendedEnv of
        Left err -> Left err
        Right val -> Right val

spec :: Spec
spec = describe "PatternLisp.GramSerializationSpec - Gram Serialization" $ do
  describe "Round-trip: Basic value types" $ do
    it "round-trip numbers" $ do
      let val = VNumber 42
      result <- runRoundTripValue val initialEnv
      if result then return () else fail "Round-trip failed: values not equal"
    
    it "round-trip strings" $ do
      let val = VString (T.pack "hello")
      result <- runRoundTripValue val initialEnv
      if result then return () else fail "Round-trip failed: values not equal"
    
    it "round-trip booleans" $ do
      let val = VBool True
      result <- runRoundTripValue val initialEnv
      if result then return () else fail "Round-trip failed: values not equal"
    
    it "round-trip lists" $ do
      let val = VList [VNumber 1, VNumber 2, VNumber 3]
      result <- runRoundTripValue val initialEnv
      if result then return () else fail "Round-trip failed: values not equal"
    
    it "round-trip patterns" $ do
      case evalToPattern "42" initialEnv of
        Left err -> fail $ "Eval error: " ++ show err
        Right pat -> do
          let val = VPattern pat
          result <- runRoundTripValue val initialEnv
          if result then return () else fail "Round-trip failed: values not equal"
  
  describe "Round-trip: Closures" $ do
    it "round-trip simple closure" $ do
      case parseExpr "(lambda (x) (+ x 1))" of
        Left err -> fail $ "Parse error: " ++ show err
        Right expr -> case evalExpr expr initialEnv of
          Left err2 -> fail $ "Eval error: " ++ show err2
          Right (VClosure closure) -> do
            let val = VClosure closure
            result <- runRoundTripValue val initialEnv
            if result then return () else fail "Round-trip failed: values not equal"
          Right val -> fail $ "Expected VClosure, got: " ++ show val
    
    it "round-trip closure with captured environment" $ do
      case parseExpr "(let ((multiplier 10)) (lambda (x) (* x multiplier)))" of
        Left err -> fail $ "Parse error: " ++ show err
        Right expr -> case evalExpr expr initialEnv of
          Left err2 -> fail $ "Eval error: " ++ show err2
          Right (VClosure closure) -> do
            let val = VClosure closure
            result <- runRoundTripValue val initialEnv
            if result then return () else fail "Round-trip failed: values not equal"
          Right val -> fail $ "Expected VClosure, got: " ++ show val
    
    it "round-trip nested closures" $ do
      case parseExpr "(let ((helper (lambda (x) (+ x 1)))) (lambda (y) (helper y)))" of
        Left err -> fail $ "Parse error: " ++ show err
        Right expr -> case evalExpr expr initialEnv of
          Left err2 -> fail $ "Eval error: " ++ show err2
          Right (VClosure closure) -> do
            let val = VClosure closure
            result <- runRoundTripValue val initialEnv
            if result then return () else fail "Round-trip failed: values not equal"
          Right val -> fail $ "Expected VClosure, got: " ++ show val
    
    it "round-trip mutually recursive closures" $ do
      case parseExpr "(let ((f (lambda (x) (if (= x 0) 1 (g (- x 1))))) (g (lambda (x) (if (= x 0) 1 (f (- x 1)))))) f)" of
        Left err -> fail $ "Parse error: " ++ show err
        Right expr -> case evalExpr expr initialEnv of
          Left err2 -> fail $ "Eval error: " ++ show err2
          Right (VClosure closure) -> do
            let val = VClosure closure
            result <- runRoundTripValue val initialEnv
            if result then return () else fail "Round-trip failed: values not equal"
          Right val -> fail $ "Expected VClosure, got: " ++ show val
    
    it "round-trip self-recursive closure" $ do
      case parseExpr "(let ((fact (lambda (n) (if (= n 0) 1 (* n (fact (- n 1))))))) fact)" of
        Left err -> fail $ "Parse error: " ++ show err
        Right expr -> case evalExpr expr initialEnv of
          Left err2 -> fail $ "Eval error: " ++ show err2
          Right (VClosure closure) -> do
            let val = VClosure closure
            result <- runRoundTripValue val initialEnv
            if result then return () else fail "Round-trip failed: values not equal"
          Right val -> fail $ "Expected VClosure, got: " ++ show val
    
    it "closure remains executable after round-trip" $ do
      case parseExpr "(lambda (x) (+ x 1))" of
        Left err -> fail $ "Parse error: " ++ show err
        Right expr -> case evalExpr expr initialEnv of
          Left err2 -> fail $ "Eval error: " ++ show err2
          Right (VClosure closure) -> do
            let val = VClosure closure
            -- Round-trip
            _ <- runRoundTripValue val initialEnv
            let pat = valueToPatternSubjectForGram val
            val' <- case gramToPattern (patternToGram pat) of
              Left parseErr -> fail $ "Parse error: " ++ show parseErr
              Right pat' -> case patternSubjectToValue pat' of
                Left err5 -> fail $ "Deserialization error: " ++ show err5
                Right v -> return v
            -- Execute original closure
            case val of
              VClosure origClosure -> do
                let args = [VNumber 5]
                result1 <- case applyClosureHelper origClosure args initialEnv of
                  Left err6 -> fail $ "Original closure execution error: " ++ show err6
                  Right r -> return r
                -- Execute deserialized closure
                case val' of
                  VClosure newClosure -> do
                    result2 <- case applyClosureHelper newClosure args initialEnv of
                      Left err7 -> fail $ "Deserialized closure execution error: " ++ show err7
                      Right r -> return r
                    result1 `shouldBe` result2
                  _ -> fail "Deserialized value is not a closure"
              _ -> fail "Original value is not a closure"
          Right val -> fail $ "Expected VClosure, got: " ++ show val
  
  describe "Round-trip: Primitives" $ do
    it "round-trip primitives" $ do
      let val = VPrimitive Add
      result <- runRoundTripValue val initialEnv
      if result then return () else fail "Round-trip failed: values not equal"
    
    it "primitive remains functional after round-trip" $ do
      let val = VPrimitive Add
      -- Round-trip
      _ <- runRoundTripValue val initialEnv
      let pat = valueToPatternSubjectForGram val
      val' <- case gramToPattern (patternToGram pat) of
        Left parseErr -> fail $ "Parse error: " ++ show parseErr
        Right pat' -> case patternSubjectToValue pat' of
          Left err3 -> fail $ "Deserialization error: " ++ show err3
          Right v -> return v
      -- Both should be the same primitive
      case (val, val') of
        (VPrimitive p1, VPrimitive p2) -> p1 `shouldBe` p2
        _ -> fail "Deserialized value is not a primitive"
  
  describe "Round-trip: Complex structures" $ do
    it "pattern containing closures round-trips" $ do
      -- Create a pattern containing a closure
      case parseExpr "(let ((f (lambda (x) (+ x 1)))) (pattern f))" of
        Left err -> fail $ "Parse error: " ++ show err
        Right expr -> case evalExpr expr initialEnv of
          Left err2 -> fail $ "Eval error: " ++ show err2
          Right (VPattern pat) -> do
            let val = VPattern pat
            result <- runRoundTripValue val initialEnv
            if result then return () else fail "Round-trip failed: values not equal"
          Right val -> fail $ "Expected VPattern, got: " ++ show val
  
  describe "Program structure serialization" $ do
    it "program with file-level metadata round-trips" $ do
      -- Test that programToGram and gramToProgram work
      let values = [VNumber 42]
      let env = initialEnv
      -- This will be implemented in Codec.hs
      pendingWith "Implement programToGram and gramToProgram"
    
    it "program with environment section round-trips" $ do
      -- Test closure with captured bindings creates environment section
      pendingWith "Implement environment section serialization"
    
    it "binding deduplication works correctly" $ do
      -- Test that multiple closures sharing bindings deduplicate correctly
      pendingWith "Implement binding deduplication"
  
  describe "Closure serialization structure" $ do
    it "parameters vs bound values distinction preserved" $ do
      -- Test that parameters are in [:Parameters | ...] and bound values are identifiers
      pendingWith "Implement parameter vs bound value distinction"
    
    it "special form labels preserved (If, Let, Begin, Define, Quote)" $ do
      -- Test that special forms use explicit labels
      pendingWith "Implement special form labels"
  
  describe "Standard library filtering" $ do
    it "standard library bindings filtered from environment" $ do
      -- Test that standard library bindings are not serialized
      pendingWith "Implement standard library filtering"
    
    it "missing primitive in registry errors correctly" $ do
      -- Test that deserializing unknown primitive returns error
      pendingWith "Implement missing primitive error handling"

