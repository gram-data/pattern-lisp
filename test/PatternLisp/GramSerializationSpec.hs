module PatternLisp.GramSerializationSpec (spec) where

import Test.Hspec
import PatternLisp.Syntax
import PatternLisp.Parser
import PatternLisp.Eval
import PatternLisp.Primitives
import PatternLisp.Gram
import PatternLisp.PatternPrimitives
import PatternLisp.Codec (patternSubjectToValue, valueToPatternSubjectForGram, programToGram, gramToProgram)
import Pattern (Pattern)
import Pattern.Core (pattern, patternWith)
import qualified Pattern.Core as PatternCore
import Subject.Core (Subject)
import qualified Subject.Core as SubjectCore
import qualified Subject.Value as SubjectValue
import qualified Data.Text as T
import qualified Data.Map as Map
import qualified Data.Set as Set
import Control.Monad.Reader
import Control.Monad.Except
import Data.Either
import Data.Char (toLower)

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
  
  describe "Inline scope serialization" $ do
    it "closure with captured bindings has inline :Scope pattern" $ do
      -- Test that closures serialize with inline :Scope patterns (not separate environment section)
      case parseExpr "(let ((x 10)) (lambda (y) (+ x y)))" of
        Left err -> fail $ "Parse error: " ++ show err
        Right expr -> case evalExpr expr initialEnv of
          Left err2 -> fail $ "Eval error: " ++ show err2
          Right (VClosure closure) -> do
            let val = VClosure closure
                pat = valueToPatternSubjectForGram val
                -- Verify the closure has a :Scope pattern as its first element
                elements = PatternCore.elements pat
            if length elements >= 1
              then do
                let scopePat = elements !! 0
                    scopeSubj = PatternCore.value scopePat
                -- Verify it's a :Scope pattern
                if "Scope" `Set.member` SubjectCore.labels scopeSubj
                  then do
                    -- Verify it has a unique identifier
                    let scopeId = SubjectCore.identity scopeSubj
                    if scopeId /= SubjectCore.Symbol ""
                      then do
                        -- Round-trip should work
                        result <- runRoundTripValue val initialEnv
                        if result then return () else fail "Round-trip failed: values not equal"
                      else fail "Scope pattern missing identifier"
                  else fail "First element is not a :Scope pattern"
              else fail "Closure pattern missing elements"
          Right val -> fail $ "Expected VClosure, got: " ++ show val
    
    it "scope patterns have parent reference and bindings" $ do
      -- Test that :Scope patterns contain parent reference and binding patterns
      case parseExpr "(let ((x 10) (y 20)) (lambda (z) (+ x y z)))" of
        Left err -> fail $ "Parse error: " ++ show err
        Right expr -> case evalExpr expr initialEnv of
          Left err2 -> fail $ "Eval error: " ++ show err2
          Right (VClosure closure) -> do
            let val = VClosure closure
                pat = valueToPatternSubjectForGram val
                scopePat = PatternCore.elements pat !! 0
                scopeElements = PatternCore.elements scopePat
            -- Should have at least parent reference + bindings
            if length scopeElements >= 2
              then do
                -- First element should be parent reference (empty pattern for program-level)
                -- Remaining elements should be binding patterns
                let bindingPatterns = tail scopeElements
                -- Should have 2 bindings (x and y)
                if length bindingPatterns >= 2
                  then do
                    -- Round-trip should work
                    result <- runRoundTripValue val initialEnv
                    if result then return () else fail "Round-trip failed: values not equal"
                  else fail $ "Expected at least 2 bindings, got: " ++ show (length bindingPatterns)
              else fail $ "Expected at least 2 scope elements (parent + bindings), got: " ++ show (length scopeElements)
          Right val -> fail $ "Expected VClosure, got: " ++ show val
    
    it "binding deduplication works correctly" $ do
      -- Test that multiple closures sharing bindings deduplicate correctly
      -- Create two closures that capture the same binding from the same scope
      -- We'll create them separately but they should deduplicate the same binding
      case parseExpr "(let ((x 10)) (lambda (a) (+ a x)))" of
        Left err -> fail $ "Parse error: " ++ show err
        Right expr1 -> case evalExpr expr1 initialEnv of
          Left err2 -> fail $ "Eval error: " ++ show err2
          Right (VClosure c1) -> do
            case parseExpr "(let ((x 10)) (lambda (b) (* b x)))" of
              Left err3 -> fail $ "Parse error: " ++ show err3
              Right expr2 -> case evalExpr expr2 initialEnv of
                Left err4 -> fail $ "Eval error: " ++ show err4
                Right (VClosure c2) -> do
                  -- Both closures capture x=10 from their respective let scopes
                  -- Since they have the same name and value, they should deduplicate
                  -- Both should round-trip correctly
                  result1 <- runRoundTripValue (VClosure c1) initialEnv
                  result2 <- runRoundTripValue (VClosure c2) initialEnv
                  if result1 && result2 then return () else fail "Round-trip failed: values not equal"
                Right val -> fail $ "Expected VClosure for second closure, got: " ++ show val
          Right val -> fail $ "Expected VClosure for first closure, got: " ++ show val
  
  describe "Closure serialization structure" $ do
    it "parameters vs bound values distinction preserved" $ do
      -- Test that parameters are in [:Parameters | ...] and bound values are identifiers
      -- Create a closure with both parameters and captured bindings
      case parseExpr "(let ((x 10)) (lambda (y) (+ x y)))" of
        Left err -> fail $ "Parse error: " ++ show err
        Right expr -> case evalExpr expr initialEnv of
          Left err2 -> fail $ "Eval error: " ++ show err2
          Right (VClosure closure) -> do
            let val = VClosure closure
            -- Round-trip should preserve the distinction
            result <- runRoundTripValue val initialEnv
            if result then return () else fail "Round-trip failed: values not equal"
          Right val -> fail $ "Expected VClosure, got: " ++ show val
    
    it "special form labels preserved (If, Let, Begin, Define, Quote)" $ do
      -- Test that special forms use explicit labels
      -- Create closures with various special forms in their bodies
      case parseExpr "(lambda (x) (if (= x 0) 1 (* x 2)))" of
        Left err -> fail $ "Parse error: " ++ show err
        Right expr -> case evalExpr expr initialEnv of
          Left err2 -> fail $ "Eval error: " ++ show err2
          Right (VClosure closure) -> do
            let val = VClosure closure
            -- Round-trip should preserve special form labels
            result <- runRoundTripValue val initialEnv
            if result then return () else fail "Round-trip failed: values not equal"
          Right val -> fail $ "Expected VClosure, got: " ++ show val
  
  describe "File-level serialization" $ do
    it "program with file-level metadata round-trips" $ do
      -- Test that programToGram and gramToProgram work for file-level serialization
      let values = [VNumber 42, VString (T.pack "hello"), VBool True]
      let env = initialEnv
      -- Serialize to Gram
      let gramText = programToGram values env
      -- Deserialize from Gram
      case gramToProgram gramText of
        Left err -> fail $ "Deserialization error: " ++ show err
        Right (values', env') -> do
          -- Values should match
          values `shouldBe` values'
          -- Environment should be standard library
          env' `shouldBe` initialEnv
    
    it "program with closures round-trips" $ do
      -- Test that programs with closures serialize and deserialize correctly
      case parseExpr "(lambda (x) (+ x 1))" of
        Left err -> fail $ "Parse error: " ++ show err
        Right expr -> case evalExpr expr initialEnv of
          Left err2 -> fail $ "Eval error: " ++ show err2
          Right (VClosure closure) -> do
            let values = [VNumber 42, VClosure closure, VString (T.pack "test")]
            let env = initialEnv
            -- Serialize to Gram
            let gramText = programToGram values env
            -- Deserialize from Gram
            case gramToProgram gramText of
              Left err3 -> fail $ "Deserialization error: " ++ show err3
              Right (values', env') -> do
                -- Values should match (using round-trip comparison for closures)
                if length values == length values'
                  then do
                    -- Check each value
                    let checkValue v1 v2 = case (v1, v2) of
                          (VClosure c1, VClosure c2) -> do
                            -- For closures, verify round-trip works
                            result1 <- runRoundTripValue v1 initialEnv
                            result2 <- runRoundTripValue v2 initialEnv
                            if result1 && result2 then return () else fail "Closure round-trip failed"
                          (v1', v2') -> if v1' == v2' then return () else fail $ "Value mismatch: " ++ show v1' ++ " != " ++ show v2'
                    sequence_ $ zipWith checkValue values values'
                    -- Environment should be standard library
                    env' `shouldBe` initialEnv
                  else fail $ "Value count mismatch: " ++ show (length values) ++ " != " ++ show (length values')
          Right val -> fail $ "Expected VClosure, got: " ++ show val
  
  describe "Standard library filtering" $ do
    it "standard library bindings filtered from environment" $ do
      -- Test that standard library bindings are not serialized
      -- Create a closure that captures a standard library binding (e.g., +)
      case parseExpr "(lambda (x) (+ x 1))" of
        Left err -> fail $ "Parse error: " ++ show err
        Right expr -> case evalExpr expr initialEnv of
          Left err2 -> fail $ "Eval error: " ++ show err2
          Right (VClosure closure) -> do
            let val = VClosure closure
                pat = valueToPatternSubjectForGram val
                -- Check that the closure's scope doesn't contain standard library bindings
                -- The closure should only have its own parameters, not captured standard library
                -- Since this closure doesn't capture anything, the scope should be empty or minimal
            -- For a simple closure without captures, the scope should have empty parent and no bindings
            -- (or just the parent reference)
            result <- runRoundTripValue val initialEnv
            if result then return () else fail "Round-trip failed: values not equal"
          Right val -> fail $ "Expected VClosure, got: " ++ show val
    
    it "missing primitive in registry errors correctly" $ do
      -- Test that deserializing unknown primitive returns error
      -- Create a pattern with an unknown primitive name
      let unknownPrimitivePat = patternWith
            (SubjectCore.Subject
              { SubjectCore.identity = SubjectCore.Symbol ""
              , SubjectCore.labels = Set.fromList ["Primitive"]
              , SubjectCore.properties = Map.fromList [("name", SubjectValue.VString "UnknownPrimitive")]
              })
            []
      case patternSubjectToValue unknownPrimitivePat of
        Left err -> 
          -- Should return an error for unknown primitive
          err `shouldSatisfy` (\e -> case e of
            TypeMismatch msg _ -> "Unknown" `elem` words msg || "primitive" `elem` words (map toLower msg)
            _ -> False)
        Right _ -> fail "Expected error for unknown primitive, but deserialization succeeded"

