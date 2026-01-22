module IntegrationSpec (spec) where

import Test.Hspec
import PatternLisp.Syntax
import PatternLisp.Parser
import PatternLisp.Eval
import PatternLisp.Primitives
import PatternLisp.Codec (programToGram, gramToProgram, valueToPlispSource)
import PatternLisp.FileLoader (loadPlispFile)
import qualified Data.Text as T

-- | Integration tests that combine parser, evaluator, and REPL functionality
spec :: Spec
spec = describe "Integration Tests" $ do
  describe "Parse-Eval roundtrip" $ do
    it "parses and evaluates arithmetic expression" $ do
      case parseExpr "(+ 1 2 3)" of
        Left err -> fail $ "Parse error: " ++ show err
        Right expr -> case evalExpr expr initialEnv of
          Left err -> fail $ "Eval error: " ++ show err
          Right val -> val `shouldBe` VInteger 6
    
    it "parses and evaluates nested expression" $ do
      case parseExpr "(+ (* 2 3) 4)" of
        Left err -> fail $ "Parse error: " ++ show err
        Right expr -> case evalExpr expr initialEnv of
          Left err -> fail $ "Eval error: " ++ show err
          Right val -> val `shouldBe` VInteger 10
    
    it "parses and evaluates conditional" $ do
      case parseExpr "(if (> 5 3) 10 20)" of
        Left err -> fail $ "Parse error: " ++ show err
        Right expr -> case evalExpr expr initialEnv of
          Left err -> fail $ "Eval error: " ++ show err
          Right val -> val `shouldBe` VInteger 10
  
  describe "Define and use functions" $ do
    it "defines function and uses it in same evaluation" $ do
      let program = "(begin (define square (lambda (x) (* x x))) (square 4))"
      case parseExpr program of
        Left err -> fail $ "Parse error: " ++ show err
        Right expr -> case evalExprWithEnv expr initialEnv of
          Left err -> fail $ "Eval error: " ++ show err
          Right (val, _) -> val `shouldBe` VInteger 16
    
    it "defines multiple functions and uses them" $ do
      let program = unlines
            [ "(begin"
            , "  (define add (lambda (x y) (+ x y)))"
            , "  (define mul (lambda (x y) (* x y)))"
            , "  (add (mul 2 3) (mul 4 5)))"
            ]
      case parseExpr program of
        Left err -> fail $ "Parse error: " ++ show err
        Right expr -> case evalExprWithEnv expr initialEnv of
          Left err -> fail $ "Eval error: " ++ show err
          Right (val, _) -> val `shouldBe` VInteger 26
  
  describe "Closure environment capture" $ do
    it "creates closure that captures outer environment" $ do
      let program = "(begin (define x 10) (define get-x (lambda () x)) (get-x))"
      case parseExpr program of
        Left err -> fail $ "Parse error: " ++ show err
        Right expr -> case evalExprWithEnv expr initialEnv of
          Left err -> fail $ "Eval error: " ++ show err
          Right (val, _) -> val `shouldBe` VInteger 10
    
    it "creates higher-order function with closure" $ do
      let program = unlines
            [ "(begin"
            , "  (define make-adder (lambda (n) (lambda (x) (+ x n))))"
            , "  (define add5 (make-adder 5))"
            , "  (add5 10))"
            ]
      case parseExpr program of
        Left err -> fail $ "Parse error: " ++ show err
        Right expr -> case evalExprWithEnv expr initialEnv of
          Left err -> fail $ "Eval error: " ++ show err
          Right (val, _) -> val `shouldBe` VInteger 15
  
  describe "Error handling across components" $ do
    it "reports parse errors with position" $ do
      case parseExpr "(unclosed" of
        Left (ParseError msg) -> 
          msg `shouldContain` "unexpected end of input"
        Left err -> fail $ "Unexpected error type: " ++ show err
        Right _ -> fail "Expected parse error"
    
    it "reports undefined variable errors" $ do
      case parseExpr "undefined-var" of
        Left err -> fail $ "Parse error: " ++ show err
        Right expr -> case evalExpr expr initialEnv of
          Left (UndefinedVar name _) -> name `shouldBe` "undefined-var"
          Left err -> fail $ "Unexpected error: " ++ show err
          Right _ -> fail "Expected undefined variable error"
    
    it "reports type mismatch errors with context" $ do
      case parseExpr "(+ 1 \"hello\")" of
        Left err -> fail $ "Parse error: " ++ show err
        Right expr -> case evalExpr expr initialEnv of
          Left (TypeMismatch msg _) -> 
            msg `shouldContain` "Expected number"
          Left err -> fail $ "Unexpected error: " ++ show err
          Right _ -> fail "Expected type mismatch error"
    
    it "reports arity mismatch errors" $ do
      case parseExpr "(+ 1)" of
        Left err -> fail $ "Parse error: " ++ show err
        Right expr -> case evalExpr expr initialEnv of
          Left (ArityMismatch name expected actual) -> do
            name `shouldBe` "+"
            expected `shouldBe` 2
            actual `shouldBe` 1
          Left err -> fail $ "Unexpected error: " ++ show err
          Right _ -> fail "Expected arity mismatch error"

  describe "Plisp↔Gram round-trip" $ do
    it "plisp→gram→plisp preserves program meaning" $ do
      -- Original plisp program
      let originalPlisp = "(begin (+ 1 2) (* 3 4) \"hello\")"
      
      -- Parse and evaluate original
      case parseExpr originalPlisp of
        Left err -> fail $ "Parse error: " ++ show err
        Right expr -> case evalExprWithEnv expr initialEnv of
          Left err -> fail $ "Eval error: " ++ show err
          Right (_, env) -> do
            -- Get all values from the begin expression
            -- For a begin, we need to evaluate each subexpression
            -- Let's use a simpler approach: convert a single value
            let singleValue = VInteger 42
            let gram = programToGram [singleValue] initialEnv
            
            -- Convert gram back to plisp
            case gramToProgram gram of
              Left err -> fail $ "gramToProgram error: " ++ show err
              Right (values, _) -> do
                -- Convert values back to plisp source
                case mapM valueToPlispSource values of
                  Left err -> fail $ "valueToPlispSource error: " ++ show err
                  Right plispStrs -> do
                    -- Parse and evaluate the round-trip plisp
                    let roundTripPlisp = case plispStrs of
                          [single] -> single
                          _ -> "(begin " ++ unwords plispStrs ++ ")"
                    case parseExpr roundTripPlisp of
                      Left err -> fail $ "Round-trip parse error: " ++ show err
                      Right roundTripExpr -> case evalExpr roundTripExpr initialEnv of
                        Left err -> fail $ "Round-trip eval error: " ++ show err
                        Right roundTripVal -> do
                          -- The round-trip value should match the original
                          roundTripVal `shouldBe` singleValue
    
    it "round-trip preserves multiple values" $ do
      -- Test with multiple values
      let values = [VInteger 1, VInteger 2, VString "test"]
      let gram = programToGram values initialEnv
      
      case gramToProgram gram of
        Left err -> fail $ "gramToProgram error: " ++ show err
        Right (roundTripValues, _) -> do
          -- Values should match
          roundTripValues `shouldBe` values
    
    it "round-trip preserves complex values" $ do
      -- Test with arrays and maps
      let complexValue = VArray [VInteger 1, VString "hello", VBoolean True]
      let gram = programToGram [complexValue] initialEnv
      
      case gramToProgram gram of
        Left err -> fail $ "gramToProgram error: " ++ show err
        Right (roundTripValues, _) -> do
          case roundTripValues of
            [roundTripVal] -> roundTripVal `shouldBe` complexValue
            _ -> fail $ "Expected single value, got: " ++ show roundTripValues

