module IntegrationSpec (spec) where

import Test.Hspec
import Lisp.Syntax
import Lisp.Parser
import Lisp.Eval
import Lisp.Primitives
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
          Right val -> val `shouldBe` VNumber 6
    
    it "parses and evaluates nested expression" $ do
      case parseExpr "(+ (* 2 3) 4)" of
        Left err -> fail $ "Parse error: " ++ show err
        Right expr -> case evalExpr expr initialEnv of
          Left err -> fail $ "Eval error: " ++ show err
          Right val -> val `shouldBe` VNumber 10
    
    it "parses and evaluates conditional" $ do
      case parseExpr "(if (> 5 3) 10 20)" of
        Left err -> fail $ "Parse error: " ++ show err
        Right expr -> case evalExpr expr initialEnv of
          Left err -> fail $ "Eval error: " ++ show err
          Right val -> val `shouldBe` VNumber 10
  
  describe "Define and use functions" $ do
    it "defines function and uses it in same evaluation" $ do
      let program = "(begin (define square (lambda (x) (* x x))) (square 4))"
      case parseExpr program of
        Left err -> fail $ "Parse error: " ++ show err
        Right expr -> case evalExprWithEnv expr initialEnv of
          Left err -> fail $ "Eval error: " ++ show err
          Right (val, _) -> val `shouldBe` VNumber 16
    
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
          Right (val, _) -> val `shouldBe` VNumber 26
  
  describe "Closure environment capture" $ do
    it "creates closure that captures outer environment" $ do
      let program = "(begin (define x 10) (define get-x (lambda () x)) (get-x))"
      case parseExpr program of
        Left err -> fail $ "Parse error: " ++ show err
        Right expr -> case evalExprWithEnv expr initialEnv of
          Left err -> fail $ "Eval error: " ++ show err
          Right (val, _) -> val `shouldBe` VNumber 10
    
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
          Right (val, _) -> val `shouldBe` VNumber 15
  
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

