module PatternLisp.PatternSpec (spec) where

import Test.Hspec
import PatternLisp.Syntax
import PatternLisp.Parser
import PatternLisp.Eval
import PatternLisp.Primitives
import qualified Data.Text as T

spec :: Spec
spec = describe "PatternLisp.Pattern - Pattern as First-Class Value" $ do
  describe "Pattern construction" $ do
    it "pure creates atomic pattern" $ do
      case parseExpr "(pure \"hello\")" of
        Left err -> fail $ "Parse error: " ++ show err
        Right expr -> case evalExpr expr initialEnv of
          Left err -> fail $ "Eval error: " ++ show err
          Right val -> case val of
            VPattern _ -> True `shouldBe` True
            _ -> fail $ "Expected VPattern, got: " ++ show val
    
    it "pattern creates pattern with elements" $ do
      case parseExpr "(pattern \"root\" '())" of
        Left err -> fail $ "Parse error: " ++ show err
        Right expr -> case evalExpr expr initialEnv of
          Left err -> fail $ "Eval error: " ++ show err
          Right val -> case val of
            VPattern _ -> True `shouldBe` True
            _ -> fail $ "Expected VPattern, got: " ++ show val
  
  describe "Pattern queries" $ do
    it "pattern-value extracts decoration correctly" $ do
      case parseExpr "(pattern-value (pure \"hello\"))" of
        Left err -> fail $ "Parse error: " ++ show err
        Right expr -> case evalExpr expr initialEnv of
          Left err -> fail $ "Eval error: " ++ show err
          Right val -> val `shouldBe` VString ( "hello")
    
    it "pattern-elements returns list of VPattern elements" $ do
      case parseExpr "(pattern-elements (pattern \"root\" '()))" of
        Left err -> fail $ "Parse error: " ++ show err
        Right expr -> case evalExpr expr initialEnv of
          Left err -> fail $ "Eval error: " ++ show err
          Right val -> case val of
            VArray [] -> True `shouldBe` True
            _ -> fail $ "Expected empty list, got: " ++ show val
    
    it "pattern-length returns correct direct element count" $ do
      case parseExpr "(pattern-length (pattern \"root\" '()))" of
        Left err -> fail $ "Parse error: " ++ show err
        Right expr -> case evalExpr expr initialEnv of
          Left err -> fail $ "Eval error: " ++ show err
          Right val -> val `shouldBe` VInteger 0
    
    it "pattern-size counts all nodes recursively" $ do
      case parseExpr "(pattern-size (pure \"hello\"))" of
        Left err -> fail $ "Parse error: " ++ show err
        Right expr -> case evalExpr expr initialEnv of
          Left err -> fail $ "Eval error: " ++ show err
          Right val -> val `shouldBe` VInteger 1
    
    it "pattern-depth returns max depth correctly" $ do
      case parseExpr "(pattern-depth (pure \"hello\"))" of
        Left err -> fail $ "Parse error: " ++ show err
        Right expr -> case evalExpr expr initialEnv of
          Left err -> fail $ "Eval error: " ++ show err
          Right val -> val `shouldBe` VInteger 0
    
    it "pattern-values flattens all values" $ do
      case parseExpr "(pattern-values (pure \"hello\"))" of
        Left err -> fail $ "Parse error: " ++ show err
        Right expr -> case evalExpr expr initialEnv of
          Left err -> fail $ "Eval error: " ++ show err
          Right val -> case val of
            VArray [VString s] -> s `shouldBe`  "hello"
            _ -> fail $ "Expected list with one string, got: " ++ show val
    
    it "nested patterns work correctly" $ do
      -- Test with a pattern containing another pattern
      -- Use pattern with empty list for now (nested pattern construction
      -- with multiple elements will be tested when list primitives are available)
      case parseExpr "(let ((p1 (pure \"child\"))) (pattern \"root\" '()))" of
        Left err -> fail $ "Parse error: " ++ show err
        Right expr -> case evalExpr expr initialEnv of
          Left err -> fail $ "Eval error: " ++ show err
          Right val -> case val of
            VPattern _ -> True `shouldBe` True
            _ -> fail $ "Expected VPattern, got: " ++ show val
  
  describe "Error handling" $ do
    it "type errors for non-pattern values" $ do
      case parseExpr "(pattern-value \"not-a-pattern\")" of
        Left err -> fail $ "Parse error: " ++ show err
        Right expr -> case evalExpr expr initialEnv of
          Left (TypeMismatch _ _) -> True `shouldBe` True
          Left err -> fail $ "Unexpected error: " ++ show err
          Right _ -> fail "Expected TypeMismatch error"
    
    it "arity errors for wrong argument counts" $ do
      case parseExpr "(pattern-value)" of
        Left err -> fail $ "Parse error: " ++ show err
        Right expr -> case evalExpr expr initialEnv of
          Left (ArityMismatch _ _ _) -> True `shouldBe` True
          Left err -> fail $ "Unexpected error: " ++ show err
          Right _ -> fail "Expected ArityMismatch error"
      
      case parseExpr "(pattern-elements (pure \"a\") (pure \"b\"))" of
        Left err -> fail $ "Parse error: " ++ show err
        Right expr -> case evalExpr expr initialEnv of
          Left (ArityMismatch _ _ _) -> True `shouldBe` True
          Left err -> fail $ "Unexpected error: " ++ show err
          Right _ -> fail "Expected ArityMismatch error"
  
  describe "Pattern predicates" $ do
    it "pattern-find finds matching subpattern" $ do
      -- Test pattern-find on atomic pattern that matches (using numbers since = only works for numbers)
      case parseExpr "(pattern-find (pure 42) (lambda (p) (= (pattern-value p) 42)))" of
        Left err -> fail $ "Parse error: " ++ show err
        Right expr -> case evalExpr expr initialEnv of
          Left err -> fail $ "Eval error: " ++ show err
          Right val -> case val of
            VPattern _ -> True `shouldBe` True
            _ -> fail $ "Expected VPattern, got: " ++ show val
    
    it "pattern-find returns nothing if no match" $ do
      -- Test pattern-find on atomic pattern that doesn't match
      case parseExpr "(pattern-find (pure 1) (lambda (p) (= (pattern-value p) 3)))" of
        Left err -> fail $ "Parse error: " ++ show err
        Right expr -> case evalExpr expr initialEnv of
          Left err -> fail $ "Eval error: " ++ show err
          Right val -> case val of
            VArray [] -> True `shouldBe` True  -- Returns empty list if no match
            _ -> fail $ "Expected empty list, got: " ++ show val
    
    it "pattern-any? checks existence correctly" $ do
      -- Test pattern-any? on atomic pattern that matches
      case parseExpr "(pattern-any? (pure 42) (lambda (p) (= (pattern-value p) 42)))" of
        Left err -> fail $ "Parse error: " ++ show err
        Right expr -> case evalExpr expr initialEnv of
          Left err -> fail $ "Eval error: " ++ show err
          Right val -> val `shouldBe` VBoolean True
    
    it "pattern-all? checks universal property correctly" $ do
      -- Test pattern-all? on atomic pattern
      case parseExpr "(pattern-all? (pure 10) (lambda (p) (= (pattern-value p) 10)))" of
        Left err -> fail $ "Parse error: " ++ show err
        Right expr -> case evalExpr expr initialEnv of
          Left err -> fail $ "Eval error: " ++ show err
          Right val -> val `shouldBe` VBoolean True
    
    it "pattern predicates work with closures" $ do
      -- Test that predicates can be closures with captured environment
      -- Use nested let to define variables in sequence
      case parseExpr "(let ((target-val 42)) (let ((pred (lambda (p) (= (pattern-value p) target-val)))) (let ((p1 (pure target-val))) (pattern-any? p1 pred))))" of
        Left err -> fail $ "Parse error: " ++ show err
        Right expr -> case evalExpr expr initialEnv of
          Left err -> fail $ "Eval error: " ++ show err
          Right val -> val `shouldBe` VBoolean True
    
    it "pattern-find type error for non-closure predicate" $ do
      case parseExpr "(pattern-find (pure 1) \"not-a-closure\")" of
        Left err -> fail $ "Parse error: " ++ show err
        Right expr -> case evalExpr expr initialEnv of
          Left (TypeMismatch _ _) -> True `shouldBe` True
          Left err -> fail $ "Unexpected error: " ++ show err
          Right _ -> fail "Expected TypeMismatch error"

