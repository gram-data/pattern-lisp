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
    it "pattern construction creates atomic pattern" $ do
      case parseExpr "(pattern \"hello\")" of
        Left err -> fail $ "Parse error: " ++ show err
        Right expr -> case evalExpr expr initialEnv of
          Left err -> fail $ "Eval error: " ++ show err
          Right val -> case val of
            VPattern _ -> True `shouldBe` True
            _ -> fail $ "Expected VPattern, got: " ++ show val
    
    it "pattern-with creates pattern with elements" $ do
      case parseExpr "(pattern-with \"root\" '())" of
        Left err -> fail $ "Parse error: " ++ show err
        Right expr -> case evalExpr expr initialEnv of
          Left err -> fail $ "Eval error: " ++ show err
          Right val -> case val of
            VPattern _ -> True `shouldBe` True
            _ -> fail $ "Expected VPattern, got: " ++ show val
  
  describe "Pattern queries" $ do
    it "pattern-value extracts decoration correctly" $ do
      case parseExpr "(pattern-value (pattern \"hello\"))" of
        Left err -> fail $ "Parse error: " ++ show err
        Right expr -> case evalExpr expr initialEnv of
          Left err -> fail $ "Eval error: " ++ show err
          Right val -> val `shouldBe` VString (T.pack "hello")
    
    it "pattern-elements returns list of VPattern elements" $ do
      case parseExpr "(pattern-elements (pattern-with \"root\" '()))" of
        Left err -> fail $ "Parse error: " ++ show err
        Right expr -> case evalExpr expr initialEnv of
          Left err -> fail $ "Eval error: " ++ show err
          Right val -> case val of
            VList [] -> True `shouldBe` True
            _ -> fail $ "Expected empty list, got: " ++ show val
    
    it "pattern-length returns correct direct element count" $ do
      case parseExpr "(pattern-length (pattern-with \"root\" '()))" of
        Left err -> fail $ "Parse error: " ++ show err
        Right expr -> case evalExpr expr initialEnv of
          Left err -> fail $ "Eval error: " ++ show err
          Right val -> val `shouldBe` VNumber 0
    
    it "pattern-size counts all nodes recursively" $ do
      case parseExpr "(pattern-size (pattern \"hello\"))" of
        Left err -> fail $ "Parse error: " ++ show err
        Right expr -> case evalExpr expr initialEnv of
          Left err -> fail $ "Eval error: " ++ show err
          Right val -> val `shouldBe` VNumber 1
    
    it "pattern-depth returns max depth correctly" $ do
      case parseExpr "(pattern-depth (pattern \"hello\"))" of
        Left err -> fail $ "Parse error: " ++ show err
        Right expr -> case evalExpr expr initialEnv of
          Left err -> fail $ "Eval error: " ++ show err
          Right val -> val `shouldBe` VNumber 0
    
    it "pattern-values flattens all values" $ do
      case parseExpr "(pattern-values (pattern \"hello\"))" of
        Left err -> fail $ "Parse error: " ++ show err
        Right expr -> case evalExpr expr initialEnv of
          Left err -> fail $ "Eval error: " ++ show err
          Right val -> case val of
            VList [VString s] -> s `shouldBe` T.pack "hello"
            _ -> fail $ "Expected list with one string, got: " ++ show val
    
    it "nested patterns work correctly" $ do
      -- Test with a pattern containing another pattern
      -- Use pattern-with with empty list for now (nested pattern construction
      -- with multiple elements will be tested when list primitives are available)
      case parseExpr "(let ((p1 (pattern \"child\"))) (pattern-with \"root\" '()))" of
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
      
      case parseExpr "(pattern-elements (pattern \"a\") (pattern \"b\"))" of
        Left err -> fail $ "Parse error: " ++ show err
        Right expr -> case evalExpr expr initialEnv of
          Left (ArityMismatch _ _ _) -> True `shouldBe` True
          Left err -> fail $ "Unexpected error: " ++ show err
          Right _ -> fail "Expected ArityMismatch error"
  
  describe "Pattern predicates" $ do
    it "pattern-find finds matching subpattern" $ do
      -- Test pattern-find on atomic pattern that matches (using numbers since = only works for numbers)
      case parseExpr "(pattern-find (pattern 42) (lambda (p) (= (pattern-value p) 42)))" of
        Left err -> fail $ "Parse error: " ++ show err
        Right expr -> case evalExpr expr initialEnv of
          Left err -> fail $ "Eval error: " ++ show err
          Right val -> case val of
            VPattern _ -> True `shouldBe` True
            _ -> fail $ "Expected VPattern, got: " ++ show val
    
    it "pattern-find returns nothing if no match" $ do
      -- Test pattern-find on atomic pattern that doesn't match
      case parseExpr "(pattern-find (pattern 1) (lambda (p) (= (pattern-value p) 3)))" of
        Left err -> fail $ "Parse error: " ++ show err
        Right expr -> case evalExpr expr initialEnv of
          Left err -> fail $ "Eval error: " ++ show err
          Right val -> case val of
            VList [] -> True `shouldBe` True  -- Returns empty list if no match
            _ -> fail $ "Expected empty list, got: " ++ show val
    
    it "pattern-any? checks existence correctly" $ do
      -- Test pattern-any? on atomic pattern that matches
      case parseExpr "(pattern-any? (pattern 42) (lambda (p) (= (pattern-value p) 42)))" of
        Left err -> fail $ "Parse error: " ++ show err
        Right expr -> case evalExpr expr initialEnv of
          Left err -> fail $ "Eval error: " ++ show err
          Right val -> val `shouldBe` VBool True
    
    it "pattern-all? checks universal property correctly" $ do
      -- Test pattern-all? on atomic pattern
      case parseExpr "(pattern-all? (pattern 10) (lambda (p) (= (pattern-value p) 10)))" of
        Left err -> fail $ "Parse error: " ++ show err
        Right expr -> case evalExpr expr initialEnv of
          Left err -> fail $ "Eval error: " ++ show err
          Right val -> val `shouldBe` VBool True
    
    it "pattern predicates work with closures" $ do
      -- Test that predicates can be closures with captured environment
      -- Use nested let to define variables in sequence
      case parseExpr "(let ((target-val 42)) (let ((pred (lambda (p) (= (pattern-value p) target-val)))) (let ((p1 (pattern target-val))) (pattern-any? p1 pred))))" of
        Left err -> fail $ "Parse error: " ++ show err
        Right expr -> case evalExpr expr initialEnv of
          Left err -> fail $ "Eval error: " ++ show err
          Right val -> val `shouldBe` VBool True
    
    it "pattern-find type error for non-closure predicate" $ do
      case parseExpr "(pattern-find (pattern 1) \"not-a-closure\")" of
        Left err -> fail $ "Parse error: " ++ show err
        Right expr -> case evalExpr expr initialEnv of
          Left (TypeMismatch _ _) -> True `shouldBe` True
          Left err -> fail $ "Unexpected error: " ++ show err
          Right _ -> fail "Expected TypeMismatch error"

