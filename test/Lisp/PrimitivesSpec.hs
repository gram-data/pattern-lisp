module Lisp.PrimitivesSpec (spec) where

import Test.Hspec
import Lisp.Syntax
import Lisp.Parser
import Lisp.Eval
import Lisp.Primitives
import qualified Data.Text as T
import qualified Data.Map as Map

spec :: Spec
spec = describe "Lisp.Primitives and Lisp.Eval" $ do
  describe "Arithmetic operations" $ do
    it "evaluates addition (+ 1 2 3)" $ do
      case parseExpr "(+ 1 2 3)" of
        Left err -> fail $ "Parse error: " ++ show err
        Right expr -> case evalExpr expr initialEnv of
          Left err -> fail $ "Eval error: " ++ show err
          Right val -> val `shouldBe` VNumber 6
    
    it "evaluates subtraction (- 10 3)" $ do
      case parseExpr "(- 10 3)" of
        Left err -> fail $ "Parse error: " ++ show err
        Right expr -> case evalExpr expr initialEnv of
          Left err -> fail $ "Eval error: " ++ show err
          Right val -> val `shouldBe` VNumber 7
    
    it "evaluates multiplication (* 4 5)" $ do
      case parseExpr "(* 4 5)" of
        Left err -> fail $ "Parse error: " ++ show err
        Right expr -> case evalExpr expr initialEnv of
          Left err -> fail $ "Eval error: " ++ show err
          Right val -> val `shouldBe` VNumber 20
    
    it "evaluates division (/ 15 3)" $ do
      case parseExpr "(/ 15 3)" of
        Left err -> fail $ "Parse error: " ++ show err
        Right expr -> case evalExpr expr initialEnv of
          Left err -> fail $ "Eval error: " ++ show err
          Right val -> val `shouldBe` VNumber 5
  
  describe "Comparison operations" $ do
    it "evaluates greater than (> 5 3)" $ do
      case parseExpr "(> 5 3)" of
        Left err -> fail $ "Parse error: " ++ show err
        Right expr -> case evalExpr expr initialEnv of
          Left err -> fail $ "Eval error: " ++ show err
          Right val -> val `shouldBe` VBool True
    
    it "evaluates less than (< 3 5)" $ do
      case parseExpr "(< 3 5)" of
        Left err -> fail $ "Parse error: " ++ show err
        Right expr -> case evalExpr expr initialEnv of
          Left err -> fail $ "Eval error: " ++ show err
          Right val -> val `shouldBe` VBool True
    
    it "evaluates equal (= 5 5)" $ do
      case parseExpr "(= 5 5)" of
        Left err -> fail $ "Parse error: " ++ show err
        Right expr -> case evalExpr expr initialEnv of
          Left err -> fail $ "Eval error: " ++ show err
          Right val -> val `shouldBe` VBool True
    
    it "evaluates not equal (/= 5 3)" $ do
      case parseExpr "(/= 5 3)" of
        Left err -> fail $ "Parse error: " ++ show err
        Right expr -> case evalExpr expr initialEnv of
          Left err -> fail $ "Eval error: " ++ show err
          Right val -> val `shouldBe` VBool True
  
  describe "String operations" $ do
    it "evaluates string-append (string-append \"hello\" \" world\")" $ do
      case parseExpr "(string-append \"hello\" \" world\")" of
        Left err -> fail $ "Parse error: " ++ show err
        Right expr -> case evalExpr expr initialEnv of
          Left err -> fail $ "Eval error: " ++ show err
          Right val -> val `shouldBe` VString (T.pack "hello world")
    
    it "evaluates string-length (string-length \"hello\")" $ do
      case parseExpr "(string-length \"hello\")" of
        Left err -> fail $ "Parse error: " ++ show err
        Right expr -> case evalExpr expr initialEnv of
          Left err -> fail $ "Eval error: " ++ show err
          Right val -> val `shouldBe` VNumber 5
    
    it "evaluates substring (substring \"hello\" 1 3)" $ do
      case parseExpr "(substring \"hello\" 1 3)" of
        Left err -> fail $ "Parse error: " ++ show err
        Right expr -> case evalExpr expr initialEnv of
          Left err -> fail $ "Eval error: " ++ show err
          Right val -> val `shouldBe` VString (T.pack "el")
  
  describe "Nested expressions" $ do
    it "evaluates nested expression (+ (* 2 3) 4)" $ do
      case parseExpr "(+ (* 2 3) 4)" of
        Left err -> fail $ "Parse error: " ++ show err
        Right expr -> case evalExpr expr initialEnv of
          Left err -> fail $ "Eval error: " ++ show err
          Right val -> val `shouldBe` VNumber 10
  
  describe "Error handling" $ do
    it "handles division by zero" $ do
      case parseExpr "(/ 10 0)" of
        Left err -> fail $ "Parse error: " ++ show err
        Right expr -> case evalExpr expr initialEnv of
          Left (DivisionByZero _) -> True `shouldBe` True
          Left err -> fail $ "Unexpected error: " ++ show err
          Right _ -> fail "Expected DivisionByZero error"
    
    it "handles type mismatch" $ do
      case parseExpr "(+ 1 \"hello\")" of
        Left err -> fail $ "Parse error: " ++ show err
        Right expr -> case evalExpr expr initialEnv of
          Left (TypeMismatch _ _) -> True `shouldBe` True
          Left err -> fail $ "Unexpected error: " ++ show err
          Right _ -> fail "Expected TypeMismatch error"
    
    it "handles arity mismatch" $ do
      case parseExpr "(+ 1)" of
        Left err -> fail $ "Parse error: " ++ show err
        Right expr -> case evalExpr expr initialEnv of
          Left (ArityMismatch _ _ _) -> True `shouldBe` True
          Left err -> fail $ "Unexpected error: " ++ show err
          Right _ -> fail "Expected ArityMismatch error"

