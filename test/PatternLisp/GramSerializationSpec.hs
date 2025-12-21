module PatternLisp.GramSerializationSpec (spec) where

import Test.Hspec
import PatternLisp.Syntax
import PatternLisp.Parser
import PatternLisp.Eval
import PatternLisp.Primitives
import PatternLisp.Gram
import PatternLisp.PatternPrimitives
import Pattern (Pattern)
import Subject.Core (Subject)
import qualified Data.Text as T
import qualified Data.Map as Map
import Control.Monad.Reader
import Control.Monad.Except

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

spec :: Spec
spec = describe "PatternLisp.GramSerializationSpec - Gram Serialization" $ do
  describe "Basic value types" $ do
    it "serializes numbers to valid gram" $ do
      case evalToPattern "42" initialEnv of
        Left err -> fail $ "Eval error: " ++ show err
        Right pat -> isValidGram pat `shouldBe` True
    
    it "serializes strings to valid gram" $ do
      case evalToPattern "'hello" initialEnv of
        Left err -> fail $ "Eval error: " ++ show err
        Right pat -> isValidGram pat `shouldBe` True
    
    it "serializes booleans to valid gram" $ do
      case evalToPattern "#t" initialEnv of
        Left err -> fail $ "Eval error: " ++ show err
        Right pat -> isValidGram pat `shouldBe` True
    
    it "serializes lists to valid gram" $ do
      case evalToPattern "'(1 2 3)" initialEnv of
        Left err -> fail $ "Eval error: " ++ show err
        Right pat -> isValidGram pat `shouldBe` True
  
  describe "value-to-pattern primitive" $ do
    it "converts number to pattern with valid gram" $ do
      case parseExpr "(value-to-pattern 42)" of
        Left err -> fail $ "Parse error: " ++ show err
        Right expr -> case evalExpr expr initialEnv of
          Left err -> fail $ "Eval error: " ++ show err
          Right (VPattern pat) -> isValidGram pat `shouldBe` True
          Right val -> fail $ "Expected VPattern, got: " ++ show val
    
    it "converts string to pattern with valid gram" $ do
      case parseExpr "(value-to-pattern 'hello)" of
        Left err -> fail $ "Parse error: " ++ show err
        Right expr -> case evalExpr expr initialEnv of
          Left err -> fail $ "Eval error: " ++ show err
          Right (VPattern pat) -> isValidGram pat `shouldBe` True
          Right val -> fail $ "Expected VPattern, got: " ++ show val
    
    it "converts boolean to pattern with valid gram" $ do
      case parseExpr "(value-to-pattern #t)" of
        Left err -> fail $ "Parse error: " ++ show err
        Right expr -> case evalExpr expr initialEnv of
          Left err -> fail $ "Eval error: " ++ show err
          Right (VPattern pat) -> isValidGram pat `shouldBe` True
          Right val -> fail $ "Expected VPattern, got: " ++ show val
    
    it "converts list to pattern with valid gram" $ do
      case parseExpr "(value-to-pattern '(1 2 3))" of
        Left err -> fail $ "Parse error: " ++ show err
        Right expr -> case evalExpr expr initialEnv of
          Left err -> fail $ "Eval error: " ++ show err
          Right (VPattern pat) -> isValidGram pat `shouldBe` True
          Right val -> fail $ "Expected VPattern, got: " ++ show val
    
    it "converts primitive to pattern with valid gram" $ do
      case parseExpr "(value-to-pattern +)" of
        Left err -> fail $ "Parse error: " ++ show err
        Right expr -> case evalExpr expr initialEnv of
          Left err -> fail $ "Eval error: " ++ show err
          Right (VPattern pat) -> isValidGram pat `shouldBe` True
          Right val -> fail $ "Expected VPattern, got: " ++ show val
    
    it "converts closure to pattern with valid gram" $ do
      -- Define a simple closure first, then convert it
      case parseExpr "(define add (lambda (x y) (+ x y)))" of
        Left err -> fail $ "Parse error: " ++ show err
        Right defineExpr -> case evalExprWithEnv defineExpr initialEnv of
          Left err -> fail $ "Eval error: " ++ show err
          Right (_, envWithAdd) -> do
            case parseExpr "(value-to-pattern add)" of
              Left err2 -> fail $ "Parse error: " ++ show err2
              Right valueExpr -> case evalExpr valueExpr envWithAdd of
                Left err3 -> fail $ "Eval error: " ++ show err3
                Right (VPattern pat) -> isValidGram pat `shouldBe` True
                Right val -> fail $ "Expected VPattern, got: " ++ show val

