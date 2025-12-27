module PatternLisp.EvalSpec (spec) where

import Test.Hspec
import PatternLisp.Syntax
import PatternLisp.Syntax (KeywordKey(..))
import PatternLisp.Parser
import PatternLisp.Eval
import PatternLisp.Primitives
import qualified Data.Text as T
import qualified Data.Map as Map
import qualified Data.Set as Set

spec :: Spec
spec = describe "PatternLisp.Eval - Core Language Forms" $ do
  describe "Lambda expressions" $ do
    it "evaluates lambda expression ((lambda (x) (+ x 1)) 5)" $ do
      case parseExpr "((lambda (x) (+ x 1)) 5)" of
        Left err -> fail $ "Parse error: " ++ show err
        Right expr -> case evalExpr expr initialEnv of
          Left err -> fail $ "Eval error: " ++ show err
          Right val -> val `shouldBe` VNumber 6
    
    it "evaluates lambda with multiple parameters" $ do
      case parseExpr "((lambda (x y) (+ x y)) 10 20)" of
        Left err -> fail $ "Parse error: " ++ show err
        Right expr -> case evalExpr expr initialEnv of
          Left err -> fail $ "Eval error: " ++ show err
          Right val -> val `shouldBe` VNumber 30
  
  describe "If expressions" $ do
    it "evaluates if with true condition" $ do
      case parseExpr "(if (> 5 3) 'positive 'negative)" of
        Left err -> fail $ "Parse error: " ++ show err
        Right expr -> do
          -- First define x=5 in environment
          let envWithX = Map.insert "x" (VNumber 5) initialEnv
          case evalExpr expr envWithX of
            Left err -> fail $ "Eval error: " ++ show err
            Right val -> val `shouldBe` VString (T.pack "positive")
    
    it "evaluates if with false condition" $ do
      case parseExpr "(if (< 5 3) 'yes 'no)" of
        Left err -> fail $ "Parse error: " ++ show err
        Right expr -> case evalExpr expr initialEnv of
          Left err -> fail $ "Eval error: " ++ show err
          Right val -> val `shouldBe` VString (T.pack "no")
  
  describe "Let expressions" $ do
    it "evaluates let expression (let ((x 10) (y 20)) (+ x y))" $ do
      case parseExpr "(let ((x 10) (y 20)) (+ x y))" of
        Left err -> fail $ "Parse error: " ++ show err
        Right expr -> case evalExpr expr initialEnv of
          Left err -> fail $ "Eval error: " ++ show err
          Right val -> val `shouldBe` VNumber 30
    
    it "evaluates nested let bindings with shadowing" $ do
      case parseExpr "(let ((x 10)) (let ((x 20)) x))" of
        Left err -> fail $ "Parse error: " ++ show err
        Right expr -> case evalExpr expr initialEnv of
          Left err -> fail $ "Eval error: " ++ show err
          Right val -> val `shouldBe` VNumber 20
  
  describe "Quote expressions" $ do
    it "evaluates quote expression (quote (a b c))" $ do
      case parseExpr "(quote (a b c))" of
        Left err -> fail $ "Parse error: " ++ show err
        Right expr -> case evalExpr expr initialEnv of
          Left err -> fail $ "Eval error: " ++ show err
          Right val -> 
            -- Quote should return a list value with symbols as strings
            case val of
              VList [VString a, VString b, VString c] -> do
                a `shouldBe` T.pack "a"
                b `shouldBe` T.pack "b"
                c `shouldBe` T.pack "c"
              _ -> fail $ "Expected quoted list, got: " ++ show val
    
    it "evaluates single quote syntax '(a b c)" $ do
      case parseExpr "'(a b c)" of
        Left err -> fail $ "Parse error: " ++ show err
        Right expr -> case evalExpr expr initialEnv of
          Left err -> fail $ "Eval error: " ++ show err
          Right val ->
            case val of
              VList [VString a, VString b, VString c] -> do
                a `shouldBe` T.pack "a"
                b `shouldBe` T.pack "b"
                c `shouldBe` T.pack "c"
              _ -> fail $ "Expected quoted list, got: " ++ show val
  
  describe "Begin expressions" $ do
    it "evaluates begin with multiple expressions" $ do
      case parseExpr "(begin (define x 5) (+ x 1))" of
        Left err -> fail $ "Parse error: " ++ show err
        Right expr -> do
          case evalExprWithEnv expr initialEnv of
            Left err -> fail $ "Eval error: " ++ show err
            Right (val, env) -> do
              val `shouldBe` VNumber 6
              -- Check that x is defined in environment
              case Map.lookup "x" env of
                Just (VNumber 5) -> True `shouldBe` True
                _ -> fail "x should be defined as 5 in environment"
  
  describe "Define expressions" $ do
    it "evaluates define and uses defined variable" $ do
      case parseExpr "(define square (lambda (x) (* x x)))" of
        Left err -> fail $ "Parse error: " ++ show err
        Right defineExpr -> do
          case evalExprWithEnv defineExpr initialEnv of
            Left err -> fail $ "Eval error: " ++ show err
            Right (val, env) -> do
              -- Define should return the symbol name
              val `shouldBe` VString (T.pack "square")
              -- Now use the defined function
              case parseExpr "(square 4)" of
                Left err -> fail $ "Parse error: " ++ show err
                Right callExpr -> case evalExpr callExpr env of
                  Left err -> fail $ "Eval error: " ++ show err
                  Right result -> result `shouldBe` VNumber 16
    
    it "evaluates define with simple value" $ do
      case parseExpr "(define x 10)" of
        Left err -> fail $ "Parse error: " ++ show err
        Right expr -> do
          case evalExprWithEnv expr initialEnv of
            Left err -> fail $ "Eval error: " ++ show err
            Right (val, env) -> do
              val `shouldBe` VString (T.pack "x")
              case Map.lookup "x" env of
                Just (VNumber 10) -> True `shouldBe` True
                _ -> fail "x should be defined as 10 in environment"
  
  describe "Closure capturing lexical environment" $ do
    it "evaluates closure that captures lexical environment" $ do
      -- Define a function that uses a variable from outer scope
      case parseExpr "(let ((x 10)) ((lambda (y) (+ x y)) 5))" of
        Left err -> fail $ "Parse error: " ++ show err
        Right expr -> case evalExpr expr initialEnv of
          Left err -> fail $ "Eval error: " ++ show err
          Right val -> val `shouldBe` VNumber 15
  
  describe "Keywords" $ do
    it "evaluates keyword to itself without environment lookup" $ do
      case parseExpr "name:" of
        Left err -> fail $ "Parse error: " ++ show err
        Right expr -> case evalExpr expr initialEnv of
          Left err -> fail $ "Eval error: " ++ show err
          Right val -> val `shouldBe` VKeyword "name"
    
    it "evaluates keyword comparison (= name: name:)" $ do
      case parseExpr "(= name: name:)" of
        Left err -> fail $ "Parse error: " ++ show err
        Right expr -> case evalExpr expr initialEnv of
          Left err -> fail $ "Eval error: " ++ show err
          Right val -> val `shouldBe` VBool True
    
    it "keywords are distinct from symbols (type error if used as symbol)" $ do
      -- Try to use keyword as a variable name (should fail)
      case parseExpr "name:" of
        Left err -> fail $ "Parse error: " ++ show err
        Right expr -> do
          -- Create an environment where "name" is defined
          let envWithName = Map.insert "name" (VString (T.pack "Alice")) initialEnv
          case evalExpr expr envWithName of
            -- Keyword should evaluate to itself, not lookup "name" in environment
            Left err -> fail $ "Eval error: " ++ show err
            Right val -> val `shouldBe` VKeyword "name"  -- Should be keyword, not "Alice"
  
  describe "Sets" $ do
    it "evaluates set literal #{1 2 3}" $ do
      case parseExpr "#{1 2 3}" of
        Left err -> fail $ "Parse error: " ++ show err
        Right expr -> case evalExpr expr initialEnv of
          Left err -> fail $ "Eval error: " ++ show err
          Right val -> do
            case val of
              VSet s -> do
                Set.size s `shouldBe` 3
                Set.member (VNumber 1) s `shouldBe` True
                Set.member (VNumber 2) s `shouldBe` True
                Set.member (VNumber 3) s `shouldBe` True
              _ -> fail $ "Expected VSet, got: " ++ show val
    
    it "removes duplicates from set literal #{1 2 2 3}" $ do
      case parseExpr "#{1 2 2 3}" of
        Left err -> fail $ "Parse error: " ++ show err
        Right expr -> case evalExpr expr initialEnv of
          Left err -> fail $ "Eval error: " ++ show err
          Right val -> do
            case val of
              VSet s -> do
                Set.size s `shouldBe` 3  -- Duplicates removed
                Set.member (VNumber 1) s `shouldBe` True
                Set.member (VNumber 2) s `shouldBe` True
                Set.member (VNumber 3) s `shouldBe` True
              _ -> fail $ "Expected VSet, got: " ++ show val
  
  describe "Maps" $ do
    it "evaluates map literal {name: \"Alice\" age: 30}" $ do
      case parseExpr "{name: \"Alice\" age: 30}" of
        Left err -> fail $ "Parse error: " ++ show err
        Right expr -> case evalExpr expr initialEnv of
          Left err -> fail $ "Eval error: " ++ show err
          Right val -> do
            case val of
              VMap m -> do
                Map.size m `shouldBe` 2
                Map.lookup (KeywordKey "name") m `shouldBe` Just (VString (T.pack "Alice"))
                Map.lookup (KeywordKey "age") m `shouldBe` Just (VNumber 30)
              _ -> fail $ "Expected VMap, got: " ++ show val

