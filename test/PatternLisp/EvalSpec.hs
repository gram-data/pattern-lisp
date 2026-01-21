module PatternLisp.EvalSpec (spec) where

import Test.Hspec
import PatternLisp.Syntax
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
          Right val -> val `shouldBe` VInteger 6
    
    it "evaluates lambda with multiple parameters" $ do
      case parseExpr "((lambda (x y) (+ x y)) 10 20)" of
        Left err -> fail $ "Parse error: " ++ show err
        Right expr -> case evalExpr expr initialEnv of
          Left err -> fail $ "Eval error: " ++ show err
          Right val -> val `shouldBe` VInteger 30
  
  describe "If expressions" $ do
    it "evaluates if with true condition" $ do
      case parseExpr "(if (> 5 3) 'positive 'negative)" of
        Left err -> fail $ "Parse error: " ++ show err
        Right expr -> do
          -- First define x=5 in environment
          let envWithX = Map.insert "x" (VInteger 5) initialEnv
          case evalExpr expr envWithX of
            Left err -> fail $ "Eval error: " ++ show err
            Right val -> val `shouldBe` VString ( "positive")
    
    it "evaluates if with false condition" $ do
      case parseExpr "(if (< 5 3) 'yes 'no)" of
        Left err -> fail $ "Parse error: " ++ show err
        Right expr -> case evalExpr expr initialEnv of
          Left err -> fail $ "Eval error: " ++ show err
          Right val -> val `shouldBe` VString ( "no")
  
  describe "Let expressions" $ do
    it "evaluates let expression (let ((x 10) (y 20)) (+ x y))" $ do
      case parseExpr "(let ((x 10) (y 20)) (+ x y))" of
        Left err -> fail $ "Parse error: " ++ show err
        Right expr -> case evalExpr expr initialEnv of
          Left err -> fail $ "Eval error: " ++ show err
          Right val -> val `shouldBe` VInteger 30
    
    it "evaluates nested let bindings with shadowing" $ do
      case parseExpr "(let ((x 10)) (let ((x 20)) x))" of
        Left err -> fail $ "Parse error: " ++ show err
        Right expr -> case evalExpr expr initialEnv of
          Left err -> fail $ "Eval error: " ++ show err
          Right val -> val `shouldBe` VInteger 20
  
  describe "Quote expressions" $ do
    it "evaluates quote expression (quote (a b c))" $ do
      case parseExpr "(quote (a b c))" of
        Left err -> fail $ "Parse error: " ++ show err
        Right expr -> case evalExpr expr initialEnv of
          Left err -> fail $ "Eval error: " ++ show err
          Right val -> 
            -- Quote should return a list value with symbols as strings
            case val of
              VArray [VString a, VString b, VString c] -> do
                a `shouldBe`  "a"
                b `shouldBe`  "b"
                c `shouldBe`  "c"
              _ -> fail $ "Expected quoted list, got: " ++ show val
    
    it "evaluates single quote syntax '(a b c)" $ do
      case parseExpr "'(a b c)" of
        Left err -> fail $ "Parse error: " ++ show err
        Right expr -> case evalExpr expr initialEnv of
          Left err -> fail $ "Eval error: " ++ show err
          Right val ->
            case val of
              VArray [VString a, VString b, VString c] -> do
                a `shouldBe`  "a"
                b `shouldBe`  "b"
                c `shouldBe`  "c"
              _ -> fail $ "Expected quoted list, got: " ++ show val
  
  describe "Begin expressions" $ do
    it "evaluates begin with multiple expressions" $ do
      case parseExpr "(begin (define x 5) (+ x 1))" of
        Left err -> fail $ "Parse error: " ++ show err
        Right expr -> do
          case evalExprWithEnv expr initialEnv of
            Left err -> fail $ "Eval error: " ++ show err
            Right (val, env) -> do
              val `shouldBe` VInteger 6
              -- Check that x is defined in environment
              case Map.lookup "x" env of
                Just (VInteger 5) -> True `shouldBe` True
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
              val `shouldBe` VString ( "square")
              -- Now use the defined function
              case parseExpr "(square 4)" of
                Left err -> fail $ "Parse error: " ++ show err
                Right callExpr -> case evalExpr callExpr env of
                  Left err -> fail $ "Eval error: " ++ show err
                  Right result -> result `shouldBe` VInteger 16
    
    it "evaluates define with simple value" $ do
      case parseExpr "(define x 10)" of
        Left err -> fail $ "Parse error: " ++ show err
        Right expr -> do
          case evalExprWithEnv expr initialEnv of
            Left err -> fail $ "Eval error: " ++ show err
            Right (val, env) -> do
              val `shouldBe` VString ( "x")
              case Map.lookup "x" env of
                Just (VInteger 10) -> True `shouldBe` True
                _ -> fail "x should be defined as 10 in environment"
  
  describe "Closure capturing lexical environment" $ do
    it "evaluates closure that captures lexical environment" $ do
      -- Define a function that uses a variable from outer scope
      case parseExpr "(let ((x 10)) ((lambda (y) (+ x y)) 5))" of
        Left err -> fail $ "Parse error: " ++ show err
        Right expr -> case evalExpr expr initialEnv of
          Left err -> fail $ "Eval error: " ++ show err
          Right val -> val `shouldBe` VInteger 15
  
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
          Right val -> val `shouldBe` VBoolean True
    
    it "keywords are distinct from symbols (type error if used as symbol)" $ do
      -- Try to use keyword as a variable name (should fail)
      case parseExpr "name:" of
        Left err -> fail $ "Parse error: " ++ show err
        Right expr -> do
          -- Create an environment where "name" is defined
          let envWithName = Map.insert "name" (VString ( "Alice")) initialEnv
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
                Set.member (VInteger 1) s `shouldBe` True
                Set.member (VInteger 2) s `shouldBe` True
                Set.member (VInteger 3) s `shouldBe` True
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
                Set.member (VInteger 1) s `shouldBe` True
                Set.member (VInteger 2) s `shouldBe` True
                Set.member (VInteger 3) s `shouldBe` True
              _ -> fail $ "Expected VSet, got: " ++ show val
  
  describe "Maps" $ do
    it "evaluates map literal {name: \"Alice\" age: 30}" $ do
      case parseExpr "{name: \"Alice\", age: 30}" of
        Left err -> fail $ "Parse error: " ++ show err
        Right expr -> case evalExpr expr initialEnv of
          Left err -> fail $ "Eval error: " ++ show err
          Right val -> do
            case val of
              VMap m -> do
                Map.size m `shouldBe` 2
                Map.lookup "name" m `shouldBe` Just (VString "Alice")
                Map.lookup "age" m `shouldBe` Just (VInteger 30)
              _ -> fail $ "Expected VMap, got: " ++ show val
    
    it "duplicate keys: last value wins {name: \"Alice\" name: \"Bob\"}" $ do
      case parseExpr "{name: \"Alice\", name: \"Bob\"}" of
        Left err -> fail $ "Parse error: " ++ show err
        Right expr -> case evalExpr expr initialEnv of
          Left err -> fail $ "Eval error: " ++ show err
          Right val -> do
            case val of
              VMap m -> do
                Map.size m `shouldBe` 1
                Map.lookup "name" m `shouldBe` Just (VString "Bob")  -- Last value wins
              _ -> fail $ "Expected VMap, got: " ++ show val
    
    it "evaluates records with different value types" $ do
      case parseExpr "{name: \"Alice\", age: 30, active: true, count: 0}" of
        Left err -> fail $ "Parse error: " ++ show err
        Right expr -> case evalExpr expr initialEnv of
          Left err -> fail $ "Eval error: " ++ show err
          Right val -> do
            case val of
              VMap m -> do
                Map.lookup "name" m `shouldBe` Just (VString "Alice")
                Map.lookup "age" m `shouldBe` Just (VInteger 30)
                Map.lookup "active" m `shouldBe` Just (VBoolean True)
                Map.lookup "count" m `shouldBe` Just (VInteger 0)
              _ -> fail $ "Expected VMap, got: " ++ show val
    
    it "record equality is structural and order-independent" $ do
      case (parseExpr "{a: 1, b: 2}", parseExpr "{b: 2, a: 1}") of
        (Right expr1, Right expr2) -> do
          val1 <- case evalExpr expr1 initialEnv of
            Left err -> fail $ "Eval error 1: " ++ show err
            Right v -> return v
          val2 <- case evalExpr expr2 initialEnv of
            Left err -> fail $ "Eval error 2: " ++ show err
            Right v -> return v
          val1 `shouldBe` val2  -- Should be equal despite different key order
  
  describe "Record quasiquotation" $ do
    it "evaluates unquoting in record literal" $ do
      case parseExpr "(let ((name \"Alice\") (age 30)) `{ name: ,name, age: ,age })" of
        Left err -> fail $ "Parse error: " ++ show err
        Right expr -> case evalExprWithEnv expr initialEnv of
          Left err -> fail $ "Eval error: " ++ show err
          Right (val, _) -> do
            case val of
              VMap m -> do
                Map.lookup "name" m `shouldBe` Just (VString "Alice")
                Map.lookup "age" m `shouldBe` Just (VInteger 30)
              _ -> fail $ "Expected VMap, got: " ++ show val
    
    it "evaluates splicing record in record literal" $ do
      case parseExpr "(let ((base { role: \"Engineer\" })) `{ name: \"Alice\", ,@base })" of
        Left err -> fail $ "Parse error: " ++ show err
        Right expr -> case evalExprWithEnv expr initialEnv of
          Left err -> fail $ "Eval error: " ++ show err
          Right (val, _) -> do
            case val of
              VMap m -> do
                Map.lookup "name" m `shouldBe` Just (VString "Alice")
                Map.lookup "role" m `shouldBe` Just (VString "Engineer")
              _ -> fail $ "Expected VMap, got: " ++ show val
    
    it "evaluates combined unquoting and splicing" $ do
      case parseExpr "(let ((name \"Alice\") (base { age: 30 })) `{ name: ,name, ,@base })" of
        Left err -> fail $ "Parse error: " ++ show err
        Right expr -> case evalExprWithEnv expr initialEnv of
          Left err -> fail $ "Eval error: " ++ show err
          Right (val, _) -> do
            case val of
              VMap m -> do
                Map.lookup "name" m `shouldBe` Just (VString "Alice")
                Map.lookup "age" m `shouldBe` Just (VInteger 30)
              _ -> fail $ "Expected VMap, got: " ++ show val
    
    it "reports error when splicing non-record" $ do
      case parseExpr "`{ name: \"Alice\", ,@42 }" of
        Left err -> fail $ "Parse error: " ++ show err
        Right expr -> case evalExpr expr initialEnv of
          Left (TypeMismatch _ _) -> True `shouldBe` True
          Left err -> fail $ "Expected TypeMismatch, got: " ++ show err
          Right _ -> fail "Expected error for splicing non-record"

    it "exprToValue/quasiquote: spliced record overrides earlier key (spliceMap takes precedence)" $ do
      -- In `{x: 1, ,@b}` with b={x: 2}, the splice must override: x=2. exprToValue
      -- (used for Quote) had used Map.union acc spliceMap; it must be
      -- Map.union spliceMap acc to match eval's semantics.
      case parseExpr "(let ((b {x: 2})) `{x: 1, ,@b})" of
        Left err -> fail $ "Parse error: " ++ show err
        Right expr -> case evalExprWithEnv expr initialEnv of
          Left err -> fail $ "Eval error: " ++ show err
          Right (val, _) -> do
            case val of
              VMap m -> Map.lookup "x" m `shouldBe` Just (VInteger 2)
              _ -> fail $ "Expected VMap with x=2, got: " ++ show val
  
  describe "Record edge cases" $ do
    it "handles nested records (2 levels deep)" $ do
      -- Test basic nesting (gram parser can handle 2 levels)
      case parseExpr "{outer: {inner: \"value\"}}" of
        Left err -> fail $ "Parse error: " ++ show err
        Right expr -> case evalExpr expr initialEnv of
          Left err -> fail $ "Eval error: " ++ show err
          Right val -> do
            case val of
              VMap m -> do
                Map.size m `shouldBe` 1
                case Map.lookup "outer" m of
                  Just (VMap inner) -> case Map.lookup "inner" inner of
                    Just (VString "value") -> True `shouldBe` True
                    _ -> fail "Expected string value in inner record"
                  _ -> fail "Expected inner record"
              _ -> fail $ "Expected VMap, got: " ++ show val
    
    it "handles records with multiple value types as values" $ do
      -- Test records with various value types (avoiding nested records and keywords to prevent gram parser issues)
      case parseExpr "{integer: 42, string: \"hello\", boolean: true}" of
        Left err -> fail $ "Parse error: " ++ show err
        Right expr -> case evalExpr expr initialEnv of
          Left err -> fail $ "Eval error: " ++ show err
          Right val -> do
            case val of
              VMap m -> do
                Map.size m `shouldBe` 3
                Map.lookup "integer" m `shouldBe` Just (VInteger 42)
                Map.lookup "string" m `shouldBe` Just (VString "hello")
                Map.lookup "boolean" m `shouldBe` Just (VBoolean True)
              _ -> fail $ "Expected VMap, got: " ++ show val
    
    it "handles empty records" $ do
      case parseExpr "{a: {}, b: {}}" of
        Left err -> fail $ "Parse error: " ++ show err
        Right expr -> case evalExpr expr initialEnv of
          Left err -> fail $ "Eval error: " ++ show err
          Right val -> do
            case val of
              VMap m -> do
                Map.size m `shouldBe` 2
                case Map.lookup "a" m of
                  Just (VMap empty1) -> Map.size empty1 `shouldBe` 0
                  _ -> fail "Expected empty record"
                case Map.lookup "b" m of
                  Just (VMap empty2) -> Map.size empty2 `shouldBe` 0
                  _ -> fail "Expected empty record"
              _ -> fail $ "Expected VMap, got: " ++ show val
  
  describe "Subject Labels as String Sets" $ do
    it "creates Subject label set #{\"Person\" \"Employee\"}" $ do
      case parseExpr "#{\"Person\" \"Employee\"}" of
        Left err -> fail $ "Parse error: " ++ show err
        Right expr -> case evalExpr expr initialEnv of
          Left err -> fail $ "Eval error: " ++ show err
          Right val -> do
            case val of
              VSet s -> do
                Set.size s `shouldBe` 2
                Set.member (VString ( "Person")) s `shouldBe` True
                Set.member (VString ( "Employee")) s `shouldBe` True
              _ -> fail $ "Expected VSet of strings, got: " ++ show val
    
    it "checks Subject label set membership (contains? #{\"Person\" \"Employee\"} \"Person\")" $ do
      case parseExpr "(contains? #{\"Person\" \"Employee\"} \"Person\")" of
        Left err -> fail $ "Parse error: " ++ show err
        Right expr -> case evalExpr expr initialEnv of
          Left err -> fail $ "Eval error: " ++ show err
          Right val -> val `shouldBe` VBoolean True

