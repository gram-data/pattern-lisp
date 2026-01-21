module PatternLisp.PrimitivesSpec (spec) where

import Test.Hspec
import PatternLisp.Syntax
import PatternLisp.Parser
import PatternLisp.Eval
import PatternLisp.Primitives
import qualified Data.Text as T
import qualified Data.Map as Map
import qualified Data.Set as Set

spec :: Spec
spec = describe "PatternLisp.Primitives and PatternLisp.Eval" $ do
  describe "Arithmetic operations" $ do
    it "evaluates addition (+ 1 2 3)" $ do
      case parseExpr "(+ 1 2 3)" of
        Left err -> fail $ "Parse error: " ++ show err
        Right expr -> case evalExpr expr initialEnv of
          Left err -> fail $ "Eval error: " ++ show err
          Right val -> val `shouldBe` VInteger 6
    
    it "evaluates subtraction (- 10 3)" $ do
      case parseExpr "(- 10 3)" of
        Left err -> fail $ "Parse error: " ++ show err
        Right expr -> case evalExpr expr initialEnv of
          Left err -> fail $ "Eval error: " ++ show err
          Right val -> val `shouldBe` VInteger 7
    
    it "evaluates multiplication (* 4 5)" $ do
      case parseExpr "(* 4 5)" of
        Left err -> fail $ "Parse error: " ++ show err
        Right expr -> case evalExpr expr initialEnv of
          Left err -> fail $ "Eval error: " ++ show err
          Right val -> val `shouldBe` VInteger 20
    
    it "evaluates division (/ 15 3)" $ do
      case parseExpr "(/ 15 3)" of
        Left err -> fail $ "Parse error: " ++ show err
        Right expr -> case evalExpr expr initialEnv of
          Left err -> fail $ "Eval error: " ++ show err
          Right val -> val `shouldBe` VInteger 5
  
  describe "Comparison operations" $ do
    it "evaluates greater than (> 5 3)" $ do
      case parseExpr "(> 5 3)" of
        Left err -> fail $ "Parse error: " ++ show err
        Right expr -> case evalExpr expr initialEnv of
          Left err -> fail $ "Eval error: " ++ show err
          Right val -> val `shouldBe` VBoolean True
    
    it "evaluates less than (< 3 5)" $ do
      case parseExpr "(< 3 5)" of
        Left err -> fail $ "Parse error: " ++ show err
        Right expr -> case evalExpr expr initialEnv of
          Left err -> fail $ "Eval error: " ++ show err
          Right val -> val `shouldBe` VBoolean True
    
    it "evaluates equal (= 5 5)" $ do
      case parseExpr "(= 5 5)" of
        Left err -> fail $ "Parse error: " ++ show err
        Right expr -> case evalExpr expr initialEnv of
          Left err -> fail $ "Eval error: " ++ show err
          Right val -> val `shouldBe` VBoolean True
    
    it "evaluates not equal (/= 5 3)" $ do
      case parseExpr "(/= 5 3)" of
        Left err -> fail $ "Parse error: " ++ show err
        Right expr -> case evalExpr expr initialEnv of
          Left err -> fail $ "Eval error: " ++ show err
          Right val -> val `shouldBe` VBoolean True
  
  describe "String operations" $ do
    it "evaluates string-append (string-append \"hello\" \" world\")" $ do
      case parseExpr "(string-append \"hello\" \" world\")" of
        Left err -> fail $ "Parse error: " ++ show err
        Right expr -> case evalExpr expr initialEnv of
          Left err -> fail $ "Eval error: " ++ show err
          Right val -> val `shouldBe` VString ( "hello world")
    
    it "evaluates string-length (string-length \"hello\")" $ do
      case parseExpr "(string-length \"hello\")" of
        Left err -> fail $ "Parse error: " ++ show err
        Right expr -> case evalExpr expr initialEnv of
          Left err -> fail $ "Eval error: " ++ show err
          Right val -> val `shouldBe` VInteger 5
    
    it "evaluates substring (substring \"hello\" 1 3)" $ do
      case parseExpr "(substring \"hello\" 1 3)" of
        Left err -> fail $ "Parse error: " ++ show err
        Right expr -> case evalExpr expr initialEnv of
          Left err -> fail $ "Eval error: " ++ show err
          Right val -> val `shouldBe` VString ( "el")
  
  describe "Nested expressions" $ do
    it "evaluates nested expression (+ (* 2 3) 4)" $ do
      case parseExpr "(+ (* 2 3) 4)" of
        Left err -> fail $ "Parse error: " ++ show err
        Right expr -> case evalExpr expr initialEnv of
          Left err -> fail $ "Eval error: " ++ show err
          Right val -> val `shouldBe` VInteger 10
  
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
  
  describe "Pattern construction operations" $ do
    it "evaluates pure construction (pure \"hello\")" $ do
      case parseExpr "(pure \"hello\")" of
        Left err -> fail $ "Parse error: " ++ show err
        Right expr -> case evalExpr expr initialEnv of
          Left err -> fail $ "Eval error: " ++ show err
          Right val -> case val of
            VPattern _ -> True `shouldBe` True
            _ -> fail $ "Expected VPattern, got: " ++ show val
    
    it "evaluates pattern construction (pattern \"root\" '())" $ do
      case parseExpr "(pattern \"root\" '())" of
        Left err -> fail $ "Parse error: " ++ show err
        Right expr -> case evalExpr expr initialEnv of
          Left err -> fail $ "Eval error: " ++ show err
          Right val -> case val of
            VPattern _ -> True `shouldBe` True
            _ -> fail $ "Expected VPattern, got: " ++ show val
    
    -- Note: Testing pattern with multiple elements requires list construction
    -- primitives that aren't yet available. This will be tested more comprehensively
    -- in Phase 3 when pattern query operations are implemented.
    
    it "handles pure arity mismatch" $ do
      case parseExpr "(pure)" of
        Left err -> fail $ "Parse error: " ++ show err
        Right expr -> case evalExpr expr initialEnv of
          Left (ArityMismatch _ _ _) -> True `shouldBe` True
          Left err -> fail $ "Unexpected error: " ++ show err
          Right _ -> fail "Expected ArityMismatch error"
    
    it "handles pattern arity mismatch (missing elements)" $ do
      case parseExpr "(pattern \"root\")" of
        Left err -> fail $ "Parse error: " ++ show err
        Right expr -> case evalExpr expr initialEnv of
          Left (ArityMismatch _ _ _) -> True `shouldBe` True
          Left err -> fail $ "Unexpected error: " ++ show err
          Right _ -> fail "Expected ArityMismatch error"
    
    it "handles pattern type error for non-list second argument" $ do
      case parseExpr "(pattern \"root\" \"not-a-list\")" of
        Left err -> fail $ "Parse error: " ++ show err
        Right expr -> case evalExpr expr initialEnv of
          Left (TypeMismatch _ _) -> True `shouldBe` True
          Left err -> fail $ "Unexpected error: " ++ show err
          Right _ -> fail "Expected TypeMismatch error"
  
  describe "Set operations" $ do
    it "evaluates contains? for sets (contains? #{1 2 3} 2)" $ do
      case parseExpr "(contains? #{1 2 3} 2)" of
        Left err -> fail $ "Parse error: " ++ show err
        Right expr -> case evalExpr expr initialEnv of
          Left err -> fail $ "Eval error: " ++ show err
          Right val -> val `shouldBe` VBoolean True
    
    it "evaluates set-union (set-union #{1 2} #{2 3})" $ do
      case parseExpr "(set-union #{1 2} #{2 3})" of
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
    
    it "evaluates set-intersection (set-intersection #{1 2 3} #{2 3 4})" $ do
      case parseExpr "(set-intersection #{1 2 3} #{2 3 4})" of
        Left err -> fail $ "Parse error: " ++ show err
        Right expr -> case evalExpr expr initialEnv of
          Left err -> fail $ "Eval error: " ++ show err
          Right val -> do
            case val of
              VSet s -> do
                Set.size s `shouldBe` 2
                Set.member (VInteger 2) s `shouldBe` True
                Set.member (VInteger 3) s `shouldBe` True
              _ -> fail $ "Expected VSet, got: " ++ show val
    
    it "evaluates set-difference (set-difference #{1 2 3} #{2})" $ do
      case parseExpr "(set-difference #{1 2 3} #{2})" of
        Left err -> fail $ "Parse error: " ++ show err
        Right expr -> case evalExpr expr initialEnv of
          Left err -> fail $ "Eval error: " ++ show err
          Right val -> do
            case val of
              VSet s -> do
                Set.size s `shouldBe` 2
                Set.member (VInteger 1) s `shouldBe` True
                Set.member (VInteger 3) s `shouldBe` True
              _ -> fail $ "Expected VSet, got: " ++ show val
    
    it "evaluates set-symmetric-difference (set-symmetric-difference #{1 2} #{2 3})" $ do
      case parseExpr "(set-symmetric-difference #{1 2} #{2 3})" of
        Left err -> fail $ "Parse error: " ++ show err
        Right expr -> case evalExpr expr initialEnv of
          Left err -> fail $ "Eval error: " ++ show err
          Right val -> do
            case val of
              VSet s -> do
                Set.size s `shouldBe` 2
                Set.member (VInteger 1) s `shouldBe` True
                Set.member (VInteger 3) s `shouldBe` True
              _ -> fail $ "Expected VSet, got: " ++ show val
    
    it "evaluates set-subset? (set-subset? #{1 2} #{1 2 3})" $ do
      case parseExpr "(set-subset? #{1 2} #{1 2 3})" of
        Left err -> fail $ "Parse error: " ++ show err
        Right expr -> case evalExpr expr initialEnv of
          Left err -> fail $ "Eval error: " ++ show err
          Right val -> val `shouldBe` VBoolean True
    
    it "evaluates set-equal? (set-equal? #{1 2 3} #{3 2 1})" $ do
      case parseExpr "(set-equal? #{1 2 3} #{3 2 1})" of
        Left err -> fail $ "Parse error: " ++ show err
        Right expr -> case evalExpr expr initialEnv of
          Left err -> fail $ "Eval error: " ++ show err
          Right val -> val `shouldBe` VBoolean True
    
    it "evaluates empty? for sets (empty? #{})" $ do
      case parseExpr "(empty? #{})" of
        Left err -> fail $ "Parse error: " ++ show err
        Right expr -> case evalExpr expr initialEnv of
          Left err -> fail $ "Eval error: " ++ show err
          Right val -> val `shouldBe` VBoolean True
    
    it "evaluates hash-set constructor (hash-set 1 2 3)" $ do
      case parseExpr "(hash-set 1 2 3)" of
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
  
  describe "Record operations" $ do
    it "evaluates get primitive (get {name: \"Alice\"} \"name\")" $ do
      case parseExpr "(get {name: \"Alice\"} \"name\")" of
        Left err -> fail $ "Parse error: " ++ show err
        Right expr -> case evalExpr expr initialEnv of
          Left err -> fail $ "Eval error: " ++ show err
          Right val -> val `shouldBe` VString ( "Alice")
    
    it "evaluates get with default (get {name: \"Alice\"} \"age\" 0)" $ do
      case parseExpr "(get {name: \"Alice\"} \"age\" 0)" of
        Left err -> fail $ "Parse error: " ++ show err
        Right expr -> case evalExpr expr initialEnv of
          Left err -> fail $ "Eval error: " ++ show err
          Right val -> val `shouldBe` VInteger 0
    
    -- get-in removed - use nested get instead
    it "evaluates nested get (get (get {user: {name: \"Alice\"}} \"user\") \"name\")" $ do
      case parseExpr "(get (get {user: {name: \"Alice\"}} \"user\") \"name\")" of
        Left err -> fail $ "Parse error: " ++ show err
        Right expr -> case evalExpr expr initialEnv of
          Left err -> fail $ "Eval error: " ++ show err
          Right val -> val `shouldBe` VString "Alice"
    
    it "nested get returns record when path ends at record (get {a: {b: 42}} \"a\")" $ do
      case parseExpr "(get {a: {b: 42}} \"a\")" of
        Left err -> fail $ "Parse error: " ++ show err
        Right expr -> case evalExpr expr initialEnv of
          Left err -> fail $ "Eval error: " ++ show err
          Right val -> do
            case val of
              VMap nestedMap -> do
                Map.lookup "b" nestedMap `shouldBe` Just (VInteger 42)
              _ -> fail $ "Expected VMap {b: 42}, got: " ++ show val
    
    it "evaluates assoc primitive (assoc {name: \"Alice\"} \"age\" 30)" $ do
      case parseExpr "(assoc {name: \"Alice\"} \"age\" 30)" of
        Left err -> fail $ "Parse error: " ++ show err
        Right expr -> case evalExpr expr initialEnv of
          Left err -> fail $ "Eval error: " ++ show err
          Right val -> do
            case val of
              VMap m -> do
                Map.lookup "name" m `shouldBe` Just (VString "Alice")
                Map.lookup "age" m `shouldBe` Just (VInteger 30)
              _ -> fail $ "Expected VMap, got: " ++ show val
    
    it "evaluates dissoc primitive (dissoc {name: \"Alice\", age: 30} \"age\")" $ do
      case parseExpr "(dissoc {name: \"Alice\", age: 30} \"age\")" of
        Left err -> fail $ "Parse error: " ++ show err
        Right expr -> case evalExpr expr initialEnv of
          Left err -> fail $ "Eval error: " ++ show err
          Right val -> do
            case val of
              VMap m -> do
                Map.lookup "name" m `shouldBe` Just (VString "Alice")
                Map.member "age" m `shouldBe` False
              _ -> fail $ "Expected VMap, got: " ++ show val
    
    -- update operation removed - use get + assoc pattern instead
    
    it "evaluates contains? for records (contains? {name: \"Alice\"} \"name\")" $ do
      case parseExpr "(contains? {name: \"Alice\"} \"name\")" of
        Left err -> fail $ "Parse error: " ++ show err
        Right expr -> case evalExpr expr initialEnv of
          Left err -> fail $ "Eval error: " ++ show err
          Right val -> val `shouldBe` VBoolean True
    
    it "evaluates empty? for records (empty? {})" $ do
      case parseExpr "(empty? {})" of
        Left err -> fail $ "Parse error: " ++ show err
        Right expr -> case evalExpr expr initialEnv of
          Left err -> fail $ "Eval error: " ++ show err
          Right val -> val `shouldBe` VBoolean True
    
    it "evaluates hash-map constructor (hash-map name: \"Alice\" age: 30)" $ do
      case parseExpr "(hash-map name: \"Alice\" age: 30)" of
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
  
  describe "Record operations" $ do
    it "evaluates record? type predicate" $ do
      case parseExpr "(record? {name: \"Alice\"})" of
        Left err -> fail $ "Parse error: " ++ show err
        Right expr -> case evalExpr expr initialEnv of
          Left err -> fail $ "Eval error: " ++ show err
          Right val -> val `shouldBe` VBoolean True
      case parseExpr "(record? 42)" of
        Left err -> fail $ "Parse error: " ++ show err
        Right expr -> case evalExpr expr initialEnv of
          Left err -> fail $ "Eval error: " ++ show err
          Right val -> val `shouldBe` VBoolean False
    
    it "evaluates get with existing key" $ do
      case parseExpr "(get {name: \"Alice\", age: 30} \"name\")" of
        Left err -> fail $ "Parse error: " ++ show err
        Right expr -> case evalExpr expr initialEnv of
          Left err -> fail $ "Eval error: " ++ show err
          Right val -> val `shouldBe` VString "Alice"
    
    it "evaluates get with missing key (no default)" $ do
      case parseExpr "(get {name: \"Alice\"} \"age\")" of
        Left err -> fail $ "Parse error: " ++ show err
        Right expr -> case evalExpr expr initialEnv of
          Left err -> fail $ "Eval error: " ++ show err
          Right val -> val `shouldBe` VArray []  -- nil
    
    it "evaluates get with missing key (with default)" $ do
      case parseExpr "(get {name: \"Alice\"} \"age\" 0)" of
        Left err -> fail $ "Parse error: " ++ show err
        Right expr -> case evalExpr expr initialEnv of
          Left err -> fail $ "Eval error: " ++ show err
          Right val -> val `shouldBe` VInteger 0
    
    it "evaluates has? predicate" $ do
      case parseExpr "(has? {name: \"Alice\"} \"name\")" of
        Left err -> fail $ "Parse error: " ++ show err
        Right expr -> case evalExpr expr initialEnv of
          Left err -> fail $ "Eval error: " ++ show err
          Right val -> val `shouldBe` VBoolean True
      case parseExpr "(has? {name: \"Alice\"} \"age\")" of
        Left err -> fail $ "Parse error: " ++ show err
        Right expr -> case evalExpr expr initialEnv of
          Left err -> fail $ "Eval error: " ++ show err
          Right val -> val `shouldBe` VBoolean False
    
    it "evaluates keys operation" $ do
      case parseExpr "(keys {name: \"Alice\", age: 30})" of
        Left err -> fail $ "Parse error: " ++ show err
        Right expr -> case evalExpr expr initialEnv of
          Left err -> fail $ "Eval error: " ++ show err
          Right val -> do
            case val of
              VArray keys -> do
                length keys `shouldBe` 2
                VString "name" `elem` keys `shouldBe` True
                VString "age" `elem` keys `shouldBe` True
              _ -> fail $ "Expected VArray of keys, got: " ++ show val
    
    it "evaluates values operation" $ do
      case parseExpr "(values {name: \"Alice\", age: 30})" of
        Left err -> fail $ "Parse error: " ++ show err
        Right expr -> case evalExpr expr initialEnv of
          Left err -> fail $ "Eval error: " ++ show err
          Right val -> do
            case val of
              VArray values -> do
                length values `shouldBe` 2
                VString "Alice" `elem` values `shouldBe` True
                VInteger 30 `elem` values `shouldBe` True
              _ -> fail $ "Expected VArray of values, got: " ++ show val
    
    it "evaluates assoc operation (immutability)" $ do
      case parseExpr "(let ((r {name: \"Alice\"})) (assoc r \"age\" 30))" of
        Left err -> fail $ "Parse error: " ++ show err
        Right expr -> case evalExprWithEnv expr initialEnv of
          Left err -> fail $ "Eval error: " ++ show err
          Right (val, _) -> do
            case val of
              VMap m -> do
                Map.lookup "name" m `shouldBe` Just (VString "Alice")
                Map.lookup "age" m `shouldBe` Just (VInteger 30)
              _ -> fail $ "Expected VMap, got: " ++ show val
    
    it "evaluates dissoc operation (immutability)" $ do
      case parseExpr "(let ((r {name: \"Alice\", age: 30})) (dissoc r \"age\"))" of
        Left err -> fail $ "Parse error: " ++ show err
        Right expr -> case evalExprWithEnv expr initialEnv of
          Left err -> fail $ "Eval error: " ++ show err
          Right (val, _) -> do
            case val of
              VMap m -> do
                Map.lookup "name" m `shouldBe` Just (VString "Alice")
                Map.member "age" m `shouldBe` False
              _ -> fail $ "Expected VMap, got: " ++ show val
    
    it "evaluates merge operation" $ do
      case parseExpr "(merge {name: \"Alice\"} {age: 30})" of
        Left err -> fail $ "Parse error: " ++ show err
        Right expr -> case evalExpr expr initialEnv of
          Left err -> fail $ "Eval error: " ++ show err
          Right val -> do
            case val of
              VMap m -> do
                Map.lookup "name" m `shouldBe` Just (VString "Alice")
                Map.lookup "age" m `shouldBe` Just (VInteger 30)
              _ -> fail $ "Expected VMap, got: " ++ show val
      case parseExpr "(merge {name: \"Alice\"} {name: \"Bob\"})" of
        Left err -> fail $ "Parse error: " ++ show err
        Right expr -> case evalExpr expr initialEnv of
          Left err -> fail $ "Eval error: " ++ show err
          Right val -> do
            case val of
              VMap m -> Map.lookup "name" m `shouldBe` Just (VString "Bob")  -- Right takes precedence
              _ -> fail $ "Expected VMap, got: " ++ show val
    
    it "evaluates map operation" $ do
      case parseExpr "(map (lambda (k v) (* v 2)) {count: 5, total: 10})" of
        Left err -> fail $ "Parse error: " ++ show err
        Right expr -> case evalExpr expr initialEnv of
          Left err -> fail $ "Eval error: " ++ show err
          Right val -> do
            case val of
              VMap m -> do
                Map.lookup "count" m `shouldBe` Just (VInteger 10)
                Map.lookup "total" m `shouldBe` Just (VInteger 20)
              _ -> fail $ "Expected VMap, got: " ++ show val
    
    it "evaluates filter operation" $ do
      case parseExpr "(filter (lambda (k v) (> v 5)) {a: 10, b: 3, c: 7})" of
        Left err -> fail $ "Parse error: " ++ show err
        Right expr -> case evalExpr expr initialEnv of
          Left err -> fail $ "Eval error: " ++ show err
          Right val -> do
            case val of
              VMap m -> do
                Map.size m `shouldBe` 2
                Map.lookup "a" m `shouldBe` Just (VInteger 10)
                Map.lookup "c" m `shouldBe` Just (VInteger 7)
                Map.member "b" m `shouldBe` False
              _ -> fail $ "Expected VMap, got: " ++ show val
    
    it "evaluates record->alist conversion" $ do
      case parseExpr "(record->alist {name: \"Alice\", age: 30})" of
        Left err -> fail $ "Parse error: " ++ show err
        Right expr -> case evalExpr expr initialEnv of
          Left err -> fail $ "Eval error: " ++ show err
          Right val -> do
            case val of
              VArray pairs -> do
                length pairs `shouldBe` 2
                -- Check that pairs are [key, value] format
                all (\pair -> case pair of VArray [_, _] -> True; _ -> False) pairs `shouldBe` True
              _ -> fail $ "Expected VArray of pairs, got: " ++ show val
    
    it "evaluates alist->record conversion" $ do
      case parseExpr "(alist->record '((\"name\" \"Alice\") (\"age\" 30)))" of
        Left err -> fail $ "Parse error: " ++ show err
        Right expr -> case evalExpr expr initialEnv of
          Left err -> fail $ "Eval error: " ++ show err
          Right val -> do
            case val of
              VMap m -> do
                Map.lookup "name" m `shouldBe` Just (VString "Alice")
                Map.lookup "age" m `shouldBe` Just (VInteger 30)
              _ -> fail $ "Expected VMap, got: " ++ show val
    
    it "evaluates record constructor function" $ do
      case parseExpr "(record \"name\" \"Alice\" \"age\" 30)" of
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
    
    it "verifies immutability (original records unchanged)" $ do
      case parseExpr "(let ((r {name: \"Alice\"})) (begin (assoc r \"age\" 30) r))" of
        Left err -> fail $ "Parse error: " ++ show err
        Right expr -> case evalExprWithEnv expr initialEnv of
          Left err -> fail $ "Eval error: " ++ show err
          Right (val, _) -> do
            case val of
              VMap m -> do
                -- Original record should be unchanged (no age key)
                Map.size m `shouldBe` 1
                Map.lookup "name" m `shouldBe` Just (VString "Alice")
                Map.member "age" m `shouldBe` False
              _ -> fail $ "Expected VMap, got: " ++ show val
  
  describe "Record performance tests" $ do
    it "handles large records (1000+ keys) efficiently" $ do
      -- Create a record with 1000 keys programmatically using record constructor
      let keys = map (\i -> "key" ++ show i) [1..1000]
          recordArgs = concatMap (\i -> ["\"key" ++ show i ++ "\"", show i]) [1..1000]
          recordExpr = "(record " ++ unwords recordArgs ++ ")"
      case parseExpr recordExpr of
        Left err -> fail $ "Parse error: " ++ show err
        Right expr -> case evalExpr expr initialEnv of
          Left err -> fail $ "Eval error: " ++ show err
          Right val -> do
            case val of
              VMap m -> do
                Map.size m `shouldBe` 1000
                -- Verify we can access keys
                Map.lookup "key1" m `shouldBe` Just (VInteger 1)
                Map.lookup "key500" m `shouldBe` Just (VInteger 500)
                Map.lookup "key1000" m `shouldBe` Just (VInteger 1000)
              _ -> fail $ "Expected VMap, got: " ++ show val
    
    it "handles large record operations (get, has?, keys, values) efficiently" $ do
      -- Create a record with 2000 keys and test operations
      let recordArgs = concatMap (\i -> ["\"k" ++ show i ++ "\"", show i]) [1..2000]
          recordExpr = "(record " ++ unwords recordArgs ++ ")"
          testExpr = "(let ((r " ++ recordExpr ++ ")) (begin (get r \"k1000\") (has? r \"k1500\") (keys r) (values r) r))"
      case parseExpr testExpr of
        Left err -> fail $ "Parse error: " ++ show err
        Right expr -> case evalExpr expr initialEnv of
          Left err -> fail $ "Eval error: " ++ show err
          Right val -> do
            case val of
              VMap m -> Map.size m `shouldBe` 2000
              _ -> fail $ "Expected VMap, got: " ++ show val
    
    it "handles large record merge operations efficiently" $ do
      -- Create two records with 500 keys each, merge them
      let record1Args = concatMap (\i -> ["\"a" ++ show i ++ "\"", "1"]) [1..500]
          record2Args = concatMap (\i -> ["\"b" ++ show i ++ "\"", "2"]) [1..500]
          record1Expr = "(record " ++ unwords record1Args ++ ")"
          record2Expr = "(record " ++ unwords record2Args ++ ")"
          mergeExpr = "(merge " ++ record1Expr ++ " " ++ record2Expr ++ ")"
      case parseExpr mergeExpr of
        Left err -> fail $ "Parse error: " ++ show err
        Right expr -> case evalExpr expr initialEnv of
          Left err -> fail $ "Eval error: " ++ show err
          Right val -> do
            case val of
              VMap m -> Map.size m `shouldBe` 1000
              _ -> fail $ "Expected VMap, got: " ++ show val

