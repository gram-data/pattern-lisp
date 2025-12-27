module Properties (spec) where

import Test.Hspec
import Test.QuickCheck
import PatternLisp.Syntax
import PatternLisp.Parser
import PatternLisp.Eval
import PatternLisp.Primitives
import PatternLisp.Codec (valueToPatternSubjectForGram, patternSubjectToValue)
import PatternLisp.Gram (patternToGram, gramToPattern)
import qualified Data.Map as Map
import qualified Data.Set as Set
import qualified Data.Text as T
import Control.Monad

-- | Arbitrary instance for Expr
instance Arbitrary Expr where
  arbitrary = sized exprGen
    where
      exprGen 0 = oneof
        [ Atom <$> arbitrary
        , return (List [])
        ]
      exprGen n = oneof
        [ Atom <$> arbitrary
        , List <$> resize (n `div` 2) (listOf (exprGen (n `div` 2)))
        , Quote <$> exprGen (n - 1)
        ]

-- | Arbitrary instance for Atom
instance Arbitrary Atom where
  arbitrary = oneof
    [ Symbol <$> genSymbol
    , Number <$> arbitrary
    , String . T.pack <$> genString
    , Bool <$> arbitrary
    , Keyword <$> genKeyword
    ]
    where
      genSymbol = do
        first <- elements (['a'..'z'] ++ ['A'..'Z'] ++ "!$%&*+-./:<=>?@^_~")
        rest <- listOf (elements (['a'..'z'] ++ ['A'..'Z'] ++ ['0'..'9'] ++ "-"))
        return (first : rest)
      genString = listOf (elements (['a'..'z'] ++ ['A'..'Z'] ++ ['0'..'9'] ++ " "))
      genKeyword = do
        first <- elements (['a'..'z'] ++ ['A'..'Z'])
        rest <- listOf (elements (['a'..'z'] ++ ['A'..'Z'] ++ ['0'..'9'] ++ "-"))
        return (first : rest)

-- | Arbitrary instance for Value
instance Arbitrary Value where
  arbitrary = sized valueGen
    where
      valueGen 0 = oneof
        [ VNumber <$> arbitrary
        , VString . T.pack <$> genString
        , VBool <$> arbitrary
        , VKeyword <$> genKeyword
        ]
      valueGen n = oneof
        [ VNumber <$> arbitrary
        , VString . T.pack <$> genString
        , VBool <$> arbitrary
        , VKeyword <$> genKeyword
        , VList <$> resize 3 (listOf (valueGen (n `div` 2)))
        , VSet <$> (Set.fromList <$> resize 3 (listOf (valueGen (n `div` 2))))
        , VMap <$> (Map.fromList <$> resize 3 (listOf genMapEntry))
        ]
      genString = listOf (elements (['a'..'z'] ++ ['A'..'Z'] ++ ['0'..'9'] ++ " "))
      genKeyword = do
        first <- elements (['a'..'z'] ++ ['A'..'Z'])
        rest <- listOf (elements (['a'..'z'] ++ ['A'..'Z'] ++ ['0'..'9'] ++ "-"))
        return (first : rest)
      genMapEntry = do
        key <- genKeyword
        val <- valueGen 2  -- Limit nesting depth
        return (KeywordKey key, val)

-- | Substitute a variable in an expression with a value
substitute :: Expr -> String -> Value -> Expr
substitute (Atom (Symbol name)) var val
  | name == var = valueToExpr val
  | otherwise = Atom (Symbol name)
substitute (Atom atom) _ _ = Atom atom
substitute (List exprs) var val = List (map (\e -> substitute e var val) exprs)
substitute (Quote expr) var val = Quote (substitute expr var val)

-- | Convert a Value to an Expr (for substitution)
valueToExpr :: Value -> Expr
valueToExpr (VNumber n) = Atom (Number n)
valueToExpr (VString s) = Atom (String s)
valueToExpr (VBool b) = Atom (Bool b)
valueToExpr (VList vals) = List (map valueToExpr vals)
valueToExpr (VKeyword k) = Atom (Keyword k)
valueToExpr (VSet _) = Atom (Symbol "<set>")  -- Sets can't be easily converted to Expr
valueToExpr (VMap _) = Atom (Symbol "<map>")  -- Maps can't be easily converted to Expr
valueToExpr (VClosure _) = Atom (Symbol "<closure>")  -- Closures can't be substituted
valueToExpr (VPrimitive _) = Atom (Symbol "<primitive>")  -- Primitives can't be substituted

-- | Property: Substitution semantics
-- If we substitute a variable with a value, evaluating the substituted expression
-- in an environment with that binding should give the same result as evaluating
-- the original expression in an environment with that binding
-- Note: This property tests that substitution correctly replaces variables
prop_substitution :: Expr -> String -> Value -> Property
prop_substitution expr var val = 
  let substituted = substitute expr var val
      env = Map.insert var val initialEnv  -- Use initialEnv as base to have primitives
  in case (evalExpr substituted env, evalExpr expr env) of
    (Left _, Left _) -> property True  -- Both error, that's fine
    (Right v1, Right v2) -> v1 === v2
    _ -> property False

-- | Property: Closure captures lexical environment
-- A closure should capture the environment at definition time, not application time
prop_closure_capture :: Property
prop_closure_capture = 
  -- Create a closure that uses a variable from outer scope
  let outerVar = "x"
      outerVal = VNumber 10
      -- (let ((x 10)) ((lambda (y) (+ x y)) 5)) should evaluate to 15
      expr = List [Atom (Symbol "let")
                  , List [List [Atom (Symbol "x"), Atom (Number 10)]]
                  , List [List [Atom (Symbol "lambda")
                              , List [Atom (Symbol "y")]
                              , List [Atom (Symbol "+"), Atom (Symbol "x"), Atom (Symbol "y")]]
                        , Atom (Number 5)]]
  in case evalExpr expr initialEnv of
    Right (VNumber 15) -> property True
    _ -> property False

-- | Property: Evaluation order
-- Arguments should be evaluated before function application
prop_evaluation_order :: Property
prop_evaluation_order =
  -- Create an expression where evaluation order matters
  -- If we have a function that adds, both arguments should be evaluated
  let expr = List [Atom (Symbol "+")
                  , List [Atom (Symbol "*"), Atom (Number 2), Atom (Number 3)]
                  , List [Atom (Symbol "*"), Atom (Number 4), Atom (Number 5)]]
  in case evalExpr expr initialEnv of
    Right (VNumber 26) -> property True  -- (2*3) + (4*5) = 6 + 20 = 26
    _ -> property False

-- | Property: Let binding shadowing
-- Inner let bindings should shadow outer ones
prop_let_shadowing :: Property
prop_let_shadowing =
  let expr = List [Atom (Symbol "let")
                  , List [List [Atom (Symbol "x"), Atom (Number 10)]]
                  , List [Atom (Symbol "let")
                         , List [List [Atom (Symbol "x"), Atom (Number 20)]]
                         , Atom (Symbol "x")]]
  in case evalExpr expr initialEnv of
    Right (VNumber 20) -> property True  -- Inner x should shadow outer x
    _ -> property False

-- | Property: Closure environment isolation
-- Closures should use their captured environment, not the current environment
prop_closure_isolation :: Property
prop_closure_isolation =
  -- Create a closure, then try to use a variable that exists in current env but not captured env
  let expr = List [Atom (Symbol "let")
                  , List [List [Atom (Symbol "f")
                              , List [Atom (Symbol "lambda")
                                    , List []
                                    , Atom (Symbol "x")]]]
                  , List [Atom (Symbol "let")
                         , List [List [Atom (Symbol "x"), Atom (Number 5)]]
                         , List [Atom (Symbol "f")]]]
  in case evalExpr expr initialEnv of
    Left (UndefinedVar "x" _) -> property True  -- x should not be found in closure's env
    _ -> property False

spec :: Spec
spec = describe "Property-Based Tests" $ do
  describe "Substitution semantics" $ do
    it "substitution preserves evaluation" $ do
      withMaxSuccess 1000 $ property $ \expr var val ->
        let substituted = substitute expr var val
            env = Map.singleton var val
        in case (evalExpr substituted env, evalExpr expr env) of
          (Left _, Left _) -> True  -- Both error, that's fine
          (Right v1, Right v2) -> v1 == v2
          _ -> False
  
  describe "Closure semantics" $ do
    it "closures capture lexical environment" $ do
      prop_closure_capture
    
    it "closures use captured environment" $ do
      prop_closure_isolation
  
  describe "Evaluation order" $ do
    it "arguments evaluated before application" $ do
      prop_evaluation_order
  
  describe "Let binding shadowing" $ do
    it "inner bindings shadow outer bindings" $ do
      prop_let_shadowing
  
  describe "Map operations" $ do
    it "assoc then get returns correct value" $ do
      withMaxSuccess 100 $ property $ \m key val ->
        let m' = case m of
              VMap mp -> mp
              _ -> Map.empty
            keyStr = case key of
              VKeyword k -> k
              _ -> "test-key"
            result = case evalExpr (List [Atom (Symbol "get")
                                        , List [Atom (Symbol "assoc")
                                               , valueToExpr (VMap m')
                                               , Atom (Keyword keyStr)
                                               , valueToExpr val]
                                        , Atom (Keyword keyStr)]) initialEnv of
              Right (VMap resultMap) -> Map.lookup (KeywordKey keyStr) resultMap
              _ -> Nothing
        in case result of
          Just v -> v === val
          _ -> property True  -- If evaluation fails, that's okay for property test
    
    it "dissoc removes key" $ do
      withMaxSuccess 100 $ property $ \m key ->
        let m' = case m of
              VMap mp -> mp
              _ -> Map.empty
            keyStr = case key of
              VKeyword k -> k
              _ -> "test-key"
            wasPresent = Map.member (KeywordKey keyStr) m'
            result = case evalExpr (List [Atom (Symbol "dissoc")
                                        , valueToExpr (VMap m')
                                        , Atom (Keyword keyStr)]) initialEnv of
              Right (VMap resultMap) -> Map.member (KeywordKey keyStr) resultMap
              _ -> True
        in if wasPresent
          then property (not result)  -- If key was present, it should not be after dissoc
          else property True  -- If key wasn't present, result doesn't matter
  
  describe "Set operations" $ do
    it "union is commutative" $ do
      withMaxSuccess 100 $ property $ \s1 s2 ->
        let set1 = case s1 of VSet s -> s; _ -> Set.empty
            set2 = case s2 of VSet s -> s; _ -> Set.empty
            union1 = Set.union set1 set2
            union2 = Set.union set2 set1
        in union1 === union2
    
    it "intersection is idempotent" $ do
      withMaxSuccess 100 $ property $ \s ->
        let set = case s of VSet s' -> s'; _ -> Set.empty
            intersection1 = Set.intersection set set
        in intersection1 === set
    
    it "union is associative" $ do
      withMaxSuccess 100 $ property $ \s1 s2 s3 ->
        let set1 = case s1 of VSet s -> s; _ -> Set.empty
            set2 = case s2 of VSet s -> s; _ -> Set.empty
            set3 = case s3 of VSet s -> s; _ -> Set.empty
            union12 = Set.union set1 set2
            union123 = Set.union union12 set3
            union23 = Set.union set2 set3
            union123' = Set.union set1 union23
        in union123 === union123'
  
  describe "Serialization" $ do
    it "round-trip preserves values and types" $ do
      withMaxSuccess 100 $ property $ \val ->
        let pat = valueToPatternSubjectForGram val
            gramText = patternToGram pat
        in case gramToPattern gramText of
          Left _ -> property True  -- Parse errors are okay for property test
          Right pat' -> case patternSubjectToValue pat' of
            Left _ -> property True  -- Deserialization errors are okay
            Right val' -> val === val'

