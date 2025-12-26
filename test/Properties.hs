module Properties (spec) where

import Test.Hspec
import Test.QuickCheck
import PatternLisp.Syntax
import PatternLisp.Parser
import PatternLisp.Eval
import PatternLisp.Primitives
import qualified Data.Map as Map
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
    ]
    where
      genSymbol = do
        first <- elements (['a'..'z'] ++ ['A'..'Z'] ++ "!$%&*+-./:<=>?@^_~")
        rest <- listOf (elements (['a'..'z'] ++ ['A'..'Z'] ++ ['0'..'9'] ++ "-"))
        return (first : rest)
      genString = listOf (elements (['a'..'z'] ++ ['A'..'Z'] ++ ['0'..'9'] ++ " "))

-- | Arbitrary instance for Value
instance Arbitrary Value where
  arbitrary = oneof
    [ VNumber <$> arbitrary
    , VString . T.pack <$> genString
    , VBool <$> arbitrary
    , VList <$> resize 5 (listOf arbitrary)
    ]
    where
      genString = listOf (elements (['a'..'z'] ++ ['A'..'Z'] ++ ['0'..'9'] ++ " "))

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

