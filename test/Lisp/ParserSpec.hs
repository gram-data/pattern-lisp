module Lisp.ParserSpec (spec) where

import Test.Hspec
import Lisp.Syntax
import Lisp.Parser
import qualified Data.Text as T

spec :: Spec
spec = describe "Lisp.Parser" $ do
  describe "parseExpr" $ do
    it "parses simple S-expression \"(+ 1 2)\"" $ do
      parseExpr "(+ 1 2)" `shouldBe` Right (List [Atom (Symbol "+"), Atom (Number 1), Atom (Number 2)])
    
    it "parses nested S-expression \"(if (> x 0) (+ x 1) (- x 1))\"" $ do
      let expected = List [Atom (Symbol "if")
                          , List [Atom (Symbol ">"), Atom (Symbol "x"), Atom (Number 0)]
                          , List [Atom (Symbol "+"), Atom (Symbol "x"), Atom (Number 1)]
                          , List [Atom (Symbol "-"), Atom (Symbol "x"), Atom (Number 1)]]
      parseExpr "(if (> x 0) (+ x 1) (- x 1))" `shouldBe` Right expected
    
    it "parses quoted expression \"(quote (a b c))\"" $ do
      let expected = Quote (List [Atom (Symbol "a"), Atom (Symbol "b"), Atom (Symbol "c")])
      parseExpr "(quote (a b c))" `shouldBe` Right expected
    
    it "parses single quote syntax \"'(a b c)\"" $ do
      let expected = Quote (List [Atom (Symbol "a"), Atom (Symbol "b"), Atom (Symbol "c")])
      parseExpr "'(a b c)" `shouldBe` Right expected
    
    it "parses invalid S-expression with error message" $ do
      case parseExpr "(unclosed" of
        Left (ParseError _) -> True `shouldBe` True
        _ -> fail "Expected ParseError"
    
    it "parses empty list \"()\"" $ do
      parseExpr "()" `shouldBe` Right (List [])
    
    it "parses single-element list \"(x)\"" $ do
      parseExpr "(x)" `shouldBe` Right (List [Atom (Symbol "x")])
    
    it "parses numbers (positive, negative, zero)" $ do
      parseExpr "42" `shouldBe` Right (Atom (Number 42))
      parseExpr "-10" `shouldBe` Right (Atom (Number (-10)))
      parseExpr "0" `shouldBe` Right (Atom (Number 0))
    
    it "parses strings with quotes and escapes" $ do
      parseExpr "\"hello\"" `shouldBe` Right (Atom (String (T.pack "hello")))
      parseExpr "\"hello \\\"world\\\"\"" `shouldBe` Right (Atom (String (T.pack "hello \"world\"")))
    
    it "parses symbols (valid identifiers)" $ do
      parseExpr "x" `shouldBe` Right (Atom (Symbol "x"))
      parseExpr "my-var" `shouldBe` Right (Atom (Symbol "my-var"))
      parseExpr "var123" `shouldBe` Right (Atom (Symbol "var123"))
    
    it "parses booleans (#t, #f)" $ do
      parseExpr "#t" `shouldBe` Right (Atom (Bool True))
      parseExpr "#f" `shouldBe` Right (Atom (Bool False))

