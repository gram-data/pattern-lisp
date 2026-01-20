module PatternLisp.ParserSpec (spec) where

import Test.Hspec
import PatternLisp.Syntax
import PatternLisp.Parser
import qualified Data.Text as T

spec :: Spec
spec = describe "PatternLisp.Parser" $ do
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
      parseExpr "\"hello\"" `shouldBe` Right (Atom (String ( "hello")))
      parseExpr "\"hello \\\"world\\\"\"" `shouldBe` Right (Atom (String ( "hello \"world\"")))
    
    it "parses symbols (valid identifiers)" $ do
      parseExpr "x" `shouldBe` Right (Atom (Symbol "x"))
      parseExpr "my-var" `shouldBe` Right (Atom (Symbol "my-var"))
      parseExpr "var123" `shouldBe` Right (Atom (Symbol "var123"))
    
    it "parses booleans (#t, #f)" $ do
      parseExpr "#t" `shouldBe` Right (Atom (Bool True))
      parseExpr "#f" `shouldBe` Right (Atom (Bool False))
    
    it "parses keywords with postfix colon syntax" $ do
      parseExpr "name:" `shouldBe` Right (Atom (Keyword "name"))
      parseExpr "age:" `shouldBe` Right (Atom (Keyword "age"))
      parseExpr "on-success:" `shouldBe` Right (Atom (Keyword "on-success"))
    
    it "parses set literals with hash set syntax" $ do
      parseExpr "#{1 2 3}" `shouldBe` Right (SetLiteral [Atom (Number 1), Atom (Number 2), Atom (Number 3)])
      parseExpr "#{}" `shouldBe` Right (SetLiteral [])
      parseExpr "#{1 \"hello\" #t}" `shouldBe` Right (SetLiteral [Atom (Number 1), Atom (String ( "hello")), Atom (Bool True)])
    
    it "parses record literals with curly brace syntax (comma-separated)" $ do
      parseExpr "{name: \"Alice\", age: 30}" `shouldBe` Right (RecordLiteral [("name", Atom (String "Alice")), ("age", Atom (Number 30))])
      parseExpr "{}" `shouldBe` Right (RecordLiteral [])
    
    it "parses nested records" $ do
      parseExpr "{user: {name: \"Bob\"}}" `shouldBe` Right (RecordLiteral [("user", RecordLiteral [("name", Atom (String "Bob"))])])
    
    it "handles duplicate keys in record literals (last value wins)" $ do
      -- Parser allows duplicate keys; evaluator handles them (last wins)
      parseExpr "{name: \"Alice\", name: \"Bob\"}" `shouldBe` Right (RecordLiteral [("name", Atom (String "Alice")), ("name", Atom (String "Bob"))])
    
    it "parses record literals with string keys" $ do
      parseExpr "{\"name\": \"Alice\", \"age\": 30}" `shouldBe` Right (RecordLiteral [("name", Atom (String "Alice")), ("age", Atom (Number 30))])
    
    it "parses record literals with mixed identifier and string keys" $ do
      parseExpr "{name: \"Alice\", \"user-id\": 123}" `shouldBe` Right (RecordLiteral [("name", Atom (String "Alice")), ("user-id", Atom (Number 123))])

