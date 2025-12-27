{-# LANGUAGE OverloadedStrings #-}

-- | S-expression parser for Pattern Lisp using Megaparsec.
--
-- This module provides parsing functionality to convert S-expression strings
-- into abstract syntax tree (AST) representations. The parser handles:
-- * Atoms: symbols, numbers, strings, booleans
-- * Lists: S-expressions with parentheses
-- * Quotes: both (quote ...) and '... syntax
--
-- Error messages include position information from Megaparsec, making it
-- easy to locate syntax errors in source code.
--
-- Example usage:
--
-- > import PatternLisp.Parser
-- >
-- > case parseExpr "(+ 1 2)" of
-- >   Left (ParseError msg) -> putStrLn $ "Parse error: " ++ msg
-- >   Right expr -> print expr
module PatternLisp.Parser
  ( parseExpr
  ) where

import PatternLisp.Syntax
import Text.Megaparsec
import Text.Megaparsec.Char
import qualified Text.Megaparsec.Char.Lexer as L
import qualified Data.Text as T
import Data.Void

type Parser = Parsec Void String

-- | Whitespace parser
skipSpace :: Parser ()
skipSpace = L.space space1 empty empty

-- | Parse an S-expression string into an Expr.
--
-- This function parses a complete S-expression from a string, returning
-- either a parse error (with position information) or a successfully parsed
-- expression. The parser expects exactly one expression and will fail if
-- there is trailing input after the expression.
--
-- Error messages include:
-- * Line and column position of the error
-- * Context showing what was expected
-- * The actual input that caused the error
--
-- @since 0.1.0.0
parseExpr :: String -> Either Error Expr
parseExpr input = case parse (skipSpace *> exprParser <* eof) "" input of
  Left err -> Left (ParseError (errorBundlePretty err))
  Right e -> Right e

-- | Main expression parser (recursive)
exprParser :: Parser Expr
exprParser = skipSpace *> (quoteParser <|> atomParser <|> try setParser <|> try mapParser <|> listParser) <* skipSpace

-- | Atom parser (keyword, symbol, number, string, bool)
-- Try keywords before symbols to catch postfix colon syntax
atomParser :: Parser Expr
atomParser = Atom <$> (stringParser <|> boolParser <|> try keywordParser <|> try symbolParser <|> numberParser)

-- | Keyword parser (symbol followed by colon)
-- Keywords use postfix colon syntax: name:, age:, etc.
keywordParser :: Parser Atom
keywordParser = Keyword <$> (identifier <* char ':')
  where
    identifier = (:) <$> firstChar <*> many restChar
    firstChar = letterChar <|> satisfy (\c -> c `elem` ("!$%&*+-./<=>?@^_~" :: String))
    restChar = firstChar <|> digitChar

-- | Symbol parser (valid identifiers)
-- Note: Does not match if it looks like a number (starts with + or - followed by digit)
-- Note: Does not match keywords (symbols ending with colon)
symbolParser :: Parser Atom
symbolParser = Symbol <$> (try (notFollowedBy numberLike) *> try (notFollowedBy (identifier <* char ':')) *> identifier)
  where
    identifier = (:) <$> firstChar <*> many restChar
    firstChar = letterChar <|> satisfy (\c -> c `elem` ("!$%&*+-./:<=>?@^_~" :: String))
    restChar = firstChar <|> digitChar
    numberLike = (char '+' <|> char '-') *> digitChar

-- | Number parser (integers)
numberParser :: Parser Atom
numberParser = try (Number <$> L.signed skipSpace L.decimal) <?> "number"

-- | String parser (with escapes)
stringParser :: Parser Atom
stringParser = String . T.pack <$> (char '"' *> manyTill stringChar (char '"'))
  where
    stringChar = escapedChar <|> noneOf ['"', '\\']
    escapedChar = char '\\' *> (escapeSeq <|> anySingle)
    escapeSeq = (char 'n' *> pure '\n')
            <|> (char 't' *> pure '\t')
            <|> (char 'r' *> pure '\r')
            <|> (char '\\' *> pure '\\')
            <|> (char '"' *> pure '"')

-- | Boolean parser (#t, #f)
boolParser :: Parser Atom
boolParser = (string "#t" *> pure (Bool True)) <|> (string "#f" *> pure (Bool False))

-- | Set parser (hash set syntax #{...})
setParser :: Parser Expr
setParser = do
  _ <- string "#{"
  skipSpace
  exprs <- many (exprParser <* skipSpace)
  skipSpace
  _ <- char '}'
  return $ SetLiteral exprs

-- | Map parser (curly brace syntax {key: value ...})
-- Maps use alternating key-value pairs where keys must be keywords
mapParser :: Parser Expr
mapParser = do
  _ <- char '{'
  skipSpace
  pairs <- many (mapPair <* skipSpace)
  skipSpace
  _ <- char '}'
  return $ MapLiteral (concat pairs)  -- Flatten pairs into single list
  where
    mapPair = do
      key <- keywordParser  -- Key must be a keyword
      skipSpace
      value <- exprParser
      return [Atom key, value]

-- | List parser (parentheses)
listParser :: Parser Expr
listParser = List <$> between (char '(') (char ')') (skipSpace *> many (exprParser <* skipSpace))

-- | Quote parser (quote form and single quote syntax)
quoteParser :: Parser Expr
quoteParser = try (char '\'' *> (Quote <$> exprParser))
          <|> try (do
                _ <- char '('
                skipSpace
                _ <- string "quote"
                skipSpace
                e <- exprParser
                skipSpace
                _ <- char ')'
                return (Quote e))

