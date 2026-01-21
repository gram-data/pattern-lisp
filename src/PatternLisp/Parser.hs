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
exprParser = skipSpace *> (quasiquoteParser <|> quoteParser <|> atomParser <|> try setParser <|> try recordParser <|> try arrayParser <|> listParser) <* skipSpace

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
-- Returns String (not Text) to match gram types (Option B)
stringParser :: Parser Atom
stringParser = String <$> (char '"' *> manyTill stringChar (char '"'))
  where
    stringChar = escapedChar <|> satisfy (\c -> c /= '"' && c /= '\\')
    escapedChar = char '\\' *> (escapeSeq <|> anySingle)
    escapeSeq = (char 'n' *> pure '\n')
            <|> (char 't' *> pure '\t')
            <|> (char 'r' *> pure '\r')
            <|> (char '\\' *> pure '\\')
            <|> (char '"' *> pure '"')

-- | Boolean parser (gram-compatible: true/false)
boolParser :: Parser Atom
boolParser = (string "true" *> notFollowedBy (letterChar <|> digitChar) *> pure (Bool True))
         <|> (string "false" *> notFollowedBy (letterChar <|> digitChar) *> pure (Bool False))

-- | Set parser (hash set syntax #{...})
setParser :: Parser Expr
setParser = do
  _ <- string "#{"
  skipSpace
  exprs <- many (exprParser <* skipSpace)
  skipSpace
  _ <- char '}'
  return $ SetLiteral exprs


-- | Record parser using Megaparsec.
--
-- Parses comma-separated record syntax: @{key: value, ...}@
-- Supports both single and double colons (@:@ and @::@) for gram compatibility.
-- Keys can be identifiers or quoted strings.
-- Values can be any pattern-lisp expression, including unquotes and splices.
--
-- Gram compatibility is validated in comprehensive tests, not during parsing.
--
-- Examples:
--
-- > parseExpr "{name: \"Alice\", age: 30}"
-- > parseExpr "{\"user-id\": 123, active: true}"
-- > parseExpr "`{name: ,nameVar, ,@baseRecord}"
recordParser :: Parser Expr
recordParser = parseRecordWithMegaparsec

-- | Parse record using Megaparsec (for pattern-lisp specific syntax).
--
-- This parser handles:
-- * Comma-separated key-value pairs
-- * Unquotes (@,expr@) and splices (@,@expr@) for quasiquotation
-- * Both identifier and string keys
-- * Both single (@:@) and double (@::@) colon syntax
--
-- Returns a @RecordLiteral@ containing a list of @(String, Expr)@ pairs.
parseRecordWithMegaparsec :: Parser Expr
parseRecordWithMegaparsec = do
  _ <- char '{'
  skipSpace
  -- Separator: comma and surrounding space. When the comma is immediately
  -- followed by @ (i.e. ,@splice), do not consume so recordSplice can parse it.
  -- Otherwise a record like {a: 1,@b} would have the separator consume the
  -- comma, leaving "@b" for recordEntry and causing recordSplice to fail.
  entries <- sepBy recordEntry recordSep
  skipSpace
  _ <- char '}'
  return $ RecordLiteral entries
  where
    recordSep =
      try (lookAhead (skipSpace *> char ',' *> char '@') *> pure ())
        <|> ((skipSpace *> char ',' <* skipSpace) *> pure ())
    recordEntry = try recordSplice <|> recordPair
    recordSplice = do
      skipSpace
      _ <- string ",@"
      skipSpace
      value <- exprParser
      return ("", UnquoteSplice value)
    recordPair = do
      keyAtom <- try (Symbol <$> identifier) <|> stringParser
      skipSpace
      _ <- try (string "::") <|> string ":"
      skipSpace
      value <- try (do
                _ <- char ','
                skipSpace
                try (do
                  _ <- char '@'
                  skipSpace
                  expr <- exprParser
                  return $ UnquoteSplice expr) <|> do
                  expr <- exprParser
                  return $ Unquote expr) <|> exprParser
      let keyStr = case keyAtom of
            Symbol name -> name
            String s -> s
            _ -> ""
      return (keyStr, value)
    identifier = (:) <$> firstChar <*> many restChar
    firstChar = letterChar <|> satisfy (\c -> c `elem` ("!$%&*+-./<=>?@^_~" :: String))
    restChar = firstChar <|> digitChar

-- | Array parser (square brackets, gram-compatible, comma-separated)
arrayParser :: Parser Expr
arrayParser = ArrayLiteral <$> between (char '[') (char ']') (skipSpace *> sepBy exprParser (skipSpace *> char ',' <* skipSpace) <* skipSpace)

-- | List parser (parentheses)
listParser :: Parser Expr
listParser = List <$> between (char '(') (char ')') (skipSpace *> many (exprParser <* skipSpace))

-- | Quasiquote parser (backtick syntax `expr)
-- Parses `expr and transforms ,expr to Unquote, ,@expr to UnquoteSplice
quasiquoteParser :: Parser Expr
quasiquoteParser = char '`' *> (quasiquoteTransform <$> exprParser)
  where
    quasiquoteTransform :: Expr -> Expr
    -- Handle unquote and splice in lists: (, expr) and (,@ expr)
    quasiquoteTransform (List [Atom (Symbol ","), expr]) = Unquote expr
    quasiquoteTransform (List [Atom (Symbol ",@"), expr]) = UnquoteSplice expr
    -- Handle unquote and splice as direct atoms (for record values): ,expr and ,@expr
    quasiquoteTransform (Atom (Symbol ",")) = error "Standalone comma not allowed - use ,expr for unquote"
    quasiquoteTransform (Atom (Symbol ",@")) = error "Standalone ,@ not allowed - use ,@expr for splice"
    -- Don't transform Unquote and UnquoteSplice - they're already transformed
    quasiquoteTransform (Unquote expr) = Unquote expr
    quasiquoteTransform (UnquoteSplice expr) = UnquoteSplice expr
    -- Recursively transform nested structures
    quasiquoteTransform (List exprs) = List (map quasiquoteTransform exprs)
    quasiquoteTransform (ArrayLiteral exprs) = ArrayLiteral (map quasiquoteTransform exprs)
    quasiquoteTransform (SetLiteral exprs) = SetLiteral (map quasiquoteTransform exprs)
    quasiquoteTransform (RecordLiteral pairs) = RecordLiteral (map (\(k, v) -> (k, quasiquoteTransform v)) pairs)
    quasiquoteTransform (Quote expr) = Quote (quasiquoteTransform expr)
    quasiquoteTransform expr = Quote expr  -- Quote everything else by default

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

