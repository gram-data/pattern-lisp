-- | Gram notation serialization and deserialization for Pattern Subject.
--
-- This module provides functions to convert between Pattern Subject data structures
-- and gram notation text format, enabling gram to serve as the intermediate
-- representation for piping between programs (same machine or network).
--
-- Example usage:
--
-- > import PatternLisp.Gram
-- > import PatternLisp.Parser
-- >
-- > let pat = ... -- Pattern Subject
-- > let gramText = patternToGram pat
-- > case gramToPattern gramText of
-- >   Right pat' -> pat' == pat  -- True (round-trip)
-- >   Left err -> error (show err)
module PatternLisp.Gram
  ( patternToGram
  , gramToPattern
  , exprToGram
  , gramToExpr
  ) where

import PatternLisp.Syntax
import PatternLisp.Codec
import Pattern (Pattern)
import Pattern.Core (point)
import qualified Pattern.Core as PatternCore
import Subject.Core (Subject)
import Gram.Serialize (toGram)
import qualified Gram.Parse as GramParse

-- | Serialize Pattern Subject to gram notation string.
--
-- This function converts a Pattern Subject to its gram notation representation,
-- which can be written to files, sent over networks, or piped between programs.
patternToGram :: Pattern Subject -> String
patternToGram p = toGram [p]

-- | Deserialize gram notation string to Pattern Subject.
--
-- This function parses gram notation text and converts it to a Pattern Subject.
-- Returns an error if the gram notation is invalid.
-- fromGram returns [Pattern]; we expect exactly one for a single-line input.
gramToPattern :: String -> Either GramParse.ParseError (Pattern Subject)
gramToPattern s = case GramParse.fromGram s of
  Left e -> Left e
  Right [p] -> Right p
  Right [] -> Left (GramParse.ParseError "expected exactly one pattern")
  Right (_ : _ : _) -> Left (GramParse.ParseError "expected exactly one pattern")

-- | Serialize an expression AST to gram notation string.
--
-- This function converts a Pattern Lisp expression to its gram notation
-- representation by first converting it to a Subject, wrapping it in an atomic
-- Pattern, and then serializing to gram.
--
-- Example usage:
--
-- > import PatternLisp.Gram
-- > import PatternLisp.Parser
-- >
-- > case parseExpr "(+ 1 2)" of
-- >   Right expr -> exprToGram expr  -- Returns gram string
-- >   Left err -> error (show err)
exprToGram :: Expr -> String
exprToGram expr =
  let subject = exprToSubject expr
      pat = point subject
  in patternToGram pat

-- | Deserialize gram notation string to an expression AST.
--
-- This function parses gram notation text, extracts the Pattern Subject,
-- and converts it back to an expression AST. Returns an error if the gram
-- notation is invalid or cannot be converted to an expression.
--
-- Example usage:
--
-- > import PatternLisp.Gram
-- >
-- > case gramToExpr "[Number {value: 42}]" of
-- >   Right expr -> print expr  -- Atom (Number 42)
-- >   Left err -> error (show err)
gramToExpr :: String -> Either Error Expr
gramToExpr gramText = do
  pat <- case gramToPattern gramText of
    Left parseErr -> Left $ ParseError (show parseErr)
    Right p -> Right p
  -- Extract the Subject from the Pattern (assuming atomic pattern for expressions)
  let subject = PatternCore.value pat
  subjectToExpr subject

