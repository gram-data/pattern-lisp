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
  ) where

import Pattern (Pattern)
import Subject.Core (Subject)
import Gram.Serialize (toGram)
import Gram.Parse (fromGram, ParseError)

-- | Serialize Pattern Subject to gram notation string.
--
-- This function converts a Pattern Subject to its gram notation representation,
-- which can be written to files, sent over networks, or piped between programs.
patternToGram :: Pattern Subject -> String
patternToGram = toGram

-- | Deserialize gram notation string to Pattern Subject.
--
-- This function parses gram notation text and converts it to a Pattern Subject.
-- Returns an error if the gram notation is invalid.
gramToPattern :: String -> Either ParseError (Pattern Subject)
gramToPattern = fromGram

