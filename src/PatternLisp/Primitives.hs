-- | Primitive functions for Pattern Lisp.
--
-- This module provides the initial environment containing all built-in
-- primitive functions. Primitives include:
-- * Arithmetic: +, -, *, /
-- * Comparison: >, <, =, /=
-- * String operations: string-append, string-length, substring
-- * Pattern construction: pattern, pattern-with
--
-- The initial environment is used as the starting point for evaluation
-- and can be extended with user-defined bindings via 'define'.
--
-- Example usage:
--
-- > import PatternLisp.Primitives
-- > import PatternLisp.Eval
-- > import PatternLisp.Parser
-- >
-- > case parseExpr "(+ 1 2)" of
-- >   Right expr -> evalExpr expr initialEnv
-- >   Left err -> Left err
module PatternLisp.Primitives
  ( initialEnv
  ) where

import PatternLisp.Syntax
import qualified Data.Map as Map

-- | Initial environment with all primitive functions registered.
--
-- This environment contains all built-in primitive functions that are
-- available by default in Pattern Lisp. User code can extend this
-- environment using 'define', but cannot remove or modify primitives.
--
-- @since 0.1.0.0
initialEnv :: Env
initialEnv = Map.fromList
  [ ("+", VPrimitive Add)
  , ("-", VPrimitive Sub)
  , ("*", VPrimitive Mul)
  , ("/", VPrimitive Div)
  , (">", VPrimitive Gt)
  , ("<", VPrimitive Lt)
  , ("=", VPrimitive Eq)
  , ("/=", VPrimitive Ne)
  , ("string-append", VPrimitive StringAppend)
  , ("string-length", VPrimitive StringLength)
  , ("substring", VPrimitive Substring)
  , ("pattern", VPrimitive PatternCreate)
  , ("pattern-with", VPrimitive PatternWith)
  , ("pattern-value", VPrimitive PatternValue)
  , ("pattern-elements", VPrimitive PatternElements)
  , ("pattern-length", VPrimitive PatternLength)
  , ("pattern-size", VPrimitive PatternSize)
  , ("pattern-depth", VPrimitive PatternDepth)
  , ("pattern-values", VPrimitive PatternValues)
  , ("pattern-find", VPrimitive PatternFind)
  , ("pattern-any?", VPrimitive PatternAny)
  , ("pattern-all?", VPrimitive PatternAll)
  , ("value-to-pattern", VPrimitive ValueToPattern)
  , ("pattern-to-value", VPrimitive PatternToValue)
  , ("contains?", VPrimitive SetContains)  -- Works for both sets and maps
  , ("set-union", VPrimitive SetUnion)
  , ("set-intersection", VPrimitive SetIntersection)
  , ("set-difference", VPrimitive SetDifference)
  , ("set-symmetric-difference", VPrimitive SetSymmetricDifference)
  , ("set-subset?", VPrimitive SetSubset)
  , ("set-equal?", VPrimitive SetEqual)
  , ("empty?", VPrimitive SetEmpty)  -- Works for both sets and maps
  , ("hash-set", VPrimitive HashSet)
  , ("get", VPrimitive MapGet)
  , ("get-in", VPrimitive MapGetIn)
  , ("assoc", VPrimitive MapAssoc)
  , ("dissoc", VPrimitive MapDissoc)
  , ("update", VPrimitive MapUpdate)
  , ("hash-map", VPrimitive HashMap)
  ]

