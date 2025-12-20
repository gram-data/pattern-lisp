-- | Primitive functions for Pattern Lisp.
--
-- This module provides the initial environment containing all built-in
-- primitive functions. Primitives include:
-- * Arithmetic: +, -, *, /
-- * Comparison: >, <, =, /=
-- * String operations: string-append, string-length, substring
--
-- The initial environment is used as the starting point for evaluation
-- and can be extended with user-defined bindings via 'define'.
--
-- Example usage:
--
-- > import Lisp.Primitives
-- > import Lisp.Eval
-- > import Lisp.Parser
-- >
-- > case parseExpr "(+ 1 2)" of
-- >   Right expr -> evalExpr expr initialEnv
-- >   Left err -> Left err
module Lisp.Primitives
  ( initialEnv
  ) where

import Lisp.Syntax
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
  ]

