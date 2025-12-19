module Lisp.Primitives
  ( initialEnv
  ) where

import Lisp.Syntax
import qualified Data.Map as Map

-- | Initial environment with all primitive functions registered
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

