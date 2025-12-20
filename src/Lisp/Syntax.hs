{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveAnyClass #-}

-- | Core data types for Pattern Lisp interpreter.
--
-- This module defines the abstract syntax tree (Expr), runtime values (Value),
-- and related types used throughout the interpreter. It serves as the foundation
-- for both parsing and evaluation.
--
-- Key types:
-- * 'Expr': Abstract syntax tree representation of Lisp code
-- * 'Value': Runtime values that expressions evaluate to
-- * 'Closure': Function closures that capture their lexical environment
-- * 'Env': Environment mapping variable names to values
-- * 'Error': Evaluation and parsing errors
--
-- All types derive 'Eq' and 'Show' for testing and debugging.
module Lisp.Syntax
  ( Expr(..)
  , Atom(..)
  , Value(..)
  , Closure(..)
  , Primitive(..)
  , Env
  , Error(..)
  ) where

import Data.Map (Map)
import Data.Text (Text)

-- | Abstract syntax tree representation of Lisp expressions
data Expr
  = Atom Atom          -- ^ Symbols, numbers, strings, booleans
  | List [Expr]        -- ^ S-expressions (function calls, special forms)
  | Quote Expr         -- ^ Quoted expressions (prevent evaluation)
  deriving (Eq, Show)

-- | Atomic values in the AST
data Atom
  = Symbol String      -- ^ Variable names, function names
  | Number Integer     -- ^ Integer literals
  | String Text        -- ^ String literals
  | Bool Bool          -- ^ Boolean literals (#t, #f)
  deriving (Eq, Show)

-- | Runtime values that expressions evaluate to
data Value
  = VNumber Integer           -- ^ Numeric values
  | VString Text              -- ^ String values
  | VBool Bool                -- ^ Boolean values
  | VList [Value]             -- ^ List values
  | VClosure Closure          -- ^ Function closures
  | VPrimitive Primitive       -- ^ Built-in primitive functions
  deriving (Eq, Show)

-- | Function value that captures its lexical environment
data Closure = Closure
  { params :: [String]    -- ^ Function parameter names
  , body   :: Expr        -- ^ Function body expression
  , env    :: Env         -- ^ Captured lexical environment
  }
  deriving (Eq, Show)

-- | Built-in primitive functions
data Primitive
  = Add | Sub | Mul | Div           -- ^ Arithmetic
  | Gt | Lt | Eq | Ne               -- ^ Comparison
  | StringAppend | StringLength | Substring  -- ^ String operations
  deriving (Eq, Show)

-- | Environment mapping variable names to values
type Env = Map String Value

-- | Evaluation and parsing errors
data Error
  = UndefinedVar String Expr         -- ^ Undefined variable (name, expression context)
  | TypeMismatch String Value        -- ^ Type mismatch in operation (message, actual value)
  | ArityMismatch String Int Int     -- ^ Function called with wrong number of args (name, expected, actual)
  | DivisionByZero Expr              -- ^ Division by zero (expression context)
  | ParseError String                -- ^ Parse error with message (includes position info from parser)
  deriving (Eq, Show)

