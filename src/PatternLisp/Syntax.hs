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
module PatternLisp.Syntax
  ( Expr(..)
  , Atom(..)
  , Value(..)
  , Closure(..)
  , Primitive(..)
  , Env
  , Error(..)
  , primitiveName
  , primitiveFromName
  ) where

import Data.Map (Map)
import Data.Text (Text)
import Subject.Core (Subject)
import Pattern (Pattern)

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
  | VPattern (Pattern Subject)  -- ^ Pattern values with Subject decoration
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
  -- Pattern construction
  | PatternCreate      -- ^ (pattern value)
  | PatternWith        -- ^ (pattern-with value elements)
  -- Pattern queries
  | PatternValue       -- ^ (pattern-value p)
  | PatternElements   -- ^ (pattern-elements p)
  | PatternLength      -- ^ (pattern-length p)
  | PatternSize        -- ^ (pattern-size p)
  | PatternDepth       -- ^ (pattern-depth p)
  | PatternValues      -- ^ (pattern-values p)
  -- Pattern predicates
  | PatternFind        -- ^ (pattern-find p pred)
  | PatternAny        -- ^ (pattern-any? p pred)
  | PatternAll         -- ^ (pattern-all? p pred)
  -- Pattern conversion
  | ValueToPattern     -- ^ (value-to-pattern v): convert any value to pattern
  | PatternToValue     -- ^ (pattern-to-value p): convert pattern to value
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

-- | Convert a Primitive to its string name for serialization
primitiveName :: Primitive -> String
primitiveName Add = "+"
primitiveName Sub = "-"
primitiveName Mul = "*"
primitiveName Div = "/"
primitiveName Gt = ">"
primitiveName Lt = "<"
primitiveName Eq = "="
primitiveName Ne = "/="
primitiveName StringAppend = "string-append"
primitiveName StringLength = "string-length"
primitiveName Substring = "substring"
primitiveName PatternCreate = "pattern"
primitiveName PatternWith = "pattern-with"
primitiveName PatternValue = "pattern-value"
primitiveName PatternElements = "pattern-elements"
primitiveName PatternLength = "pattern-length"
primitiveName PatternSize = "pattern-size"
primitiveName PatternDepth = "pattern-depth"
primitiveName PatternValues = "pattern-values"
primitiveName PatternFind = "pattern-find"
primitiveName PatternAny = "pattern-any?"
primitiveName PatternAll = "pattern-all?"
primitiveName ValueToPattern = "value-to-pattern"
primitiveName PatternToValue = "pattern-to-value"

-- | Look up a Primitive by its string name (for deserialization)
primitiveFromName :: String -> Maybe Primitive
primitiveFromName "+" = Just Add
primitiveFromName "-" = Just Sub
primitiveFromName "*" = Just Mul
primitiveFromName "/" = Just Div
primitiveFromName ">" = Just Gt
primitiveFromName "<" = Just Lt
primitiveFromName "=" = Just Eq
primitiveFromName "/=" = Just Ne
primitiveFromName "string-append" = Just StringAppend
primitiveFromName "string-length" = Just StringLength
primitiveFromName "substring" = Just Substring
primitiveFromName "pattern" = Just PatternCreate
primitiveFromName "pattern-with" = Just PatternWith
primitiveFromName "pattern-value" = Just PatternValue
primitiveFromName "pattern-elements" = Just PatternElements
primitiveFromName "pattern-length" = Just PatternLength
primitiveFromName "pattern-size" = Just PatternSize
primitiveFromName "pattern-depth" = Just PatternDepth
primitiveFromName "pattern-values" = Just PatternValues
primitiveFromName "pattern-find" = Just PatternFind
primitiveFromName "pattern-any?" = Just PatternAny
primitiveFromName "pattern-all?" = Just PatternAll
primitiveFromName "value-to-pattern" = Just ValueToPattern
primitiveFromName "pattern-to-value" = Just PatternToValue
primitiveFromName _ = Nothing

