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

import qualified Data.Map.Strict as Map
import qualified Data.Set as Set
import Subject.Core (Subject)
import Pattern (Pattern)
import qualified Subject.Value as SubjectValue

-- | Abstract syntax tree representation of Lisp expressions
data Expr
  = Atom Atom          -- ^ Symbols, numbers, strings, booleans
  | List [Expr]        -- ^ S-expressions (function calls, special forms)
  | SetLiteral [Expr]  -- ^ Set literals #{...}
  | RecordLiteral [(String, Expr)]  -- ^ Record literals {key: value, ...} (comma-separated, gram-compatible)
  | Quote Expr         -- ^ Quoted expressions (prevent evaluation)
  | Unquote Expr       -- ^ Unquote marker for quasiquotation (`,expr)
  | UnquoteSplice Expr -- ^ Unquote-splice marker for quasiquotation (`,@expr)
  deriving (Eq, Show)

-- | Atomic values in the AST
data Atom
  = Symbol String      -- ^ Variable names, function names
  | Number Integer     -- ^ Integer literals
  | String String      -- ^ String literals (gram uses String, not Text)
  | Bool Bool          -- ^ Boolean literals (#t, #f)
  | Keyword String     -- ^ Keywords with postfix colon syntax (name:)
  deriving (Eq, Show)

-- | Runtime values that expressions evaluate to
-- Uses gram Subject.Value types for basic values (Option B - gram types throughout)
-- Pattern-lisp specific types (VPattern, VClosure, VPrimitive) are kept as separate constructors
data Value
  = VInteger Integer           -- ^ Integer values (gram VInteger)
  | VDecimal Double            -- ^ Decimal values (gram VDecimal)
  | VBoolean Bool             -- ^ Boolean values (gram VBoolean)
  | VString String            -- ^ String values (gram VString, was Text)
  | VSymbol String            -- ^ Symbol values (gram VSymbol)
  | VTaggedString String String  -- ^ Tagged string values (gram VTaggedString)
  | VArray [Value]            -- ^ Array values (gram VArray, replaces VList)
  | VMap (Map.Map String Value)  -- ^ Map values with string keys (gram VMap, replaces VMap with MapKey)
  | VRange SubjectValue.RangeValue  -- ^ Range values (gram VRange)
  | VMeasurement String Double  -- ^ Measurement values (gram VMeasurement)
  | VKeyword String           -- ^ Keyword values (pattern-lisp specific, serializes as VTaggedString)
  | VSet (Set.Set Value)      -- ^ Set values (pattern-lisp specific, serializes as VArray)
  | VPattern (Pattern Subject)  -- ^ Pattern values (pattern-lisp specific)
  | VClosure Closure          -- ^ Function closures (pattern-lisp specific)
  | VPrimitive Primitive       -- ^ Built-in primitive functions (pattern-lisp specific)
  deriving (Eq, Show)

-- | Ord instance for Value (needed for Set operations)
-- Uses tag-based ordering: compares constructor tags first, then values
instance Ord Value where
  compare (VInteger a) (VInteger b) = compare a b
  compare (VInteger _) _ = LT
  compare _ (VInteger _) = GT
  
  compare (VDecimal a) (VDecimal b) = compare a b
  compare (VDecimal _) _ = LT
  compare _ (VDecimal _) = GT
  
  compare (VBoolean a) (VBoolean b) = compare a b
  compare (VBoolean _) _ = LT
  compare _ (VBoolean _) = GT
  
  compare (VString a) (VString b) = compare a b
  compare (VString _) _ = LT
  compare _ (VString _) = GT
  
  compare (VSymbol a) (VSymbol b) = compare a b
  compare (VSymbol _) _ = LT
  compare _ (VSymbol _) = GT
  
  compare (VTaggedString tag1 c1) (VTaggedString tag2 c2) = 
    case compare tag1 tag2 of
      EQ -> compare c1 c2
      x -> x
  compare (VTaggedString _ _) _ = LT
  compare _ (VTaggedString _ _) = GT
  
  compare (VArray a) (VArray b) = compare a b
  compare (VArray _) _ = LT
  compare _ (VArray _) = GT
  
  compare (VMap a) (VMap b) = compare (Map.toAscList a) (Map.toAscList b)
  compare (VMap _) _ = LT
  compare _ (VMap _) = GT
  
  compare (VRange a) (VRange b) = compare a b
  compare (VRange _) _ = LT
  compare _ (VRange _) = GT
  
  compare (VMeasurement u1 v1) (VMeasurement u2 v2) =
    case compare u1 u2 of
      EQ -> compare v1 v2
      x -> x
  compare (VMeasurement _ _) _ = LT
  compare _ (VMeasurement _ _) = GT
  
  compare (VKeyword a) (VKeyword b) = compare a b
  compare (VKeyword _) _ = LT
  compare _ (VKeyword _) = GT
  
  compare (VSet a) (VSet b) = compare (Set.toList a) (Set.toList b)
  compare (VSet _) _ = LT
  compare _ (VSet _) = GT
  
  compare (VPattern p1) (VPattern p2)
    | VPattern p1 == VPattern p2 = EQ
    | otherwise = compare (show p1) (show p2)
  compare (VPattern _) _ = LT
  compare _ (VPattern _) = GT
  
  compare (VClosure c1) (VClosure c2)
    | VClosure c1 == VClosure c2 = EQ
    | otherwise = compare (show c1) (show c2)
  compare (VClosure _) _ = LT
  compare _ (VClosure _) = GT
  
  compare (VPrimitive a) (VPrimitive b) = compare a b

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
  | Pure               -- ^ (pure value) - creates atomic pattern
  | PatternCreate      -- ^ (pattern value elements) - creates pattern with elements
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
  -- Set operations
  | SetContains        -- ^ (contains? set value): check membership
  | SetUnion          -- ^ (set-union set1 set2): union of two sets
  | SetIntersection   -- ^ (set-intersection set1 set2): intersection of two sets
  | SetDifference     -- ^ (set-difference set1 set2): elements in set1 not in set2
  | SetSymmetricDifference  -- ^ (set-symmetric-difference set1 set2): elements in either but not both
  | SetSubset         -- ^ (set-subset? set1 set2): check if set1 is subset of set2
  | SetEqual          -- ^ (set-equal? set1 set2): check if sets are equal
  | SetEmpty          -- ^ (empty? set): check if set is empty
  | HashSet           -- ^ (hash-set ...): create set from arguments
  -- Map operations
  | MapGet            -- ^ (get map key [default]): get value at key, return default or nil if not found
  | MapGetIn          -- ^ (get-in map [key1 key2 ...]): nested access via keyword path
  | MapAssoc          -- ^ (assoc map key value): add/update key-value pair
  | MapDissoc         -- ^ (dissoc map key): remove key from map
  | MapUpdate         -- ^ (update map key f): apply function to value at key, create with f(nil) if missing
  | Record            -- ^ (record key1 val1 key2 val2 ...): create record from alternating keyword-value pairs (replaces HashMap)
  deriving (Eq, Show, Ord)

-- | Environment mapping variable names to values
type Env = Map.Map String Value

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
primitiveName Pure = "pure"
primitiveName PatternCreate = "pattern"
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
primitiveName SetContains = "contains?"
primitiveName SetUnion = "set-union"
primitiveName SetIntersection = "set-intersection"
primitiveName SetDifference = "set-difference"
primitiveName SetSymmetricDifference = "set-symmetric-difference"
primitiveName SetSubset = "set-subset?"
primitiveName SetEqual = "set-equal?"
primitiveName SetEmpty = "empty?"
primitiveName HashSet = "hash-set"
primitiveName MapGet = "get"
primitiveName MapGetIn = "get-in"
primitiveName MapAssoc = "assoc"
primitiveName MapDissoc = "dissoc"
primitiveName MapUpdate = "update"
primitiveName Record = "record"

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
primitiveFromName "pure" = Just Pure
primitiveFromName "pattern" = Just PatternCreate
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
primitiveFromName "contains?" = Just SetContains
primitiveFromName "set-union" = Just SetUnion
primitiveFromName "set-intersection" = Just SetIntersection
primitiveFromName "set-difference" = Just SetDifference
primitiveFromName "set-symmetric-difference" = Just SetSymmetricDifference
primitiveFromName "set-subset?" = Just SetSubset
primitiveFromName "set-equal?" = Just SetEqual
primitiveFromName "empty?" = Just SetEmpty  -- Note: empty? works for both sets and maps
primitiveFromName "hash-set" = Just HashSet
primitiveFromName "get" = Just MapGet
primitiveFromName "get-in" = Just MapGetIn
primitiveFromName "assoc" = Just MapAssoc
primitiveFromName "dissoc" = Just MapDissoc
primitiveFromName "update" = Just MapUpdate
primitiveFromName "record" = Just Record
primitiveFromName "hash-map" = Just Record  -- Backward compatibility alias (deprecated)
primitiveFromName _ = Nothing

