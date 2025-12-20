# API Contracts: Pattern State Functions

**Feature**: Pattern State Functions  
**Date**: 2025-01-27  
**Phase**: 1 - Design

## Overview

This feature extends Pattern Lisp with Pattern as a first-class value type, complete serialization, and runtime state management. The contracts define the function signatures and behaviors for the new modules.

**Note**: CLI state initialization approach selected: Option F (Convention-Based Auto-Loading). All non-flag CLI arguments are files (`.plisp` → functions, `.gram` → state variables). See `state-initialization-proposal.md` for details.

This feature extends Pattern Lisp with Pattern as a first-class value type, complete serialization, and runtime state management. The contracts define the function signatures and behaviors for the new modules.

## Module: PatternLisp.Subject

### Serialization Functions

#### `valueToSubject :: Value -> Subject`

Converts a Pattern Lisp Value to Subject representation.

**Input**: Any Value (VNumber, VString, VBool, VList, VPattern, VClosure, VPrimitive)

**Output**: Subject with appropriate label and structure

**Behavior**:
- Numbers: Label "Number", property `{value: Integer}`
- Strings: Label "String", property `{text: Text}`
- Booleans: Label "Bool", property `{value: Bool}`
- Lists: Label "List", elements are recursive Subjects
- Patterns: Recursive structure preserving decoration and elements
- Closures: Label "Closure", property `params`, elements `[envSubject, bodySubject]`
- Primitives: Label "Primitive", property `name`

**Errors**: None (all values are serializable)

---

#### `subjectToValue :: Subject -> Either Error Value`

Converts a Subject representation back to Pattern Lisp Value.

**Input**: Subject with appropriate label and structure

**Output**: Either Error Value

**Behavior**:
- Match on label to determine value type
- Deserialize all Value types including closures and primitives
- Reconstruct captured environments recursively
- Look up primitives by name in registry

**Errors**:
- Invalid structure (wrong label, missing properties/elements)
- Missing primitive in registry
- Invalid expression structure in closure body

---

#### `exprToSubject :: Expr -> Subject`

Converts an expression AST to Subject representation.

**Input**: Expr (Atom, List, Quote)

**Output**: Subject

**Behavior**:
- Atom (Symbol name): Label "Var", property `{name: String}`
- Atom (Number n): Label "Number", property `{value: Integer}`
- Atom (String s): Label "String", property `{text: Text}`
- Atom (Bool b): Label "Bool", property `{value: Bool}`
- List exprs: Label "List", elements from recursive exprToSubject
- Quote expr: Label "Quote", single element from exprToSubject

**Errors**: None (all expressions are serializable)

---

#### `subjectToExpr :: Subject -> Either Error Expr`

Converts a Subject representation back to expression AST.

**Input**: Subject with appropriate label

**Output**: Either Error Expr

**Behavior**:
- Match on label to determine expression type
- Reconstruct Atom, List, Quote forms from Subject structure
- Variable names extracted from name properties

**Errors**:
- Invalid structure (wrong label, missing properties/elements)

---

## Module: PatternLisp.PatternPrimitives

### Construction Primitives

#### `evalPatternCreate :: Value -> EvalM Value`

Creates an atomic Pattern from a Value.

**Input**: Any Value

**Output**: EvalM Value (VPattern)

**Behavior**:
- Convert Value to Subject
- Create atomic Pattern with Subject decoration
- Return VPattern

**Errors**: None (all values can be converted to Subject)

---

#### `evalPatternWith :: Value -> [Value] -> EvalM Value`

Creates a Pattern with elements.

**Input**: 
- Value (decoration)
- [Value] (elements, should be VPattern or convertible)

**Output**: EvalM Value (VPattern)

**Behavior**:
- Convert decoration Value to Subject
- Convert elements: expect VPattern values or convert Values to patterns
- Create Pattern with elements
- Return VPattern

**Errors**:
- Type mismatch if elements cannot be converted to Patterns

---

### Query Primitives

#### `evalPatternValue :: Pattern Subject -> EvalM Value`

Extracts the Subject decoration from a Pattern.

**Input**: Pattern Subject

**Output**: EvalM Value

**Behavior**:
- Extract Subject decoration
- Convert Subject back to Value
- Return Value

**Errors**: None (Pattern always has decoration)

---

#### `evalPatternElements :: Pattern Subject -> EvalM Value`

Extracts the elements list from a Pattern.

**Input**: Pattern Subject

**Output**: EvalM Value (VList of VPattern)

**Behavior**:
- Extract elements list
- Wrap each as VPattern
- Return VList of VPattern elements

**Errors**: None (Pattern may have empty elements list)

---

#### `evalPatternLength :: Pattern Subject -> EvalM Value`

Returns the number of direct elements.

**Input**: Pattern Subject

**Output**: EvalM Value (VNumber)

**Behavior**:
- Count direct elements (not recursive)
- Return VNumber

**Errors**: None

---

#### `evalPatternSize :: Pattern Subject -> EvalM Value`

Returns the total node count (recursive).

**Input**: Pattern Subject

**Output**: EvalM Value (VNumber)

**Behavior**:
- Count all nodes recursively
- Return VNumber

**Errors**: None

---

#### `evalPatternDepth :: Pattern Subject -> EvalM Value`

Returns the maximum nesting depth.

**Input**: Pattern Subject

**Output**: EvalM Value (VNumber)

**Behavior**:
- Calculate max depth recursively
- Return VNumber

**Errors**: None

---

#### `evalPatternValues :: Pattern Subject -> EvalM Value`

Flattens Pattern to list of all Subject values.

**Input**: Pattern Subject

**Output**: EvalM Value (VList of Values)

**Behavior**:
- Flatten Pattern to list of all Subject values
- Convert each Subject to Value
- Return VList

**Errors**: None

---

### Predicate Primitives

#### `evalPatternFind :: Pattern Subject -> Value -> EvalM Value`

Finds the first subpattern matching a predicate.

**Input**:
- Pattern Subject
- Value (VClosure predicate: Pattern -> Bool)

**Output**: EvalM Value (VPattern or "nothing" representation)

**Behavior**:
- Traverse Pattern structure
- Apply predicate closure to each subpattern
- Return first matching subpattern
- Return "nothing" if no match

**Errors**:
- Type mismatch if predicate is not VClosure
- Evaluation error if predicate application fails

---

#### `evalPatternAny :: Pattern Subject -> Value -> EvalM Value`

Checks if any Subject value satisfies a predicate.

**Input**:
- Pattern Subject
- Value (VClosure predicate: Value -> Bool)

**Output**: EvalM Value (VBool)

**Behavior**:
- Flatten Pattern to all Subject values
- Convert each Subject to Value
- Apply predicate to each Value
- Return VBool (true if any match)

**Errors**:
- Type mismatch if predicate is not VClosure
- Evaluation error if predicate application fails

---

#### `evalPatternAll :: Pattern Subject -> Value -> EvalM Value`

Checks if all Subject values satisfy a predicate.

**Input**:
- Pattern Subject
- Value (VClosure predicate: Value -> Bool)

**Output**: EvalM Value (VBool)

**Behavior**:
- Flatten Pattern to all Subject values
- Convert each Subject to Value
- Apply predicate to each Value
- Return VBool (true if all match)

**Errors**:
- Type mismatch if predicate is not VClosure
- Evaluation error if predicate application fails

---

## Module: PatternLisp.Runtime

### File Loading

#### `loadPlispFile :: FilePath -> Env -> IO (Either Error Env)`

Loads a `.plisp` file and adds its result to the environment.

**Input**:
- FilePath (`.plisp` file path)
- Env (current environment)

**Output**: IO (Either Error Env)

**Behavior**:
- Read file content
- Parse and evaluate as Lisp program
- Derive identifier from filename (remove `.plisp` extension)
- If result is lambda: Bind to filename-derived name
- If result is named function (from `define`): Use that name (filename ignored)
- Return updated environment

**Errors**:
- File read error
- Parse error
- Evaluation error
- Invalid identifier from filename

---

#### `loadGramFile :: FilePath -> IO (Either Error (String, Pattern Subject))`

Loads a `.gram` file and parses it to Pattern Subject.

**Input**:
- FilePath (`.gram` file path)

**Output**: IO (Either Error (String, Pattern Subject))

**Behavior**:
- Read file content
- Parse Gram notation to Subject
- Convert Subject to Pattern Subject
- Derive identifier from filename (remove `.gram` extension)
- Return (identifier, Pattern Subject)

**Errors**:
- File read error
- Parse error (invalid Gram syntax)
- Conversion error (Subject to Pattern Subject)

---

#### `processFiles :: [FilePath] -> Env -> IO (Either Error Env)`

Processes all files in order (`.gram` first, then `.plisp`).

**Input**:
- [FilePath] (list of file paths)
- Env (initial environment)

**Output**: IO (Either Error Env)

**Behavior**:
- Separate files by extension (`.gram` vs `.plisp`)
- Process `.gram` files first (create state variables)
- Process `.plisp` files second (create functions that may use states)
- Return environment with all bindings

**Errors**:
- File load errors (fail fast on first error)
- Duplicate identifiers (same filename-derived name)

---

#### `serializeRuntime :: RuntimeState -> Subject`

Serializes runtime state to Subject.

**Input**: RuntimeState

**Output**: Subject

**Behavior**:
- Convert environment to Subject (Map serialization)
- Convert executionTrace to Subject (list serialization, if present)
- Combine into Subject structure

**Errors**: None (all components are serializable)

---

#### `deserializeRuntime :: Subject -> Either Error RuntimeState`

Deserializes runtime state from Subject.

**Input**: Subject

**Output**: Either Error RuntimeState

**Behavior**:
- Extract environment from Subject
- Extract executionTrace from Subject (if present)
- Reconstruct RuntimeState

**Errors**:
- Invalid structure (missing components, wrong types)

---

## Error Types

All functions use the existing `Error` type from `PatternLisp.Syntax`:
- `TypeMismatch String Value` - Wrong type in operation
- `ArityMismatch String Int Int` - Wrong number of arguments
- `UndefinedVar String Expr` - Undefined variable
- `ParseError String` - Parse/serialization error

Additional error cases:
- Missing primitive in registry (during deserialization)
- Invalid tool form (during validation)
- Non-Pattern result from tool (during execution)

