# Gram as Intermediate Representation: Implementation Plan

**Date**: 2025-01-27  
**Feature**: Pattern State Functions - Phase 6  
**Goal**: Adopt approach where all `.plisp` files are implicitly `(lambda (state) ...)` and gram becomes the intermediate representation for piping between programs.

## Overview

This phase implements the symmetry where:
- **`.gram` files** = `Pattern Subject` (property record + sequence of patterns)
- **`.plisp` files** = body of `(lambda (state) ...)` that returns `Pattern Subject`
- **Gram notation** = intermediate representation for piping between programs (same machine or network)

## Key Changes

1. **Value to Pattern Subject Mapping**: Every s-expression can be represented as `Pattern Subject`
2. **Implicit Lambda Wrapping**: `.plisp` files are automatically wrapped in `(lambda (state) ...)`
3. **Automatic Return Value Mapping**: Tool return values are automatically mapped to `Pattern Subject`
4. **Gram Serialization/Deserialization**: Add functions to convert between `Pattern Subject` and gram notation
5. **CLI Enhancements**: Support stdin/stdout for gram files, piping between programs

## Implementation Steps

### Step 1: Value to Pattern Subject Mapping

**File**: `src/PatternLisp/PatternPrimitives.hs`

Add function to map any `Value` to its `Pattern Subject` representation:

```haskell
-- | Maps any Value to its Pattern Subject representation.
-- This enables all s-expressions to be represented as Pattern Subject.
valueToPatternSubject :: Value -> EvalM (Pattern Subject)
valueToPatternSubject (VPattern pat) = return pat
valueToPatternSubject val = do
  -- Convert Value to Subject, then wrap in Pattern
  let subject = valueToSubject val
      pat = pattern subject
  return pat
```

**For lists**: Need to handle `VList` specially - convert to `pattern-with` with elements:
- If list is empty: `(pattern empty-subject)` (atomic pattern)
- If list has elements: `(pattern-with decoration [pattern-elements])` where decoration is empty Subject and elements are recursively converted

### Step 2: Update executeTool

**File**: `src/PatternLisp/Runtime.hs`

Modify `executeTool` to automatically map return values to `Pattern Subject`:

```haskell
executeTool :: Value -> Pattern Subject -> Env -> Either Error (Pattern Subject)
executeTool toolVal inputState env = do
  -- ... existing closure application logic ...
  result <- evalExpr bodyExpr extendedEnv
  
  -- Map result to Pattern Subject (instead of requiring it to be VPattern)
  case result of
    VPattern outputState -> Right outputState
    _ -> do
      -- Automatically map to Pattern Subject representation
      let mappedPattern = runReader (valueToPatternSubject result) env
      case mappedPattern of
        Right pat -> Right pat
        Left err -> Left err
```

### Step 3: Implicit Lambda Wrapping for .plisp Files

**File**: `app/Main.hs` or new `src/PatternLisp/FileLoader.hs`

When loading `.plisp` files:
1. Parse file as sequence of expressions
2. If file doesn't start with `(lambda (state) ...)`, wrap it:
   ```scheme
   (lambda (state)
     expr1
     expr2
     ...
     exprN)  ; last expression is return value
   ```
3. Evaluate wrapped lambda to get closure
4. Bind to filename-derived name (or `define` name if present)

**Implementation**:
```haskell
loadPlispFile :: FilePath -> Env -> IO (Either Error (String, Value))
loadPlispFile filepath env = do
  content <- readFile filepath
  case parseExpr content of
    Left err -> return $ Left err
    Right expr -> do
      -- Check if already a lambda with "state" parameter
      let wrappedExpr = if isStateLambda expr
                        then expr
                        else wrapInStateLambda expr
      case evalExpr wrappedExpr env of
        Left err -> return $ Left err
        Right val -> do
          let name = deriveNameFromFilename filepath
          return $ Right (name, val)
```

### Step 4: Gram Serialization/Deserialization

**File**: `src/PatternLisp/Gram.hs` (new module)

Add functions to convert between `Pattern Subject` and gram notation:

```haskell
module PatternLisp.Gram
  ( patternToGram
  , gramToPattern
  ) where

import Pattern (Pattern)
import Subject.Core (Subject)
import Gram.Serialize (toGram)
import Gram.Parse (fromGram)

-- | Serialize Pattern Subject to gram notation string
patternToGram :: Pattern Subject -> String
patternToGram = toGram

-- | Deserialize gram notation string to Pattern Subject
gramToPattern :: String -> Either ParseError (Pattern Subject)
gramToPattern = fromGram
```

**Dependencies**: Already have `gram` package in `cabal.project`

### Step 5: CLI Enhancements for Stdin/Stdout

**File**: `app/Main.hs`

Add support for:
- Reading gram from stdin: `pattern-lisp tool.plisp < input.gram`
- Writing gram to stdout: `pattern-lisp tool.plisp input.gram > output.gram`
- Piping: `pattern-lisp tool1.plisp input.gram | pattern-lisp tool2.plisp`

**Implementation**:
```haskell
main :: IO ()
main = do
  args <- getArgs
  case args of
    [] -> repl initialEnv  -- Interactive REPL
    ["-"] -> do
      -- Read gram from stdin, execute with default tool, write to stdout
      gramInput <- getContents
      case gramToPattern gramInput of
        Left err -> hPutStrLn stderr (show err) >> exitFailure
        Right inputState -> do
          -- Execute with default tool (or require tool argument)
          -- Write result as gram to stdout
    [toolFile] -> do
      -- Load tool, read gram from stdin, execute, write to stdout
    [toolFile, inputFile] -> do
      -- Load tool, read gram from inputFile, execute, write to stdout
    _ -> usage
```

### Step 6: File Loading Updates

**File**: `app/Main.hs` or `src/PatternLisp/FileLoader.hs`

Update file loading to:
1. Process `.gram` files first (load as `Pattern Subject`)
2. Process `.plisp` files second (wrap in lambda, evaluate to closure)
3. Populate environment with both

**Implementation**:
```haskell
processFiles :: [FilePath] -> Env -> IO (Either Error Env)
processFiles files initialEnv = do
  let (gramFiles, plispFiles) = partition (isSuffixOf ".gram") files
  -- Process gram files first
  gramEnv <- foldM loadGramFile initialEnv gramFiles
  -- Process plisp files second
  foldM loadPlispFile gramEnv plispFiles
```

### Step 7: Tests

**File**: `test/PatternLisp/GramSpec.hs` (new)

Add tests for:
- `valueToPatternSubject` for all value types
- `patternToGram` and `gramToPattern` round-trips
- Implicit lambda wrapping
- CLI stdin/stdout/piping

## Dependencies

- `gram` package: Already in `cabal.project` ✅
- `pattern` package: Already in `cabal.project` ✅
- `subject` package: Already in `cabal.project` ✅

## Benefits

1. **Symmetry**: `.gram` and `.plisp` files both represent `Pattern Subject`
2. **Composability**: Programs can be piped together using gram as IR
3. **Network-friendly**: Gram can be sent over network as text
4. **Simplicity**: No need to explicitly wrap return values in `(pattern ...)`
5. **Consistency**: All programs follow same pattern: `Pattern Subject -> Pattern Subject`

## Migration Notes

- Existing `.plisp` files will continue to work (auto-wrapped)
- Tools that explicitly return `VPattern` will work unchanged
- Tools that return other values will be automatically mapped
- No breaking changes to existing functionality

## Next Steps

1. Implement `valueToPatternSubject` function
2. Update `executeTool` to use automatic mapping
3. Add gram serialization/deserialization module
4. Implement implicit lambda wrapping for file loading
5. Update CLI for stdin/stdout/piping support
6. Add comprehensive tests

