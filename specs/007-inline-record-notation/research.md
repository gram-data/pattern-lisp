# Research: Inline Record Notation for Pattern-Lisp

**Date**: 2025-01-30  
**Feature**: Inline Record Notation  
**Purpose**: Resolve technical unknowns and establish implementation patterns

## Research Questions

### 1. Gram Parser Integration for Record Parsing

**Question**: How can we integrate the gram parser to parse record syntax from within the pattern-lisp reader, while maintaining proper error reporting and stream position management?

**Decision**: Use a two-phase approach: (1) Detect `{` character in the Megaparsec parser, (2) Extract the record substring (from `{` to matching `}`), (3) Invoke gram parser on the substring to parse the record, (4) Convert gram's parsed record structure to pattern-lisp's record representation, (5) Translate gram parse errors to pattern-lisp parse errors with position information.

**Rationale**:
- Gram parser already handles record syntax correctly and maintains consistency with gram notation
- Two-phase approach avoids complex stream handoff between parsers
- Substring extraction is straightforward with Megaparsec's position tracking
- Error translation preserves position information from both parsers
- Maintains separation of concerns: Megaparsec handles Lisp syntax, gram parser handles record syntax

**Alternatives Considered**:
- Reimplementing record parsing in Megaparsec: Duplicates gram's logic, risks inconsistency
- Full stream handoff to gram parser: Complex stream management, difficult error translation
- Preprocessing step: Adds complexity, breaks parser composition

**Implementation Pattern**:
```haskell
-- Pseudo-code structure
recordParser :: Parser Expr
recordParser = do
  startPos <- getSourcePos
  _ <- char '{'
  -- Extract record substring by parsing to matching '}'
  recordText <- extractRecordText
  endPos <- getSourcePos
  -- Parse with gram parser
  case parseRecordFromGram recordText of
    Left gramErr -> 
      fail $ translateGramError gramErr startPos
    Right gramRecord ->
      return $ RecordLiteral (gramRecordToPatternLispRecord gramRecord)
```

**Verification**: Test with various record formats including nested records, empty records, and malformed syntax. Verify error messages include correct line/column information.

### 2. Record Representation in Value Type

**Question**: How should records be represented in the pattern-lisp Value type to support immutability, structural equality, and efficient operations?

**Decision**: Use gram's `Subject.Value.VMap (Map String Value)` directly (Option B - gram types throughout). This replaces the existing `VMap` type. Use `Data.Map.Strict` for O(log n) key lookup and efficient iteration. Keys are strings (converted from gram identifiers during parsing). Values are gram `Subject.Value` types, enabling nested records.

**Rationale**:
- Map provides efficient operations (lookup, insert, delete, merge)
- Immutability is natural with Map (operations return new Map)
- Structural equality works correctly with Map's Eq instance
- String keys match gram's identifier-to-string conversion
- Nested records supported by recursive Value type
- **Uses gram's VMap type directly** - aligns with Option B (gram types throughout), eliminates conversion layer

**Alternatives Considered**:
- Separate VRecord type: Not needed - gram's VMap IS the record type (Option B)
- Association list: O(n) lookup, less efficient
- Custom record type: Unnecessary complexity, gram's VMap provides needed operations
- Symbol keys: Requires conversion, strings are more natural for gram compatibility

**Implementation Pattern**:
```haskell
-- In Syntax.hs - use gram types directly (Option B)
import Subject.Value (Value(..))  -- Use gram Value type

-- Records use Subject.Value.VMap directly
-- No separate VRecord type - VMap IS the record type

-- Record operations use Map operations on VMap
recordGet :: Map String Value -> String -> Maybe Value
recordGet = Map.lookup

recordSet :: Map String Value -> String -> Value -> Map String Value
recordSet = Map.insert

recordRemove :: Map String Value -> String -> Map String Value
recordRemove = Map.delete
```

**Verification**: Test structural equality with different key orderings, verify immutability (operations return new records), test nested record support.

### 3. Stream Position Management and Error Translation

**Question**: How do we maintain accurate position information when delegating to gram parser and translate gram parse errors to pattern-lisp parse errors?

**Decision**: Capture start position before invoking gram parser. Extract record substring with position tracking. When gram parser fails, translate error by adding offset from start position. Use Megaparsec's `getSourcePos` and `setSourcePos` to maintain position context.

**Rationale**:
- Position tracking is essential for good error messages
- Offset calculation is straightforward when extracting substring
- Megaparsec provides position tracking utilities
- Error translation preserves user-facing error information

**Alternatives Considered**:
- Ignoring position in errors: Poor developer experience
- Complex position tracking: Unnecessary, simple offset works
- Separate error types: Loses information, complicates error handling

**Implementation Pattern**:
```haskell
-- Pseudo-code structure
translateGramError :: Gram.ParseError -> SourcePos -> String
translateGramError gramErr startPos = 
  let (gramLine, gramCol) = extractGramErrorPosition gramErr
      adjustedLine = sourceLine startPos
      adjustedCol = sourceColumn startPos + gramCol - 1  -- Adjust for '{' character
  in formatError adjustedLine adjustedCol (gramErrorMessage gramErr)
```

**Verification**: Test error messages for various malformed records, verify line/column numbers are correct, test nested record error positions.

### 4. Record Operations Implementation

**Question**: How should record operations (get, set, remove, merge, etc.) be implemented to ensure immutability and efficient performance?

**Decision**: Implement all operations using Map operations, which are naturally immutable. Operations return new Map instances. Use Map's efficient operations: `lookup` (O(log n)), `insert` (O(log n)), `delete` (O(log n)), `union` (O(n log n)). Expose as primitives in Primitives.hs, following existing primitive patterns.

**Rationale**:
- Map operations are naturally immutable (return new Map)
- Efficient performance meets requirements (<100ms for 1000 keys)
- Standard Map API is well-tested and reliable
- Consistent with existing primitive implementation patterns
- Operations are composable and predictable

**Alternatives Considered**:
- Mutable operations: Violates immutability requirement
- Custom data structure: Unnecessary, Map provides needed operations
- Lazy evaluation: Not needed, strict Map is appropriate

**Implementation Pattern**:
```haskell
-- In Primitives.hs
evalPrimitive :: Primitive -> [Value] -> EvalM Value
evalPrimitive RecordGet [VRecord m, VString key] = 
  return $ maybe VNil id (Map.lookup key m)
evalPrimitive RecordSet [VRecord m, VString key, val] = 
  return $ VRecord (Map.insert key val m)
evalPrimitive RecordRemove [VRecord m, VString key] = 
  return $ VRecord (Map.delete key m)
evalPrimitive RecordMerge [VRecord m1, VRecord m2] = 
  return $ VRecord (Map.union m2 m1)  -- Right takes precedence
```

**Verification**: Test all operations with various record sizes, verify immutability (original records unchanged), test edge cases (empty records, missing keys, duplicate keys in merge).

### 5. Quasiquotation Integration with Records

**Question**: How should records integrate with quasiquotation (unquoting and splicing) to enable dynamic record construction?

**Decision**: Extend the Quote Expr type to support unquote markers within record literals. During parsing, mark positions where unquotes appear. During evaluation of quasiquoted records, evaluate unquoted expressions and splice results into the record structure. For splicing (`@`), the unquoted expression must evaluate to a record, and its entries are merged into the containing record.

**Rationale**:
- Quasiquotation is essential for dynamic record construction
- Marking unquote positions during parsing is cleanest approach
- Evaluation-time substitution maintains correct semantics
- Splicing enables record composition patterns
- Consistent with existing quasiquotation patterns

**Alternatives Considered**:
- Preprocessing quasiquotes: Loses position information, complicates parsing
- Macro expansion: Overkill, evaluation-time substitution is sufficient
- Separate record construction functions: Less convenient, breaks quasiquotation consistency

**Implementation Pattern**:
```haskell
-- Extend Expr to support unquotes in records
data Expr = ...
          | RecordLiteral [(String, Expr)]  -- Keys and values (values may be unquoted)
          | Unquote Expr                    -- Unquote marker
          | UnquoteSplice Expr              -- Splicing marker

-- During evaluation
evalRecordLiteral :: [(String, Expr)] -> EvalM Value
evalRecordLiteral pairs = do
  evaluatedPairs <- mapM (\(k, v) -> do
    val <- case v of
      Unquote expr -> evalExpr expr
      UnquoteSplice expr -> do
        VRecord m <- evalExpr expr
        return $ VRecord m  -- Will be merged
      _ -> evalExpr v
    return (k, val)
    ) pairs
  -- Merge spliced records
  let (regularPairs, splicedRecords) = partition isRegular evaluatedPairs
      merged = foldl Map.union Map.empty (map recordMap splicedRecords)
      regular = Map.fromList regularPairs
  return $ VRecord (Map.union merged regular)
```

**Verification**: Test unquoting with various value types, test splicing with nested records, test error cases (splicing non-records), verify evaluation order.

### 6. Records as Values

**Question**: How should records be used with pattern construction in pattern-lisp?

**Decision**: Records are `Subject.Value.VMap` values (gram's PropertyRecord) that can be used anywhere in pattern-lisp code. They can be passed as arguments to existing pattern construction functions. Records ARE `Subject.properties` - no conversion needed (Option B alignment).

**Rationale**:
- Records are values, not special syntax
- Pattern-lisp uses function calls `()` for pattern construction, not square bracket notation `[]`
- Square brackets `[Label { ... }]` are gram notation (serialization format), not pattern-lisp syntax
- Records can be used naturally with existing pattern-lisp functions
- No new syntax needed beyond the record literal `{ key: value }`

**Alternatives Considered**:
- Adding `[Label { ... }]` syntax: Not needed - pattern-lisp uses function calls, not square brackets
- Special subject notation: Unnecessary - records are just values

**Implementation Pattern**:
```haskell
-- Records are just values - no special integration needed
-- They can be used with existing pattern functions:
-- (define rec { name: "Alice" })
-- (define pat (some-pattern-function rec))
```

**Verification**: Test that records can be used as values, passed to functions, and converted to Subject.properties when needed for serialization.

## Summary

All technical unknowns have been resolved with patterns that:

1. **Leverage existing infrastructure**: Gram parser for record parsing, Map for representation
2. **Maintain consistency**: Follow gram notation, match existing pattern-lisp patterns
3. **Ensure correctness**: Immutability, structural equality, proper error handling
4. **Enable integration**: Pattern subjects, quasiquotation, standard operations

**Key Implementation Patterns**:
- Two-phase parsing: Megaparsec detects, gram parser parses records
- Gram type alignment: Use `Subject.Value.VMap` directly (Option B) - replaces VMap
- Position tracking: Capture and translate positions for error reporting
- Immutable operations: All operations return new records
- Quasiquotation support: Unquote and splice markers in record literals
- Records as values: Records can be used anywhere in pattern-lisp code

**Next Steps**:
- Implement record parser integration in Parser.hs (replaces mapParser)
- Replace VMap with Subject.Value.VMap in Syntax.hs (Option B)
- Replace MapLiteral with RecordLiteral in Syntax.hs
- Update all map operations to work with Subject.Value.VMap in Primitives.hs
- Rename hash-map → record in Runtime.hs and Syntax.hs
- Write comprehensive tests for all scenarios
- Verify all success criteria are met
