# Research: Keywords, Maps, and Sets Implementation

**Date**: 2025-01-27  
**Feature**: Keywords, Maps, and Sets  
**Status**: Complete

## Research Questions

### 1. Keyword Implementation Pattern

**Question**: How should keywords be represented in Haskell? Should they be interned?

**Decision**: Keywords are represented as a distinct `Atom` variant `Keyword String` and `Value` variant `VKeyword String`. Keywords are NOT interned (no global symbol table) - they are compared by string equality.

**Rationale**: 
- Simpler implementation (no global state)
- Keywords are self-evaluating, so no environment lookup needed
- String comparison is sufficient for keyword equality
- Matches Clojure's approach (keywords are values, not interned symbols)

**Alternatives Considered**:
- Interned keywords (global symbol table): Rejected - adds complexity, no clear benefit
- Keywords as special symbols: Rejected - keywords must be distinct from symbols for type safety

**Implementation Notes**:
- Add `Keyword String` to `Atom` type
- Add `VKeyword String` to `Value` type
- Parser recognizes `symbol:` pattern (postfix colon)
- Evaluator returns `VKeyword` directly (no lookup)

---

### 2. Map Implementation Pattern

**Question**: How should maps be represented? Use `Data.Map` with keywords as keys?

**Decision**: Use `Data.Map.Strict` with `VKeyword` as keys: `VMap (Map.Map VKeyword Value)`. Maps are immutable (all operations return new maps).

**Rationale**:
- `Data.Map.Strict` provides efficient O(log n) operations
- Immutable maps align with Pattern Lisp's functional semantics
- Using `VKeyword` as keys ensures type safety (only keywords can be map keys)
- Strict maps are more memory-efficient for large maps

**Alternatives Considered**:
- `Data.Map.Lazy`: Rejected - strict maps are more predictable for language implementation
- Hash maps (unordered): Rejected - `Data.Map` provides ordered iteration which is useful for serialization
- Association lists: Rejected - O(n) lookup vs O(log n) for maps

**Implementation Notes**:
- Add `VMap (Map.Map VKeyword Value)` to `Value` type
- Parser recognizes `{key: value ...}` syntax
- All map operations (get, assoc, dissoc, update) return new maps
- Duplicate keys: last value wins (silently overwrites)

---

### 3. Set Implementation Pattern

**Question**: How should sets be represented? Use `Data.Set`?

**Decision**: Use `Data.Set` with `Value` as elements: `VSet (Set.Set Value)`. Sets are immutable and automatically remove duplicates.

**Rationale**:
- `Data.Set` provides efficient O(log n) membership and operations
- Immutable sets align with Pattern Lisp's functional semantics
- `Set Value` allows sets to contain any value type (numbers, strings, keywords, maps, other sets)
- Subject labels are `Set String`, so sets of strings work directly

**Alternatives Considered**:
- Hash sets (unordered, O(1) operations): Rejected - `Data.Set` is standard, provides ordered iteration
- Lists as sets: Rejected - O(n) membership vs O(log n) for sets
- Custom set type: Rejected - `Data.Set` is well-tested and efficient

**Implementation Notes**:
- Add `VSet (Set.Set Value)` to `Value` type
- Parser recognizes `#{...}` syntax (hash set literal)
- Set operations (union, intersection, difference) use `Data.Set` functions
- Duplicate elements automatically removed during construction

---

### 4. Parser Implementation Pattern

**Question**: How to parse new syntax (keywords, maps, sets) with Megaparsec?

**Decision**: Extend existing Megaparsec parser with new parsers:
- Keyword parser: `symbol <* char ':'` (symbol followed by colon)
- Map parser: `char '{' *> ... <* char '}'` (curly braces with key-value pairs)
- Set parser: `string "#{" *> ... <* char '}'` (hash set syntax)

**Rationale**:
- Megaparsec already used in codebase
- Parser combinators make syntax extensions straightforward
- Error messages include position information automatically
- Can reuse existing whitespace and atom parsers

**Alternatives Considered**:
- Custom parser: Rejected - Megaparsec is already integrated and well-tested
- Reader macros: Rejected - Pattern Lisp doesn't use reader macros, uses parser directly

**Implementation Notes**:
- Keywords: Parse `symbol:` pattern, ensure colon is postfix (not prefix)
- Maps: Parse `{key: value ...}` with whitespace handling
- Sets: Parse `#{...}` with whitespace handling
- All parsers must handle nested structures (maps in maps, sets in sets)

---

### 5. Serialization Pattern

**Question**: How to serialize keywords, maps, and sets to/from Subject representation?

**Decision**: Extend existing `Codec.hs` serialization:
- Keywords: Store as `VString` in Subject properties (or new `VKeyword` type if Subject supports it)
- Maps: Store as `VMap (Map String Value)` in Subject properties (convert keywords to strings for keys)
- Sets: Store as `VArray` in Subject properties (convert to list, then array)

**Rationale**:
- Existing serialization uses Subject properties (`VMap`, `VArray`, `VString`)
- Subject.Value types don't include keywords/maps/sets, so conversion needed
- Round-trip serialization must preserve types (keywords remain keywords, not strings)

**Alternatives Considered**:
- Extend Subject.Value types: Rejected - would require changes to `subject` library
- Custom serialization format: Rejected - must use existing Subject format for gram interop

**Implementation Notes**:
- Keywords: Serialize as `VString` with special marker (e.g., `":keyword:name"`) OR use `VTaggedString`
- Maps: Convert `Map VKeyword Value` to `Map String Value` (keyword to string) for serialization
- Sets: Convert `Set Value` to `VArray [Value]` for serialization
- Deserialization: Detect markers/format and reconstruct original types

---

### 6. Label Syntax (Prefix Colon)

**Question**: Should prefix colon syntax (`:Person`) be implemented?

**Decision**: Deferred - not required for core feature. Subject labels are `Set String`, so plain strings in sets (`#{"Person" "Employee"}`) are sufficient. Prefix colon can be added later as optional syntactic sugar.

**Rationale**:
- Clarification from spec: labels are just strings, prefix colon is optional
- Simpler implementation (no new syntax needed)
- Can be added later without breaking changes
- Gram interop works with string sets directly

**Alternatives Considered**:
- Implement prefix colon now: Rejected - adds complexity, not required
- Never implement: Deferred - may add later for gram notation compatibility

**Implementation Notes**:
- For now: Use `#{"Person" "Employee"}` for Subject labels
- Future: If prefix colon added, `:Person` evaluates to `"Person"` string

---

## Implementation Order

Based on dependencies:

1. **Keywords** (P1) - Required for maps
   - Add `Keyword` to `Atom`, `VKeyword` to `Value`
   - Parser: keyword syntax
   - Evaluator: keyword evaluation
   - Serialization: keyword serialization

2. **Sets** (P2) - Can be parallel with keywords
   - Add `VSet` to `Value`
   - Parser: set literal syntax
   - Evaluator: set literal evaluation
   - Primitives: set operations
   - Serialization: set serialization

3. **Maps** (P1) - Requires keywords
   - Add `VMap` to `Value`
   - Parser: map literal syntax
   - Evaluator: map literal evaluation
   - Primitives: map operations
   - Serialization: map serialization

4. **Labels** (P3) - Optional, deferred
   - Not implemented in this phase

---

## Testing Strategy

**Unit Tests** (Hspec):
- Parser tests: keyword, map, set literal parsing
- Evaluator tests: keyword evaluation, map/set evaluation
- Primitive tests: all map/set operations
- Serialization tests: round-trip serialization

**Property Tests** (QuickCheck):
- Map operations: `assoc` then `get` returns correct value
- Set operations: union is commutative, intersection is idempotent
- Serialization: round-trip preserves values and types

**Integration Tests**:
- Gram interop: Subject labels as string sets
- Serialization: keywords, maps, sets in Pattern Subject
- Error handling: type mismatches, invalid syntax

---

## Performance Considerations

- **Map operations**: `Data.Map` provides O(log n) operations (acceptable for language implementation)
- **Set operations**: `Data.Set` provides O(log n) operations (acceptable)
- **Serialization**: Conversion overhead (keywords→strings, sets→arrays) is acceptable for gram interop
- **Memory**: Immutable structures may create temporary copies during operations (acceptable tradeoff for functional semantics)

---

## Open Questions (Resolved)

✅ All research questions resolved. No remaining ambiguities.

---

## References

- Pattern Lisp codebase: `src/PatternLisp/`
- Megaparsec documentation: https://hackage.haskell.org/package/megaparsec
- Data.Map documentation: https://hackage.haskell.org/package/containers/docs/Data-Map-Strict.html
- Data.Set documentation: https://hackage.haskell.org/package/containers/docs/Data-Set.html
- Clojure syntax conventions: https://clojure.org/reference/reader#_literals

