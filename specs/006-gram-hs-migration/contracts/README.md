# Contracts: Gram-HS Constructor Migration

**Date**: 2025-01-28

## Overview

This migration is an internal refactoring task that updates constructor function names. No external API contracts change - this is purely an internal implementation detail.

## Internal API Changes

### Pattern Constructor Functions

**Before**:
- `pattern :: v -> Pattern v` (atomic)
- `patternWith :: v -> [Pattern v] -> Pattern v` (with elements)

**After**:
- `point :: v -> Pattern v` (atomic)
- `pattern :: v -> [Pattern v] -> Pattern v` (with elements)

## Impact

- **External API**: No changes - Pattern Lisp public API unchanged
- **Internal API**: Constructor function names changed
- **Serialization Format**: Unchanged - gram notation format unchanged
- **Behavior**: Unchanged - only constructor names differ

## Migration Contract

All internal code using Pattern constructors must:
1. Use `point` for atomic patterns
2. Use `pattern` for patterns with elements
3. Import `Pattern.Core (point, pattern, ...)` instead of `Pattern.Core (pattern, patternWith, ...)`

## Verification

- Type checker enforces correct usage
- Test suite verifies functional correctness
- No external contract changes required

