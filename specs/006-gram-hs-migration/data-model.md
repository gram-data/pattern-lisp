# Data Model: Gram-HS Constructor Migration

**Date**: 2025-01-28

## Overview

This migration does not change the data model. Patterns remain the same structure; only the constructor function names change. This document describes the pattern structure for reference during migration.

## Pattern Structure

### Pattern Type
```haskell
Pattern v  -- where v is the decoration type (Subject in our case)
```

### Atomic Pattern
**Old API**: `pattern :: v -> Pattern v`  
**New API**: `point :: v -> Pattern v`

**Structure**: Pattern with no elements, decoration only.

**Example**:
```haskell
-- Old
atomic = pattern (Subject { ... })

-- New
atomic = point (Subject { ... })
```

### Pattern with Elements
**Old API**: `patternWith :: v -> [Pattern v] -> Pattern v`  
**New API**: `pattern :: v -> [Pattern v] -> Pattern v`

**Structure**: Pattern with decoration and list of child patterns.

**Example**:
```haskell
-- Old
withElements = patternWith decoration [child1, child2]

-- New
withElements = pattern decoration [child1, child2]
```

## Entities

### Pattern Constructor
- **Type**: Function
- **Old Names**: `pattern` (atomic), `patternWith` (with elements)
- **New Names**: `point` (atomic), `pattern` (with elements)
- **Signature**: `v -> Pattern v` (atomic) or `v -> [Pattern v] -> Pattern v` (with elements)

### Pattern Value
- **Type**: `Pattern Subject`
- **Structure**: Unchanged - decoration (Subject) + optional elements (list of Pattern Subject)
- **Migration Impact**: None - structure unchanged, only construction changes

## Relationships

- Pattern can contain other Patterns as elements (nested structure)
- Pattern decoration is a Subject (unchanged)
- Pattern elements are list of Patterns (unchanged)

## Validation Rules

- Atomic patterns: Must use `point`, take single argument
- Patterns with elements: Must use `pattern`, take decoration and list of elements
- Type checker enforces correct usage

## State Transitions

N/A - This is a refactoring task, not a stateful system.

## Migration Impact

**No data model changes** - Only constructor function names change. All pattern structures, relationships, and validation rules remain identical.

