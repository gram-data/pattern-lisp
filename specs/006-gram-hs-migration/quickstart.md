# Quickstart: Gram-HS Constructor Migration

**Date**: 2025-01-28

## Overview

This guide provides a quick reference for migrating Pattern Lisp codebase to use the new gram-hs constructor API.

## Migration Steps

### Step 1: Update Imports

Find all modules importing `Pattern.Core`:

```haskell
-- Old
import Pattern.Core (pattern, patternWith, ...)

-- New
import Pattern.Core (point, pattern, ...)
```

**Affected Files**:
- `src/PatternLisp/Codec.hs`
- `src/PatternLisp/PatternPrimitives.hs`
- `src/PatternLisp/Gram.hs`
- `test/PatternLisp/CodecSpec.hs`
- `test/PatternLisp/GramSpec.hs`
- `test/PatternLisp/GramSerializationSpec.hs`
- `test/PatternLisp/RuntimeSpec.hs`

### Step 2: Replace `patternWith` → `pattern`

Find all occurrences of `patternWith`:

```bash
grep -r "patternWith" src/ test/
```

Replace each occurrence:
```haskell
-- Old
patternWith decoration elements

-- New
pattern decoration elements
```

### Step 3: Replace Atomic `pattern` → `point`

Find atomic pattern constructions (single argument, not a list):

```haskell
-- Old
pattern subject
pattern $ valueToSubjectForGram val

-- New
point subject
point $ valueToSubjectForGram val
```

**Key Indicator**: If `pattern` takes one argument (not a list), it should be `point`.

### Step 4: Update Comments and Documentation

Review code comments and module documentation for references to the old API:

```haskell
-- Old (in Haddock comments)
-- > patternWith decoration [element1, element2]

-- New
-- > pattern decoration [point element1, point element2]
```

**Areas to check**:
- Module header examples (Haddock `-- >` examples)
- Inline comments explaining pattern construction
- Function documentation examples

### Step 5: Verify

After each module migration:

```bash
# Compile
cabal build

# Run tests
cabal test
```

## Common Patterns

### Atomic Pattern Creation
```haskell
-- Old
let pat = pattern $ valueToSubjectForGram (VNumber n)

-- New
let pat = point $ valueToSubjectForGram (VNumber n)
```

### Pattern with Elements
```haskell
-- Old
let pat = patternWith decoration elementPatterns

-- New
let pat = pattern decoration elementPatterns
```

### Nested Patterns
```haskell
-- Old
let nested = patternWith outerDecoration
      [ patternWith middleDecoration
          [ pattern innerSubject ]
      ]

-- New
let nested = pattern outerDecoration
      [ pattern middleDecoration
          [ point innerSubject ]
      ]
```

## Verification Checklist

- [ ] All imports updated
- [ ] All `patternWith` replaced with `pattern`
- [ ] All atomic `pattern` calls replaced with `point`
- [ ] Code comments and documentation examples updated
- [ ] Code compiles: `cabal build`
- [ ] All tests pass: `cabal test`
- [ ] No `patternWith` in codebase (except historical references in comments if appropriate): `grep -r "patternWith" src/ test/`

## Troubleshooting

### Compilation Error: "Not in scope: `patternWith`"
- Check imports: Should be `import Pattern.Core (point, pattern, ...)`
- Verify all `patternWith` calls replaced with `pattern`

### Compilation Error: "Couldn't match type"
- Check if atomic pattern uses `point` (one argument)
- Check if pattern with elements uses `pattern` (two arguments: decoration and list)

### Test Failures
- Verify pattern structure unchanged (only constructor names changed)
- Check serialization/deserialization round-trips still work

