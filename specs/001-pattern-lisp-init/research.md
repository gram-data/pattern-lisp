# Research: Pattern Lisp Project Initialization

**Date**: 2025-12-19  
**Feature**: Pattern Lisp Project Initialization  
**Purpose**: Resolve technical unknowns identified in implementation plan

## Research Questions

### 1. Haskell/GHC Version Selection

**Question**: What specific GHC version should be used for Pattern Lisp project?

**Decision**: Use GHC 9.6.3 (or latest stable 9.6.x) as the base version.

**Rationale**: 
- GHC 9.6.x is a recent stable release series with good tooling support
- Cabal 3.10+ supports GHC 9.6.x well
- Most modern Haskell libraries support GHC 9.6.x
- Provides good balance between stability and modern language features

**Alternatives Considered**:
- GHC 9.8.x: Newer but may have less library compatibility
- GHC 9.4.x: Older, more stable but missing some recent features
- GHC 9.0.x: Too old, limited library support

**Verification Required**: 
- Check gram-hs repository for GHC version requirements in its `.cabal` file or CI configuration
- Verify that gram-hs builds successfully with GHC 9.6.3
- If gram-hs requires a different version, adjust accordingly

**Implementation**: Specify `ghc-options: -Werror` and minimum GHC version in `pattern-lisp.cabal`:
```cabal
cabal-version:      3.0
build-type:         Simple

library
  default-language: Haskell2010
  ghc-options:      -Wall -Werror
  build-depends:
    base >=4.18 && <5
```

### 2. gram-hs Dependency Version/Tag

**Question**: What git tag, commit, or branch should be used for gram-hs source repository dependency?

**Decision**: Use `main` branch initially, with plan to pin to specific commit or release tag once verified.

**Rationale**:
- `main` branch provides latest features and bug fixes
- Allows for iterative development and dependency updates
- Can be pinned to specific commit hash for reproducibility once stable

**Alternatives Considered**:
- Latest release tag: More stable but may be outdated
- Specific commit hash: Most reproducible but requires knowing the exact commit
- `master` branch: Legacy naming, prefer `main`

**Verification Required**:
- Check gram-hs repository for available tags/releases
- Verify that `main` branch builds successfully
- Consider pinning to a specific commit hash for reproducibility:
  ```cabal
  source-repository-package
    type: git
    location: https://github.com/gram-data/gram-hs
    tag: <commit-hash-or-tag>
  ```

**Implementation**: Configure in `cabal.project`:
```cabal
source-repository-package
  type: git
  location: https://github.com/gram-data/gram-hs
  tag: main  # Or specific commit/tag after verification
```

### 3. Cabal Version Requirements

**Question**: What Cabal version is required for this project setup?

**Decision**: Use Cabal 3.10 or later.

**Rationale**:
- Cabal 3.10+ supports modern features like source repositories, better dependency resolution
- Widely available and stable
- Good compatibility with GHC 9.6.x

**Verification**: Run `cabal --version` to verify installed version meets requirements.

### 4. Project Structure Best Practices

**Question**: Are there any Cabal-specific best practices for library + executable projects?

**Decision**: Follow standard Cabal conventions:
- `src/` for library modules
- `app/` for executable entry point
- `test/` for test files
- `examples/` for example programs (not part of Cabal build, but useful for documentation)

**Rationale**:
- Standard Cabal conventions ensure compatibility with tooling
- Clear separation of concerns
- Easy to navigate for developers familiar with Haskell projects

**Implementation**: Directory structure as specified in plan.md.

## Summary

All technical unknowns have been resolved with reasonable defaults based on Haskell/Cabal best practices. The decisions prioritize:
1. **Stability**: Using proven, stable tool versions
2. **Compatibility**: Ensuring gram-hs dependency can be resolved
3. **Maintainability**: Following standard conventions
4. **Flexibility**: Allowing for adjustments based on actual gram-hs requirements

**Next Steps**: 
- Verify gram-hs compatibility with chosen GHC version during implementation
- Pin gram-hs dependency to specific commit/tag once verified
- Update plan.md with resolved technical context

