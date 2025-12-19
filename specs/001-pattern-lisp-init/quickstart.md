# Quickstart: Pattern Lisp Project Initialization

**Date**: 2025-12-19  
**Feature**: Pattern Lisp Project Initialization  
**Purpose**: Verify that project setup is complete and functional

## Prerequisites

- Cabal 3.10 or later installed
- GHC 9.6.3 (or compatible version) installed
- Git installed and configured
- Network access to GitHub (for gram-hs dependency)

## Verification Steps

### Step 1: Verify Project Structure

Check that all required directories exist:

```bash
cd pattern-lisp
ls -la
```

**Expected Output**: Directories `src/`, `app/`, `test/`, `examples/` should be present.

**Success Criteria**: All four directories exist.

---

### Step 2: Verify Cabal Configuration Files

Check that Cabal configuration files exist and are readable:

```bash
ls -la cabal.project pattern-lisp.cabal
cat cabal.project
cat pattern-lisp.cabal
```

**Expected Output**: 
- Both files should exist
- `cabal.project` should contain `source-repository-package` section for gram-hs
- `pattern-lisp.cabal` should define `library`, `executable`, and `test-suite` sections

**Success Criteria**: Both files exist and contain expected configuration.

---

### Step 3: Update Cabal Index and Resolve Dependencies

Fetch dependency information and resolve gram-hs:

```bash
cabal update
```

**Expected Output**: 
- Command completes successfully
- No errors about unreachable repositories
- gram-hs dependency information is fetched

**Success Criteria**: `cabal update` completes with exit code 0.

---

### Step 4: Build All Components

Build library, executable, and test suite:

```bash
cabal build all
```

**Expected Output**:
- Build process starts
- Dependencies are resolved and built
- Library, executable, and test suite compile successfully
- No compilation errors

**Success Criteria**: Build completes successfully with exit code 0.

**Time Target**: Build should complete within 5 minutes for a fresh setup (per SC-001).

---

### Step 5: Verify gram-hs Import

Create a minimal test import to verify gram-hs is accessible:

```bash
# Create a temporary test file
cat > src/Lisp/Test.hs << 'EOF'
module Lisp.Test where

-- Attempt to import a gram-hs module
-- Replace with actual gram-hs module name once verified
-- import Gram.Something

testFunction :: String
testFunction = "gram-hs import test"
EOF

# Try to build
cabal build
```

**Expected Output**: 
- File compiles successfully
- No import resolution errors (once actual gram-hs module is imported)

**Success Criteria**: Library builds with gram-hs import (after uncommenting and using correct module name).

---

### Step 6: Execute Test Suite

Run the test framework:

```bash
cabal test
```

**Expected Output**:
- Test framework executes
- Test suite runs (even if no test cases are defined yet)
- Framework reports test results

**Success Criteria**: Test framework executes successfully with exit code 0 (per SC-004).

---

### Step 7: Execute CLI

Run the CLI executable:

```bash
cabal run pattern-lisp
# Or if executable has different name:
# cabal exec pattern-lisp
```

**Expected Output**:
- Executable runs
- Produces some output (even if minimal)
- No immediate crash

**Success Criteria**: CLI executes and responds within 1 second (per SC-003).

---

### Step 8: Verify Git Repository (Optional)

If git repository was initialized:

```bash
git status
```

**Expected Output**:
- Git repository is initialized
- Project files are tracked or ready to be committed

**Success Criteria**: `.git` directory exists and `git status` works.

---

## Troubleshooting

### Issue: `cabal update` fails

**Possible Causes**:
- Network connectivity issues
- gram-hs repository URL is incorrect
- Cabal index is corrupted

**Solutions**:
- Check network connection
- Verify gram-hs repository URL in `cabal.project`
- Try `cabal update --index-state=HEAD`

### Issue: `cabal build` fails with dependency errors

**Possible Causes**:
- gram-hs dependency cannot be resolved
- GHC version incompatibility
- Missing system dependencies

**Solutions**:
- Verify gram-hs repository is accessible
- Check GHC version compatibility (see research.md)
- Review build error messages for specific dependency issues

### Issue: Import errors for gram-hs modules

**Possible Causes**:
- Incorrect module name
- gram-hs not properly built
- Module not exposed in gram-hs package

**Solutions**:
- Check gram-hs documentation for correct module names
- Verify gram-hs builds successfully: `cabal build gram-hs` (if in same project)
- Check gram-hs `.cabal` file for exposed modules

### Issue: Test suite doesn't run

**Possible Causes**:
- Test framework not properly configured
- Missing test dependencies
- Test entry point not found

**Solutions**:
- Verify `test-suite` section in `pattern-lisp.cabal`
- Check that test file exists and is referenced correctly
- Ensure hspec dependency is listed in test-suite `build-depends`

## Success Validation

All steps should complete successfully. The project is ready for development when:

✅ All directories exist  
✅ Cabal files are valid  
✅ Dependencies resolve (`cabal update` succeeds)  
✅ Build succeeds (`cabal build all` succeeds)  
✅ Tests execute (`cabal test` succeeds)  
✅ CLI runs (`cabal run` succeeds)  
✅ gram-hs can be imported  

## Next Steps

After successful verification:
1. Proceed to Phase 1: Core Lisp Evaluator development
2. Begin implementing `Lisp.Syntax`, `Lisp.Parser`, `Lisp.Eval` modules
3. Add test cases following test-first development principles

