#!/bin/bash
# Local CI script equivalent to .github/workflows/build-and-test.yml
# Runs the same build and test steps locally

set -e  # Exit on error

# Colors for output
RED='\033[0;31m'
GREEN='\033[0;32m'
YELLOW='\033[1;33m'
NC='\033[0m' # No Color

# Function to print colored output
info() {
    echo -e "${GREEN}[INFO]${NC} $1"
}

warn() {
    echo -e "${YELLOW}[WARN]${NC} $1"
}

error() {
    echo -e "${RED}[ERROR]${NC} $1"
}

# Check if we're in the project root
if [ ! -f "pattern-lisp.cabal" ] || [ ! -f "cabal.project" ]; then
    error "Must be run from project root directory"
    exit 1
fi

info "Starting local CI build and test..."

# Step 1: Initialize git submodules (equivalent to checkout with submodules: recursive)
info "Initializing git submodules..."
if [ -f ".gitmodules" ]; then
    git submodule update --init --recursive || warn "Git submodules may not be needed or already initialized"
else
    info "No .gitmodules found, skipping submodule initialization"
fi

# Step 2: Check for required tools
info "Checking for required tools..."
if ! command -v cabal &> /dev/null; then
    error "cabal not found. Please install Cabal."
    exit 1
fi

if ! command -v ghc &> /dev/null; then
    error "ghc not found. Please install GHC."
    exit 1
fi

CABAL_VERSION=$(cabal --version | head -n1 | awk '{print $3}')
GHC_VERSION=$(ghc --version | awk '{print $NF}')
info "Found cabal $CABAL_VERSION and ghc $GHC_VERSION"

# Step 3: Configure Cabal (equivalent to cabal update)
info "Updating Cabal index..."
cabal update

# Step 4: Build project with -Wall (equivalent to cabal build --ghc-options="-Wall" all)
info "Building project with -Wall..."
cabal build --ghc-options="-Wall" all

# Step 5: Run tests (equivalent to cabal test --test-show-details=direct all)
info "Running tests..."
cabal test --test-show-details=direct all

# Step 6: Build executable (equivalent to cabal build exe:pattern-lisp)
info "Building executable..."
cabal build exe:pattern-lisp

info "All CI steps completed successfully!"
