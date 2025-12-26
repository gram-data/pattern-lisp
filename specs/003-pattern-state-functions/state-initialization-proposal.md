# State Initialization Proposal

**Feature**: Pattern State Functions  
**Date**: 2025-01-27  
**Status**: Proposal for User Selection

## Problem Statement

Pattern Lisp programs are pure functions `(lambda (state) ...)` that transform Pattern Subject state. Before tools can be executed, the initial state must be established. We need to determine how users should initialize state in the CLI/REPL.

## Proposed Approaches

### Option A: Empty Pattern as Default

**Description**: Runtime starts with an empty Pattern Subject. Users must explicitly initialize state.

**Commands**:
- `:init <expr>` - Set initial state from Lisp expression
- `:state` - Display current state

**Example Usage**:
```bash
> :init (pattern "my-initial-state")
> :exec my-tool
> :state
```

**Pros**:
- Simple and explicit
- Forces users to think about initial state
- No hidden state

**Cons**:
- Requires explicit initialization step
- Empty state may not be useful
- Users must remember to initialize

---

### Option B: Pattern from Expression

**Description**: Set state from a Lisp expression that evaluates to a Pattern.

**Commands**:
- `:set-state <expr>` - Set state from expression
- `:state` - Display current state

**Example Usage**:
```bash
> :set-state (pattern-with "root" (list (pattern "a") (pattern "b")))
> :exec my-tool
```

**Pros**:
- Flexible - any Pattern structure
- Uses existing Lisp evaluation
- Can update state at any time

**Cons**:
- Requires Pattern construction expressions
- May be verbose for simple cases
- No clear initialization vs update distinction

---

### Option C: Pattern from File

**Description**: Load initial state from a Gram file or serialized Subject file.

**Commands**:
- `:load-state <file>` - Load Pattern Subject from file
- `:state` - Display current state

**Example Usage**:
```bash
> :load-state initial-state.gram
> :exec my-tool
```

**Pros**:
- Enables state persistence and reuse
- Complex state structures in files
- Version control friendly

**Cons**:
- Requires file I/O
- Users must create state files
- Less interactive for quick testing

---

### Option D: Pattern from Command-Line

**Description**: Accept initial state as CLI argument when starting REPL.

**Commands**:
- CLI: `pattern-lisp --init-state <file>` - Initialize from file
- CLI: `pattern-lisp --init-expr <expr>` - Initialize from expression
- REPL: `:state` - Display current state

**Example Usage**:
```bash
$ pattern-lisp --init-state initial.gram
> :exec my-tool
```

**Pros**:
- Convenient for scripting
- State ready at startup
- Supports both file and expression

**Cons**:
- Only works at startup
- Less flexible for interactive use
- Requires CLI argument parsing

---

### Option E: Hybrid Approach ⭐ (Recommended)

**Description**: Combine multiple approaches - default empty state, with commands for expression, file, or CLI initialization.

**Commands**:
- Default: Empty Pattern Subject on REPL startup
- `:init <expr>` - Set state from Lisp expression (initialization)
- `:load-state <file>` - Load state from Gram/Subject file
- `:set-state <expr>` - Update state from expression (if updates needed)
- `:state` - Display current state (read-only)
- CLI: `pattern-lisp --init-state <file>` - Initialize from file at startup
- CLI: `pattern-lisp --init-expr <expr>` - Initialize from expression at startup

**Example Usage**:
```bash
# Interactive
> :init (pattern "my-state")
> :exec my-tool

# From file
> :load-state initial.gram
> :exec my-tool

# Scripted
$ pattern-lisp --init-state initial.gram --exec my-tool
```

**Pros**:
- Maximum flexibility
- Supports interactive and scripted workflows
- Clear separation: initialization vs updates
- Principle of least surprise (empty default, explicit init)

**Cons**:
- More commands to implement
- Users must learn multiple commands
- More complex than single approach

---

### Option F: Convention-Based Auto-Loading ⭐⭐ (Selected)

**Description**: Use file extension conventions to auto-populate REPL environment. `.plisp` files become functions, `.gram` files become state variables. Filenames become identifiers. Files are provided as explicit CLI arguments (no directory scanning).

**Conventions**:
- `.plisp` files: Evaluated as Lisp programs
  - If result is lambda: Derive function name from filename (e.g., `hello-tool.plisp` → `hello-tool`)
  - If result is named function: Use that name
  - Tools become callable functions in environment
- `.gram` files: Parsed as Pattern Subject state
  - Filename becomes variable name (e.g., `initial-state.gram` → `initial-state`)
  - Each `.gram` file is a separate pattern variable

**REPL Behavior**:
- On startup: Process all non-flag arguments as files
  - `.plisp` files: Evaluate and bind to filename-derived name (or defined name)
  - `.gram` files: Parse to Pattern Subject and bind to filename-derived name
- Environment populated with functions and states from provided files
- Functions and states are first-class in environment
- No explicit `:load` commands needed

**Commands**:
- `:reload` - Re-process files from startup arguments (optional, for development)
- `:env` - Display current environment (functions and states available)
- `:state` - Display current state (if using single current state model)
- CLI: `pattern-lisp [flags] <file1> <file2> ...` - Process files and start REPL
- CLI: `pattern-lisp -e <expr>` or `--eval <expr>` - Evaluate expression (non-interactive, files still loaded)
- CLI: `pattern-lisp` (no args) - Interactive REPL with empty environment (or minimal default)

**Example Usage**:
```bash
# Shell globbing provides files explicitly
$ pattern-lisp hello-tool.plisp initial-state.gram user-data.gram
> (hello-tool initial-state)     ; Call function with state
> (hello-tool user-data)          ; Same function, different state
> (define result (hello-tool initial-state))
> result

# Multiple tools and states
$ pattern-lisp *.plisp *.gram
> (tool1 state1)
> (tool2 (tool1 state1))          ; Composition

# Non-interactive (stdin)
$ echo "(hello-tool initial-state)" | pattern-lisp hello-tool.plisp initial-state.gram

# Non-interactive (eval)
$ pattern-lisp -e "(hello-tool initial-state)" hello-tool.plisp initial-state.gram

# Empty environment (no files)
$ pattern-lisp
> (pattern "test")                ; Can still use primitives
```

**Function Invocation Model**:
- Tools are functions: `(tool-name state-var)` or `(tool-name (pattern "new-state"))`
- No special `:exec` command needed
- Functions can be composed: `(tool2 (tool1 state1))`
- State variables are first-class values
- Multiple states available simultaneously

**Filename-to-Identifier Conversion**:
- Remove file extension: `hello-tool.plisp` → `hello-tool`
- Handle hyphens/underscores: Keep as-is (valid Lisp identifiers)
- Handle special characters: Convert to hyphens or error (TBD)
- Case sensitivity: Preserve (Lisp is case-sensitive)

**Pros**:
- Convention over configuration (file extensions)
- Explicit file list (no hidden directory scanning)
- Shell-friendly (works with globbing: `*.plisp *.gram`)
- Functions and states are first-class (Lisp-like)
- Less ceremony (no `:load`, `:exec` commands)
- Natural composition (function calls)
- Works for both interactive and non-interactive
- Filenames become identifiers (intuitive)
- Supports multiple states (not just one "current" state)
- Predictable behavior (explicit file list)

**Cons**:
- Must provide files explicitly (but shell globbing handles this)
- Filename-to-identifier conversion (sanitization needed)
- Multiple states might be confusing (which is "current"?)
- File reloading requires restart or `:reload` command
- File order matters if files depend on each other

**Implementation Considerations**:
- File processing order: Process `.gram` files first (states), then `.plisp` files (functions that may use states)
- Lambda naming: If file contains `(lambda (state) ...)`, bind to filename-derived name
- Named functions: If file contains `(define my-tool (lambda ...))`, use `my-tool` (filename ignored)
- State variables: `initial-state.gram` → `initial-state` (Pattern Subject variable)
- Error handling: If file fails to load, skip with warning and continue, or fail fast? (Recommend: fail fast for explicit files)
- File dependencies: If `tool1.plisp` depends on `tool2.plisp`, order matters (document this)

**Hybrid with Current State Model** (Optional):
- Could combine with single "current state" if needed
- `:set-current <state-var>` - Set which state variable is "current"
- `:exec <tool-name>` - Execute tool with current state (if single-state model desired)
- But function call model `(tool-name state-var)` is more flexible and recommended

---

## Comparison Table

| Approach | Simplicity | Flexibility | Persistence | Scripting | Interactive | Lisp-like | Explicit |
|----------|------------|-------------|-------------|-----------|-------------|-----------|----------|
| A: Empty Default | ⭐⭐⭐ | ⭐⭐ | ⭐ | ⭐ | ⭐⭐ | ⭐⭐ | ⭐⭐⭐ |
| B: Expression | ⭐⭐ | ⭐⭐⭐ | ⭐ | ⭐ | ⭐⭐⭐ | ⭐⭐⭐ | ⭐⭐ |
| C: File | ⭐⭐ | ⭐⭐ | ⭐⭐⭐ | ⭐⭐ | ⭐ | ⭐⭐ | ⭐⭐ |
| D: CLI Arg | ⭐⭐ | ⭐ | ⭐⭐ | ⭐⭐⭐ | ⭐ | ⭐ | ⭐⭐⭐ |
| E: Hybrid | ⭐ | ⭐⭐⭐ | ⭐⭐⭐ | ⭐⭐⭐ | ⭐⭐⭐ | ⭐⭐ | ⭐⭐ |
| F: Convention-Based | ⭐⭐ | ⭐⭐⭐ | ⭐⭐⭐ | ⭐⭐⭐ | ⭐⭐⭐ | ⭐⭐⭐ | ⭐⭐⭐ |

## Recommendation

**Option F (Convention-Based Auto-Loading)** is **SELECTED** because:
1. Convention over configuration (file extensions)
2. Explicit file list (no hidden directory scanning, shell-friendly)
3. Functions and states are first-class (more Lisp-like)
4. Natural composition through function calls
5. Supports multiple states (not just one "current" state)
6. Works seamlessly for both interactive and non-interactive use
7. Less ceremony than command-based approaches
8. Predictable behavior (explicit file arguments)
9. Shell-friendly (works with globbing: `pattern-lisp *.plisp *.gram`)

**Key Advantages**:
- No directory scanning overhead
- Shell globbing handles file selection: `pattern-lisp *.plisp *.gram`
- Explicit and predictable (you see exactly what files are loaded)
- More Lisp-like (functions are functions, not special commands)
- Multiple states available simultaneously
- Works great with shell scripts and automation

**Implementation Notes**:
- Process `.gram` files first (states), then `.plisp` files (functions)
- Filename sanitization: `hello-tool.plisp` → `hello-tool` (remove extension)
- If file contains `(define name ...)`, use that name; otherwise use filename
- Error handling: Fail fast on file load errors (explicit files should be valid)

## Questions for Decision

1. **Default Behavior**: Should REPL start with empty state, or require explicit initialization before any tool execution?
2. **File Support**: Is loading state from files important for your use case?
3. **CLI Arguments**: Do you need CLI arguments for state initialization (for scripting/automation)?
4. **Update vs Init**: Should state initialization be separate from state updates, or can they share commands?

## Selection

**✅ Option F (Convention-Based Auto-Loading) - SELECTED**

This approach has been selected for implementation. The CLI will:
- Process all non-flag arguments as files (`.plisp` → functions, `.gram` → state variables)
- Derive identifiers from filenames
- Populate REPL environment with functions and states
- Support function invocation: `(tool-name state-var)`
- Support non-interactive execution via `-e/--eval` or stdin

---

**Note**: This decision affects the implementation of:
- `PatternLisp.Runtime` module (initial state handling)
- `app/Main.hs` REPL commands
- CLI argument parsing
- Documentation and examples

