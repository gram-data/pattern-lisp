# Quick Start: Pattern State Functions

**Feature**: Pattern State Functions  
**Date**: 2025-01-27  
**Phase**: 1 - Design

## Overview

This feature integrates Pattern as a first-class value type in Pattern Lisp, enabling programs to work with Pattern Subject state. All programs follow the canonical form `(lambda (state) ...)` transforming Pattern Subject → Pattern Subject. Complete serialization enables code-as-data capabilities.

## Prerequisites

- Pattern Lisp interpreter (from previous features)
- gram-data dependency (Pattern and Subject types)
- Haskell/Cabal build environment

## Installation

Add gram-data dependency to `pattern-lisp.cabal`:

```yaml
build-depends:
  , gram-data
```

Build the project:

```bash
cabal build all
```

## Basic Usage

### 1. Creating Pattern Values

```scheme
;; Create an atomic pattern
(define atomic (pattern "hello"))

;; Create a pattern with elements
(define with-elements (pattern-with "root" 
  (list (pattern "child1") (pattern "child2"))))
```

### 2. Querying Pattern Values

```scheme
;; Extract decoration
(pattern-value atomic)        ; => "hello"

;; Get elements
(pattern-elements with-elements) ; => list of patterns

;; Get structural information
(pattern-length with-elements)   ; => 2
(pattern-size with-elements)     ; => 3 (total nodes)
(pattern-depth with-elements)    ; => 1 (max depth)
```

### 3. Writing Tools (Canonical Form)

All tools must follow the canonical form `(lambda (state) ...)`:

```scheme
;; Simple tool that adds a greeting
(lambda (state)
  (pattern-with 
    (pattern-value state)  ; Preserve decoration
    (cons 
      (pattern "Hello from Pattern Lisp!")
      (pattern-elements state))))
```

### 4. Using Pattern Predicates

```scheme
;; Find patterns matching a predicate
(define large-patterns
  (pattern-find state 
    (lambda (p) 
      (> (pattern-size p) 10))))

;; Check if any value satisfies predicate
(define has-numbers
  (pattern-any? state 
    (lambda (v) 
      (number? v))))

;; Check if all values satisfy predicate
(define all-strings
  (pattern-all? state 
    (lambda (v) 
      (string? v))))
```

## Runtime Commands

**Note**: State initialization uses Option F (Convention-Based Auto-Loading). Files are provided as CLI arguments.

### Loading Files

Files are loaded automatically from CLI arguments:

```bash
# Load tools and states from files
$ pattern-lisp hello-tool.plisp initial-state.gram user-data.gram
> (hello-tool initial-state)     ; Call function with state
> (hello-tool user-data)          ; Same function, different state

# Shell globbing
$ pattern-lisp *.plisp *.gram
> (tool1 state1)
> (tool2 (tool1 state1))          ; Composition
```

**REPL Commands**:
- `:reload` - Re-process files from startup arguments
- `:env` - Display current environment (functions and states available)
- `:state` - Display current state (if using single current state model)

### Loading Tools

```bash
:load examples/hello-tool.plisp
```

Loads a tool definition from a file and validates it as canonical form.

### Executing Tools

```bash
:exec hello-tool
```

Executes a tool by name with the current state.

### Inspecting State

```bash
:state
```

Displays the current Pattern Subject state.

### Viewing Execution Trace

```bash
:trace
```

Shows the execution trace (tool name, input state, output state for each execution).

### Persisting Runtime

```bash
:save runtime.gram
```

Serializes the entire runtime (state, tools, trace) to a Gram file.

### Restoring Runtime

```bash
:restore runtime.gram
```

Deserializes runtime from a Gram file and resumes execution.

## Example: Complete Tool Workflow

```scheme
;; 1. Define a tool that reads state and creates summary
(lambda (state)
  (let ((count (pattern-size state))
        (depth (pattern-depth state)))
    (pattern-with
      (pattern-value state)
      (cons 
        (pattern-with "summary" 
          (list 
            (pattern count)
            (pattern depth)))
        (pattern-elements state)))))

;; 2. Save to file: state-reader.plisp

;; 3. In REPL:
:load state-reader.plisp
:exec state-reader

;; 4. Check result:
:state

;; 5. View trace:
:trace
```

## Serialization Examples

### Serializing Values

All Pattern Lisp values can be serialized to Subject:

```haskell
-- In Haskell
valueToSubject (VNumber 42)
valueToSubject (VString "hello")
valueToSubject (VPattern (pattern subject))
valueToSubject (VClosure closure)
valueToSubject (VPrimitive Add)
```

### Round-Trip Serialization

```haskell
-- Serialize and deserialize
let subject = valueToSubject value
    result = subjectToValue subject
-- result should equal original value (or be functionally equivalent)
```

### Serializing Closures

Closures serialize with their captured environment:

```scheme
;; Closure with captured variable
(let ((x 10))
  (lambda (y) (+ x y)))

;; Serializes to Subject with:
;; - params: ["y"]
;; - env: [Binding {name: "x", value: Subject(10)}]
;; - body: Expression AST as Subject
```

## Testing

### Running Tests

```bash
cabal test
```

### Test Structure

- `test/PatternLisp/SubjectSpec.hs` - Serialization round-trip tests
- `test/PatternLisp/PatternSpec.hs` - Pattern primitive tests
- `test/PatternLisp/RuntimeSpec.hs` - Runtime and tool execution tests
- `test/Properties.hs` - Property-based tests for serialization

### Example Test

```haskell
it "round-trips closures" $ do
  let closure = Closure ["x"] (Atom (Symbol "x")) (Map.singleton "x" (VNumber 1))
      value = VClosure closure
      subject = valueToSubject value
      result = subjectToValue subject
  result `shouldBe` Right value
  -- Closure should remain executable
```

## Common Patterns

### Tool Composition

```scheme
;; Define multiple tools
(define add-timestamp
  (lambda (state)
    (pattern-with (pattern-value state)
      (cons (pattern "timestamp") (pattern-elements state)))))

(define add-metadata
  (lambda (state)
    (pattern-with (pattern-value state)
      (cons (pattern "metadata") (pattern-elements state)))))

;; Compose tools
(lambda (state)
  (add-metadata (add-timestamp state)))
```

### Code-as-Data

```scheme
;; Store closures in state
(lambda (state)
  (let ((incrementer (lambda (x) (+ x 1)))
        (doubler (lambda (x) (* x 2))))
    (pattern-with "functions"
      (list (pattern incrementer)
            (pattern doubler)))))
```

### State Filtering

```scheme
;; Filter state elements
(lambda (state)
  (let ((filtered (pattern-find state 
                    (lambda (p) 
                      (> (pattern-size p) 1)))))
    (pattern-with "filtered" (list filtered))))
```

## Troubleshooting

### Tool Validation Errors

**Error**: "Tool must be lambda with single 'state' parameter"

**Solution**: Ensure tool is `(lambda (state) ...)` form, not `(lambda (x) ...)` or `(lambda (x y) ...)`

### Type Errors

**Error**: "Expected pattern, but got: VNumber"

**Solution**: Use `(pattern value)` to create Pattern values before using pattern operations

### Serialization Errors

**Error**: "Primitive not found in registry"

**Solution**: Ensure all primitives are registered in `PatternLisp.Primitives.initialEnv`

### Runtime Errors

**Error**: "Tool returned non-Pattern value"

**Solution**: Ensure tool returns a Pattern Subject, not a raw value

## Next Steps

- Read [data-model.md](data-model.md) for entity definitions
- Read [contracts/README.md](contracts/README.md) for API documentation
- Explore examples in `examples/` directory
- Review [pattern-state-lisp-design.md](../../docs/pattern-state-lisp-design.md) for design rationale

## Verification

After implementation, verify:

1. ✅ Pattern values can be created and queried
2. ✅ Tools can be loaded and executed
3. ✅ Serialization round-trips preserve functionality
4. ✅ Closures remain executable after serialization
5. ✅ Runtime can be saved and restored
6. ✅ Execution traces record state transformations

Run tests:

```bash
cabal test
```

All tests should pass.

