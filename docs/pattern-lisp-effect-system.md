# Pattern-Lisp Effect System

**⚠️ Future: Not Yet Implemented - Design Document v0.1**

This document describes a planned effect system for Pattern Lisp. The features described here are **not yet implemented** in the current codebase.

### Overview

This document specifies a minimal effect system for pattern-lisp that enables pure functional programs to describe I/O and other side effects. The key architectural principle is **separation of description from execution**: pattern-lisp programs construct effect descriptions, while the host runtime interprets and executes them.

This design is intentionally host-agnostic. Whether the host is implemented in Haskell, Rust, TypeScript, or another language, it must provide the primitives described here. Throughout this document, we use Effect-TS examples to illustrate concepts, as it provides a mature reference implementation of these ideas.

---

## Design Principles

### 1. Effects as Values

Effects are **lazy descriptions** of computations, not immediate actions. Creating an effect does nothing—it produces a data structure representing what *should* happen.

```typescript
// Effect-TS: This does NOT perform I/O—it describes I/O
const readFile = Effect.tryPromise(() => fs.readFile("config.txt"))

// The effect only executes when explicitly run
Effect.runPromise(readFile)
```

### 2. Explicit Error Tracking

Errors are tracked in the type system. The effect type has three parameters:

```
Effect<Success, Error, Requirements>
       │        │      │
       │        │      └── Services/context needed to run
       │        └── Expected errors that may occur  
       └── Value produced on success
```

### 3. Host Executes, Pattern-Lisp Describes

Pattern-lisp programs return effect descriptions. The host runtime:
- Interprets effect constructors
- Provides service implementations
- Manages resources, concurrency, and scheduling
- Handles actual I/O operations

### 4. Minimal Core, Rich Host

Pattern-lisp's effect vocabulary is deliberately small (~20 operations). Advanced features (concurrency, scheduling, resource management) are declared by pattern-lisp but implemented entirely by the host.

### 5. Composition via Pipe, Not Generators

Pattern-lisp favors explicit functional composition over imperative-style generator syntax. The `pipe` function threads values through a sequence of transformations, making the Kleisli composition visible:

```lisp
;; Pipe makes the arrows explicit
(pipe
  (effect/service "FileSystem")           ;; Effect<FS, never, FS>
  (effect/flatMap (fn [fs] (fs.read p)))  ;; FS -> Effect<String, IOError, never>
  (effect/map parse-json)                 ;; String -> Config
  (effect/catchAll use-default))          ;; IOError -> Effect<Config, never, never>
```

This is more idiomatic for a Lisp, preserves categorical structure, and avoids the imperative "feel" of generators.

---

## Type Definitions

### The Effect Type

The core effect type represents a computation that:
- Produces a value of type `A` on success
- May fail with an error of type `E`
- Requires a context containing services of type `R`

```
Effect<A, E, R>
```

**Pattern-lisp syntax:**
```lisp
;; Type annotations (documentation only—pattern-lisp is dynamically typed)
;; (Effect String)                    -- succeeds with String
;; (Effect String Error)              -- may fail with Error
;; (Effect String Error FileSystem)   -- requires FileSystem service
```

**Effect-TS equivalent:**
```typescript
// Effect-TS has both "Effect" (full) and "Micro" (lightweight)
// Pattern-lisp uses "Effect" as the concept name, hosts may use either
type Micro<A, E = never, R = never> = Effect.Effect<A, E, R>
```

### Exit

Represents the result of executing an effect:

```lisp
(deftype Exit
  (Success value)    ;; Computation succeeded with value
  (Failure cause))   ;; Computation failed with cause
```

**Effect-TS equivalent:**
```typescript
type Exit<A, E> = 
  | { _tag: "Success"; value: A }
  | { _tag: "Failure"; cause: Cause<E> }
```

### Cause

Describes why a computation failed:

```lisp
(deftype Cause
  (Fail error)    ;; Expected, recoverable error (tracked in E)
  (Die defect)    ;; Unexpected defect/bug (not tracked in E)
  (Interrupt))    ;; Computation was cancelled
```

**Effect-TS equivalent:**
```typescript
type Cause<E> =
  | { _tag: "Fail"; error: E }
  | { _tag: "Die"; defect: unknown }
  | { _tag: "Interrupt" }
```

The distinction between `Fail` and `Die` is crucial:
- **Fail**: Business logic errors (user not found, validation failed). These are expected and typed.
- **Die**: Bugs and defects (null pointer, out of bounds). These are unexpected and untyped.

### Option

Represents an optional value:

```lisp
(deftype Option
  (Some value)
  None)
```

**Effect-TS equivalent:**
```typescript
type Option<A> = { _tag: "Some"; value: A } | { _tag: "None" }
```

### Either

Represents one of two possible values:

```lisp
(deftype Either
  (Left value)
  (Right value))
```

By convention, `Left` holds errors and `Right` holds success values.

**Effect-TS equivalent:**
```typescript
type Either<E, A> = { _tag: "Left"; left: E } | { _tag: "Right"; right: A }
```

---

## Tier 1: Foundational Operations

These operations form the irreducible core. Without them, pattern-lisp cannot describe effectful computations.

### succeed

Lifts a pure value into an effect that immediately succeeds.

**Signature:**
```
succeed : A -> Effect<A, never, never>
```

**Pattern-lisp:**
```lisp
(effect/succeed 42)
(effect/succeed "hello")
(effect/succeed {name: "Alice" age: 30})
```

**Effect-TS equivalent:**
```typescript
Micro.succeed(42)
Micro.succeed("hello")
Micro.succeed({ name: "Alice", age: 30 })
```

**Host implementation notes:**
- This is the "pure" or "return" operation of the monad
- Must not perform any side effects
- The simplest effect constructor

---

### fail

Creates an effect that fails with an expected error.

**Signature:**
```
fail : E -> Effect<never, E, never>
```

**Pattern-lisp:**
```lisp
(effect/fail (NotFoundError {id: 123}))
(effect/fail (ValidationError {field: "email" reason: "invalid format"}))
```

**Effect-TS equivalent:**
```typescript
Micro.fail(new NotFoundError({ id: 123 }))
Micro.fail(new ValidationError({ field: "email", reason: "invalid format" }))
```

**Host implementation notes:**
- Creates a `Fail` variant of `Cause`
- Error is tracked in the type's `E` parameter
- Error should propagate until caught by an error handler

---

### sync

Wraps a synchronous host operation that may have side effects.

**Signature:**
```
sync : (() -> A) -> Effect<A, never, never>
```

**Pattern-lisp:**
```lisp
(effect/sync (fn [] (host/current-time-millis)))
(effect/sync (fn [] (host/random)))
(effect/sync (fn [] (host/env "HOME")))
```

**Effect-TS equivalent:**
```typescript
Micro.sync(() => Date.now())
Micro.sync(() => Math.random())
Micro.sync(() => process.env.HOME)
```

**Host implementation notes:**
- The thunk is executed when the effect is run, not when created
- If the thunk throws, the error becomes a `Die` (defect), not a `Fail`
- For operations that may throw expected errors, use `try` instead

---

### map

Transforms the success value of an effect.

**Signature:**
```
map : Effect<A, E, R> -> (A -> B) -> Effect<B, E, R>
```

**Pattern-lisp:**
```lisp
(effect/map 
  (effect/succeed 10)
  (fn [x] (* x 2)))
;; => Effect<20, never, never>

(effect/map
  (read-file "data.txt")
  parse-json)
;; => Effect<JsonValue, IOError, FileSystem>
```

**Effect-TS equivalent:**
```typescript
Micro.succeed(10).pipe(Micro.map((x) => x * 2))

readFile("data.txt").pipe(Micro.map(parseJson))
```

**Host implementation notes:**
- This is the functor `fmap` operation
- Does not change error type or requirements
- Function `f` should be pure; side effects in `f` are not tracked

---

### flatMap

Sequences two effects, where the second depends on the result of the first.

**Signature:**
```
flatMap : Effect<A, E1, R1> -> (A -> Effect<B, E2, R2>) -> Effect<B, E1 | E2, R1 | R2>
```

**Pattern-lisp:**
```lisp
(effect/flatMap
  (get-user-id)
  (fn [id] (fetch-user id)))
;; => Effect<User, AuthError | NotFoundError, Database>

(effect/flatMap
  (effect/succeed 5)
  (fn [x] 
    (if (> x 0)
      (effect/succeed x)
      (effect/fail (NegativeError x)))))
```

**Effect-TS equivalent:**
```typescript
getUserId().pipe(
  Micro.flatMap((id) => fetchUser(id))
)

Micro.succeed(5).pipe(
  Micro.flatMap((x) => 
    x > 0 ? Micro.succeed(x) : Micro.fail(new NegativeError(x))
  )
)
```

**Host implementation notes:**
- This is the monadic `bind` (>>=) operation
- Errors from either effect propagate
- Requirements from both effects are merged (union)
- Fundamental to all effect sequencing

---

### catchAll

Recovers from any expected error by providing a fallback effect.

**Signature:**
```
catchAll : Effect<A, E, R> -> (E -> Effect<B, E2, R2>) -> Effect<A | B, E2, R | R2>
```

**Pattern-lisp:**
```lisp
(effect/catchAll
  (fetch-user id)
  (fn [error] (effect/succeed default-user)))
;; => Effect<User, never, Database>

(effect/catchAll
  (parse-config path)
  (fn [error] 
    (log-warning (str "Config parse failed: " error))
    (effect/succeed default-config)))
```

**Effect-TS equivalent:**
```typescript
fetchUser(id).pipe(
  Micro.catchAll((error) => Micro.succeed(defaultUser))
)

parseConfig(path).pipe(
  Micro.catchAll((error) => {
    console.warn(`Config parse failed: ${error}`)
    return Micro.succeed(defaultConfig)
  })
)
```

**Host implementation notes:**
- Only catches `Fail` errors, not `Die` defects or `Interrupt`
- The handler receives the error value and returns a new effect
- If handler succeeds, error type becomes the handler's error type

---

### service

Requests a service from the environment by its tag.

**Signature:**
```
service : Tag -> Effect<Service, never, Service>
```

**Pattern-lisp:**
```lisp
(effect/service "FileSystem")
;; => Effect<FileSystemService, never, FileSystem>

(effect/service "Logger")
;; => Effect<LoggerService, never, Logger>

;; Usage with pipe (when only final value needed)
(pipe
  (effect/service "FileSystem")
  (effect/flatMap (fn [fs] (fs.read "config.txt"))))

;; Usage with do-notation (when intermediate values needed)
(effect/do
  [fs   <- (effect/service "FileSystem")
   data <- (fs.read "input.txt")
   _    <- (fs.write "output.txt" (transform data))]
  (effect/succeed data))
```

**Effect-TS equivalent:**
```typescript
// Define service tag
class FileSystem extends Context.Tag("FileSystem")<
  FileSystem,
  { read: (path: string) => Micro<string, IOError> }
>() {}

// Request service with pipe
pipe(
  FileSystem,
  Effect.flatMap((fs) => fs.read("config.txt"))
)

// Or using gen syntax for multiple bindings
Effect.gen(function* () {
  const fs = yield* FileSystem
  const content = yield* fs.read("config.txt")
  return content
})
```

**Host implementation notes:**
- The tag is an identifier (string) that the host uses to look up implementations
- Pattern-lisp doesn't define services—it only requests them
- The host must provide all requested services before running the effect
- Requesting an unprovided service is a type error (in typed hosts) or runtime error

---

## Tier 2: Essential Operations

These operations significantly improve ergonomics. They could theoretically be built from Tier 1, but are common enough to warrant direct support. This tier includes `pipe` for composition and `do` for complex binding, as well as async support, richer error handling, and pattern matching.

### async

Wraps an asynchronous callback-based operation.

**Signature:**
```
async : ((Effect<A, E, never> -> Unit) -> Unit) -> Effect<A, E, never>
```

**Pattern-lisp:**
```lisp
(effect/async 
  (fn [resume]
    (host/set-timeout 
      (fn [] (resume (effect/succeed "done")))
      1000)))

(effect/async
  (fn [resume]
    (host/fetch url
      (fn [response] (resume (effect/succeed response)))
      (fn [error] (resume (effect/fail (HttpError error)))))))
```

**Effect-TS equivalent:**
```typescript
Micro.async((resume) => {
  setTimeout(() => resume(Micro.succeed("done")), 1000)
})

Micro.async((resume) => {
  fetch(url)
    .then((response) => resume(Micro.succeed(response)))
    .catch((error) => resume(Micro.fail(new HttpError(error))))
})
```

**Host implementation notes:**
- The callback `resume` should be called exactly once
- Host must handle the case where `resume` is never called (potential leak)
- May need to support cancellation via an abort signal

---

### try

Wraps a synchronous operation that may throw, converting exceptions to typed errors.

**Signature:**
```
try : { try: () -> A, catch: (Unknown -> E) } -> Effect<A, E, never>
```

**Pattern-lisp:**
```lisp
(effect/try
  {try:   (fn [] (host/parse-json str))
   catch: (fn [e] (ParseError (host/error-message e)))})

(effect/try
  {try:   (fn [] (host/read-file-sync path))
   catch: (fn [e] (IOError {path: path cause: e}))})
```

**Effect-TS equivalent:**
```typescript
Micro.try({
  try: () => JSON.parse(str),
  catch: (e) => new ParseError(String(e))
})

Micro.try({
  try: () => fs.readFileSync(path, "utf-8"),
  catch: (e) => new IOError({ path, cause: e })
})
```

**Host implementation notes:**
- Catches any exception from `try` and passes it to `catch`
- The `catch` function maps the raw exception to a typed error
- Result is a `Fail`, not a `Die`

---

### die

Creates an effect that fails with an unrecoverable defect.

**Signature:**
```
die : Unknown -> Effect<never, never, never>
```

**Pattern-lisp:**
```lisp
(effect/die "Impossible state reached")
(effect/die (AssertionError "invariant violated"))
```

**Effect-TS equivalent:**
```typescript
Micro.die("Impossible state reached")
Micro.die(new Error("invariant violated"))
```

**Host implementation notes:**
- Creates a `Die` variant of `Cause`
- Not tracked in the error type (hence `E = never`)
- Cannot be caught by `catchAll`—only by `catchAllDefect`
- Use for bugs and programming errors, not business logic errors

---

### pipe

Threads a value through a sequence of functions, enabling left-to-right composition.

**Signature:**
```
pipe : A -> (A -> B) -> (B -> C) -> ... -> Z
```

**Pattern-lisp:**
```lisp
;; Basic threading
(pipe 5
  (fn [x] (* x 2))
  (fn [x] (+ x 1)))
;; => 11

;; Effect composition (Kleisli arrows)
(pipe
  (effect/service "FileSystem")
  (effect/flatMap (fn [fs] (fs.read "config.json")))
  (effect/map parse-json)
  (effect/catchAll (fn [_] (effect/succeed default-config))))
;; => Effect<Config, never, FileSystem>

;; Complex pipeline with error handling
(def load-user-prefs
  (pipe
    (effect/service "Database")
    (effect/flatMap (fn [db] (db.get-user user-id)))
    (effect/flatMap (fn [user] 
      (pipe
        (effect/service "Cache")
        (effect/flatMap (fn [cache] (cache.get (.-prefs-key user)))))))
    (effect/catchTag "NotFoundError" (fn [_] (effect/succeed default-prefs)))
    (effect/map validate-prefs)))
```

**Effect-TS equivalent:**
```typescript
// Effect-TS uses .pipe() method or pipe() function
import { pipe } from "effect"

pipe(
  FileSystem,
  Effect.flatMap((fs) => fs.read("config.json")),
  Effect.map(parseJson),
  Effect.catchAll(() => Effect.succeed(defaultConfig))
)

// Or using the fluent .pipe() method
FileSystem.pipe(
  Effect.flatMap((fs) => fs.read("config.json")),
  Effect.map(parseJson),
  Effect.catchAll(() => Effect.succeed(defaultConfig))
)
```

**Host implementation notes:**
- `pipe` is a pure function, not effect-specific
- Implemented as simple function application: `pipe(x, f, g, h)` = `h(g(f(x)))`
- All effect combinators should be curried to work with pipe
- This is the primary composition mechanism—all examples should use it

---

### do (do-notation)

Syntactic sugar for sequential effect binding when intermediate values are needed across multiple steps.

**Signature:**
```
do : [Bindings...] -> Body -> Effect<A, E, R>
```

**Pattern-lisp:**
```lisp
;; When you need to reference multiple intermediate values
(effect/do
  [fs      <- (effect/service "FileSystem")
   content <- (fs.read "config.json")
   config  <- (effect/try {try: (fn [] (parse-json content)) catch: ParseError})
   valid   <- (validate config fs)]  ;; needs both config AND fs
  (effect/succeed valid))

;; Desugars to nested flatMap:
(effect/flatMap (effect/service "FileSystem") (fn [fs]
  (effect/flatMap (fs.read "config.json") (fn [content]
    (effect/flatMap (effect/try {...}) (fn [config]
      (effect/flatMap (validate config fs) (fn [valid]
        (effect/succeed valid)))))))))
```

**Effect-TS equivalent:**
```typescript
// Effect-TS uses generator syntax for this, but the semantics are the same
Effect.gen(function* () {
  const fs = yield* FileSystem
  const content = yield* fs.read("config.json")
  const config = yield* Effect.try({
    try: () => JSON.parse(content),
    catch: (e) => new ParseError(e)
  })
  const valid = yield* validate(config, fs)
  return valid
})
```

**Host implementation notes:**
- This is a macro that expands to nested `flatMap` calls
- The `<-` arrow indicates monadic bind
- Final expression is the return value (often wrapped in `succeed`)
- Use sparingly—prefer `pipe` when intermediate values aren't needed across steps
- Each binding introduces a new scope where previous bindings are visible

---

### When to Use Pipe vs Do

**Use `pipe` when:**
- Each step only needs the result of the previous step
- You're building a linear transformation pipeline
- You want to emphasize the compositional structure

```lisp
;; Good use of pipe: linear chain
(pipe
  (fetch-user id)
  (effect/map get-email)
  (effect/flatMap send-welcome-email)
  (effect/catchAll log-and-continue))
```

**Use `do` when:**
- Later steps need values from multiple earlier steps
- You need to combine results from different effects
- The control flow would require deeply nested lambdas with pipe

```lisp
;; Good use of do: multiple values needed together
(effect/do
  [user    <- (fetch-user id)
   prefs   <- (fetch-preferences (.-id user))
   template <- (load-template (.-theme prefs))]
  ;; Final step needs user, prefs, AND template
  (render-dashboard user prefs template))
```

**Anti-pattern: using `do` for linear chains**
```lisp
;; Unnecessary do—should be pipe
(effect/do
  [a <- (step-1)]
  [b <- (step-2 a)]   ;; only needs a
  [c <- (step-3 b)]   ;; only needs b
  (effect/succeed c))

;; Better as pipe
(pipe (step-1) (effect/flatMap step-2) (effect/flatMap step-3))
```

---

### either

Converts an effect's error into a success value using `Either`.

**Signature:**
```
either : Effect<A, E, R> -> Effect<Either<E, A>, never, R>
```

**Pattern-lisp:**
```lisp
(effect/either (fetch-user id))
;; => Effect<Either<NotFoundError, User>, never, Database>

;; Handle both cases with match
(pipe
  (fetch-user id)
  effect/either
  (effect/map (fn [result]
    (match result
      (Left error)  (str "Failed: " error)
      (Right value) (str "Success: " value)))))
```

**Effect-TS equivalent:**
```typescript
Micro.either(fetchUser(id))
// => Micro<Either<NotFoundError, User>, never, Database>

pipe(
  fetchUser(id),
  Micro.either,
  Micro.map((result) =>
    Either.match(result, {
      onLeft: (error) => `Failed: ${error}`,
      onRight: (value) => `Success: ${value}`
    })
  )
)
```

**Host implementation notes:**
- Success becomes `Right(value)`
- Failure becomes `Left(error)`
- Error type becomes `never` since error is now in the success channel
- Useful for handling errors as values within `pipe` or `do` blocks

---

### catchTag

Catches a specific error type by its tag, leaving others unhandled.

**Signature:**
```
catchTag : Effect<A, E, R> -> Tag -> (E' -> Effect<B, E2, R2>) -> Effect<A | B, Exclude<E, E'> | E2, R | R2>
```

Where `E'` is the subset of `E` matching the tag.

**Pattern-lisp:**
```lisp
(effect/catchTag
  program
  "NotFoundError"
  (fn [e] (effect/succeed default-value)))
;; Only NotFoundError is handled; other errors propagate

(effect/catchTag
  (effect/catchTag
    program
    "NotFoundError"
    handle-not-found)
  "ValidationError"
  handle-validation)
```

**Effect-TS equivalent:**
```typescript
program.pipe(
  Micro.catchTag("NotFoundError", (e) => Micro.succeed(defaultValue))
)

program.pipe(
  Micro.catchTag("NotFoundError", handleNotFound),
  Micro.catchTag("ValidationError", handleValidation)
)
```

**Host implementation notes:**
- Requires errors to have a `_tag` field (or equivalent discriminator)
- Only catches errors where `error._tag === tag`
- Other error types pass through unchanged
- Enables precise, type-safe error handling

---

### orElse

Tries a fallback effect if the first effect fails.

**Signature:**
```
orElse : Effect<A, E1, R1> -> Effect<B, E2, R2> -> Effect<A | B, E2, R1 | R2>
```

**Pattern-lisp:**
```lisp
(effect/orElse
  (fetch-from-cache key)
  (fetch-from-database key))

(effect/orElse
  (effect/orElse
    (fetch-from-local-cache key)
    (fetch-from-remote-cache key))
  (fetch-from-origin key))
```

**Effect-TS equivalent:**
```typescript
fetchFromCache(key).pipe(
  Micro.orElse(() => fetchFromDatabase(key))
)

fetchFromLocalCache(key).pipe(
  Micro.orElse(() => fetchFromRemoteCache(key)),
  Micro.orElse(() => fetchFromOrigin(key))
)
```

**Host implementation notes:**
- If first effect succeeds, its value is returned
- If first effect fails, the second effect is tried
- First effect's error is discarded (not propagated)
- Final error type is from the last fallback

---

### orDie

Converts expected errors into defects (removes from type).

**Signature:**
```
orDie : Effect<A, E, R> -> Effect<A, never, R>
```

**Pattern-lisp:**
```lisp
(effect/orDie (parse-config "config.json"))
;; Errors become defects—not tracked in type

;; Use when you're certain operation won't fail
;; or when failure is truly unrecoverable
(effect/orDie (effect/service "Logger"))
```

**Effect-TS equivalent:**
```typescript
parseConfig("config.json").pipe(Micro.orDie)
```

**Host implementation notes:**
- Catches any `Fail` and re-raises as `Die`
- Error type becomes `never`
- Use sparingly—you lose type-level error tracking
- Appropriate when failure indicates a bug, not a business case

---

### match

Pattern matches on the outcome of an effect.

**Signature:**
```
match : Effect<A, E, R> -> { onFailure: E -> B, onSuccess: A -> B } -> Effect<B, never, R>
```

**Pattern-lisp:**
```lisp
(effect/match
  (fetch-user id)
  {on-failure: (fn [e] {error: (.-message e)})
   on-success: (fn [user] {user: user})})
;; => Effect<Response, never, Database>
```

**Effect-TS equivalent:**
```typescript
fetchUser(id).pipe(
  Micro.match({
    onFailure: (e) => ({ error: e.message }),
    onSuccess: (user) => ({ user })
  })
)
```

**Host implementation notes:**
- Both branches must return the same type
- Error type becomes `never` (both cases handled)
- Only handles `Fail`, not `Die` or `Interrupt`

---

### matchCause

Pattern matches on the full cause, including defects and interruption.

**Signature:**
```
matchCause : Effect<A, E, R> -> { onFailure: Cause<E> -> B, onSuccess: A -> B } -> Effect<B, never, R>
```

**Pattern-lisp:**
```lisp
(effect/matchCause
  program
  {on-failure: (fn [cause]
                 (match cause
                   (Fail e)    {type: "error" error: e}
                   (Die d)     {type: "defect" defect: d}
                   (Interrupt) {type: "interrupted"}))
   on-success: (fn [v] {type: "success" value: v})})
```

**Effect-TS equivalent:**
```typescript
program.pipe(
  Micro.matchCause({
    onFailure: (cause) => {
      if (cause._tag === "Fail") return { type: "error", error: cause.error }
      if (cause._tag === "Die") return { type: "defect", defect: cause.defect }
      return { type: "interrupted" }
    },
    onSuccess: (v) => ({ type: "success", value: v })
  })
)
```

**Host implementation notes:**
- Provides access to full `Cause` structure
- Can distinguish between expected errors, defects, and interruption
- Use for top-level error reporting or comprehensive error handling

---

## Tier 3+: Host-Managed Operations

These operations are **declared** in pattern-lisp but **executed** entirely by the host. Pattern-lisp expresses intent; the host provides implementation.

### with-timeout

Bounds the execution time of an effect.

**Pattern-lisp:**
```lisp
(effect/with-timeout 5000 slow-operation)
;; => Effect<Option<A>, E, R>
;; Returns None if timeout, Some(value) if completed

(effect/with-timeout-fail 5000 
  (TimeoutError "Operation timed out")
  slow-operation)
;; => Effect<A, E | TimeoutError, R>
```

**Effect-TS equivalent:**
```typescript
slowOperation.pipe(Micro.timeout(5000))
// => Micro<Option<A>, E, R>

slowOperation.pipe(
  Micro.timeoutFail({
    duration: 5000,
    onTimeout: () => new TimeoutError("Operation timed out")
  })
)
```

**Host implementation notes:**
- Host manages timer/clock
- Must handle cleanup if operation is interrupted
- May need integration with host's async runtime

---

### with-delay

Delays execution of an effect.

**Pattern-lisp:**
```lisp
(effect/with-delay 1000 effect)
;; Waits 1000ms before running effect
```

**Effect-TS equivalent:**
```typescript
Micro.sleep(1000).pipe(Micro.andThen(effect))
// or
effect.pipe(Micro.delay(1000))
```

**Host implementation notes:**
- Host provides timer implementation
- Should be cancellable if fiber is interrupted

---

### with-retry

Retries a failing effect according to a policy.

**Pattern-lisp:**
```lisp
(effect/with-retry {times: 3} flaky-operation)

(effect/with-retry 
  {times: 5
   delay: 1000
   exponential: true}
  flaky-operation)

(effect/with-retry
  {while: (fn [error attempt] (< attempt 3))}
  flaky-operation)
```

**Effect-TS equivalent:**
```typescript
flakyOperation.pipe(Micro.retry({ times: 3 }))

flakyOperation.pipe(
  Micro.retry({
    schedule: Schedule.exponential(1000).pipe(
      Schedule.compose(Schedule.recurs(5))
    )
  })
)
```

**Host implementation notes:**
- Host implements scheduling logic
- Retry policies can be complex (exponential backoff, jitter, etc.)
- Should respect interruption between retries

---

### with-resource

Acquires a resource, uses it, and guarantees cleanup.

**Pattern-lisp:**
```lisp
(effect/with-resource
  {acquire: (open-file "data.txt")
   release: (fn [handle] (close-file handle))
   use:     (fn [handle] (read-all handle))})
;; Release is called whether use succeeds or fails
```

**Effect-TS equivalent:**
```typescript
Micro.acquireUseRelease(
  openFile("data.txt"),
  (handle) => readAll(handle),
  (handle) => closeFile(handle)
)
```

**Host implementation notes:**
- Release **must** be called, even on error or interruption
- Host manages the scope/lifetime of the resource
- May need to track multiple resources for nested acquisitions

---

### with-concurrency

Runs multiple effects with controlled concurrency.

**Pattern-lisp:**
```lisp
(effect/with-concurrency
  {mode: parallel:}
  [effect1 effect2 effect3])
;; Run all in parallel, collect results

(effect/with-concurrency
  {mode: parallel:
   max: 2}
  effects)
;; Run with max 2 concurrent

(effect/with-concurrency
  {mode: sequential:}
  effects)
;; Run one at a time
```

**Effect-TS equivalent:**
```typescript
Micro.all([effect1, effect2, effect3], { concurrency: "unbounded" })

Micro.all(effects, { concurrency: 2 })

Micro.all(effects, { concurrency: 1 }) // sequential
```

**Host implementation notes:**
- Host manages fiber/task spawning
- Must handle partial failures (some succeed, some fail)
- Should support interruption of in-flight effects

---

### race

Returns the result of the first effect to complete.

**Pattern-lisp:**
```lisp
(effect/race [fetch-from-server-a fetch-from-server-b])
;; First to complete wins; others are interrupted
```

**Effect-TS equivalent:**
```typescript
Micro.race(fetchFromServerA, fetchFromServerB)
```

**Host implementation notes:**
- Must interrupt losing effects
- Winner's result (success or failure) is returned
- Handle cleanup for interrupted effects

---

## Service Architecture

### How Services Work

1. **Pattern-lisp requests** a service by tag (string identifier)
2. **Host provides** implementations when running the effect
3. **Host resolves** service requests during interpretation

```
┌──────────────────────────────────────────────────────────────┐
│  Pattern-Lisp Program                                        │
│                                                              │
│  (effect/do                                                   │
│    [fs  <- (effect/service "FileSystem")                      │
│     log <- (effect/service "Logger")                          │
│     data <- (fs.read "input.txt")]                           │
│    (pipe                                                     │
│      (log.info (str "Read " (count data) " bytes"))          │
│      (effect/flatMap (fn [_] (process data)))))               │
│                                                              │
│  Type: Effect<Result, IOError, FileSystem | Logger>           │
└─────────────────────────┬────────────────────────────────────┘
                          │
                          │ Effect description with requirements
                          ▼
┌──────────────────────────────────────────────────────────────┐
│  Host Runtime                                                │
│                                                              │
│  services = {                                                │
│    "FileSystem": { read: ..., write: ..., exists: ... },     │
│    "Logger": { info: ..., warn: ..., error: ... }            │
│  }                                                           │
│                                                              │
│  result = run(program, services)                             │
└──────────────────────────────────────────────────────────────┘
```

### Service Shape

Services are opaque records with effectful methods. Pattern-lisp knows the method names but not their implementation:

```lisp
;; Pattern-lisp sees services as opaque values with methods
;; The host defines what methods exist and their implementations

;; Using pipe for simple chains
(pipe
  (effect/service "FileSystem")
  (effect/flatMap (fn [fs] (fs.read "config.txt")))
  (effect/flatMap (fn [content] (fs.write "backup.txt" content))))

;; Using do-notation when you need multiple services
(effect/do
  [fs <- (effect/service "FileSystem")]
  (pipe
    (fs.read "config.txt")
    (effect/flatMap (fn [_] (fs.write "output.txt" data)))
    (effect/flatMap (fn [_] (fs.exists "backup.txt")))))
```

### Standard Services

Hosts should provide these common services:

| Service | Methods |
|---------|---------|
| `FileSystem` | `read`, `write`, `exists`, `delete`, `mkdir`, `readdir` |
| `Logger` | `debug`, `info`, `warn`, `error` |
| `Console` | `print`, `println`, `readLine` |
| `Http` | `get`, `post`, `put`, `delete`, `request` |
| `Clock` | `now`, `sleep` |
| `Random` | `next`, `nextInt`, `nextRange` |
| `Env` | `get`, `getOrElse`, `set` |

### Testing with Mock Services

The service architecture enables testing by providing mock implementations:

```typescript
// Effect-TS example of testing with mock services

// Production implementation
const RealFileSystem = {
  read: (path) => Micro.tryPromise(() => fs.readFile(path, "utf-8")),
  write: (path, data) => Micro.tryPromise(() => fs.writeFile(path, data)),
  exists: (path) => Micro.sync(() => fs.existsSync(path))
}

// Test implementation
const MockFileSystem = (files: Map<string, string>) => ({
  read: (path) => {
    const content = files.get(path)
    return content 
      ? Micro.succeed(content)
      : Micro.fail(new IOError(`File not found: ${path}`))
  },
  write: (path, data) => Micro.sync(() => files.set(path, data)),
  exists: (path) => Micro.succeed(files.has(path))
})

// Test
test("config loading", async () => {
  const mockFs = MockFileSystem(new Map([
    ["config.json", '{"debug": true}']
  ]))
  
  const result = await Micro.runPromise(
    program.pipe(Micro.provideService(FileSystem, mockFs))
  )
  
  expect(result.debug).toBe(true)
})
```

---

## Host Implementation Guide

### Effect Representation

The host must represent effects as a data structure that can be inspected and interpreted. A typical approach:

```typescript
// Algebraic data type for effect constructors
type Effect<A, E, R> =
  | { _tag: "Succeed"; value: A }
  | { _tag: "Fail"; error: E }
  | { _tag: "Sync"; thunk: () => A }
  | { _tag: "Async"; register: (cb: (effect: Micro<A, E, never>) => void) => void }
  | { _tag: "FlatMap"; effect: Micro<any, any, any>; f: (a: any) => Effect<A, E, R> }
  | { _tag: "CatchAll"; effect: Micro<any, any, any>; handler: (e: any) => Effect<A, E, R> }
  | { _tag: "Service"; tag: string }
  | { _tag: "Die"; defect: unknown }
  // ... other constructors
```

### Interpreter Loop

The host interprets effects by pattern matching on constructors:

```typescript
function interpret<A, E>(
  effect: Micro<A, E, never>,
  services: Record<string, any>
): Promise<Exit<A, E>> {
  switch (effect._tag) {
    case "Succeed":
      return Promise.resolve({ _tag: "Success", value: effect.value })
    
    case "Fail":
      return Promise.resolve({ 
        _tag: "Failure", 
        cause: { _tag: "Fail", error: effect.error } 
      })
    
    case "Sync":
      try {
        return Promise.resolve({ _tag: "Success", value: effect.thunk() })
      } catch (defect) {
        return Promise.resolve({ 
          _tag: "Failure", 
          cause: { _tag: "Die", defect } 
        })
      }
    
    case "FlatMap":
      return interpret(effect.effect, services).then((exit) => {
        if (exit._tag === "Failure") return exit
        return interpret(effect.f(exit.value), services)
      })
    
    case "CatchAll":
      return interpret(effect.effect, services).then((exit) => {
        if (exit._tag === "Success") return exit
        if (exit.cause._tag !== "Fail") return exit // Don't catch Die/Interrupt
        return interpret(effect.handler(exit.cause.error), services)
      })
    
    case "Service":
      const service = services[effect.tag]
      if (!service) {
        return Promise.resolve({
          _tag: "Failure",
          cause: { _tag: "Die", defect: new Error(`Service not found: ${effect.tag}`) }
        })
      }
      return Promise.resolve({ _tag: "Success", value: service })
    
    // ... other cases
  }
}
```

### Stack Safety

For deeply nested effects, the interpreter must be stack-safe. Techniques include:
- Trampolining
- Continuation-passing style
- Explicit stack/heap allocation

### Async Handling

The host must integrate with its async runtime:
- Node.js: Promises, async/await
- Haskell: IO monad, async library
- Rust: Futures, tokio/async-std

### Interruption

Proper interruption support requires:
- Tracking running fibers
- Propagating cancellation signals
- Cleaning up resources on interruption
- Respecting `uninterruptible` regions

---

## Appendix A: Complete API Reference

### Tier 1 (Foundational)

| Operation | Signature | Description |
|-----------|-----------|-------------|
| `pipe` | `A -> (A->B) -> ... -> Z` | Left-to-right composition |
| `succeed` | `A -> Effect<A, never, never>` | Lift pure value |
| `fail` | `E -> Effect<never, E, never>` | Expected failure |
| `sync` | `(() -> A) -> Effect<A, never, never>` | Sync side effect |
| `map` | `Effect<A,E,R> -> (A->B) -> Effect<B,E,R>` | Transform success |
| `flatMap` | `Effect<A,E1,R1> -> (A->Effect<B,E2,R2>) -> Effect<B,E1\|E2,R1\|R2>` | Sequence effects |
| `catchAll` | `Effect<A,E,R> -> (E->Effect<B,E2,R2>) -> Effect<A\|B,E2,R\|R2>` | Handle all errors |
| `service` | `Tag -> Effect<S, never, S>` | Request service |

### Tier 2 (Essential)

| Operation | Signature | Description |
|-----------|-----------|-------------|
| `pipe` | `A -> (A->B) -> (B->C) -> ... -> Z` | Left-to-right composition |
| `do` | `[Bindings] -> Body -> Effect<A,E,R>` | Do-notation for binding |
| `async` | `((Effect<A,E,never>->Unit)->Unit) -> Effect<A,E,never>` | Async operation |
| `try` | `{try:()->A, catch:Err->E} -> Effect<A,E,never>` | Catch exceptions |
| `die` | `Unknown -> Effect<never,never,never>` | Unrecoverable defect |
| `either` | `Effect<A,E,R> -> Effect<Either<E,A>,never,R>` | Error as value |
| `catchTag` | `Effect<A,E,R> -> Tag -> Handler -> Effect<...>` | Catch by tag |
| `orElse` | `Effect<A,E1,R1> -> Effect<B,E2,R2> -> Effect<A\|B,E2,R1\|R2>` | Fallback |
| `orDie` | `Effect<A,E,R> -> Effect<A,never,R>` | Error to defect |
| `match` | `Effect<A,E,R> -> Handlers -> Effect<B,never,R>` | Match outcome |
| `matchCause` | `Effect<A,E,R> -> Handlers -> Effect<B,never,R>` | Match full cause |

### Tier 3+ (Host-Managed)

| Operation | Description |
|-----------|-------------|
| `with-timeout` | Bound execution time |
| `with-delay` | Delay execution |
| `with-retry` | Retry with policy |
| `with-resource` | Bracket pattern |
| `with-concurrency` | Parallel/sequential execution |
| `race` | First to complete |

---

## Appendix B: Error Handling Decision Tree

```
Is this an expected business error?
│
├─ YES: Use `fail` with a typed error
│       Can be caught with `catchAll` or `catchTag`
│
└─ NO: Is this a programming bug/impossible state?
       │
       ├─ YES: Use `die` with a defect
       │       Only caught by `catchAllDefect`
       │
       └─ NO: Is this from external code that throws?
              │
              ├─ YES: Use `try` to convert to typed error
              │
              └─ NO: Use `sync` (exceptions become defects)
```

---

## Appendix C: Migration from Promises

For hosts coming from Promise-based code:

| Promise | Effect |
|---------|-------|
| `Promise.resolve(x)` | `effect/succeed x` |
| `Promise.reject(e)` | `effect/fail e` |
| `p.then(f)` | `(pipe p (effect/map f))` |
| `p.then(f)` (f returns Promise) | `(pipe p (effect/flatMap f))` |
| `p.catch(handler)` | `(pipe p (effect/catchAll handler))` |
| `Promise.all([...])` | `effect/with-concurrency {mode: parallel:} [...]` |
| `Promise.race([...])` | `effect/race [...]` |
| `new Promise((resolve, reject) => ...)` | `effect/async (fn [resume] ...)` |
| `async/await` | `effect/do` or nested `pipe` |

Key differences:
- Effects are lazy (don't run until interpreted)
- Errors are typed and tracked
- Services enable dependency injection
- Resources are safely managed
- Composition is explicit via `pipe` (or `do` for complex binding)

---

## Version History

- **v0.1** - Initial design document
