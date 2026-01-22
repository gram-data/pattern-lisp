# Feature Specification: Plisp–Gram Convert CLI Option

**Feature Branch**: `008-plisp-gram-convert`  
**Created**: 2025-01-30  
**Status**: Draft  
**Input**: User description: "Add an option to the main pattern-lisp CLI application to convert a given plisp file to/from gram format. Note that plisp to gram should always work, while not all gram files will contain pattern-lisp expressions, though all patterns should be valid pattern-lisp data. Consider a filename convention like helloworld.plisp.gram for gram files expected to contain pattern-lisp expressions."

## Clarifications

### Session 2025-01-30

- **Q:** Should gram that does not contain a pattern-lisp program fail, or should we parse gram into data and explore whether any of the data can be evaluated?  
  **A:** Do not assume that gram which lacks pattern-lisp program structure fails. All gram files contain Pattern Subject data that should be valid pattern-lisp data. Explicit design exploration for planning: parse gram into that data, then determine if any of the data can be evaluated. The resulting gram→plisp behavior (output, partial output, or error) is left as an open question.
- **Q:** How can a pattern-lisp program efficiently combine "raw data" with evaluatable expressions that operate on that data? Does the data need to be turned into variable declarations? Could the data be combined into a single `global` variable?  
  **A:** Left as an open question for the technical plan. See Research (Lisp-family conventions for global data).

## Research (for open design explorations)

### Lisp-family conventions for global data

Web research on Clojure (preferred), Racket/Scheme, and Common Lisp suggests the following patterns relevant to combining raw data with evaluatable expressions.

**Clojure**

- **Global data:** `def` creates a Var with a root binding; `defonce` for one-time init. A single `(def config (edn/read-string (slurp "config.edn")))` holding an immutable map is idiomatic; one structure per global is common rather than one `def` per datum.
- **Data + code:** EDN (or data) is read into a structure; then either (a) **data-driven dispatch**—code interprets the structure (e.g. `{:type :circle :radius 5}` and `case`/multimethods), no `eval`—or (b) **evaluate forms from data**—store s-exprs in EDN, `eval` with `binding [*ns* ...]` for symbol resolution; `eval` on untrusted input is discouraged.
- **Single global for data:** Common pattern: one `def`/`defonce` holding a map or atom-wrapped map; code references it. Data does not need to be "one variable per field"; one binding for a structured value is standard.

**Racket / Scheme**

- **Global data and code:** Top-level `define` binds both data and functions. Example: `(define *config* (list 'host "localhost" 'port 8080))` and `(define (get-port) (cdr (assoc 'port *config*)))`. Data and code are mixed via `define`; functions reference global data.
- **One structure vs many defs:** A single global structure (e.g. `*config*`) referenced by functions is typical; one does not need one `define` per config key. Racket’s `define-global` (from the `global` package) supports cross-module globals.

**Common Lisp**

- **Global data:** `defparameter` / `defvar` for globals. Literal data can be embedded in source; `#.` read-time evaluation can load external data into a literal. One `*my-data*` or `*data-json*` holding a string or structure is typical.
- **Data in one binding:** One global holding the whole structure is standard; the data does not need to be split into many variable declarations.

**Summary for pattern-lisp**

- Data does **not** need to be turned into one variable declaration per field; a **single global** (or a small set of globals) holding a structured value is idiomatic in Clojure, Racket, and Common Lisp.
- **Data-driven** patterns: keep data as data, write interpreters/dispatch in code—avoids `eval` and can be safer. **Eval-based** patterns: store forms in data and evaluate with proper namespace/context—more flexible but needs care for safety and symbol resolution.
- The technical plan should explore whether a single `global` (or analogous) binding for raw Pattern Subject data is efficient and idiomatic for pattern-lisp when converting gram (with mixed data and expressions) to plisp, and how evaluatable expressions should reference that data.

## User Scenarios & Testing *(mandatory)*

### User Story 1 - Convert plisp file to gram (Priority: P1)

A user has a `.plisp` file and wants to produce an equivalent gram file (e.g. for tooling, storage, or interchange). They run the pattern-lisp CLI with a convert option, pass the plisp file path, and optionally an output path. The system writes gram that represents the same pattern-lisp program. This conversion must always succeed for valid plisp input.

**Why this priority**: Core value of the feature; enables gram as an interchange format from plisp.

**Independent Test**: Run conversion on a valid plisp file and confirm the output is well-formed gram that deserializes to the same logical program.

**Acceptance Scenarios**:

1. **Given** a valid `.plisp` file, **When** the user runs the convert-to-gram command with that file as input, **Then** the system writes gram to the specified output (or a default derived from the input path) and exits successfully.
2. **Given** a valid `.plisp` file and an output path ending in `.plisp.gram`, **When** the user runs the convert-to-gram command, **Then** the system writes gram to that path and the output is recognizable as pattern-lisp-origin gram (e.g. for round-trip or tooling that expects the `.plisp.gram` convention).
3. **Given** an invalid or syntactically bad plisp file, **When** the user runs convert-to-gram, **Then** the system reports an error and does not produce output.

---

### User Story 2 - Convert gram file to plisp when it contains pattern-lisp (Priority: P2)

A user has a gram file that represents a pattern-lisp program (e.g. produced by plisp→gram or conforming to the pattern-lisp gram schema). They run the CLI with a convert option and the gram file path. The system produces an equivalent `.plisp` file. Conversion from gram to plisp succeeds when the gram contains a valid pattern-lisp program. When the gram does not have that structure, the system parses it into Pattern Subject data; whether it then outputs plisp, partial plisp, or an error is an open design exploration (see Clarifications).

**Why this priority**: Enables round-trip and editing of pattern-lisp via gram; depends on P1 for the “pattern-lisp gram” format to be well-defined.

**Independent Test**: Convert a `.plisp.gram` (or gram known to be pattern-lisp) to plisp, then convert that plisp back to gram; the second gram should match the first in meaning.

**Acceptance Scenarios**:

1. **Given** a gram file that contains a valid pattern-lisp program (e.g. with `{ kind: "Pattern Lisp" }` and expression patterns), **When** the user runs the convert-to-plisp command, **Then** the system writes an equivalent `.plisp` file and exits successfully.
2. **Given** a gram file that does not represent a pattern-lisp program (e.g. lacks `{ kind: "Pattern Lisp" }` or expression sequence), **When** the user runs convert-to-plisp, **Then** the system parses the gram into Pattern Subject data; whether it outputs plisp, partial plisp, or an error is an **open design exploration** (parse into data, then determine if any of the data can be evaluated—see Clarifications). The system MUST NOT emit plisp that misrepresents the input.
3. **Given** a gram file with the `.plisp.gram` naming convention, **When** the user runs convert-to-plisp without specifying output, **Then** the system infers an output path (e.g. `foo.plisp.gram` → `foo.plisp`) where reasonable.

---

### User Story 3 - Filename convention for pattern-lisp gram (Priority: P3)

Users and tools can distinguish “gram that is pattern-lisp” from “generic gram” via a filename convention. The system supports producing and recognizing `.plisp.gram` for gram files that represent pattern-lisp programs.

**Why this priority**: Improves interoperability and makes intent explicit; builds on P1 and P2.

**Independent Test**: Convert plisp → gram with output `example.plisp.gram`, then use that file as input for gram → plisp; both directions work and the convention is respected.

**Acceptance Scenarios**:

1. **Given** a plisp→gram conversion, **When** the user requests or accepts the `.plisp.gram` suffix for the output file, **Then** the system writes to that path and the resulting file is valid for gram→plisp when it contains pattern-lisp.
2. **Given** an input path ending in `.plisp.gram`, **When** the user runs gram→plisp, **Then** the system treats it as a candidate pattern-lisp gram file (same validation as for any gram, but the convention is documented and supported).

---

### Edge Cases

- **Invalid plisp (parse or eval error)**: Convert to gram MUST fail with a clear error; no partial or corrupt output.
- **Valid plisp, evaluation error (e.g. undefined variable at top level)**: Plisp-to-gram conversion MUST fail with a clear error (plisp→gram operates on evaluated program values; see Assumptions).
- **Gram that is well-formed but does not have pattern-lisp program structure**: All gram contains Pattern Subject data that should be valid pattern-lisp data. The system parses gram into that data. **Open design exploration (for planning):** then determine if any of the data can be evaluated; the resulting behavior (output plisp, partial plisp, or error) is TBD. The system MUST NOT emit plisp that misrepresents the input.
- **Missing input file**: Report a clear “file not found” (or equivalent) error; no output.
- **Output file exists**: The output file is overwritten by default (see Assumptions).
- **Empty plisp file**: Treated as valid (empty program); gram output should be valid, minimal pattern-lisp gram (e.g. metadata only, no expressions).
- **Empty or malformed gram file**: Convert to plisp MUST fail with a clear parse or structure error.

## Requirements *(mandatory)*

### Functional Requirements

- **FR-001**: The pattern-lisp CLI MUST provide an option or subcommand to convert a single given plisp file to gram format.
- **FR-002**: Plisp-to-gram conversion MUST always succeed for any valid, well-formed plisp file (parse and evaluation succeed).
- **FR-003**: The CLI MUST support specifying an output path for plisp→gram; if unspecified, the system MUST use a default derived from the input path (e.g. replace or append `.plisp` to get a `.gram` or `.plisp.gram` path).
- **FR-004**: The pattern-lisp CLI MUST provide an option or subcommand to convert a single given gram file to plisp format when that gram contains a valid pattern-lisp program.
- **FR-005**: When the gram does not contain a valid pattern-lisp program (e.g. missing `kind: "Pattern Lisp"` or expression sequence), the system SHALL parse the gram into Pattern Subject data (all gram is expected to be valid pattern-lisp data at the pattern level). Whether the system then outputs plisp, partial plisp, or an error is an **open design exploration** (parse into data, then determine if any of the data can be evaluated—see Clarifications). The system MUST NOT emit plisp that misrepresents the input.
- **FR-006**: The system MUST support the `.plisp.gram` filename convention: when producing gram from plisp, the user MAY request an output path ending in `.plisp.gram`; when converting gram to plisp, an input path ending in `.plisp.gram` is accepted and treated like any other gram file (subject to pattern-lisp validation).
- **FR-007**: Gram files contain Pattern Subject data that should be valid pattern-lisp data. For both directions, the system MUST parse/validate that patterns in gram are well-formed and representable in the pattern-lisp data model; malformed or unrepresentable gram MUST cause a clear error.
- **FR-008**: The system MUST report clear, actionable errors for: missing input file, unreadable input, invalid plisp syntax, plisp evaluation errors (if conversion requires evaluation), gram parse or structure errors, and write failures for the output file. For gram that parses successfully but lacks pattern-lisp program structure, whether and how to report an error is part of the open design exploration (see Clarifications).
- **FR-009**: Converting an empty but well-formed plisp file to gram MUST produce valid gram (minimal pattern-lisp structure); converting empty or structurally invalid gram to plisp MUST fail.

### Key Entities

- **Plisp file**: A text file containing pattern-lisp s-expressions; extension `.plisp`. Input or output of conversion.
- **Gram file**: A text file in gram notation representing patterns; extensions `.gram` or `.plisp.gram`. All gram files contain **Pattern Subject** data that should be valid pattern-lisp data. The `.plisp.gram` suffix conventionally indicates pattern-lisp–origin or pattern-lisp–intended content.
- **Pattern-lisp program**: A gram structure with file-level metadata (e.g. `kind: "Pattern Lisp"`) and a sequence of expression patterns that correspond to pattern-lisp values (numbers, strings, closures, etc.). When present, gram→plisp can produce equivalent plisp; when absent, behavior is an open design exploration (parse into Pattern Subject data, then determine if any can be evaluated).
- **Pattern (gram) / Pattern Subject**: A single gram pattern; the in-memory form is Pattern Subject. All patterns in a gram file are expected to be valid as pattern-lisp data (representable in the pattern-lisp model).

### Assumptions and Dependencies

- **One file per invocation**: Conversion operates on one input file per run; batch or directory recursion is out of scope unless explicitly added later.
- **Plisp→gram uses evaluated program**: Plisp-to-gram encodes the result of loading and evaluating the plisp file; if evaluation fails, conversion fails.
- **Overwrite by default**: If the output file exists, it is overwritten; no explicit “overwrite” flag in initial scope.
- **Dependency on existing serialization**: The feature depends on existing pattern-lisp↔gram serialization; the CLI adds the conversion entry points and file I/O.

### Open design explorations (for technical plan)

- **Gram→plisp when gram lacks pattern-lisp program structure:** Parse gram into Pattern Subject data (all gram is expected to be valid pattern-lisp data at the pattern level), then determine if any of the data can be evaluated. The resulting behavior (output plisp, partial plisp, or error) and any new acceptance/error cases are left as an open question for the technical plan to explore and decide.
- **Combining raw data with evaluatable expressions:** How can a pattern-lisp program efficiently combine "raw data" (e.g. from gram) with evaluatable expressions that operate on that data? Does the data need to be turned into variable declarations (e.g. one `define` per piece of data), or could the data be combined into a single `global` (or similar) variable that expressions reference? Left as an open question for the technical plan. See Research (Lisp-family conventions for global data) and Clarifications.

## Success Criteria *(mandatory)*

### Measurable Outcomes

- **SC-001**: Users can convert any valid plisp file to gram in a single CLI invocation, with success observable via exit code and correct output file contents.
- **SC-002**: Users can convert a gram file that contains a valid pattern-lisp program to plisp in a single CLI invocation, with success observable via exit code and correct plisp output.
- **SC-003**: Round-trip (plisp→gram→plisp) preserves program meaning: converting a plisp file to gram and then that gram back to plisp yields plisp that evaluates to the same values as the original.
- **SC-004**: All defined error cases (invalid plisp, gram parse or structure errors, missing file, write failure) produce a clear, non-empty error message and a non-zero exit code; no silent failure or corrupt output. For gram that parses but lacks pattern-lisp program structure, the expected outcome is TBD in the design exploration (see Clarifications).
- **SC-005**: The `.plisp.gram` convention is documented and supported for both output (plisp→gram) and input (gram→plisp) so that users and tools can reliably identify pattern-lisp gram files.
