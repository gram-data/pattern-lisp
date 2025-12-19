<!--
Sync Impact Report:
Version change: (none) → 1.0.0
Modified principles: (none - initial creation)
Added sections: Core Principles (5 principles), Governance
Removed sections: (none - initial creation)
Templates requiring updates:
  ✅ plan-template.md - Constitution Check section aligns with principles
  ✅ spec-template.md - No constitution-specific constraints to update
  ✅ tasks-template.md - Task structure aligns with principles
Follow-up TODOs: None
-->

# Pattern Script Constitution

## Core Principles

### I. Library-First
Every feature starts as a standalone library. Libraries MUST be self-contained, independently testable, and documented. Each library requires a clear purpose—no organizational-only libraries allowed.

**Rationale**: Libraries provide reusable, testable building blocks that can be composed into larger systems. This approach ensures modularity and reduces coupling.

### II. CLI Interface
Every library exposes functionality via CLI. Text in/out protocol: stdin/args → stdout, errors → stderr. Support both JSON and human-readable formats.

**Rationale**: CLI interfaces enable easy integration, testing, and debugging. Text-based I/O ensures debuggability and allows libraries to be composed via shell pipelines.

### III. Test-First (NON-NEGOTIABLE)
TDD mandatory: Tests written → User approved → Tests fail → Then implement. Red-Green-Refactor cycle strictly enforced.

**Rationale**: Test-first development ensures requirements are clearly understood before implementation and provides immediate feedback on correctness.

### IV. Integration Testing
Focus areas requiring integration tests: New library contract tests, Contract changes, Inter-service communication, Shared schemas.

**Rationale**: Integration tests verify that components work together correctly and catch issues that unit tests alone cannot detect.

### V. Observability
Text I/O ensures debuggability. Structured logging required for all operations. Logs MUST include context sufficient to trace operations through the system.

**Rationale**: Observability is essential for debugging production issues and understanding system behavior. Text-based I/O and structured logging provide the necessary visibility.

## Governance

All PRs and reviews MUST verify compliance with this constitution. Complexity MUST be justified with clear rationale. Use this constitution as the authoritative source for development practices.

**Amendment Procedure**: Amendments require documentation of the rationale, approval from project maintainers, and a migration plan if the change affects existing code.

**Versioning Policy**: Constitution versions follow semantic versioning:
- MAJOR: Backward incompatible governance/principle removals or redefinitions
- MINOR: New principle/section added or materially expanded guidance
- PATCH: Clarifications, wording, typo fixes, non-semantic refinements

**Compliance Review**: All feature specifications and implementation plans MUST include a Constitution Check section that verifies alignment with these principles.

**Version**: 1.0.0 | **Ratified**: 2025-12-19 | **Last Amended**: 2025-12-19
