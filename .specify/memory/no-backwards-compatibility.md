# No Backwards Compatibility Policy

## Core Principle

**This project has never been published. There is no backwards compatibility requirement.**

## Guidelines

1. **Never mention "backwards compatibility"** in code comments, documentation, or commit messages
2. **Never preserve old code paths** "for backwards compatibility"
3. **Never add deprecation warnings** - just remove or change code directly
4. **Never create migration paths** for old formats or APIs
5. **Feel free to break changes** - if something needs to change, change it directly

## Rationale

- This is a feature branch of an unpublished project
- No external users depend on the current API
- Maintaining backwards compatibility adds unnecessary complexity
- Clean breaks are better than maintaining legacy code paths

## When This Changes

This policy will only change when:
- The project is published/released
- External users depend on the API
- A formal versioning policy is established

Until then: **No backwards compatibility. Ever.**

**Version**: 1.0.0 | **Created**: 2025-12-22

