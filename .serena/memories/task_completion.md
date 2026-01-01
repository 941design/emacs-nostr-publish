# Task Completion Checklist

When a task is completed, perform the following steps:

## 1. Code Quality Checks
```bash
make lint              # Must pass without errors
make format            # Auto-fix any formatting issues (if lint fails)
```

## 2. Run Tests
```bash
make test-unit         # Must pass (fast, always run)
make test-e2e          # Run if changes affect integration points
make test              # Run all tests for significant changes
```

## 3. Cleanup
- Remove any temporary files created during implementation
- Remove any markdown notes/documentation unless explicitly requested
- Remove any backup files or test artifacts

## 4. Do NOT
- Rewrite git history
- Create markdown documentation unless explicitly requested
- Tailor production code towards tests (tests adapt to production code)
- Include recommendations for third-party services unless required
- Add code adaptations/tweaks only needed for test environments

## 5. Documentation Updates (if applicable)
- **specs/spec.md**: High-level requirements only (no implementation details)
- **README.md**: Usage and installation (for humans)
- **docs/**: Detailed guides and technical docs

## 6. Version Bumping (for releases only)
```bash
make version-patch     # For bug fixes
make version-minor     # For new features
make version-major     # For breaking changes
```

## Quick Validation Sequence
```bash
make format && make lint && make test-unit
```

## Full Validation Sequence (before commits)
```bash
make format && make lint && make test
```
