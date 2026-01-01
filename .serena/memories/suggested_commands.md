# Development Commands

## Package Management (ALWAYS use uv)
```bash
make sync-dev          # Sync venv with dev dependencies (recommended)
uv sync --extra dev    # Manual equivalent
```

## Testing
```bash
make test              # Run ALL tests (unit + integration)
make test-unit         # Run unit tests only (fast, no Docker)
make test-e2e          # Run integration tests only (requires Docker, nak, Emacs)

# Direct pytest (only runs unit tests due to pyproject.toml config)
uv run pytest          # Unit tests only
uv run pytest -v       # Verbose output
uv run pytest -k name  # Run specific test by name pattern
```

## Linting & Formatting
```bash
make lint              # Check linting and formatting (ruff)
make format            # Auto-fix linting and formatting issues
make format-md         # Format markdown tables

# Manual ruff commands
uv run ruff check src/ tests/
uv run ruff check --fix src/ tests/
uv run ruff format src/ tests/
uv run ruff format --check src/ tests/
```

## Building & Installing
```bash
make build             # Build distribution packages
make install           # Install CLI globally via uv tool
make clean             # Remove build artifacts
```

## Publishing
```bash
make publish-test      # Publish to TestPyPI
make publish           # Publish to PyPI (requires UV_PUBLISH_TOKEN)
```

## Version Management
```bash
make version-patch     # Bump patch (0.1.0 -> 0.1.1)
make version-minor     # Bump minor (0.1.0 -> 0.2.0)
make version-major     # Bump major (0.1.0 -> 1.0.0)
```

## Git Hooks
```bash
make install-hooks     # Install pre-commit and pre-push hooks
```

## Dry Run (Testing Event Construction)
```bash
make dry-run           # Test event construction without publishing
```

## System Utilities (macOS/Darwin)
- `git` - Version control
- `ls`, `cd`, `find`, `grep` - Standard file operations
- `docker compose` - Integration test infrastructure
- `nak` - Nostr CLI tool for NIP-46 signing
