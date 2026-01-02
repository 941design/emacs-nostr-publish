# nostr-publish Project Overview

## Purpose
nostr-publish is a deterministic, cross-platform publisher for Nostr long-form content (NIP-23). It publishes Markdown files with YAML frontmatter to Nostr relays using remote signing via NIP-46.

## Key Principles
- **Composition over reinvention**: Leverages existing tools (nak CLI, remote signers) rather than reimplementing cryptographic protocols
- **No key management**: Never touches private keys; signing delegated to NIP-46 remote signers
- **Emacs-first**: Designed as bridge to bring Nostr publishing to Emacs

## Tech Stack
- **Language**: Python 3.9+ (single dependency: PyYAML)
- **Package Manager**: uv (NOT pip or poetry)
- **Testing**: pytest + Hypothesis (property-based testing)
- **Linting/Formatting**: ruff
- **Build System**: setuptools (via pyproject.toml)
- **External Dependencies**: nak CLI (v0.17.4+) for NIP-46 signing

## Codebase Structure
```
src/nostr_publish/
├── __init__.py      # Package init with version
├── cli.py           # CLI entry point (main, parse_arguments)
├── models.py        # Data models (Frontmatter, UnsignedEvent, PublishResult)
├── frontmatter.py   # YAML frontmatter parsing
├── validator.py     # Frontmatter validation
├── event.py         # Nostr event construction
├── relay.py         # Relay URL handling
├── nak.py           # nak CLI subprocess wrapper
├── errors.py        # Custom exception hierarchy
└── utils.py         # Utility functions

tests/
├── unit/            # Unit tests (fast, no external dependencies)
└── integration/     # E2E tests (require Docker, nak, Emacs)
    └── fixtures/    # Test markdown files

specs/spec.md        # Complete technical specification
docs/                # Documentation (local-setup, test-setup, etc.)
```

## Key Features
- Markdown authoring with YAML frontmatter
- Remote signing via NIP-46 (bunker URIs)
- Deterministic event construction (same input = identical events)
- CLI + Emacs integration (C-c C-p)
- Strict validation with fail-fast semantics

## Entry Points
- **CLI**: `nostr-publish` command (defined in cli.py:main)
- **Emacs**: `nostr-publish.el` package

## Integration Points
- Depends on external `nak` CLI tool for NIP-46 signing
- Uses Docker Compose for integration tests (local relay + signer)
