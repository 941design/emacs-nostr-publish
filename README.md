# nostr-publish

Deterministic, cross-platform publisher for Nostr long-form content ([NIP-23](https://github.com/nostr-protocol/nips/blob/master/23.md)).

Publishes Markdown files with YAML frontmatter to Nostr relays using remote signing via [NIP-46](https://github.com/nostr-protocol/nips/blob/master/46.md).

## Rationale

1. **Composition over reinvention** - Leverages existing tools (nak, remote signers) rather than reimplementing cryptographic protocols
2. **No key management** - This tool never touches your private keys; signing is delegated entirely to NIP-46 remote signers
3. **Bridge to the best editor** - Brings Nostr publishing to Emacs, where long-form content belongs

## Features

- **Markdown authoring**: Write in Markdown with YAML frontmatter
- **Remote signing**: NIP-46 support via any compatible signer
- **Deterministic**: Same input always produces identical events
- **CLI + Emacs**: Use from command line or Emacs (`C-c C-p`)
- **Strict validation**: Fail-fast on invalid frontmatter

## Prerequisites

- [nak](https://github.com/fiatjaf/nak) CLI tool (v0.17.4+) for NIP-46 signing
- NIP-46 compatible remote signer (any signer supporting [NIP-46](https://github.com/nostr-protocol/nips/blob/master/46.md))

### NIP-46 Remote Signing

nostr-publish uses NIP-46 (Nostr Connect) for remote signing, meaning your private keys never leave your signer application. The connection is established via a "bunker URI":

```
bunker://<signer-pubkey>?relay=wss://relay.example.com&secret=<optional-secret>
```

Components:
- **signer-pubkey**: The hex public key of your signer
- **relay**: The relay both client and signer connect to for message exchange
- **secret**: Optional authentication token (required by some signers)

To use nostr-publish:

1. Obtain a bunker URI from your NIP-46 signer
2. Ensure the relay in the URI is accessible from your machine
3. Configure nostr-publish with the bunker URI (see [Configuration](#configuration))
4. Approve connection/signing requests in your signer when prompted

**Security best practices**:
- Never share your bunker URI (it contains authentication credentials)
- Use unique URIs per application
- Review signing requests before approving
- Rotate secrets periodically

For local development without a remote signer, see [Local Setup](docs/local-setup.md).

## Installation

### From PyPI

```bash
pipx install nostr-publish
```

Or with pip: `pip install nostr-publish`

### From Source

```bash
git clone https://github.com/941design/emacs-nostr-publish.git
cd emacs-nostr-publish

# Install to user space (adds nostr-publish to ~/.local/bin)
make install

# Or install globally (may require sudo)
pip install .
```

For development setup, see [Local Setup](docs/local-setup.md).

### Emacs Package

The Emacs package requires the CLI to be installed separately (see above).

#### From MELPA

```elisp
;; Ensure MELPA is in your package-archives
(require 'package)
(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/") t)
(package-initialize)

;; Install
M-x package-refresh-contents
M-x package-install RET nostr-publish RET
```

#### Configuration

Basic setup with `use-package`:

```elisp
(use-package nostr-publish
  :ensure t
  :custom
  (nostr-publish-bunker-uri "bunker://pubkey?relay=wss://relay.example.com")
  (nostr-publish-default-relays '("wss://relay1.example.com" "wss://relay2.example.com"))
  (nostr-publish-timeout 60))
```

Or configure variables directly:

```elisp
;; Required: bunker URI for signing
(setq nostr-publish-bunker-uri "bunker://pubkey?relay=wss://relay.example.com")

;; Required: relay allowlist (also serves as defaults)
(setq nostr-publish-default-relays '("wss://relay1.example.com"
                                      "wss://relay2.example.com"))

;; Optional: signing timeout (seconds, default 30)
(setq nostr-publish-timeout 60)
```

**Directory-local configuration** for project-specific settings (`.dir-locals.el`):

```elisp
((markdown-mode
  . ((nostr-publish-bunker-uri . "bunker://project-pubkey?relay=wss://relay.example.com")
     (nostr-publish-default-relays . ("wss://project-relay.example.com")))))
```

**Secure credential storage** with auth-source:

```elisp
;; Store in ~/.authinfo.gpg:
;; machine nostr-publish login bunker password bunker://pubkey?relay=...&secret=...

(setq nostr-publish-bunker-uri
      (auth-source-pick-first-password :host "nostr-publish" :user "bunker"))
```

For development with local source, see [Local Setup](docs/local-setup.md#emacs-setup-with-local-source).

## Usage

### CLI

```bash
# Publish to relay (--bunker and --relay are required)
nostr-publish article.md --bunker "bunker://..." --relay wss://relay.example.com

# Multiple relays (serves as allowlist and defaults)
nostr-publish article.md --bunker "bunker://..." --relay wss://relay1.example.com --relay wss://relay2.example.com

# Dry run: validate and construct event without publishing (--bunker not required)
nostr-publish article.md --relay wss://relay.example.com --dry-run

# Custom timeout for signer operations (default: 30 seconds)
nostr-publish article.md --bunker "bunker://..." --relay wss://relay.example.com --timeout 60

# Show version
nostr-publish --version
```

> **Note**: `--bunker` is required for publishing but not needed with `--dry-run`.
> Use `--dry-run` to validate your article and preview the constructed event without a signer connection.

### Emacs

Open a Markdown file and press `C-c C-p` to publish as long form content to nostr.

## Frontmatter Format

```yaml
---
title: Article Title          # Required
slug: article-slug            # Required (stable identifier)
summary: Short description    # Optional
published_at: 1700000000      # Optional (Unix timestamp)
tags:                         # Optional
  - nostr
  - writing
relays:                       # Optional (subset of CLI --relay allowlist)
  - wss://relay.example.com   # Must be in CLI allowlist
---
```

### Available Fields

| Field          | Required | Type    | Description                                        |
|----------------|----------|---------|----------------------------------------------------|
| `title`        | Yes      | string  | Article title (becomes "title" tag)                |
| `slug`         | Yes      | string  | Stable identifier (becomes "d" tag)                |
| `summary`      | No       | string  | Short description (becomes "summary" tag)          |
| `published_at` | No       | integer | Unix timestamp (becomes "published_at" tag)        |
| `tags`         | No       | list    | Hashtags (become "t" tags)                         |
| `relays`       | No       | list    | Subset of CLI relays (or `"*"` for all CLI relays) |

### Relay Precedence

CLI `--relay` arguments serve as both an **allowlist** and **default relay set**:

1. **Frontmatter specifies `relays`**: Only those relays are used (must all be in CLI allowlist)
2. **Frontmatter specifies `relays: ["*"]`**: All CLI relays are used
3. **Frontmatter omits `relays`**: All CLI relays are used (same as `["*"]`)

```bash
# CLI: --relay wss://relay1 --relay wss://relay2 --relay wss://relay3

# Frontmatter: relays: [wss://relay1]
# Result: publishes to relay1 only

# Frontmatter: relays: [wss://relay4]
# Result: ERROR - relay4 not in allowlist

# Frontmatter: (no relays field)
# Result: publishes to relay1, relay2, relay3
```

See [specs/spec.md](specs/spec.md) for complete specification.

## Documentation

- [Specification](specs/spec.md) - Complete technical specification (v1.0)
- [Local Development](docs/local-setup.md) - From source setup and local test stack
- [Integration Tests](docs/test-setup.md) - Test architecture and fixtures
- [Troubleshooting](docs/troubleshooting.md) - Common issues and solutions
- [Publishing Guide](docs/publishing.md) - PyPI and MELPA release process
- [User Stories](user-stories.md) - User personas, epics, and feature stories

> **Note on naming**: The GitHub repository is `emacs-nostr-publish` to reflect the Emacs-first focus, while both the PyPI and MELPA packages use the shorter name `nostr-publish`.

## Development

### Setup

```bash
# Sync venv with dev dependencies
make sync-dev

# Or manually
uv sync --extra dev
```

### Run Tests

```bash
# All tests (unit + integration)
make test

# Unit tests only (fast, no Docker required)
make test-unit

# Integration tests only (requires Docker, nak, Emacs 27.1+)
make test-e2e
```

> **Note:** Running `pytest` directly (without make) only runs unit tests. This is configured in `pyproject.toml` for faster iteration. Use `make test` or `make test-e2e` to include integration tests.

**Integration tests** use Docker Compose to run end-to-end publishing tests against a real Nostr relay and NIP-46 signer. See [docs/test-setup.md](docs/test-setup.md) for details.

### Available Make Targets

Run `make help` to see all available targets:

```bash
make help              # Show all targets with descriptions
make sync              # Sync venv with production dependencies
make sync-dev          # Sync venv with dev dependencies
make install           # Install package to user space from source
make build             # Build distribution packages
make publish-test      # Publish to TestPyPI
make publish           # Publish to PyPI
make test              # Run all tests
make test-unit         # Run unit tests only
make test-e2e          # Run integration tests only
make lint              # Run linter and formatter check
make format            # Auto-fix linting and formatting
make format-md         # Format markdown tables
make install-hooks     # Install git pre-commit/pre-push hooks
make clean             # Remove build artifacts
make dry-run           # Test event construction without publishing
make version-patch     # Bump patch version (0.1.0 -> 0.1.1)
make version-minor     # Bump minor version (0.1.0 -> 0.2.0)
make version-major     # Bump major version (0.1.0 -> 1.0.0)
```

### Property-Based Testing

This project uses [Hypothesis](https://hypothesis.readthedocs.io/) for property-based testing. All implementations include comprehensive property tests verifying invariants and edge cases across thousands of generated test cases.

## Related Projects

- [nostr-publisher-cli](https://github.com/ed253/nostr-publisher-cli) - JavaScript CLI for fetching and publishing notes to Nostr

## Disclaimer

This project is 100% AI-generated using a spec-driven, property-based testing approach.

**Use at your own risk.** No warranty whatsoever is provided. The authors accept no responsibility if usage of this tool results in unintended content being posted.

If you want to show your appreciation, just say GM.

## Author

Markus Rother <mail@markusrother.de>

## License

GPLv3+
