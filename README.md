# nostr-publish

Deterministic, cross-platform publisher for Nostr long-form content ([NIP-23](https://github.com/nostr-protocol/nips/blob/master/23.md)).

Publishes Markdown files with YAML frontmatter to Nostr relays using remote signing via [NIP-46](https://github.com/nostr-protocol/nips/blob/master/46.md).

## Rationale

1. **Composition over reinvention** - Leverages existing tools (nak, remote signers) rather than reimplementing cryptographic protocols
2. **No key management** - This tool never touches your private keys; signing is delegated entirely to NIP-46 remote signers
3. **Bridge to the best editor** - Brings Nostr publishing to Emacs, where long-form content belongs

## Features

- **Markdown authoring**: Write in Markdown with YAML frontmatter
- **Cover images**: Support for NIP-92 image metadata tags
- **Local image upload**: Process and upload local cover images to Blossom servers
- **Idempotent publishing**: Hash-based upload skip for unchanged cover images
- **Shareable addresses**: NIP-19 naddr encoding for cross-relay article discovery
- **Remote signing**: NIP-46 support via any compatible signer
- **Deterministic**: Same input always produces identical events
- **CLI + Emacs**: Use from command line or Emacs (`C-c C-p`)
- **JSON output**: Machine-parseable publish results for automation
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
  :hook (markdown-mode . nostr-publish-mode)
  :custom
  (nostr-publish-bunker-uri "bunker://pubkey?relay=wss://relay.example.com")
  (nostr-publish-default-relays '("wss://relay1.example.com" "wss://relay2.example.com"))
  (nostr-publish-timeout 60)
  (nostr-publish-blossom-url "https://blossom.example.com"))  ; for cover.file uploads
```

Or configure variables directly:

```elisp
;; Enable nostr-publish-mode in markdown buffers (activates C-c C-p binding)
(add-hook 'markdown-mode-hook #'nostr-publish-mode)

;; Required: bunker URI for signing
(setq nostr-publish-bunker-uri "bunker://pubkey?relay=wss://relay.example.com")

;; Required: relay allowlist (also serves as defaults)
(setq nostr-publish-default-relays '("wss://relay1.example.com"
                                      "wss://relay2.example.com"))

;; Optional: signing timeout (seconds, default 30)
(setq nostr-publish-timeout 60)

;; Optional: Blossom server for cover image uploads (required if using cover.file)
(setq nostr-publish-blossom-url "https://blossom.example.com")
```

> **Note**: The `:hook` (or `add-hook`) is required to enable `nostr-publish-mode`, which provides the `C-c C-p` keybinding. This minor mode binding takes precedence over markdown-mode's default `C-c C-p`.

**Directory-local configuration** for project-specific settings (`.dir-locals.el`):

```elisp
((markdown-mode
  . ((nostr-publish-bunker-uri . "bunker://project-pubkey?relay=wss://relay.example.com")
     (nostr-publish-default-relays . ("wss://project-relay.example.com"))
     (nostr-publish-blossom-url . "https://blossom.example.com"))))
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

# Publish with local cover image (requires --blossom)
nostr-publish article.md --bunker "bunker://..." --relay wss://relay.example.com \
  --blossom https://blossom.example.com

# Custom cover image size (default: 1200x630)
nostr-publish article.md --bunker "bunker://..." --relay wss://relay.example.com \
  --blossom https://blossom.example.com --cover-size 800x400

# Show version
nostr-publish --version
```

> **Note**: `--bunker` is required for publishing but not needed with `--dry-run`.
> Use `--dry-run` to validate your article and preview the constructed event without a signer connection.

### Emacs

Open a Markdown file and press `C-c C-p` to publish as long form content to nostr.

On success, Emacs displays:
- Event ID (hex)
- Public key (hex)
- Article address (naddr - shareable NIP-19 encoded reference)

### Example: Idempotent Publishing with Cover Images

When using local cover images, nostr-publish automatically manages uploads to avoid duplication:

**Initial article** (`article.md`):
```yaml
---
title: My Article
slug: my-article
image:
  file: ./cover.jpg
---

Article content here.
```

**First publish**:
```bash
nostr-publish article.md --bunker "bunker://..." --relay wss://relay.example.com \
  --blossom https://blossom.example.com
```

After successful publish, frontmatter is automatically updated:
```yaml
---
title: My Article
slug: my-article
image:
  file: ./cover.jpg
  url: https://cdn.example.com/abc123...jpg
  hash: abc123def456...
---
```

**Second publish** (same image file):
- Hash computed from `cover.jpg` matches stored `hash`
- Upload is **skipped**
- Existing `url` is reused
- Saves bandwidth and avoids duplicate storage

**Modified image**:
- If you update `cover.jpg`, the computed hash will differ
- New upload occurs automatically
- Frontmatter updated with new `url` and `hash`

This workflow works seamlessly in Emacs with `C-c C-p` - frontmatter updates happen automatically after each publish.

### CLI Output Format

After successful publish, the CLI outputs JSON containing all publish results:

**Example output:**
```json
{"event_id": "abc123def456...", "naddr": "naddr1qqxnzd3cxsmnjv3hx56rjwf3...", "pubkey": "3bf0c63fcb..."}
```

**Output fields:**
- `event_id`: Published event ID (hex, 64 characters)
- `pubkey`: Author public key (hex, 64 characters)
- `naddr`: NIP-19 encoded article address (optional, for sharing/discovery)
- `cover`: Cover image metadata (optional, when using `cover.file`)

The naddr field provides a shareable address that Nostr clients can use to fetch and display your article across any relay.

## Frontmatter Format

```yaml
---
title: Article Title          # Required
slug: article-slug            # Required (stable identifier)
summary: Short description    # Optional
published_at: 1700000000      # Optional (Unix timestamp)
image: https://example.com/cover.jpg  # Optional (cover image, simple format)
tags:                         # Optional
  - nostr
  - writing
relays:                       # Optional (subset of CLI --relay allowlist)
  - wss://relay.example.com   # Must be in CLI allowlist
---
```

> **Breaking Change (v2.0.0)**: The `cover:` field has been renamed to `image:`. Update your frontmatter to use `image:` instead of `cover:`. The old field name is no longer supported.

### Cover Image (Extended Format)

The `image` field supports three formats. Simple format (string URL):

```yaml
image: https://example.com/cover.jpg
```

Extended format with optional metadata (NIP-92):

```yaml
image:
  url: https://example.com/cover.jpg
  mime: image/jpeg        # Optional (inferred from URL if omitted)
  alt: Image description  # Optional (accessibility text)
  dim: 1200x630          # Optional (dimensions in WIDTHxHEIGHT format)
```

Local file format with idempotent publishing (requires `--blossom` flag):

```yaml
image:
  file: ./images/cover.jpg  # Local file path (relative to Markdown file)
  alt: Image description    # Optional (accessibility text)
  # After first publish, these fields are auto-populated:
  url: https://cdn.example.com/abc123...jpg  # Blossom CDN URL
  hash: abc123def456...     # SHA-256 hash (64-char hex)
```

When using local files, the image is automatically:
- Stripped of EXIF metadata (privacy protection)
- Resized/cropped to 1200x630 (configurable via `--cover-size`)
- Converted to JPEG
- Uploaded to the specified Blossom server
- The `url` and `hash` fields are written back to frontmatter

**Idempotent Publishing**: On subsequent publishes, if the `hash` field matches the computed hash of the processed image, upload is skipped and the existing `url` is reused. This avoids duplicate uploads and saves bandwidth. If you modify the image file, the hash will differ and a new upload will occur.

### Available Fields

| Field          | Required | Type        | Description                                          |
|----------------|----------|-------------|------------------------------------------------------|
| `title`        | Yes      | string      | Article title (becomes "title" tag)                  |
| `slug`         | Yes      | string      | Stable identifier (becomes "d" tag)                  |
| `summary`      | No       | string      | Short description (becomes "summary" tag)            |
| `published_at` | No       | integer     | Unix timestamp (becomes "published_at" tag)          |
| `image`        | No       | string/dict | Cover image URL, metadata, or local file (see above) |
| `tags`         | No       | list        | Hashtags (become "t" tags)                           |
| `relays`       | No       | list        | Subset of CLI relays (or `"*"` for all CLI relays)   |

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
make build             # Build distribution packages
make clean-emacs       # Clean Emacs byte-compiled files
make clean             # Clean build artifacts
make dry-run           # Dry-run publish example (requires test fixture)
make format            # Auto-fix linting and formatting issues
make format-md         # Format markdown tables in all .md files
make help              # Show this help message
make install           # Install CLI tool globally via uv
make install-hooks     # Install git hooks
make lint              # Run linter and formatter check
make publish           # Publish to PyPI (requires UV_PUBLISH_TOKEN)
make publish-test      # Publish to TestPyPI
make stack-down        # Stop local test stack and remove volumes
make stack-up          # Start local test stack (relay + signer + blossom)
make sync              # Sync venv with production dependencies
make sync-dev          # Sync venv with dev dependencies
make test              # Run all tests
make test-e2e          # Run integration tests only
make test-emacs        # Run all Emacs tests (compile + unit)
make test-emacs-compile # Byte-compile Emacs Lisp files
make test-emacs-unit   # Run Emacs ERT unit tests
make test-unit         # Run unit tests only
make version-major     # Bump major version (0.1.0 -> 1.0.0)
make version-minor     # Bump minor version (0.1.0 -> 0.2.0)
make version-patch     # Bump patch version (0.1.0 -> 0.1.1)
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
