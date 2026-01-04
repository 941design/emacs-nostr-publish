# Local Development Setup

Complete guide for setting up nostr-publish from source and running against a local test stack.

## Prerequisites

- **Python 3.9+** with [uv](https://docs.astral.sh/uv/) package manager
- **Docker and Docker Compose** for local relay and signer
- **[nak](https://github.com/fiatjaf/nak)** CLI tool (v0.17.4+)
- **Emacs 27.1+** (for Emacs integration)
- **envsubst** (typically included with `gettext` package)

## Install from Source

```bash
# Clone the repository
git clone https://github.com/941design/emacs-nostr-publish.git
cd emacs-nostr-publish

# Install with dev dependencies
make install-dev

# Or manually with uv
uv sync --extra dev
```

Verify installation:

```bash
uv run nostr-publish --help
```

## Local Test Stack

The test stack provides a local Nostr relay, NIP-46 signer, and Blossom media server for development and testing without needing a mobile signer app.

### Configuration

Port configuration is managed via environment variables. Create a `.env` file from the example:

```bash
cp .env.example .env
```

Available variables (with defaults):

| Variable                     | Default | Description          |
|------------------------------|---------|----------------------|
| `NOSTR_PUBLISH_RELAY_PORT`   | 8080    | Relay WebSocket port |
| `NOSTR_PUBLISH_BLOSSOM_PORT` | 3000    | Blossom HTTP port    |

The Makefile and test suite automatically load these values.

### Start the Stack

```bash
make stack-up
```

This starts:
- **nostr-rs-relay** on `ws://localhost:${NOSTR_PUBLISH_RELAY_PORT}` (default: 8080)
- **nak bunker** - NIP-46 remote signer with test keys
- **blossom** on `http://localhost:${NOSTR_PUBLISH_BLOSSOM_PORT}` (default: 3000)

### Test Stack Credentials

The local signer uses fixed test keys. With default ports:

| Component     | Value                                                                                                         |
|---------------|---------------------------------------------------------------------------------------------------------------|
| Bunker URI    | `bunker://79be667ef9dcbbac55a06295ce870b07029bfcdb2dce28d959f2815b16f81798?relay=ws%3A%2F%2Flocalhost%3A8080` |
| Relay URL     | `ws://localhost:8080`                                                                                         |
| Blossom URL   | `http://localhost:3000`                                                                                       |
| Client Secret | `0000000000000000000000000000000000000000000000000000000000000002`                                            |

If you change the relay port, update the `relay=` parameter in the bunker URI accordingly (URL-encoded).

### Stop the Stack

```bash
make stack-down
```

This also removes volumes to start fresh.

## CLI Usage with Local Stack

Test publishing against the local stack (using default ports):

```bash
# Set environment variable for client authentication
export NOSTR_CLIENT_KEY=0000000000000000000000000000000000000000000000000000000000000002

# Publish a test article
uv run nostr-publish your-article.md \
  --bunker "bunker://79be667ef9dcbbac55a06295ce870b07029bfcdb2dce28d959f2815b16f81798?relay=ws%3A%2F%2Flocalhost%3A8080" \
  --relay ws://localhost:8080

# Dry run (validate without publishing)
make dry-run
```

Verify published events (adjust port if changed):

```bash
# Fetch recent long-form content from local relay
nak req -k 30023 -l 5 ws://localhost:8080
```

## Emacs Setup with Local Source

Configure Emacs to use your local checkout:

```elisp
;; Point to local source directory
(add-to-list 'load-path "/path/to/emacs-nostr-publish")
(require 'nostr-publish)

;; Enable nostr-publish-mode in markdown buffers (activates C-c C-p binding)
(add-hook 'markdown-mode-hook #'nostr-publish-mode)

;; Configure for local test stack (adjust ports if using custom .env)
(setq nostr-publish-bunker-uri
      "bunker://79be667ef9dcbbac55a06295ce870b07029bfcdb2dce28d959f2815b16f81798?relay=ws%3A%2F%2Flocalhost%3A8080")
(setq nostr-publish-default-relays '("ws://localhost:8080"))
(setq nostr-publish-timeout 60)

;; Optional: Blossom server for cover image uploads (required if using cover.file)
(setq nostr-publish-blossom-url "http://localhost:3000")

;; Required: set client key for bunker authentication
(setenv "NOSTR_CLIENT_KEY" "0000000000000000000000000000000000000000000000000000000000000002")
```

Or with `use-package`:

```elisp
(use-package nostr-publish
  :load-path "/path/to/emacs-nostr-publish"
  :hook (markdown-mode . nostr-publish-mode)
  :init
  (setenv "NOSTR_CLIENT_KEY" "0000000000000000000000000000000000000000000000000000000000000002")
  :custom
  (nostr-publish-bunker-uri
   "bunker://79be667ef9dcbbac55a06295ce870b07029bfcdb2dce28d959f2815b16f81798?relay=ws%3A%2F%2Flocalhost%3A8080")
  (nostr-publish-default-relays '("ws://localhost:8080"))
  (nostr-publish-timeout 60)
  (nostr-publish-blossom-url "http://localhost:3000"))
```

### Interactive Testing

1. Start the local test stack (see above)
2. Load your Emacs configuration
3. Open a Markdown file with valid frontmatter
4. Press `C-c C-p` to publish

Example test article:

```markdown
---
title: Local Test Article
slug: local-test-2024
summary: Testing local development setup
tags:
  - test
  - development
---

This is a test article published from my local development environment.
```

## Running Tests

With the local stack running:

```bash
# Run all tests
make test

# Unit tests only (no Docker required)
make test-unit

# Integration tests (requires Docker stack)
make test-e2e
```

See [test-setup.md](test-setup.md) for detailed test architecture documentation.

## CI/Automation

For automated publishing in CI pipelines:

```yaml
# GitHub Actions example
steps:
  - name: Publish article
    run: |
      nostr-publish article.md \
        --bunker "${{ secrets.BUNKER_URI }}" \
        --relay wss://relay1.example.com \
        --relay wss://relay2.example.com \
        --timeout 120
```

## Troubleshooting

See [troubleshooting.md](troubleshooting.md) for common issues including Docker failures, connection problems, signing timeouts, and Emacs integration issues.
