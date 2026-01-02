# Local Development Setup

Complete guide for setting up nostr-publish from source and running against a local test stack.

## Prerequisites

- **Python 3.8+** with [uv](https://docs.astral.sh/uv/) package manager
- **Docker and Docker Compose** for local relay and signer
- **[nak](https://github.com/fiatjaf/nak)** CLI tool (v0.17.4+)
- **Emacs 27.1+** (for Emacs integration)

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

The test stack provides a local Nostr relay and NIP-46 signer for development and testing without needing a mobile signer app.

### Start the Stack

```bash
cd tests/integration
docker compose up -d
```

This starts:
- **nostr-rs-relay** on `ws://localhost:8081` - local Nostr relay
- **nak bunker** - NIP-46 remote signer with test keys

### Test Stack Credentials

The local signer uses fixed test keys:

| Component     | Value                                                                                                         |
|---------------|---------------------------------------------------------------------------------------------------------------|
| Bunker URI    | `bunker://79be667ef9dcbbac55a06295ce870b07029bfcdb2dce28d959f2815b16f81798?relay=ws%3A%2F%2Flocalhost%3A8081` |
| Relay URL     | `ws://localhost:8081`                                                                                         |
| Client Secret | `0000000000000000000000000000000000000000000000000000000000000002`                                            |

### Stop the Stack

```bash
cd tests/integration
docker compose down
```

To remove volumes and start fresh:

```bash
docker compose down -v
```

## CLI Usage with Local Stack

Test publishing against the local stack:

```bash
# Set environment variable for client authentication
export NOSTR_CLIENT_KEY=0000000000000000000000000000000000000000000000000000000000000002

# Publish a test article
uv run nostr-publish your-article.md \
  --bunker "bunker://79be667ef9dcbbac55a06295ce870b07029bfcdb2dce28d959f2815b16f81798?relay=ws%3A%2F%2Flocalhost%3A8081" \
  --relay ws://localhost:8081

# Dry run (validate without publishing)
uv run nostr-publish your-article.md \
  --relay ws://localhost:8081 \
  --dry-run
```

Verify published events:

```bash
# Fetch recent long-form content from local relay
nak req -k 30023 -l 5 ws://localhost:8081
```

## Emacs Setup with Local Source

Configure Emacs to use your local checkout:

```elisp
;; Point to local source directory
(add-to-list 'load-path "/path/to/emacs-nostr-publish")
(require 'nostr-publish)

;; Enable nostr-publish-mode in markdown buffers (activates C-c C-p binding)
(add-hook 'markdown-mode-hook #'nostr-publish-mode)

;; Configure for local test stack
(setq nostr-publish-bunker-uri
      "bunker://79be667ef9dcbbac55a06295ce870b07029bfcdb2dce28d959f2815b16f81798?relay=ws%3A%2F%2Flocalhost%3A8081")
(setq nostr-publish-default-relays '("ws://localhost:8081"))
(setq nostr-publish-timeout 60)

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
   "bunker://79be667ef9dcbbac55a06295ce870b07029bfcdb2dce28d959f2815b16f81798?relay=ws%3A%2F%2Flocalhost%3A8081")
  (nostr-publish-default-relays '("ws://localhost:8081"))
  (nostr-publish-timeout 60))
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
