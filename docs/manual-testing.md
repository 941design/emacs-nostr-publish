# Manual Testing

Step-by-step verification of the complete publish workflow.

## Prerequisites

- Local test stack credentials from [local-setup.md](local-setup.md#test-stack-credentials)
- Dependencies installed: `make sync-dev`

## 1. Start Local Stack

```bash
make stack-up
```

Verify services are running:

```bash
docker compose -f tests/integration/docker-compose.yml ps
```

## 2. Listen to Events

In a separate terminal, subscribe to long-form content events:

```bash
nak req -k 30023 ws://localhost:8081 --stream
```

This will print events as they arrive.

## 3. Create Event from CLI

```bash
export NOSTR_CLIENT_KEY=0000000000000000000000000000000000000000000000000000000000000002

uv run nostr-publish tests/integration/fixtures/test-article.md \
  --bunker "bunker://79be667ef9dcbbac55a06295ce870b07029bfcdb2dce28d959f2815b16f81798?relay=ws%3A%2F%2Flocalhost%3A8081" \
  --relay ws://localhost:8081
```

Verify the event appears in the listening terminal.

## 4. Configure Emacs

See [local-setup.md](local-setup.md#emacs-setup-with-local-source) for full configuration.

Minimal setup:

```elisp
(add-to-list 'load-path "/path/to/nostr-publish")
(require 'nostr-publish)

;; Enable nostr-publish-mode in markdown buffers (activates C-c C-p binding)
(add-hook 'markdown-mode-hook #'nostr-publish-mode)

(setenv "NOSTR_CLIENT_KEY" "0000000000000000000000000000000000000000000000000000000000000002")
(setq nostr-publish-bunker-uri
      "bunker://79be667ef9dcbbac55a06295ce870b07029bfcdb2dce28d959f2815b16f81798?relay=ws%3A%2F%2Flocalhost%3A8081")
(setq nostr-publish-default-relays '("ws://localhost:8081"))
```

## 5. Publish from Emacs

1. Open a Markdown file with valid [frontmatter](../README.md#frontmatter-format)
2. Press `C-c C-p`
3. Verify the event appears in the listening terminal

## Cleanup

```bash
make stack-down
```
