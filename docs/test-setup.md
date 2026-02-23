# Integration Test Setup

End-to-end tests verifying the complete publish workflow using real Docker services.

## Prerequisites

1. **Docker and Docker Compose** installed and running
2. **nak** CLI tool installed (Nostr Army Knife)
3. **Emacs 27.1+** (for Emacs integration tests)

For local stack setup and credentials, see [local-setup.md](local-setup.md).

## Port Configuration

Integration tests read port configuration from environment variables:

| Variable                   | Default | Description     |
|----------------------------|---------|-----------------|
| `NOSTR_PUBLISH_RELAY_PORT` | 8080    | Relay host port |

These are loaded from `.env` file (if present) via `python-dotenv`, allowing tests to work regardless of invocation method (`make` vs `pytest` directly).

### Why Not Hardcode Ports in Test Fixtures?

**IMPORTANT:** Test fixtures must NOT hardcode relay URLs with specific ports.

If a fixture contains `relays: [ws://localhost:8080]` and:
1. The test stack runs on a different port (e.g., 8585 from `.env`)
2. A production or third-party relay happens to be running on port 8080

Then either:
- Tests fail with `RelayNotInAllowlistError` (port mismatch), or
- Tests **accidentally publish to an unintended relay** (security/privacy risk)

**Safe patterns for test fixtures:**
- Omit `relays` field entirely (defaults to CLI-provided relays)
- Use `relays: ["*"]` (explicit wildcard, uses CLI relays per spec)

This ensures tests always publish to the dynamically-configured test relay.

## Running Integration Tests

From the project root:

```bash
pytest tests/integration/ -v
```

Or using Make:

```bash
make test-e2e
```

The test suite automatically manages Docker services (starts before tests, stops after).

## Test Architecture

The integration test suite uses:

- **nostr-rs-relay**: Local relay for event storage/retrieval
- **nak bunker**: NIP-46 remote signer running as Docker container
- **blossom**: Media server for cover image uploads
- **njump**: Preview reader for article rendering
- **nak** (host): CLI tool for signing and publishing events
- **nostr-publish**: The CLI being tested

### Two nak Instances

The test setup runs two distinct nak instances:

1. **nak bunker (Docker container)**: Acts as the NIP-46 remote signer
   - Runs with a fixed test secret key
   - Communicates via the relay (authentic NIP-46 behavior)
   - Configured with authorized secret for test connections

2. **nak (host CLI)**: Used by nostr-publish for signing requests
   - Connects to the bunker via the relay
   - Requests signatures for event publishing

This architecture accurately simulates real-world usage where the signer is a remote service.

### Test Flow

1. Docker Compose starts relay and nak bunker services
2. nak bunker connects to relay and listens for NIP-46 requests
3. Test fixture Markdown files are published via `nostr-publish` CLI
4. nostr-publish uses nak to request signatures from the bunker
5. Published events are fetched from relay using `nak`
6. Event structure is validated against NIP-23 specification

## Test Cases

### Python CLI Tests (`test_e2e.py`)

1. **test_publish_basic_article**: Full article with all standard fields
2. **test_publish_minimal_frontmatter**: Article with only required fields
3. **test_publish_all_fields**: Article testing all optional frontmatter fields
4. **test_relay_precedence**: Verify CLI --relay overrides frontmatter
5. **test_dry_run**: Verify --dry-run mode prevents publishing
6. **test_timeout**: Verify timeout error handling

### Emacs Integration Tests (`test_emacs_integration.py`)

These tests verify the complete Emacs → CLI → nak → bunker → relay flow:

1. **test_emacs_integration_full_suite**: Runs `nostr-publish.el` in Emacs batch mode
   - Creates test articles dynamically
   - Calls `nostr-publish-buffer`
   - Verifies success messages and event publishing
2. **test_emacs_files_exist**: Verifies required Elisp files are present
3. **test_emacs_syntax_valid**: Validates Elisp syntax loads without errors

The Emacs tests run in batch mode (`emacs --batch`), loading `nostr-publish.el` and executing against the same Docker infrastructure as the Python CLI tests.

## Troubleshooting

**Tests skipped?** Verify prerequisites: `docker ps`, `which nak`, `emacs --version`

**Docker issues?** See [troubleshooting.md](troubleshooting.md) for Docker Compose debugging.

**First run slow?** Normal — the nak Docker image builds on first run. Subsequent runs use cache.

## Fixtures

Test Markdown files are located in `tests/integration/fixtures/`:

| Fixture                 | Purpose                              | Relays Config            |
|-------------------------|--------------------------------------|--------------------------|
| `test-article.md`       | Standard article with common fields  | None (uses CLI defaults) |
| `minimal-article.md`    | Minimal article (title + slug only)  | None (uses CLI defaults) |
| `all-fields-article.md` | All optional fields including relays | `["*"]` (wildcard)       |

Note: Fixtures use dynamic relay resolution (no hardcoded ports) to ensure tests work with any port configuration. See [Port Configuration](#port-configuration) above.
