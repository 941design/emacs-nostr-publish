# Integration Test Setup

End-to-end tests verifying the complete publish workflow using real Docker services.

## Prerequisites

1. **Docker and Docker Compose** installed and running
2. **nak** CLI tool installed (Nostr Army Knife)
3. **Emacs 27.1+** (for Emacs integration tests)

For local stack setup and credentials, see [local-setup.md](local-setup.md).

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

- `test-article.md`: Standard article with all common fields
- `minimal-article.md`: Minimal article (title + slug only)
- `all-fields-article.md`: Comprehensive article with all optional fields
