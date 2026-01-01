# User Stories

## Personas

### Emacs Writer
A developer or technical writer who uses Emacs as their primary text editor. Needs quick publishing workflow without leaving their editor, keyboard-driven interface, and integration with existing markdown workflow. High technical level.

### Nostr Content Creator
Writers and journalists publishing long-form articles to Nostr network. Needs reliable publishing to multiple relays, ability to update articles, deterministic publishing, and metadata management. Medium technical level.

### Security-Conscious Publisher
Users concerned about key management and security practices. Needs remote signing without handling private keys locally, support for multiple signer apps, and deterministic operations for auditability. High technical level.

### CLI Power User
Developers integrating publishing into scripts, CI/CD pipelines, or automation workflows. Needs scriptable CLI, dry-run mode for validation, configurable timeouts, and deterministic output for automation. High technical level.

### NIP-46 Signer User
Users with NIP-46 compatible signer apps managing their Nostr identities. Needs seamless integration with existing signer apps, clear bunker URI configuration, and timeout management for signing requests. Medium-high technical level.

---

## Epics

### Epic 1: Markdown-to-Nostr Publishing
Core publishing functionality from Markdown files to Nostr relays

**Related Features**: Frontmatter parsing, event construction, NIP-23 compliance

### Epic 2: Emacs Integration
Seamless integration with Emacs editor for one-keystroke publishing

**Related Features**: C-c C-p keybinding, buffer management, CLI invocation from Elisp

### Epic 3: Remote Signing & Key Management
Secure signing without local key handling via NIP-46

**Related Features**: NIP-46 bunker URI support, signer app integration, nak invocation

### Epic 4: Relay Management
Flexible relay selection and allowlist model for publish targets

**Related Features**: CLI relay flags, frontmatter relay specification, allowlist validation

### Epic 5: Content Validation & Error Handling
Strict validation and deterministic error reporting

**Related Features**: Frontmatter validation, fail-fast semantics, error messages

### Epic 6: CI/Automation Integration
Support for automated publishing in scripts and CI pipelines

**Related Features**: Dry-run mode, exit codes, structured output, timeout configuration

---

## Stories

### Epic 1: Markdown-to-Nostr Publishing

#### [Implemented] Story: Author writes Markdown with YAML frontmatter
**As a** content creator, **I want to** write articles in Markdown with structured metadata in YAML, **so that** I have a clean, portable format for my articles.

**Acceptance Criteria**:
- [x] Markdown files can include YAML frontmatter with title, slug, summary, published_at, tags
- [x] Required fields: title, slug
- [x] Optional fields: summary, published_at, tags, relays
- [x] All fields validated according to spec section 5

**Implementation**: `src/nostr_publish/frontmatter.py`, `src/nostr_publish/validator.py`

#### [Implemented] Story: Publish article to Nostr relays
**As a** content creator, **I want to** publish my Markdown article to Nostr relays using NIP-23 long-form events, **so that** my content is available on the Nostr network.

**Acceptance Criteria**:
- [x] Event kind is 30023 (long-form)
- [x] Content contains article body (no frontmatter)
- [x] Tags generated deterministically in correct order (d, title, summary, published_at, t tags)
- [x] Article identified by "d" tag (slug)
- [x] Multiple articles with same slug replace prior versions

**Implementation**: `src/nostr_publish/event.py`, `src/nostr_publish/nak.py`

#### [Implemented] Story: Publish to multiple relays for redundancy
**As a** content creator, **I want to** publish to multiple relays at once, **so that** my content is distributed and resilient to relay outages.

**Acceptance Criteria**:
- [x] CLI accepts multiple `--relay` flags
- [x] CLI relays serve as both allowlist and defaults
- [x] Can publish to subset via frontmatter `relays:` field
- [x] Frontmatter relays must be subset of CLI relays (validated)
- [x] Frontmatter `relays: ["*"]` or omission uses all CLI relays

**Implementation**: `src/nostr_publish/relay.py`, `src/nostr_publish/cli.py`

#### [Implemented] Story: Preview event without publishing (dry-run)
**As a** content creator, **I want to** validate my article and see the constructed event without publishing, **so that** I can catch errors before publishing.

**Acceptance Criteria**:
- [x] `--dry-run` flag parses frontmatter and constructs event
- [x] `--bunker` not required for dry-run
- [x] Outputs JSON representation of unsigned event
- [x] Returns exit code 0 on valid frontmatter
- [x] Returns non-zero on validation errors

**Implementation**: `src/nostr_publish/cli.py:main()`, Makefile target `dry-run`

### Epic 2: Emacs Integration

#### [Implemented] Story: Publish from Emacs with single keystroke
**As an** Emacs user, **I want to** press C-c C-p to publish the current buffer, **so that** I can publish without leaving my editor.

**Acceptance Criteria**:
- [x] C-c C-p bound to `nostr-publish-buffer` function
- [x] Works in markdown-mode
- [x] Displays success message with event ID
- [x] Displays error message on failure
- [x] Blocks until completion or timeout

**Implementation**: `nostr-publish.el:nostr-publish-buffer`

#### [Implemented] Story: Configure signer and relays in Emacs
**As an** Emacs user, **I want to** configure bunker URI and default relays in my .emacs, **so that** I don't repeat settings on every publish.

**Acceptance Criteria**:
- [x] `nostr-publish-bunker-uri` customizable variable
- [x] `nostr-publish-default-relays` customizable variable
- [x] `nostr-publish-timeout` customizable variable
- [x] Configuration via `use-package` supported
- [x] Directory-local configuration (.dir-locals.el) supported

**Implementation**: `nostr-publish.el` (defcustom variables), documented in [README.md](README.md#configuration)

#### [Implemented] Story: Auto-save buffer before publishing
**As an** Emacs user, **I want to** have my buffer auto-saved before publishing, **so that** the published content matches my latest edits.

**Acceptance Criteria**:
- [x] If buffer modified, save automatically
- [x] Publish uses saved file content
- [x] Error if buffer not associated with file

**Implementation**: `nostr-publish.el:nostr-publish-buffer` (save-buffer call)

### Epic 3: Remote Signing & Key Management

#### [Implemented] Story: Sign articles without handling private keys
**As a** security-conscious publisher, **I want to** use NIP-46 remote signing to sign events, **so that** my private keys are never exposed to nostr-publish.

**Acceptance Criteria**:
- [x] Bunker URI format: `bunker://pubkey?relay=wss://...`
- [x] Uses nak as NIP-46 client
- [x] No local key handling
- [x] Unsigned event passed to nak via stdin
- [x] Event signed by remote NIP-46 signer

**Implementation**: `src/nostr_publish/nak.py:invoke_nak()`

#### [Implemented] Story: Connect to NIP-46 remote signer
**As a** user with a NIP-46 compatible signer, **I want to** use my signer app to sign publishing requests, **so that** my Nostr identity remains protected.

**Acceptance Criteria**:
- [x] Generate bunker URI from signer settings
- [x] Configure bunker URI in CLI or Emacs
- [x] Signing requests appear in signer app
- [x] User approves/rejects in signer
- [x] Publish succeeds after approval

**Implementation**: Documented in [README.md](README.md#nip-46-remote-signing), tested in integration tests

### Epic 4: Relay Management

#### [Implemented] Story: Specify relays from command line
**As a** CLI user, **I want to** specify relays with `--relay` flags, **so that** I control where my content is published.

**Acceptance Criteria**:
- [x] `--relay` required (at least one)
- [x] Multiple `--relay` flags supported
- [x] Relays must use `wss://` or `ws://` scheme
- [x] CLI relays serve as allowlist
- [x] Frontmatter relays validated against allowlist

**Implementation**: `src/nostr_publish/cli.py:parse_arguments()`, `src/nostr_publish/relay.py`

#### [Implemented] Story: Override relay list per article
**As a** content creator, **I want to** specify custom relays in frontmatter for specific articles, **so that** some articles publish to different relay sets.

**Acceptance Criteria**:
- [x] `relays:` field in frontmatter accepts list of relay URLs
- [x] Each frontmatter relay must exist in CLI allowlist
- [x] Only frontmatter relays used if specified
- [x] Error if frontmatter relay not in allowlist
- [x] `relays: ["*"]` means use all CLI relays

**Implementation**: `src/nostr_publish/relay.py:resolve_relays()`, spec section 7

#### [Implemented] Story: Use reasonable defaults if relays omitted
**As a** content creator, **I want to** omit `relays:` from frontmatter to use CLI defaults, **so that** most articles use the default relay set.

**Acceptance Criteria**:
- [x] Omitting `relays:` field uses all CLI relays
- [x] `relays: ["*"]` also uses all CLI relays
- [x] CLI relays must be provided (error otherwise)

**Implementation**: `src/nostr_publish/relay.py:resolve_relays()`

### Epic 5: Content Validation & Error Handling

#### [Implemented] Story: Validate required frontmatter fields
**As the** tool, **I want to** require title and slug fields in all articles, **so that** published events have the minimum necessary metadata.

**Acceptance Criteria**:
- [x] Title required, must be non-empty after trim
- [x] Slug required, must match `^[a-z0-9-]+$`
- [x] Missing required fields → error
- [x] Empty strings → error
- [x] Error message specifies missing field

**Implementation**: `src/nostr_publish/validator.py:validate_frontmatter_dict()`, `validate_frontmatter()`

#### [Implemented] Story: Reject unknown frontmatter fields
**As the** tool, **I want to** reject unknown frontmatter fields, **so that** users catch typos and unexpected configurations early.

**Acceptance Criteria**:
- [x] Only allowed fields: title, slug, summary, published_at, tags, relays
- [x] Unknown fields → fatal error
- [x] Error message names the unknown field

**Implementation**: `src/nostr_publish/validator.py:validate_frontmatter_dict()` (spec v1.0 strict mode)

#### [Implemented] Story: Provide deterministic error messages
**As a** user and developer, **I want to** receive consistent, human-readable error messages, **so that** I can understand and fix problems.

**Acceptance Criteria**:
- [x] Error format: "ERROR: {ErrorType}: {message}"
- [x] One primary error reported (fail-fast)
- [x] Non-zero exit code on error
- [x] Message specifies problem and (when possible) solution

**Implementation**: `src/nostr_publish/errors.py` (error hierarchy), `src/nostr_publish/cli.py` (error handling)

### Epic 6: CI/Automation Integration

#### [Implemented] Story: Automate publishing in CI/CD pipelines
**As a** CI/CD operator, **I want to** invoke nostr-publish from scripts and workflows, **so that** articles are published automatically on commit/merge.

**Acceptance Criteria**:
- [x] CLI works from scripts and GitHub Actions
- [x] Bunker URI provided via environment variable or CLI flag
- [x] Relays specified via CLI flags
- [x] Exit code 0 on success, non-zero on failure
- [x] Output structured for parsing (event ID on success)
- [x] Timeout configurable via `--timeout`

**Implementation**: `src/nostr_publish/cli.py`, documented in [docs/local-setup.md](docs/local-setup.md#ciautomation)

#### [Implemented] Story: Validate content without publishing
**As a** CI/CD operator, **I want to** validate articles in CI without requiring signer/relays, **so that** I can catch errors early in the pipeline.

**Acceptance Criteria**:
- [x] `--dry-run` mode validates without bunker/relays
- [x] Only `--relay` required (not `--bunker`)
- [x] Outputs constructed event JSON
- [x] Returns exit code 0 on valid frontmatter/event
- [x] Can be used in pre-commit hooks

**Implementation**: `src/nostr_publish/cli.py:main()` (dry-run branch), Makefile `dry-run` target

---

## Future Considerations

The following items are mentioned in TODO.org but not formally specified as features:

- **Emacs "post and browse" command** - Potential enhancement to open published article in browser after posting
- **Environment variable support** - Design decision documented against using `NOSTR_RELAYS` env var (precedence rules would be unclear)

These would require formal feature specifications before implementation.
