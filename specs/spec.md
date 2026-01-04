# Specification: `nostr-publish` — Markdown → Nostr Long-Form Publisher (Production Reference)

## 1. Purpose & Scope

### 1.1 Purpose

Provide a **deterministic, cross-platform publishing workflow** for long-form writing that:

* Uses **Markdown** as the authoring format
* Stores unpublished metadata in **YAML frontmatter**
* Publishes as **NIP-23 long-form events (`kind: 30023`)**
* Uses **remote signing exclusively via NIP-46**
* Supports **cover images** with local file upload via Blossom servers
* Supports invocation from:

  * **Emacs** (`C-c C-p`)
  * **CLI** (`nostr-publish FILE.md`)

No private keys are handled locally.

### 1.2 Scope

This specification defines the authoritative behavior of:

* The **CLI wrapper** (`nostr-publish`)
* The **Emacs Lisp package** (`nostr-publish.el`) that shells out to the CLI
* The **image processing pipeline** for cover images
* **Blossom upload integration** via `nak`
* A **Dockerized integration test harness** verifying end-to-end publishing

### 1.3 Non-Goals

Out of scope:

* Local key management
* Implementing NIP-46, cryptography, or event ID calculation
* Rich editor UI, previews, or HTML rendering
* Draft syncing, background queues
* Relay discovery heuristics
* Event deletion
* Supporting arbitrary embedded images inside Markdown body
* Implementing Blossom server (only consuming it)
* Implementing image hashing algorithms inside Emacs Lisp
* UI previews or selecting images via file picker

---

## 2. Target Release Type & Licensing

* Target: **Production-grade reference implementation**
* License: **GPLv3+**

Independent implementations must behave identically for the same inputs.

---

## 3. Architecture Overview

```
Markdown file / Emacs buffer
  ↓
Wrapper (CLI or Elisp orchestration)
  ↓
[Image Processing Pipeline] (if image.file present)
  ↓
[Blossom Upload via nak] (if image.file present)
  ↓
nak (NIP-46 client)
  ↓
Remote signer (NIP-46 compatible)
  ↓
Relays (wss://…)
```

### 3.1 Responsibility Boundaries

| Component           | Responsibility                                                                                                           |
|---------------------|--------------------------------------------------------------------------------------------------------------------------|
| Wrapper (CLI/Elisp) | Parsing, validation, image processing, Blossom upload, deterministic event construction, relay selection, invoking `nak` |
| `nak`               | NIP-46 request/response, signing, publishing, Blossom upload                                                             |
| Remote signer       | Identity selection, key custody, user consent                                                                            |
| Blossom server      | Blob storage, authenticated uploads                                                                                      |
| Relays              | Event propagation and query                                                                                              |

The wrapper must not implement cryptography or NIP-46.

---

## 4. Input Format

### 4.1 Markdown File Structure

* UTF-8 encoded text
* Optional YAML frontmatter
* If present, frontmatter must be the **first bytes in the file**
* Frontmatter is delimited by `---` on its own line at start and end
* Body begins immediately after the closing delimiter (one newline allowed)

Example:

```yaml
---
title: My Article
slug: my-article-slug
summary: Short description
published_at: 1700000000
image:
  file: ./images/cover.jpg
  alt: Article cover image
tags:
  - nostr
  - writing
relays:
  - wss://relay1
  - wss://relay2
---
# Article body

Markdown content here.
```

---

## 5. Frontmatter Semantics (Strict, Normative)

### 5.1 Parsing Rules

* Frontmatter must be valid **YAML**
* Parsing failures must **abort publishing**
* Unknown fields must cause a **hard error**
* Keys are **case-sensitive**
* Empty frontmatter (`---\n---`) is invalid

### 5.2 Allowed Fields & Constraints

| Field          | Required | Type                  | Constraints                                                               |
|----------------|----------|-----------------------|---------------------------------------------------------------------------|
| `title`        | yes      | string                | non-empty after trim                                                      |
| `slug`         | yes      | string                | lowercase alphanumeric + hyphens only (`^[a-z0-9-]+$`); stable identifier |
| `summary`      | no       | string                | trimmed (may be empty only if omitted)                                    |
| `published_at` | no       | integer               | Unix timestamp in seconds; must be ≥ 0                                    |
| `tags`         | no       | list[string]          | each entry non-empty after trim                                           |
| `relays`       | no       | list[string]          | each must be a valid WebSocket URL (`wss://` or `ws://`)                  |
| `image`        | no       | string or dict object | cover image; see Section 5.4 for format details                           |
| `naddr`        | no       | string                | NIP-19 bech32-encoded address; set by Emacs after publish                 |

### 5.3 Validation Rules

* Missing required fields → error
* Empty or whitespace-only strings → error
* Invalid types (e.g., `tags` not a list) → error
* Duplicate tags → deduplicate (case-sensitive)
* `slug` change represents a **new article**

### 5.4 Image Field Format (NIP-92 Cover Image Support)

The optional `image` field supports three formats:

**Simple format (string URL):**

```yaml
image: https://example.com/cover.jpg
```

**Extended format with remote URL (object):**

```yaml
image:
  url: https://example.com/cover.jpg     # Required (mutually exclusive with file)
  mime: image/jpeg                       # Optional (inferred from URL if omitted)
  alt: Description text                  # Optional
  dim: 1200x630                          # Optional (WIDTHxHEIGHT)
```

**Local file format (object, requires `--blossom` flag):**

```yaml
image:
  file: ./path/to/cover.png              # Required (mutually exclusive with url)
  alt: Description text                  # Optional
  # After upload, these are added automatically:
  # url: https://blossom.example/abc123.jpg
  # hash: abc123def456...
```

### 5.5 Image Field Validation Rules

* `image` string format:
  * Must be `http://` or `https://` URL
* `image` object format:
  * Must be a YAML mapping/object
  * Must contain **exactly one** of:
    * `url` (remote)
    * `file` (local)
  * `file` must reference a path that exists at publish time
  * Absolute and relative paths are allowed; relative paths resolve relative to the Markdown file directory
  * Unknown keys in `image` object are a **hard error** (strict mode)
* If `image.file` is present:
  * `image.url` must be absent
  * `image.hash` may be absent (first run) or present (repeat publish)
* If `image.url` is present:
  * `image.file` must be absent
  * `image.hash` is ignored if present

### 5.6 Image Deterministic Update Rule

When `image.file` is present, publishing MUST ensure that:

* The Blossom upload happens **before** the event is constructed/sent
* The resulting blob identifier (hash) is written into:
  * YAML: `image.hash: <value>`
  * YAML: `image.url: <blossom_public_url>`
* After upload, `imeta` tag generation must use the **uploaded URL**, not the local file path

### 5.7 Tag Mapping for Images

When an image is specified, an `imeta` tag is added to the event following NIP-92 format:

```
["imeta", "url <URL>", "m <MIME>", "alt <ALT>", "dim <DIMENSIONS>"]
```

Only fields that are present (or successfully inferred) are included in the imeta tag.

### 5.8 Publication Rule

Frontmatter is never included in the published `content`.

---

## 6. Image Processing Pipeline (Normative)

When `image.file` is present, the wrapper MUST process the image before upload.

### 6.1 Supported Input Formats

* Supported formats: **PNG, JPEG, WebP**
* Unsupported formats must cause a fatal error with a single primary cause

### 6.2 Input Validation

Before processing, images MUST be validated:

* **Minimum dimensions**: Both width and height must be at least 200px
* **Aspect ratio**: Must be between 1:4 and 4:1 (not too tall or too wide)

Images failing validation cause a fatal error:

* "image too narrow: Npx (minimum 200px)"
* "image too short: Npx (minimum 200px)"
* "image aspect ratio too extreme (too tall): WxH"
* "image aspect ratio too extreme (too wide): WxH"

### 6.3 EXIF Stripping

* The processed cover file MUST have all EXIF metadata removed
* If EXIF stripping fails, publishing aborts

### 6.4 Resizing / Normalization

* **Default target dimensions:** `1200x630` (landscape)
* Resizing rules:
  * Preserve aspect ratio
  * If input aspect ratio differs, perform **center-crop** to exactly match target dimensions
  * Never upscale images smaller than target; leave as-is and set `dim` to actual dimensions
* Output format:
  * Always output **JPEG** (quality=95) for all inputs

### 6.5 Deterministic Output Location

* Processed cover file location: `./.nostr-publish/cache/covers/<slug>/cover.jpg` (relative to Markdown file directory)
* Re-running publish with unchanged input must produce identical bytes (best effort)
* The wrapper must not modify the original input file
* Old cached covers are deleted on republish (auto-cleanup)

---

## 7. Event Construction (NIP-23)

### 7.1 Unsigned Event Produced by Wrapper

The wrapper must construct an **unsigned** event object:

* `kind`: `30023`
* `content`: Markdown body with frontmatter stripped
* `tags`: per mapping below
* `id`, `sig`, `pubkey`, `created_at`: omitted (signer/`nak` sets as applicable)

### 7.2 Deterministic Tag Mapping (Order is Normative)

Tags must be generated exactly in this order:

1. `["d", slug]`
2. `["title", title]`
3. `["summary", summary]` (only if present)
4. `["published_at", "<published_at>"]` (only if present; integer string)
5. `["imeta", "url <URL>", ...]` (only if cover field present; NIP-92 format)
6. For each tag in `tags` (after trim + dedupe), **sorted lexicographically**:

   * `["t", tag]`

The `imeta` tag format follows NIP-92, containing space-separated key-value pairs for image metadata. Only fields that are present or successfully inferred are included.

No additional tags beyond those listed above are permitted.

---

## 8. Relay Selection (Allowlist Model)

CLI `--relay` arguments serve as both an **allowlist** and **default relay set**.

### 8.1 Requirements

* CLI must specify at least one `--relay` (mandatory)
* All relay URLs must use WebSocket scheme (`wss://` or `ws://`)
* Production deployments should use `wss://` for encrypted transport
* `ws://` is permitted for local development and testing (e.g., `ws://localhost:...`)

### 8.2 Resolution Algorithm

1. **If frontmatter specifies `relays`:**
   * Each frontmatter relay must exist in CLI allowlist, otherwise **fatal error**
   * Only frontmatter relays are used for publishing (subset of allowlist)
   * Duplicates removed, preserving frontmatter order

2. **If frontmatter specifies `relays: ["*"]` or omits `relays` field:**
   * All CLI relays are used as defaults
   * Duplicates removed, preserving CLI order

### 8.3 Error Conditions

| Condition                              | Error                                     |
|----------------------------------------|-------------------------------------------|
| No `--relay` arguments provided        | Fatal: "At least one --relay is required" |
| Frontmatter relay not in CLI allowlist | Fatal: "Relay not in allowlist: {url}"    |

### 8.4 Examples

```bash
# CLI allowlist: relay1, relay2, relay3
nostr-publish article.md --relay wss://relay1 --relay wss://relay2 --relay wss://relay3

# Frontmatter: relays: [wss://relay1]
# Result: publishes to wss://relay1 only

# Frontmatter: relays: [wss://relay4]
# Result: ERROR - relay4 not in allowlist

# Frontmatter: (no relays field)
# Result: publishes to relay1, relay2, relay3

# Frontmatter: relays: ["*"]
# Result: publishes to relay1, relay2, relay3
```

---

## 9. Update Semantics

* Articles are identified by `["d", slug]`
* Republishing with the same `slug` replaces prior versions (NIP-33 semantics)
* `title`, `summary`, `published_at`, and body may change across updates
* Deletion is not supported
* No event ID tracking is required; file + `slug` are the source of truth

---

## 10. Signing Model

### 10.1 Protocol

* NIP-46 Remote Signing / Nostr Connect

### 10.2 Supported Signers

Any NIP-46 compatible signer, including:
* Interactive mobile/desktop signers
* nak bunker (daemon, non-interactive) for testing and automation

### 10.3 Identity Rules

* The signer's public key must be retrieved via NIP-46 `get_public_key` at least once per publish (delegated to `nak`)
* Wrapper must not specify or assume an `npub`
* The signer is authoritative for identity

---

## 11. Blossom Upload (Normative)

When `image.file` is present, the wrapper MUST upload the processed image to a Blossom server.

### 11.1 Upload Contract

* Input: processed cover file path
* Output: machine-readable JSON containing:
  * `sha256` (blob hash, 64 hex characters)
  * `url` (public URL)

**Normative command:**
```
nak blossom --server <blossom_url> --sec <bunker_uri> upload <file_path> --json
```

### 11.2 Error Handling

* Non-zero exit from `nak` upload is fatal
* Missing/invalid `hash` or `url` in upload output is fatal
* The wrapper must surface a single primary error message
* Error messages must be sanitized (no file paths or internal details exposed)

### 11.3 URL Normalization

* The `url` used in NIP-92 `imeta` MUST be HTTP(S)
* If the Blossom server returns a relative URL, the wrapper must join it with `--blossom` base URL

---

## 12. CLI Interface

### 12.1 Command

```
nostr-publish FILE.md
  --relay wss://...
  [--relay wss://...]
  [--bunker URI]
  [--dry-run]
  [--timeout SECONDS]
  [--blossom URL]
  [--blossom-timeout SECONDS]
  [--cover-size WxH]
  [--allow-dry-run-without-upload]
```

### 12.2 Flags

| Flag                             | Required                 | Default  | Description                                       |
|----------------------------------|--------------------------|----------|---------------------------------------------------|
| `--relay`                        | Yes (at least one)       | -        | Relay URL (allowlist and defaults)                |
| `--bunker`                       | Yes (unless `--dry-run`) | -        | NIP-46 bunker URI                                 |
| `--dry-run`                      | No                       | false    | Validate and construct event without publishing   |
| `--timeout`                      | No                       | 30       | Signer/publish timeout in seconds                 |
| `--blossom`                      | If `image.file` present  | -        | Blossom server URL for cover upload               |
| `--blossom-timeout`              | No                       | 30       | Blossom upload timeout in seconds                 |
| `--cover-size`                   | No                       | 1200x630 | Target cover dimensions (WxH format)              |
| `--allow-dry-run-without-upload` | No                       | false    | Allow dry-run with image.file without Blossom URL |

### 12.3 Behavior

* `FILE.md` is required
* At least one `--relay` is required (serves as allowlist and defaults; see Section 8)
* Multiple `--relay` flags allowed
* `--dry-run` performs parse + validation + event construction, but does not publish or upload
* `--timeout` applies to signer interaction and publish operations; timeout expiry is a fatal error
* `--blossom` is required when `image.file` is present (unless `--dry-run` with `--allow-dry-run-without-upload`)

### 12.4 JSON Output Format (Breaking Change)

After successful publish, CLI MUST print structured JSON to stdout containing all publish results:

**Minimal output (no cover, no naddr):**
```json
{"event_id": "abc123...", "pubkey": "def456..."}
```

**With naddr (NIP-19 address encoding):**
```json
{"event_id": "abc123...", "naddr": "naddr1qqxnzd3c...", "pubkey": "def456..."}
```

**With image metadata:**
```json
{"event_id": "abc123...", "image": {"dim": "1200x630", "hash": "abc123def456", "mime": "image/jpeg", "url": "https://blossom.example/abc123.jpg"}, "pubkey": "def456..."}
```

**Complete output (all fields):**
```json
{"event_id": "abc123...", "image": {"dim": "1200x630", "hash": "sha256...", "mime": "image/jpeg", "url": "https://..."}, "naddr": "naddr1...", "pubkey": "def456..."}
```

**JSON Format Requirements:**
- Single line (no embedded newlines)
- Keys sorted alphabetically
- UTF-8 encoding preserved (not escaped to ASCII)
- Valid JSON parseable by standard libraries
- `naddr` field included when encoding succeeds (non-fatal if encoding fails)
- `image` field included when image upload completes

This output is parsed by Emacs for buffer write-back and machine consumption.

---

## 13. NIP-19 Address Encoding (naddr)

After successful event publish, the CLI MUST attempt to encode a NIP-19 address reference (naddr) for the published article.

### 13.1 Encoding Requirements

* Uses `nak encode naddr` command to generate bech32-encoded address
* Includes pubkey (from publish result), kind 30023, and slug (d-tag identifier)
* No relay hints included (per specification preference)
* Output format: `naddr1{bech32-encoded-data}`

### 13.2 Error Handling

* naddr encoding is **non-fatal** - if encoding fails, publish still succeeds
* Encoding failure logs warning to stderr: `WARNING: naddr encoding failed: {error}`
* Failed encoding results in `naddr` field omitted from JSON output
* Successful encoding includes `naddr` field in JSON output

### 13.3 Purpose

The naddr encoding provides a shareable, human-friendly reference to published articles that can be:
- Used in Nostr clients to fetch/display the article
- Shared as a permalink that resolves across any relay carrying the event
- Parsed to extract pubkey, kind, and identifier without relay queries

---

## 14. `nak` Invocation Contract (Normative)

### 14.1 Event Publishing

* Wrapper invokes `nak` as an external process
* Unsigned event is passed as **JSON via stdin**
* `nak` receives:

  * signer connection string (`--sec <bunker_uri>`)
  * relay list
  * the event JSON
* Exit code:

  * `0` = success
  * non-zero = failure
* Success output includes the signed event JSON containing:

  * published event `id`
  * `pubkey` used
* Partial relay acceptance counts as success (nak exit code 0)
* Note: nak outputs relay status as human-readable text, not structured data; per-relay success/failure tracking is not available

### 14.2 Blossom Upload

* Command: `nak blossom --server <url> --sec <bunker_uri> upload <file> --json`
* Output: JSON with `sha256` and `url` fields
* Exit code 0 = success, non-zero = failure

### 14.3 NIP-19 Address Encoding

* Command: `nak encode naddr --kind 30023 --identifier <slug> --pubkey <pubkey>`
* Output: bech32-encoded naddr string starting with "naddr1"
* Exit code 0 = success, non-zero = failure
* Non-fatal: encoding failure does not abort publish workflow

---

## 15. Emacs Integration (`nostr-publish.el`)

### 15.1 Distribution

* Emacs package name: **`nostr-publish`**
* Distribution target: **MELPA only**
* Repository must include standard package headers and autoloads
* Elisp is a thin orchestration layer that shells out to the CLI

### 15.2 User Command

* Keybinding: `C-c C-p` (user-configurable)
* Interactive command publishes current buffer

### 15.3 Configuration Variables

| Variable                       | Type   | Description                          |
|--------------------------------|--------|--------------------------------------|
| `nostr-publish-bunker-uri`     | string | NIP-46 bunker URI                    |
| `nostr-publish-default-relays` | list   | Default relay URLs                   |
| `nostr-publish-timeout`        | int    | Publish timeout in seconds           |
| `nostr-publish-blossom-url`    | string | Blossom server URL for cover uploads |

### 15.4 Basic Behavior

* If buffer is modified, save before publishing
* Invoke CLI synchronously
* Respect configured timeout
* On success: parse JSON output and display event `id`, `pubkey`, and `naddr` (if present)
* On failure: display the primary fatal error (single message)

### 15.5 Image Metadata Buffer Write-Back

When publishing from Emacs and `image.file` is present:

* After successful Blossom upload, Emacs MUST update the YAML frontmatter in the current buffer
* Insert/update the following fields:
  * `image.hash: <hash>` (string scalar)
  * `image.url: <url>` (string scalar)
* Preserve other `image` keys (`file`, `alt`, etc.)
* Preserve all other frontmatter fields

### 15.6 Write-Back Semantics

* If `image.hash` exists, overwrite it
* If `image.url` exists, overwrite it
* `image` must remain an object (mapping)
* Buffer MUST be marked modified after write-back
* Buffer MUST be automatically saved after write-back

### 15.7 JSON Output Parsing

* Elisp parses single-line JSON output from CLI
* Extract all fields: `event_id`, `pubkey`, `naddr` (optional), `image` (optional)
* Display `event_id`, `pubkey`, and `naddr` (if present) in success message
* Extract image metadata from `image` field for buffer write-back

### 15.8 Image Frontmatter Update

* Elisp function `nostr-publish--update-cover-frontmatter` modifies buffer content
* Insert/update `hash:` and `url:` keys within `image:` YAML block
* Use regex/string manipulation (no external YAML library)
* Deterministic updates: same input produces same buffer modification

### 15.9 naddr Frontmatter Write-Back

When publishing from Emacs and naddr encoding succeeds:

* After successful publish, Emacs MUST update the YAML frontmatter with the naddr
* Insert/update the `naddr` field at top level of frontmatter
* If `naddr` field exists, overwrite it
* If `naddr` field does not exist, add it after the last existing field
* Buffer MUST be marked modified and automatically saved after write-back
* naddr write-back uses same atomicity guarantees as image write-back (Section 20.2)

---

## 16. Error Handling (Fail-Fast)

### 16.1 Fatal Errors (Abort)

* Invalid frontmatter format
* Unknown frontmatter fields
* Missing required fields
* Invalid metadata types/values
* No relays resolved
* Signer unreachable or times out
* User rejects signing
* `nak` exits non-zero
* `image` object contains both `file` and `url`
* `image.file` missing/unreadable
* Unsupported image format
* Image validation fails (dimensions/aspect ratio)
* EXIF stripping fails
* Resize fails
* Blossom upload fails or returns invalid output

### 16.2 Error Output Requirements

* Deterministic and human-readable
* Single primary cause
* Non-zero exit from CLI on failure

---

## 17. Testing Requirements

### 17.1 Test Categories

Implementations must include:

1. **Unit tests** (pure logic)

   * frontmatter parse + validation
   * tag mapping determinism
   * relay precedence resolution
   * CLI argument normalization
   * `image.file` vs `image.url` mutual exclusivity
   * Path resolution rules (relative to Markdown file)
   * Deterministic processed output path construction
   * Cover-size parsing and validation
   * Image dimension validation

2. **Integration tests (mandatory, Dockerized)**

   * End-to-end publish and fetch verification against a real relay
   * Use NIP-46 signer (e.g., nak bunker) in container
   * Cover image upload via Blossom server

3. **Emacs unit tests (ERT)**

   * `nostr-publish--extract-cover-metadata` parsing
   * `nostr-publish--update-cover-frontmatter` modification
   * `nostr-publish--sanitize-yaml-value` sanitization
   * Buffer write-back integration

### 17.2 Emacs Version Support (LTS-Style)

Test matrix must include:

* **Minimum supported Emacs** (floor): **27.1**
* **Latest stable Emacs** available in CI at test time

### 17.3 Integration Test Topology (Normative)

Docker Compose (or equivalent) must provision:

* **Relay**: `scsibug/nostr-rs-relay`
* **Signer**: NIP-46 daemon (e.g., nak bunker) configured with a **fixed, known keypair**
* **Blossom server**: `ghcr.io/hzrd149/blossom-server:master` with NIP-46 authentication
* **nak client** (host binary) used to publish

Test runner must:

1. Publish an article
2. Receive event `id`
3. Query the relay for the event by `id`
4. Assert all of the following:

   * event `kind == 30023`
   * event `content` matches expected body exactly
   * tags match expected set and required ordering rules as applicable
   * event `pubkey` equals the **known fixed pubkey** from the signer container

### 17.4 Image Integration Tests

Test runner must:

1. Create a Markdown article with `image.file: ./fixtures/cover_with_exif.jpg`
2. Publish via CLI with `--blossom http://blossom:PORT`
3. Assert:
   * Upload occurred (server received blob)
   * Wrapper produced event with `imeta url` pointing to Blossom URL
   * Event published to relay and fetchable by `id`
   * `imeta` contains `m`, `dim`, and `url`
4. Assert EXIF stripped (processed artifact has no EXIF metadata)
5. Assert resize behavior (processed dimensions match default `1200x630`)

### 17.5 Emacs Integration Tests

Emacs batch-mode integration test must:

* Open a buffer containing frontmatter with `image.file`
* Run publish (with the dockerized Blossom server)
* Assert the buffer now contains:
  * `image.hash: ...`
  * `image.url: ...`
  * `image` remains a mapping/object
* Assert the buffer was saved

### 17.6 Determinism Requirements

* Tests must not depend on interactive approval
* All secrets/keys used in tests must be non-production fixtures
* Relay persistence must be isolated per test run (fresh state or unique `slug` per run)

---

## 18. Release & Packaging Requirements (MELPA)

The repository must include:

* Primary file `nostr-publish.el` with:

  * package headers (`Version`, `Package-Requires`, `URL`, etc.)
  * `lexical-binding: t`
  * autoload cookies for interactive entry points
* A stable CLI entrypoint named `nostr-publish`
* Documentation:

  * installation instructions (MELPA + CLI deps)
  * configuration examples (bunker URI, default relays)
  * troubleshooting section for signer/relay failures
* CI that runs:

  * byte compilation
  * unit tests
  * dockerized integration tests

---

## 19. Acceptance Criteria

A conforming implementation must satisfy:

* Given identical input file and config, wrapper produces identical unsigned event JSON
* Wrapper never handles private keys locally
* Publishing via CLI and via Emacs yields identical published event semantics
* Integration tests prove:

  * publish succeeds via NIP-46 signer + `nak`
  * relay stores the event
  * fetched event matches expected kind/content/tags/pubkey exactly
* Package installs from MELPA and functions with documented dependencies
* Given `image.file`, the published event references only Blossom URL, never a local path
* Processed cover is EXIF-free
* Processed cover is normalized to the configured size (default `1200x630`), respecting no-upscale policy
* `nak` is used for Blossom upload, and failures abort publishing
* Emacs updates YAML frontmatter by inserting/updating `image.hash` and `image.url` in-buffer deterministically
* Docker integration tests provision a Blossom server and verify end-to-end behavior

---

## 20. Security Requirements

### 20.1 YAML Injection Prevention

The implementation must prevent YAML injection attacks when processing frontmatter and when writing metadata back to files:

**Threat Model:**
* Malicious or compromised external services (e.g., Blossom servers) could return crafted metadata containing YAML structural characters
* Untrusted input could contain newlines, brackets, braces, or other YAML control sequences
* Injection could modify frontmatter structure, add new fields, or corrupt the document

**Required Defenses:**

1. **Input Validation (Python CLI)**
   * All frontmatter values must pass strict validation before acceptance
   * Values containing newlines or YAML structural characters (`[`, `]`, `{`, `}`) must be rejected
   * Validation applied at parse time, before any processing

2. **Output Sanitization (Emacs Elisp)**
   * When writing metadata back to buffers (e.g., image upload results), all values must be sanitized
   * Remove newlines (both `\n` and `\r`)
   * Remove YAML structural characters (`[`, `]`, `{`, `}`)
   * Defense-in-depth: applies even though Python validation already prevents these

3. **Nil Handling**
   * Sanitization functions must handle nil/null values gracefully
   * Return nil when input is nil (no crash, no empty string substitution)

**Validation Points:**
* Frontmatter parsing: `src/nostr_publish/validator.py`
* Buffer writeback: `nostr-publish.el:nostr-publish--sanitize-yaml-value`

### 20.2 Atomicity Guarantees

File modifications must be atomic to prevent partial writes that corrupt documents:

**Requirements:**

1. **Emacs Buffer Updates**
   * All frontmatter modifications must use Emacs undo mechanism for rollback capability
   * Capture `buffer-undo-list` state before modifications
   * On error during update, roll back all changes via `primitive-undo`
   * Save buffer only after all modifications succeed
   * Either all changes are applied and saved, or all changes are rolled back

2. **Error Recovery**
   * If frontmatter structure is invalid (missing delimiters, missing image block), signal error before any modifications
   * If buffer changes during async operation, abort rather than apply stale updates
   * No partial updates to YAML structure

**Implementation:**
* `nostr-publish.el:nostr-publish--update-cover-frontmatter` provides atomic guarantee via undo mechanism

### 20.3 Input Validation Rules

All user-supplied and externally-sourced data must be validated before use:

**Frontmatter Validation:**
* Unknown fields → fatal error (fail-fast, strict mode)
* Type mismatches → fatal error
* Value constraints (e.g., slug format, relay URLs) → fatal error
* Empty required fields → fatal error

**External Data Validation:**
* Image metadata from Blossom servers must be JSON-parseable
* URLs must use `http://` or `https://` schemes
* Hash values must be present and non-empty
* Malformed responses → graceful failure, no buffer update

**Implementation:**
* Python validation: `src/nostr_publish/validator.py`
* Elisp validation: `nostr-publish.el` (relay URL validation, cover metadata parsing)

### 20.4 Subprocess Invocation Security

**Requirements:**
* CLI must invoke `nak` via controlled subprocess invocation (not shell interpretation)
* No user input directly interpolated into shell commands
* Proper argument escaping for external processes
* Timeout enforcement to prevent hanging on malicious/broken external tools

**Implementation:**
* Python: `subprocess.run()` with argument list (not shell string)
* Emacs: `call-process` with argument list (not `shell-command` with interpolation)

---
