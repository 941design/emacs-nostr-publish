# Specification: `nostr-publish` — Markdown → Nostr Long-Form Publisher (Production Reference)

## 1. Purpose & Scope

### 1.1 Purpose

Provide a **deterministic, cross-platform publishing workflow** for long-form writing that:

* Uses **Markdown** as the authoring format
* Stores unpublished metadata in **YAML frontmatter**
* Publishes as **NIP-23 long-form events (`kind: 30023`)**
* Uses **remote signing exclusively via NIP-46**
* Supports invocation from:

  * **Emacs** (`C-c C-p`)
  * **CLI** (`nostr-publish FILE.md`)

No private keys are handled locally.

### 1.2 Scope

This specification defines the authoritative behavior of:

* The **CLI wrapper** (`nostr-publish`)
* The **Emacs Lisp package** (`nostr-publish.el`) that shells out to the CLI
* A **Dockerized integration test harness** verifying end-to-end publishing

### 1.3 Non-Goals

Out of scope:

* Local key management
* Implementing NIP-46, cryptography, or event ID calculation
* Rich editor UI, previews, or HTML rendering
* Draft syncing, background queues
* Relay discovery heuristics
* Event deletion

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
nak (NIP-46 client)
  ↓
Remote signer (NIP-46 compatible)
  ↓
Relays (wss://…)
```

### 3.1 Responsibility Boundaries

| Component           | Responsibility                                                                                            |
|---------------------|-----------------------------------------------------------------------------------------------------------|
| Wrapper (CLI/Elisp) | Parsing, validation, deterministic event construction, relay selection, invoking `nak`, reporting results |
| `nak`               | NIP-46 request/response, signing, publishing                                                              |
| Remote signer       | Identity selection, key custody, user consent                                                             |
| Relays              | Event propagation and query                                                                               |

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

| Field          | Required | Type         | Constraints                                                               |
|----------------|----------|--------------|---------------------------------------------------------------------------|
| `title`        | yes      | string       | non-empty after trim                                                      |
| `slug`         | yes      | string       | lowercase alphanumeric + hyphens only (`^[a-z0-9-]+$`); stable identifier |
| `summary`      | no       | string       | trimmed (may be empty only if omitted)                                    |
| `published_at` | no       | integer      | Unix timestamp in seconds; must be ≥ 0                                    |
| `tags`         | no       | list[string] | each entry non-empty after trim                                           |
| `relays`       | no       | list[string] | each must be a valid WebSocket URL (`wss://` or `ws://`)                  |

### 5.3 Validation Rules

* Missing required fields → error
* Empty or whitespace-only strings → error
* Invalid types (e.g., `tags` not a list) → error
* Duplicate tags → deduplicate (case-sensitive)
* `slug` change represents a **new article**

### 5.4 Publication Rule

Frontmatter is never included in the published `content`.

---

## 6. Event Construction (NIP-23)

### 6.1 Unsigned Event Produced by Wrapper

The wrapper must construct an **unsigned** event object:

* `kind`: `30023`
* `content`: Markdown body with frontmatter stripped
* `tags`: per mapping below
* `id`, `sig`, `pubkey`, `created_at`: omitted (signer/`nak` sets as applicable)

### 6.2 Deterministic Tag Mapping (Order is Normative)

Tags must be generated exactly in this order:

1. `["d", slug]`
2. `["title", title]`
3. `["summary", summary]` (only if present)
4. `["published_at", "<published_at>"]` (only if present; integer string)
5. For each tag in `tags` (after trim + dedupe), **sorted lexicographically**:

   * `["t", tag]`

No additional tags are permitted.

---

## 7. Relay Selection (Allowlist Model)

CLI `--relay` arguments serve as both an **allowlist** and **default relay set**.

### 7.1 Requirements

* CLI must specify at least one `--relay` (mandatory)
* All relay URLs must use WebSocket scheme (`wss://` or `ws://`)
* Production deployments should use `wss://` for encrypted transport
* `ws://` is permitted for local development and testing (e.g., `ws://localhost:...`)

### 7.2 Resolution Algorithm

1. **If frontmatter specifies `relays`:**
   * Each frontmatter relay must exist in CLI allowlist, otherwise **fatal error**
   * Only frontmatter relays are used for publishing (subset of allowlist)
   * Duplicates removed, preserving frontmatter order

2. **If frontmatter specifies `relays: ["*"]` or omits `relays` field:**
   * All CLI relays are used as defaults
   * Duplicates removed, preserving CLI order

### 7.3 Error Conditions

| Condition                              | Error                                     |
|----------------------------------------|-------------------------------------------|
| No `--relay` arguments provided        | Fatal: "At least one --relay is required" |
| Frontmatter relay not in CLI allowlist | Fatal: "Relay not in allowlist: {url}"    |

### 7.4 Examples

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

## 8. Update Semantics

* Articles are identified by `["d", slug]`
* Republishing with the same `slug` replaces prior versions (NIP-33 semantics)
* `title`, `summary`, `published_at`, and body may change across updates
* Deletion is not supported
* No event ID tracking is required; file + `slug` are the source of truth

---

## 9. Signing Model

### 9.1 Protocol

* NIP-46 Remote Signing / Nostr Connect

### 9.2 Supported Signers

Any NIP-46 compatible signer, including:
* Interactive mobile/desktop signers
* nak bunker (daemon, non-interactive) for testing and automation

### 9.3 Identity Rules

* The signer's public key must be retrieved via NIP-46 `get_public_key` at least once per publish (delegated to `nak`)
* Wrapper must not specify or assume an `npub`
* The signer is authoritative for identity

---

## 10. CLI Interface

### 10.1 Command

```
nostr-publish FILE.md
  --relay wss://...
  [--relay wss://...]
  [--bunker URI]
  [--dry-run]
  [--timeout SECONDS]
```

### 10.2 Behavior

* `FILE.md` is required
* At least one `--relay` is required (serves as allowlist and defaults; see Section 7)
* Multiple `--relay` flags allowed
* `--dry-run` performs parse + validation + event construction, but does not publish
* `--timeout` applies to signer interaction and publish operations; timeout expiry is a fatal error

---

## 11. `nak` Invocation Contract (Normative)

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

---

## 12. Emacs Integration (`nostr-publish.el`)

### 12.1 Distribution

* Emacs package name: **`nostr-publish`**
* Distribution target: **MELPA only**
* Repository must include standard package headers and autoloads
* Elisp is a thin orchestration layer that shells out to the CLI

### 12.2 User Command

* Keybinding: `C-c C-p` (user-configurable)
* Interactive command publishes current buffer

### 12.3 Behavior

* If buffer is modified, save before publishing
* Invoke CLI synchronously
* Respect configured timeout
* On success: display event `id` and `pubkey`
* On failure: display the primary fatal error (single message)

---

## 13. Error Handling (Fail-Fast)

### 13.1 Fatal Errors (Abort)

* Invalid frontmatter format
* Unknown frontmatter fields
* Missing required fields
* Invalid metadata types/values
* No relays resolved
* Signer unreachable or times out
* User rejects signing
* `nak` exits non-zero

### 13.2 Error Output Requirements

* Deterministic and human-readable
* Single primary cause
* Non-zero exit from CLI on failure

---

## 14. Testing Requirements

### 14.1 Test Categories

Implementations must include:

1. **Unit tests** (pure logic)

   * frontmatter parse + validation
   * tag mapping determinism
   * relay precedence resolution
   * CLI argument normalization
2. **Integration tests (mandatory, Dockerized)**

   * End-to-end publish and fetch verification against a real relay
   * Use NIP-46 signer (e.g., nak bunker) in container

### 14.2 Emacs Version Support (LTS-Style)

Test matrix must include:

* **Minimum supported Emacs** (floor): **27.1**
* **Latest stable Emacs** available in CI at test time

### 14.3 Integration Test Topology (Normative)

Docker Compose (or equivalent) must provision:

* Relay: `scsibug/nostr-rs-relay`
* Signer: NIP-46 daemon (e.g., nak bunker) configured with a **fixed, known keypair**
* `nak` client (host binary) used to publish
* Test runner (shell/Python/Emacs batch) that:

  1. Publishes an article
  2. Receives event `id`
  3. Queries the relay for the event by `id`
  4. Asserts all of the following:

     * event `kind == 30023`
     * event `content` matches expected body exactly
     * tags match expected set and required ordering rules as applicable
     * event `pubkey` equals the **known fixed pubkey** from the signer container

### 14.4 Determinism Requirements

* Tests must not depend on interactive approval
* All secrets/keys used in tests must be non-production fixtures
* Relay persistence must be isolated per test run (fresh state or unique `slug` per run)

---

## 15. Release & Packaging Requirements (MELPA)

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

## 16. Spec Versioning

* Spec version: **1.0**
* Unknown frontmatter fields are invalid (strict mode)

---

## 17. Acceptance Criteria

A conforming implementation must satisfy:

* Given identical input file and config, wrapper produces identical unsigned event JSON
* Wrapper never handles private keys locally
* Publishing via CLI and via Emacs yields identical published event semantics
* Integration tests prove:

  * publish succeeds via NIP-46 signer + `nak`
  * relay stores the event
  * fetched event matches expected kind/content/tags/pubkey exactly
* Package installs from MELPA and functions with documented dependencies

---
