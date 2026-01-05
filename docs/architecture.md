# Technical Architecture

This document provides technical architecture details for developers working on nostr-publish.

## System Overview

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

## Component Responsibilities

| Component              | Does                                                                                                       | Does NOT                               |
|------------------------|------------------------------------------------------------------------------------------------------------|----------------------------------------|
| Wrapper (Python/Elisp) | Parsing, validation, image processing, Blossom upload, event construction, relay selection, invoking `nak` | Cryptography, signing, NIP-46 protocol |
| `nak`                  | NIP-46 signing, event publishing, Blossom upload                                                           | Frontmatter parsing, validation        |
| Remote signer          | Key custody, user consent                                                                                  | Event construction                     |
| Blossom server         | Blob storage, authenticated uploads                                                                        | Event generation                       |
| Relays                 | Event storage and propagation                                                                              | Validation                             |

## Module Architecture

### Python CLI (`src/nostr_publish/`)

| Module                | Responsibility                                                                 |
|-----------------------|--------------------------------------------------------------------------------|
| `cli.py`              | Argument parsing, orchestration, error handling                                |
| `cli_cover_upload.py` | Cover image upload workflow coordination                                       |
| `models.py`           | Data classes: `Frontmatter`, `ImageMetadata`, `UnsignedEvent`, `PublishResult` |
| `frontmatter.py`      | YAML parsing and extraction from Markdown                                      |
| `validator.py`        | Strict frontmatter validation per spec                                         |
| `event.py`            | NIP-23 event construction, tag building, NIP-92 imeta generation               |
| `image_processing.py` | Image EXIF stripping, resizing, dimension validation                           |
| `image_metadata.py`   | Image metadata parsing and validation                                          |
| `naddr_encoder.py`    | NIP-19 bech32 address encoding                                                 |
| `nak.py`              | External `nak` subprocess invocation, Blossom upload                           |
| `relay.py`            | Relay URL validation, fallback resolution                                      |
| `cli_output.py`       | JSON result formatting                                                         |
| `utils.py`            | Utility functions (deduplication, etc.)                                        |
| `errors.py`           | Custom exception types                                                         |

### Emacs Integration (`nostr-publish.el`)

**Functions:**
- `nostr-publish-mode` - Minor mode providing `C-c C-p` keybinding (also available via `M-x nostr-publish-buffer`)
- `nostr-publish-buffer` - Main publish command
- `nostr-publish-preview-buffer` - Preview mode command
- `nostr-publish--sanitize-yaml-value` - YAML injection prevention
- `nostr-publish--update-cover-frontmatter` - Atomic frontmatter updates

**Configuration Variables:**
- Production: `nostr-publish-bunker-uri`, `nostr-publish-default-relays`, `nostr-publish-timeout`, `nostr-publish-blossom-url`
- Preview: `nostr-publish-preview-relay`, `nostr-publish-preview-bunker`, `nostr-publish-preview-blossom`, `nostr-publish-preview-reader`, `nostr-publish-preview-open-browser`

## Design Patterns

### 1. Composition Over Reimplementation

The tool leverages existing, well-tested components rather than reimplementing standards:
- Delegates cryptography to `nak` (NIP-46 client)
- Uses `nak` for Blossom uploads
- No direct NIP-46 or signing implementation

### 2. Fail-Fast Validation

Frontmatter validation aborts on first error:
- Unknown fields rejected immediately
- Type and value constraint checking before event construction
- Single primary error reported

### 3. Determinism

Same input produces identical output:
- Event tags ordered deterministically per spec
- Reproducible event JSON for signing
- No timestamps, UUIDs, or non-deterministic data

### 4. Idempotent Publishing

Cover image hash comparison prevents re-uploads:
- Hash computed from processed image file (SHA-256)
- Matching hash skips upload, reuses existing URL
- Unchanged images are skipped on republish

### 5. Responsibility Boundaries

Clear separation of concerns:
- Python CLI remains generic and environment-agnostic
- Emacs provides orchestration and UX (preview mode, file updates)
- Preview mode is an invocation profile, not a CLI mode

### 6. Modular Event Construction

Tag building supports arbitrary tag injection:
- Extra tags inserted after `imeta` but before content tags
- Deterministic ordering maintained
- Extensible without breaking core logic

### 7. Configuration Isolation

CLI serves as allowlist for relays:
- Frontmatter relay specs validated against CLI allowlist
- Prevents accidentally publishing to wrong relays

## Data Flow

### Publish Flow

```
1. Parse Markdown file (frontmatter + body)
2. Validate frontmatter (strict, fail-fast)
3. If image.file present:
   a. Process image (EXIF strip, resize, convert to JPEG)
   b. Compute SHA-256 hash
   c. Compare with frontmatter hash (if present)
   d. If hash differs or absent:
      - Upload to Blossom via nak
      - Get URL from response
   e. If hash matches: skip upload, reuse URL
4. Construct unsigned NIP-23 event
   - Build tags in deterministic order
   - Include imeta if cover image present
   - Inject arbitrary tags (if provided)
5. Resolve relay list (CLI as allowlist)
6. If dry-run: output event JSON and exit
7. Sign event via nak + NIP-46
8. Publish to relays via nak
9. Encode naddr (NIP-19)
10. Output JSON result
11. (Emacs only) Update frontmatter with image metadata
```

### Preview Flow

```
Same as Publish Flow, except:
- Uses preview relay (from Emacs config)
- Uses preview bunker (from Emacs config)
- Uses preview Blossom (from Emacs config)
- Injects preview tag: ["x-emacs-nostr-publish", "preview"]
- Skips step 11 (no frontmatter updates)
- Opens preview reader in browser (Emacs)
```

## Event Construction

### Tag Ordering (Deterministic)

```json
{
  "kind": 30023,
  "tags": [
    ["d", "slug"],                                    // identifier
    ["title", "Article Title"],                       // required
    ["summary", "Description"],                       // optional
    ["published_at", "1700000000"],                   // optional
    ["imeta", "url https://...", "mime image/jpeg"],  // optional (cover)
    ["x-emacs-nostr-publish", "preview"],             // injected (preview only)
    ["t", "nostr"], ["t", "writing"]                  // hashtags
  ],
  "content": "# Article body\n\nMarkdown content..."
}
```

### NIP-92 imeta Tag Format

```json
["imeta",
  "url https://cdn.example.com/abc123.jpg",
  "mime image/jpeg",
  "dim 1200x630",
  "alt Cover image description",
  "x abc123def456..."  // SHA-256 hash
]
```

## Image Processing Pipeline

### Processing Steps

```
1. Read local file
2. Load image with Pillow
3. Strip EXIF metadata (privacy)
4. Calculate target dimensions:
   - Default: 1200x630 (configurable via --cover-size)
   - Maintain aspect ratio
   - No upscaling (preserve smaller images)
5. Crop to aspect ratio (center crop)
6. Resize to target dimensions
7. Convert to JPEG (quality: 85)
8. Write to cache: .nostr-publish/cache/covers/<slug>/cover.jpg
9. Compute SHA-256 hash
10. Upload to Blossom via nak
11. Parse upload response (URL)
12. Return metadata: {url, hash, mime, dim}
```

### Cache Management

- Location: `.nostr-publish/cache/covers/<slug>/cover.jpg`
- One cached cover per slug
- Old cache auto-deleted on republish with new image
- Cache survives across publishes with same image (idempotent)

## Security Architecture

### YAML Injection Prevention

**Threat Model:**
- Malicious Blossom servers could return crafted metadata
- Untrusted input could contain YAML structural characters

**Defenses:**
1. Python: Strict validation at parse time (reject newlines, brackets)
2. Emacs: Output sanitization before writing (remove control characters)
3. Nil handling: Graceful handling of nil/null values

### Atomicity Guarantees

**Emacs Buffer Updates:**
- All frontmatter modifications use undo mechanism
- Capture `buffer-undo-list` before changes
- On error: roll back all changes via `primitive-undo`
- Either all changes applied and saved, or all rolled back

### Subprocess Invocation

**Security Requirements:**
- Python: `subprocess.run()` with argument list (not shell)
- Emacs: `call-process` with argument list (not `shell-command`)
- No user input directly interpolated into shell commands
- Timeout enforcement to prevent hanging

## Testing Architecture

### Unit Tests (pytest)

Location: `tests/unit/`

Test categories:
- Frontmatter parsing and validation
- Event construction and tag ordering
- Image processing and metadata
- Relay resolution and allowlist enforcement
- CLI argument parsing
- naddr encoding

### Integration Tests (pytest + Docker)

Location: `tests/integration/`

Docker services:
- `relay`: scsibug/nostr-rs-relay (Nostr relay)
- `bunker`: Custom nak bunker (NIP-46 signer)
- `blossom`: ghcr.io/hzrd149/blossom-server:4.4 (media server)

Test scenarios:
- End-to-end publishing workflow
- Cover image upload and metadata
- naddr integration
- Preview mode with tag injection
- Emacs subprocess communication

### Property-Based Testing

Uses Hypothesis for property-based testing:
- Generates thousands of test cases
- Verifies invariants across edge cases
- Tests include preview mode properties

## Build & Release

### Version Management

Synchronized across three files:
1. `pyproject.toml` - `version = "x.y.z"`
2. `nostr-publish.el` - `;; Version: x.y.z`
3. `src/nostr_publish/__init__.py` - `__version__ = "x.y.z"`

### Distribution

- **PyPI**: `nostr-publish` (Python CLI)
- **MELPA**: `nostr-publish` (Emacs Lisp)
- **GitHub**: `941design/emacs-nostr-publish`

### CI/CD (GitHub Actions)

**test.yml** (on push/PR):
- Lint (ruff)
- Python tests (matrix: 3.11, 3.12, 3.13)
- Emacs tests (matrix: 27.1, 28.2, 29.4)
- Integration tests (Docker)

**release.yml** (on version tag):
- Run test workflow
- Create GitHub release
- Publish to PyPI (trusted publishing)
- MELPA picks up automatically

## References

- [NIP-23: Long-form Content](https://github.com/nostr-protocol/nips/blob/master/23.md)
- [NIP-46: Nostr Connect](https://github.com/nostr-protocol/nips/blob/master/46.md)
- [NIP-92: Image Metadata](https://github.com/nostr-protocol/nips/blob/master/92.md)
- [NIP-19: bech32-encoded entities](https://github.com/nostr-protocol/nips/blob/master/19.md)
- [Blossom: Media server protocol](https://github.com/hzrd149/blossom)
- [nak: Nostr Army Knife](https://github.com/fiatjaf/nak)
