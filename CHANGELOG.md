# Changelog

All notable changes to this project will be documented in this file.

The format is based on [Keep a Changelog](https://keepachangelog.com/en/1.1.0/).

## [Unreleased]

### Added

- **Preview Mode (Emacs)**: Review articles before publishing using preview infrastructure
  - New Emacs command `nostr-publish-preview-buffer` (or `M-x nostr-publish-preview-buffer`)
  - Uses same publishing pipeline as production (validation, signing, image upload)
  - Targets preview relay, bunker, and Blossom server instead of production
  - Adds client annotation tag `x-emacs-nostr-publish: preview` to published events
  - Suppresses frontmatter write-back (source file remains unmodified)
  - Opens preview reader (njump) in browser with naddr (configurable)
  - New configuration variables: `nostr-publish-preview-relay`, `nostr-publish-preview-bunker`, `nostr-publish-preview-blossom`, `nostr-publish-preview-reader`, `nostr-publish-preview-open-browser`

- **NIP-23 image tag for cover images**: Dual-tag emission for client compatibility
  - Emits `["image", url]` tag alongside NIP-92 `imeta` tag when cover image is present
  - Ensures compatibility with clients like njump that expect NIP-23 format
  - Image tag positioned before imeta tag per spec section 7.2

- **Arbitrary tag injection (CLI)**: Add custom tags to events via `--tag KEY VALUE`
  - Repeatable flag for multiple tags
  - Tags positioned after structural metadata, before content tags
  - Enables client-specific annotations without modifying core event construction
  - Used by preview mode to add `x-emacs-nostr-publish: preview` tag

- **Idempotent Blossom uploads**: Check blob existence before uploading
  - Uses `nak blossom check` to verify if blob already exists on server
  - Skips upload when blob with matching hash is already present
  - Reduces redundant uploads during republishing workflow

- **NIP-19 address encoding (naddr)**: Generate shareable article addresses
  - CLI output includes `naddr` field in JSON response after successful publish
  - Emacs displays naddr in success message
  - Non-fatal encoding: publish succeeds even if naddr generation fails
  - Format: `naddr1{bech32-data}` encoding pubkey, kind 30023, and slug
  - Enables cross-relay article discovery and sharing

- **NIP-92 cover image support**: Add `image` field to frontmatter with `imeta` tag generation
  - Accepts URL string or object with `url`, `mime`, `alt`, `dim`, `file`, `hash` properties
  - MIME types inferred from URL extension when not explicitly provided

- **Local cover image upload via Blossom**: Upload local images to Blossom servers
  - New `image.file` field for local image references in frontmatter
  - Image processing: EXIF metadata stripping, resize to configurable dimensions
  - Upload via `nak blossom upload` with NIP-98 authentication
  - CLI flags: `--blossom`, `--blossom-timeout`, `--cover-size`
  - Processed image caching at `.nostr-publish/cache/covers/<slug>/`
  - Auto-cleanup of old cached covers on republish

- **Emacs buffer write-back**: Automatic frontmatter updates after Blossom upload
  - CLI outputs image metadata in JSON response
  - Elisp extracts JSON and updates YAML frontmatter atomically
  - Buffer identity verification prevents race conditions
  - Atomic updates with undo-based rollback on failure
  - New custom variable `nostr-publish-blossom-url`

- **Image validation**: Input validation for cover images
  - Minimum 200px for width and height
  - Aspect ratio constraints between 1:4 and 4:1

- **Dry-run safety**: `--dry-run` skips cover image processing and upload
  - New `--allow-dry-run-without-upload` flag for dry-run with `image.file`

- **Idempotent publishing**: Hash-based upload skip for cover images
  - Frontmatter now accepts `image.file`, `image.url`, and `image.hash` simultaneously
  - SHA-256 hash computed from processed image file
  - First publish uploads to Blossom and writes `hash`/`url` back to frontmatter
  - Second publish with matching hash skips upload, reuses existing URL
  - Changed hash triggers re-upload with new URL
  - Validation security: YAML injection protection for hash/url values

- **Test infrastructure improvements**
  - Blossom server in docker-compose for E2E testing
  - njump preview reader service in docker-compose for local preview testing
  - `.env` configuration for local test stack ports
  - Emacs test targets in Makefile (`test-emacs`, `test-emacs-compile`, `test-emacs-unit`)

### Changed

- **BREAKING: CLI output format changed to JSON** (previously plain text)
  - All publish results now returned as single-line JSON
  - Fields: `event_id`, `pubkey`, `naddr` (optional), `image` (optional)
  - Keys sorted alphabetically for deterministic output
  - Enables Emacs integration and machine parsing
  - Update any scripts parsing CLI output to expect JSON format

- **BREAKING: Frontmatter field renamed: `cover:` → `image:`**
  - The `cover:` field is no longer supported
  - Update all frontmatter to use `image:` instead
  - Validation will reject `cover:` field as unknown
  - Migration: rename all `cover:` to `image:` in existing articles
