"""End-to-end integration tests for Preview Mode.

Tests complete preview workflow:
- CLI invocation with preview tag
- Event construction and signing via bunker
- Preview tag presence in published event
- Image upload in preview mode (Blossom integration)
"""

import json
import os
import subprocess
import tempfile
from pathlib import Path
from typing import Any

# Local constants
FIXTURES_DIR = Path(__file__).parent / "fixtures"
TEST_TIMEOUT = 60


def run_nostr_publish_preview(
    article_path: Path,
    bunker_uri: str,
    relay: str,
    blossom_url: str | None = None,
    timeout: int = TEST_TIMEOUT,
    client_secret: str | None = None,
) -> subprocess.CompletedProcess:
    """Run nostr-publish CLI in preview mode and return completed process."""
    cmd = [
        "nostr-publish",
        str(article_path),
        "--bunker",
        bunker_uri,
        "--relay",
        relay,
        "--tag",
        "x-emacs-nostr-publish",
        "preview",
        "--timeout",
        str(timeout),
    ]

    if blossom_url:
        cmd.extend(["--blossom", blossom_url])

    # Set NOSTR_CLIENT_KEY for nak to use the authorized client key
    env = dict(os.environ)
    if client_secret:
        env["NOSTR_CLIENT_KEY"] = client_secret

    return subprocess.run(cmd, capture_output=True, text=True, timeout=timeout + 10, env=env)


def parse_publish_result(stdout: str) -> dict[str, Any]:
    """Parse JSON publish result from CLI stdout."""
    stdout = stdout.strip()
    if not stdout:
        return {}

    try:
        return json.loads(stdout)
    except json.JSONDecodeError:
        return {}


def fetch_event_from_relay(event_id: str, relay_url: str) -> dict[str, Any] | None:
    """Fetch published event from relay using nak."""
    try:
        result = subprocess.run(["nak", "req", "-i", event_id, relay_url], capture_output=True, text=True, timeout=10)

        if result.returncode != 0:
            return None

        for line in result.stdout.strip().split("\n"):
            if not line.strip():
                continue
            try:
                event = json.loads(line)
                if event.get("id") == event_id:
                    return event
            except json.JSONDecodeError:
                continue

        return None
    except (subprocess.TimeoutExpired, FileNotFoundError, Exception):
        return None


class TestPreviewModeEndToEnd:
    """End-to-end tests for preview mode workflow."""

    def test_preview_complete_workflow(self, docker_services):
        """Test complete preview workflow from CLI through to signed event."""
        # Create test article with all frontmatter fields
        with tempfile.NamedTemporaryFile(mode="w", suffix=".md", delete=False) as f:
            f.write(
                """---
title: Preview Test Article
slug: preview-test-article
summary: Testing preview mode end-to-end
tags:
  - test
  - preview
---

# Preview Test Article

This is a complete preview mode test with all optional fields.
"""
            )
            article_path = Path(f.name)

        try:
            # Run preview publish
            result = run_nostr_publish_preview(
                article_path=article_path,
                bunker_uri=docker_services["bunker_uri"],
                relay=docker_services["relay_url"],
                client_secret=docker_services["client_secret"],
            )

            # Verify success
            assert result.returncode == 0, f"Preview publish failed: {result.stderr}"

            # Parse result
            output = parse_publish_result(result.stdout)
            assert "event_id" in output
            assert "pubkey" in output
            assert "naddr" in output

            # Fetch event from relay
            event = fetch_event_from_relay(output["event_id"], docker_services["relay_url"])
            assert event is not None, "Event not found on relay"

            # Verify event structure
            assert event["kind"] == 30023
            assert event["pubkey"] == docker_services["bunker_pubkey"]

            # Verify preview tag is present
            preview_tag = ["x-emacs-nostr-publish", "preview"]
            assert preview_tag in event["tags"], "Preview tag not found in event"

            # Verify structural tags
            tag_keys = [tag[0] for tag in event["tags"]]
            assert "d" in tag_keys
            assert "title" in tag_keys
            assert "summary" in tag_keys
            assert "t" in tag_keys  # Content tags

            # Verify preview tag appears after structural metadata
            preview_index = next(i for i, tag in enumerate(event["tags"]) if tag[0] == "x-emacs-nostr-publish")
            d_index = next(i for i, tag in enumerate(event["tags"]) if tag[0] == "d")
            title_index = next(i for i, tag in enumerate(event["tags"]) if tag[0] == "title")

            assert d_index < preview_index
            assert title_index < preview_index

        finally:
            # Cleanup
            article_path.unlink(missing_ok=True)

    def test_preview_with_image_upload(self, docker_services):
        """Test preview mode with image upload via Blossom."""
        # Use existing test image from fixtures
        image_path = FIXTURES_DIR / "test-cover-with-exif.jpg"
        assert image_path.exists(), f"Test image not found: {image_path}"

        # Create article with image.file
        with tempfile.NamedTemporaryFile(mode="w", suffix=".md", delete=False) as f:
            f.write(
                f"""---
title: Preview with Image
slug: preview-image-test
image:
  file: {image_path}
tags:
  - preview
  - media
---

# Preview with Image

Testing preview mode with Blossom image upload.
"""
            )
            article_path = Path(f.name)

        try:
            # Run preview publish with Blossom
            result = run_nostr_publish_preview(
                article_path=article_path,
                bunker_uri=docker_services["bunker_uri"],
                relay=docker_services["relay_url"],
                blossom_url=docker_services.get("blossom_url"),
                client_secret=docker_services["client_secret"],
            )

            # Verify success
            assert result.returncode == 0, f"Preview with image failed: {result.stderr}"

            # Parse result
            output = parse_publish_result(result.stdout)
            assert "event_id" in output

            # Fetch event from relay
            event = fetch_event_from_relay(output["event_id"], docker_services["relay_url"])
            assert event is not None

            # Verify preview tag present
            assert ["x-emacs-nostr-publish", "preview"] in event["tags"]

            # Verify imeta tag present (image was uploaded)
            imeta_tags = [tag for tag in event["tags"] if tag[0] == "imeta"]
            assert len(imeta_tags) > 0, "Image metadata tag not found"

            # Verify tag ordering: structural metadata, imeta, preview tag, content tags
            preview_index = next(i for i, tag in enumerate(event["tags"]) if tag[0] == "x-emacs-nostr-publish")
            imeta_index = next(i for i, tag in enumerate(event["tags"]) if tag[0] == "imeta")

            assert imeta_index < preview_index, "Preview tag should come after imeta"

        finally:
            # Cleanup - only delete the article, not the fixture image
            article_path.unlink(missing_ok=True)

    def test_preview_with_image_produces_image_tag(self, docker_services):
        """Test preview mode produces NIP-23 image tag for njump compatibility.

        Property: Preview event includes ["image", url] alongside imeta for clients like njump.
        """
        # Use existing test image from fixtures
        image_path = FIXTURES_DIR / "test-cover-with-exif.jpg"
        assert image_path.exists(), f"Test image not found: {image_path}"

        # Create article with image.file
        with tempfile.NamedTemporaryFile(mode="w", suffix=".md", delete=False) as f:
            f.write(
                f"""---
title: Preview Image Tag Test
slug: preview-image-tag-test
image:
  file: {image_path}
tags:
  - preview
---

# Preview Image Tag Test

Testing that preview mode includes NIP-23 image tag.
"""
            )
            article_path = Path(f.name)

        try:
            # Run preview publish with Blossom
            result = run_nostr_publish_preview(
                article_path=article_path,
                bunker_uri=docker_services["bunker_uri"],
                relay=docker_services["relay_url"],
                blossom_url=docker_services.get("blossom_url"),
                client_secret=docker_services["client_secret"],
            )

            # Verify success
            assert result.returncode == 0, f"Preview failed: {result.stderr}"

            # Parse result
            output = parse_publish_result(result.stdout)
            assert "event_id" in output

            # Fetch event from relay
            event = fetch_event_from_relay(output["event_id"], docker_services["relay_url"])
            assert event is not None

            # Verify image tag present (NIP-23 simple format)
            image_tags = [tag for tag in event["tags"] if tag[0] == "image"]
            assert len(image_tags) == 1, f"Expected 1 image tag, found {len(image_tags)}: {event['tags']}"

            # Verify imeta tag also present (NIP-92 rich format)
            imeta_tags = [tag for tag in event["tags"] if tag[0] == "imeta"]
            assert len(imeta_tags) == 1, f"Expected 1 imeta tag, found {len(imeta_tags)}"

            # Verify both tags reference the same URL
            image_url = image_tags[0][1]
            imeta_url_elem = next((e for e in imeta_tags[0] if e.startswith("url ")), None)
            assert imeta_url_elem is not None, "imeta tag missing url element"
            imeta_url = imeta_url_elem.split(" ", 1)[1]
            assert image_url == imeta_url, f"URL mismatch: image={image_url}, imeta={imeta_url}"

            # Verify ordering: image before imeta before preview tag
            image_index = next(i for i, tag in enumerate(event["tags"]) if tag[0] == "image")
            imeta_index = next(i for i, tag in enumerate(event["tags"]) if tag[0] == "imeta")
            preview_index = next(i for i, tag in enumerate(event["tags"]) if tag[0] == "x-emacs-nostr-publish")

            assert image_index < imeta_index, "image tag should come before imeta"
            assert imeta_index < preview_index, "imeta tag should come before preview tag"

        finally:
            article_path.unlink(missing_ok=True)

    def test_preview_minimal_frontmatter(self, docker_services):
        """Test preview mode with minimal frontmatter (title and slug only)."""
        with tempfile.NamedTemporaryFile(mode="w", suffix=".md", delete=False) as f:
            f.write(
                """---
title: Minimal Preview
slug: minimal-preview
---

# Minimal Preview

Testing preview with minimal frontmatter.
"""
            )
            article_path = Path(f.name)

        try:
            result = run_nostr_publish_preview(
                article_path=article_path,
                bunker_uri=docker_services["bunker_uri"],
                relay=docker_services["relay_url"],
                client_secret=docker_services["client_secret"],
            )

            assert result.returncode == 0, f"Minimal preview failed: {result.stderr}"

            output = parse_publish_result(result.stdout)
            event = fetch_event_from_relay(output["event_id"], docker_services["relay_url"])
            assert event is not None

            # Verify preview tag present even with minimal frontmatter
            assert ["x-emacs-nostr-publish", "preview"] in event["tags"]

            # Verify only required structural tags + preview tag
            tag_keys = [tag[0] for tag in event["tags"]]
            assert "d" in tag_keys
            assert "title" in tag_keys
            assert "x-emacs-nostr-publish" in tag_keys

        finally:
            article_path.unlink(missing_ok=True)

    def test_preview_multiple_extra_tags(self, docker_services):
        """Test preview mode supports multiple arbitrary tags."""
        with tempfile.NamedTemporaryFile(mode="w", suffix=".md", delete=False) as f:
            f.write(
                """---
title: Multiple Tags Test
slug: multi-tag-preview
---

# Multiple Tags

Testing multiple extra tags in preview mode.
"""
            )
            article_path = Path(f.name)

        try:
            # Run with multiple --tag arguments
            cmd = [
                "nostr-publish",
                str(article_path),
                "--bunker",
                docker_services["bunker_uri"],
                "--relay",
                docker_services["relay_url"],
                "--tag",
                "x-emacs-nostr-publish",
                "preview",
                "--tag",
                "client",
                "emacs",
                "--tag",
                "client-version",
                "0.1.0",
                "--timeout",
                str(TEST_TIMEOUT),
            ]

            # Set NOSTR_CLIENT_KEY for nak to use the authorized client key
            env = dict(os.environ)
            env["NOSTR_CLIENT_KEY"] = docker_services["client_secret"]

            result = subprocess.run(cmd, capture_output=True, text=True, timeout=TEST_TIMEOUT + 10, env=env)

            assert result.returncode == 0, f"Multi-tag preview failed: {result.stderr}"

            output = parse_publish_result(result.stdout)
            event = fetch_event_from_relay(output["event_id"], docker_services["relay_url"])
            assert event is not None

            # Verify all extra tags present
            assert ["x-emacs-nostr-publish", "preview"] in event["tags"]
            assert ["client", "emacs"] in event["tags"]
            assert ["client-version", "0.1.0"] in event["tags"]

            # Verify extra tags appear in order
            extra_indices = [
                next(i for i, t in enumerate(event["tags"]) if t == ["x-emacs-nostr-publish", "preview"]),
                next(i for i, t in enumerate(event["tags"]) if t == ["client", "emacs"]),
                next(i for i, t in enumerate(event["tags"]) if t == ["client-version", "0.1.0"]),
            ]
            assert extra_indices == sorted(extra_indices), "Extra tags not in specified order"

        finally:
            article_path.unlink(missing_ok=True)

    def test_preview_naddr_includes_relay_hint(self, docker_services):
        """Test that preview naddr includes the preview relay as hint (spec section 10)."""
        with tempfile.NamedTemporaryFile(mode="w", suffix=".md", delete=False) as f:
            f.write(
                """---
title: naddr Relay Hint Test
slug: naddr-relay-hint-test
---

# naddr Relay Hint

Testing that naddr includes relay hint.
"""
            )
            article_path = Path(f.name)

        try:
            result = run_nostr_publish_preview(
                article_path=article_path,
                bunker_uri=docker_services["bunker_uri"],
                relay=docker_services["relay_url"],
                client_secret=docker_services["client_secret"],
            )

            assert result.returncode == 0, f"Preview failed: {result.stderr}"

            output = parse_publish_result(result.stdout)
            naddr = output.get("naddr", "")

            # Verify naddr is valid and non-empty
            assert naddr.startswith("naddr1"), f"Invalid naddr format: {naddr}"
            assert len(naddr) > 20, "naddr seems too short to contain relay hint"

        finally:
            article_path.unlink(missing_ok=True)


class TestPreviewFileImmutability:
    """Tests verifying preview mode never modifies source files (spec section 12)."""

    def test_preview_does_not_modify_source_file(self, docker_services):
        """Test that source file content is unchanged after preview publish."""
        original_content = """---
title: File Immutability Test
slug: file-immutability-test
summary: Testing that preview doesn't modify files
tags:
  - test
---

# File Immutability Test

This file should remain exactly as written.
"""
        with tempfile.NamedTemporaryFile(mode="w", suffix=".md", delete=False) as f:
            f.write(original_content)
            article_path = Path(f.name)

        try:
            # Run preview
            result = run_nostr_publish_preview(
                article_path=article_path,
                bunker_uri=docker_services["bunker_uri"],
                relay=docker_services["relay_url"],
                client_secret=docker_services["client_secret"],
            )

            assert result.returncode == 0, f"Preview failed: {result.stderr}"

            # Read file after preview
            with open(article_path) as f:
                final_content = f.read()

            # Content must be identical
            assert final_content == original_content, "Source file was modified during preview"

            # Specifically check no naddr was written
            assert "naddr:" not in final_content, "naddr was written to file"

        finally:
            article_path.unlink(missing_ok=True)

    def test_preview_with_image_does_not_modify_source_file(self, docker_services):
        """Test that preview with image upload doesn't write hash/url back to source."""
        image_path = FIXTURES_DIR / "test-cover-with-exif.jpg"
        assert image_path.exists(), f"Test image not found: {image_path}"

        original_content = f"""---
title: Image Immutability Test
slug: image-immutability-test
image:
  file: {image_path}
  alt: Test image
tags:
  - test
---

# Image Immutability Test

Image metadata should NOT be written back to this file.
"""
        with tempfile.NamedTemporaryFile(mode="w", suffix=".md", delete=False) as f:
            f.write(original_content)
            article_path = Path(f.name)

        try:
            result = run_nostr_publish_preview(
                article_path=article_path,
                bunker_uri=docker_services["bunker_uri"],
                relay=docker_services["relay_url"],
                blossom_url=docker_services.get("blossom_url"),
                client_secret=docker_services["client_secret"],
            )

            assert result.returncode == 0, f"Preview with image failed: {result.stderr}"

            # Read file after preview
            with open(article_path) as f:
                final_content = f.read()

            # Content must be identical
            assert final_content == original_content, "Source file was modified during preview"

            # Specifically check no hash/url was written
            assert "  hash:" not in final_content, "hash was written to file"
            assert "  url:" not in final_content, "url was written to file"
            assert "naddr:" not in final_content, "naddr was written to file"

        finally:
            article_path.unlink(missing_ok=True)


class TestPreviewErrorHandling:
    """Error handling in preview is identical to publish (spec section 13)."""

    def test_preview_validation_failure_exits_nonzero(self, docker_services):
        """Preview with invalid frontmatter fails with non-zero exit code."""
        # Missing required 'title' field
        with tempfile.NamedTemporaryFile(mode="w", suffix=".md", delete=False) as f:
            f.write(
                """---
slug: missing-title
---

# Missing Title

This should fail validation.
"""
            )
            article_path = Path(f.name)

        try:
            result = run_nostr_publish_preview(
                article_path=article_path,
                bunker_uri=docker_services["bunker_uri"],
                relay=docker_services["relay_url"],
                client_secret=docker_services["client_secret"],
            )

            # Should fail
            assert result.returncode != 0, "Should have failed with missing title"
            assert "title" in result.stderr.lower() or "required" in result.stderr.lower()

        finally:
            article_path.unlink(missing_ok=True)

    def test_preview_missing_bunker_fails(self, docker_services):
        """Preview without bunker URI fails."""
        with tempfile.NamedTemporaryFile(mode="w", suffix=".md", delete=False) as f:
            f.write(
                """---
title: No Bunker Test
slug: no-bunker-test
---

Content.
"""
            )
            article_path = Path(f.name)

        try:
            # Run without bunker
            cmd = [
                "nostr-publish",
                str(article_path),
                "--relay",
                docker_services["relay_url"],
                "--tag",
                "x-emacs-nostr-publish",
                "preview",
                "--timeout",
                "10",
            ]

            result = subprocess.run(cmd, capture_output=True, text=True, timeout=20)

            # Should fail
            assert result.returncode != 0, "Should have failed without bunker"

        finally:
            article_path.unlink(missing_ok=True)

    def test_preview_missing_relay_fails(self, docker_services):
        """Preview without relay fails."""
        with tempfile.NamedTemporaryFile(mode="w", suffix=".md", delete=False) as f:
            f.write(
                """---
title: No Relay Test
slug: no-relay-test
---

Content.
"""
            )
            article_path = Path(f.name)

        try:
            # Run without relay
            cmd = [
                "nostr-publish",
                str(article_path),
                "--bunker",
                docker_services["bunker_uri"],
                "--tag",
                "x-emacs-nostr-publish",
                "preview",
                "--timeout",
                "10",
            ]

            result = subprocess.run(cmd, capture_output=True, text=True, timeout=20)

            # Should fail
            assert result.returncode != 0, "Should have failed without relay"

        finally:
            article_path.unlink(missing_ok=True)

    def test_preview_invalid_relay_url_fails(self, docker_services):
        """Preview with invalid relay URL fails."""
        with tempfile.NamedTemporaryFile(mode="w", suffix=".md", delete=False) as f:
            f.write(
                """---
title: Invalid Relay Test
slug: invalid-relay-test
---

Content.
"""
            )
            article_path = Path(f.name)

        try:
            cmd = [
                "nostr-publish",
                str(article_path),
                "--bunker",
                docker_services["bunker_uri"],
                "--relay",
                "http://invalid.relay.com",  # HTTP not WS
                "--tag",
                "x-emacs-nostr-publish",
                "preview",
                "--timeout",
                "10",
            ]

            result = subprocess.run(cmd, capture_output=True, text=True, timeout=20)

            # Should fail
            assert result.returncode != 0, "Should have failed with invalid relay URL"

        finally:
            article_path.unlink(missing_ok=True)


class TestPreviewSingleRelayConstraint:
    """Preview uses exactly one relay (spec section 6)."""

    def test_preview_publishes_to_single_relay(self, docker_services):
        """Preview publishes only to the provided relay."""
        with tempfile.NamedTemporaryFile(mode="w", suffix=".md", delete=False) as f:
            f.write(
                """---
title: Single Relay Test
slug: single-relay-test
---

# Single Relay

Testing single relay constraint.
"""
            )
            article_path = Path(f.name)

        try:
            result = run_nostr_publish_preview(
                article_path=article_path,
                bunker_uri=docker_services["bunker_uri"],
                relay=docker_services["relay_url"],
                client_secret=docker_services["client_secret"],
            )

            assert result.returncode == 0, f"Preview failed: {result.stderr}"

            # Verify event is on the relay
            output = parse_publish_result(result.stdout)
            event = fetch_event_from_relay(output["event_id"], docker_services["relay_url"])
            assert event is not None, "Event not found on the specified relay"

        finally:
            article_path.unlink(missing_ok=True)


class TestPreviewImageUrlDirectlySpecified:
    """Tests for image.url specified directly in frontmatter (not via file upload)."""

    def test_preview_with_direct_image_url_produces_image_tag(self, docker_services):
        """Test preview mode publishes image tag when image.url is specified directly.

        Property: When frontmatter specifies image.url directly (not image.file),
        the published event MUST contain ["image", url] tag with the specified URL.

        This tests a code path different from Blossom upload:
        - No --blossom flag needed
        - URL comes directly from frontmatter, not upload result
        - Both image and imeta tags should reference the frontmatter URL
        """
        test_image_url = "https://example.com/test-cover-image.jpg"

        with tempfile.NamedTemporaryFile(mode="w", suffix=".md", delete=False) as f:
            f.write(
                f"""---
title: Direct Image URL Test
slug: direct-image-url-test
image: {test_image_url}
tags:
  - preview
---

# Direct Image URL Test

Testing that image.url specified directly in frontmatter is published.
"""
            )
            article_path = Path(f.name)

        try:
            # Run preview WITHOUT --blossom (not needed for direct URL)
            result = run_nostr_publish_preview(
                article_path=article_path,
                bunker_uri=docker_services["bunker_uri"],
                relay=docker_services["relay_url"],
                blossom_url=None,  # Explicitly no Blossom
                client_secret=docker_services["client_secret"],
            )

            # Verify success
            assert result.returncode == 0, f"Preview failed: {result.stderr}"

            # Parse result
            output = parse_publish_result(result.stdout)
            assert "event_id" in output

            # Fetch event from relay
            event = fetch_event_from_relay(output["event_id"], docker_services["relay_url"])
            assert event is not None, "Event not found on relay"

            # Verify image tag present with the exact URL from frontmatter
            image_tags = [tag for tag in event["tags"] if tag[0] == "image"]
            assert len(image_tags) == 1, f"Expected 1 image tag, found {len(image_tags)}: {event['tags']}"

            image_tag = image_tags[0]
            assert len(image_tag) == 2, f"image tag should have exactly 2 elements: {image_tag}"
            assert image_tag[1] == test_image_url, (
                f"image tag URL mismatch: expected {test_image_url}, got {image_tag[1]}"
            )
            assert image_tag[1] is not None, "image tag URL is None"

            # Verify imeta tag also present with the URL
            imeta_tags = [tag for tag in event["tags"] if tag[0] == "imeta"]
            assert len(imeta_tags) == 1, f"Expected 1 imeta tag, found {len(imeta_tags)}"

            imeta_url_elem = next((e for e in imeta_tags[0] if e.startswith("url ")), None)
            assert imeta_url_elem is not None, "imeta tag missing url element"
            imeta_url = imeta_url_elem.split(" ", 1)[1]
            assert imeta_url == test_image_url, f"imeta URL mismatch: expected {test_image_url}, got {imeta_url}"

        finally:
            article_path.unlink(missing_ok=True)

    def test_preview_with_image_url_object_produces_image_tag(self, docker_services):
        """Test preview mode publishes image tag when image.url is in object format.

        Property: When frontmatter specifies image: {url: ...} (object format),
        the published event MUST contain ["image", url] tag.
        """
        test_image_url = "https://example.com/cover-object-format.jpg"

        with tempfile.NamedTemporaryFile(mode="w", suffix=".md", delete=False) as f:
            f.write(
                f"""---
title: Image URL Object Test
slug: image-url-object-test
image:
  url: {test_image_url}
  alt: Test cover description
  mime: image/jpeg
  dim: 1200x630
tags:
  - preview
---

# Image URL Object Test

Testing that image object with url field is published correctly.
"""
            )
            article_path = Path(f.name)

        try:
            # Run preview WITHOUT --blossom
            result = run_nostr_publish_preview(
                article_path=article_path,
                bunker_uri=docker_services["bunker_uri"],
                relay=docker_services["relay_url"],
                blossom_url=None,
                client_secret=docker_services["client_secret"],
            )

            assert result.returncode == 0, f"Preview failed: {result.stderr}"

            output = parse_publish_result(result.stdout)
            event = fetch_event_from_relay(output["event_id"], docker_services["relay_url"])
            assert event is not None

            # Verify image tag present with exact URL
            image_tags = [tag for tag in event["tags"] if tag[0] == "image"]
            assert len(image_tags) == 1, f"Expected 1 image tag, found {len(image_tags)}: {event['tags']}"
            assert image_tags[0][1] == test_image_url, f"image tag URL wrong: {image_tags[0]}"
            assert image_tags[0][1] is not None, "image tag URL is None"

            # Verify imeta contains all provided metadata
            imeta_tags = [tag for tag in event["tags"] if tag[0] == "imeta"]
            assert len(imeta_tags) == 1

            imeta = imeta_tags[0]
            assert f"url {test_image_url}" in imeta, f"imeta missing url: {imeta}"
            assert "m image/jpeg" in imeta, f"imeta missing mime: {imeta}"
            assert "alt Test cover description" in imeta, f"imeta missing alt: {imeta}"
            assert "dim 1200x630" in imeta, f"imeta missing dim: {imeta}"

        finally:
            article_path.unlink(missing_ok=True)


class TestPreviewMissingImageTag:
    """Tests for missing NIP-23 image tag bug.

    BUG: When an image is specified, the published event contains imeta tag
    but is MISSING the NIP-23 ["image", url] tag required for njump compatibility.

    Per spec section 5.7 and 7.2, BOTH tags must be present:
    1. ["image", "<URL>"] - NIP-23 simple format
    2. ["imeta", "url <URL>", ...] - NIP-92 rich metadata
    """

    def test_preview_with_image_file_must_include_image_tag(self, docker_services):
        """Test that preview with image.file includes NIP-23 image tag.

        BUG REPRODUCTION: Published event has imeta tag but NO image tag.
        The build_tags function only adds imeta, missing the image tag entirely.

        EXPECTED: Event contains ["image", url] tag alongside imeta tag.
        """
        image_path = FIXTURES_DIR / "test-cover-with-exif.jpg"
        assert image_path.exists(), f"Test image not found: {image_path}"

        with tempfile.NamedTemporaryFile(mode="w", suffix=".md", delete=False) as f:
            f.write(
                f"""---
title: Missing Image Tag Test
slug: missing-image-tag-test
image:
  file: {image_path}
tags:
  - test
---

# Missing Image Tag Test

This article should have BOTH image and imeta tags in the published event.
"""
            )
            article_path = Path(f.name)

        try:
            result = run_nostr_publish_preview(
                article_path=article_path,
                bunker_uri=docker_services["bunker_uri"],
                relay=docker_services["relay_url"],
                blossom_url=docker_services.get("blossom_url"),
                client_secret=docker_services["client_secret"],
            )

            assert result.returncode == 0, f"Preview failed: {result.stderr}"
            output = parse_publish_result(result.stdout)
            assert "event_id" in output

            event = fetch_event_from_relay(output["event_id"], docker_services["relay_url"])
            assert event is not None, "Event not found on relay"

            # Verify imeta tag is present (this passes - imeta works)
            imeta_tags = [tag for tag in event["tags"] if tag[0] == "imeta"]
            assert len(imeta_tags) == 1, f"Expected 1 imeta tag, found {len(imeta_tags)}"

            # BUG: image tag is MISSING
            # Per spec section 5.7: "two tags are added... NIP-23 simple format"
            image_tags = [tag for tag in event["tags"] if tag[0] == "image"]
            assert len(image_tags) == 1, (
                f"BUG: Expected 1 NIP-23 image tag, found {len(image_tags)}. "
                f"Event only has imeta but missing ['image', url] tag. "
                f"All tags: {event['tags']}"
            )

            # Verify image tag has correct URL
            image_url = image_tags[0][1]
            assert image_url is not None, "image tag URL is None"
            assert image_url.startswith("http"), f"image tag URL invalid: {image_url}"

            # Verify both tags reference the same URL
            imeta_url_elem = next((e for e in imeta_tags[0] if e.startswith("url ")), None)
            imeta_url = imeta_url_elem.split(" ", 1)[1] if imeta_url_elem else None
            assert image_url == imeta_url, f"URL mismatch: image={image_url}, imeta={imeta_url}"

        finally:
            article_path.unlink(missing_ok=True)
