"""End-to-end integration tests for cover.file upload to Blossom.

Tests complete cover image workflow: EXIF strip → resize → Blossom upload → event construction.

CONTRACT:
  Test Environment:
    - Docker Compose provides: relay + nak bunker + Blossom server
    - Fixed test keypair: secret key '1', pubkey 79be667ef...
    - Blossom server at http://localhost:3000
    - Test image with EXIF metadata in fixtures/

  Test Properties:
    - EXIF metadata stripped from uploaded image
    - Image resized to target dimensions
    - Blossom upload returns sha256 hash and URL
    - CLI outputs Cover: {JSON} for Emacs buffer update
    - Published event contains imeta tag with correct metadata
"""

import json
import os
import re
import subprocess
import time
from pathlib import Path
from typing import Any

# Local constants (fixture-independent)
FIXTURES_DIR = Path(__file__).parent / "fixtures"
TEST_TIMEOUT = 60


def run_nostr_publish_with_blossom(
    article_path: Path,
    bunker_uri: str | None = None,
    relays: list[str] | None = None,
    blossom_url: str | None = None,
    cover_size: str = "1200x630",
    timeout: int = TEST_TIMEOUT,
    client_secret: str | None = None,
) -> subprocess.CompletedProcess:
    """Run nostr-publish CLI with Blossom upload support."""
    cmd = ["nostr-publish", str(article_path)]

    if bunker_uri:
        cmd.extend(["--bunker", bunker_uri])

    if relays:
        for relay in relays:
            cmd.extend(["--relay", relay])

    if blossom_url:
        cmd.extend(["--blossom", blossom_url])

    cmd.extend(["--cover-size", cover_size])
    cmd.extend(["--timeout", str(timeout)])

    # Set NOSTR_CLIENT_KEY for nak to use the authorized client key
    env = dict(os.environ)
    if client_secret:
        env["NOSTR_CLIENT_KEY"] = client_secret

    return subprocess.run(cmd, capture_output=True, text=True, timeout=timeout + 30, env=env)


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


def parse_publish_result(stdout: str) -> dict[str, Any]:
    """Parse JSON publish result from CLI stdout.

    Returns dict with event_id, pubkey, and optionally naddr and cover.
    """
    stdout = stdout.strip()
    if not stdout:
        return {}

    try:
        return json.loads(stdout)
    except json.JSONDecodeError:
        return {}


def fetch_blob_from_blossom(blob_url: str) -> bytes | None:
    """Fetch blob content from Blossom server."""
    try:
        result = subprocess.run(["curl", "-s", blob_url], capture_output=True, timeout=30)

        if result.returncode == 0 and len(result.stdout) > 0:
            return result.stdout

        return None
    except (subprocess.TimeoutExpired, FileNotFoundError, Exception):
        return None


class TestCoverUploadWorkflow:
    """End-to-end tests for cover.file upload workflow."""

    def test_cover_file_upload_success(self, docker_services):
        """Test complete cover.file upload workflow.

        Property: Cover image is processed, uploaded to Blossom, and metadata returned.
        """
        article_path = FIXTURES_DIR / "article-with-cover-file.md"
        assert article_path.exists(), f"Test fixture not found: {article_path}"

        # Verify test image exists
        test_image = FIXTURES_DIR / "test-cover-with-exif.jpg"
        assert test_image.exists(), f"Test image not found: {test_image}"

        result = run_nostr_publish_with_blossom(
            article_path,
            bunker_uri=docker_services["bunker_uri"],
            relays=[docker_services["relay_url"]],
            blossom_url=docker_services["blossom_url"],
            client_secret=docker_services["client_secret"],
        )

        assert result.returncode == 0, f"CLI failed: {result.stderr}"

        # Parse JSON output
        publish_result = parse_publish_result(result.stdout)
        assert "image" in publish_result, f"Image metadata not in output. Output: {result.stdout}"

        cover_metadata = publish_result["image"]

        # Verify cover metadata structure
        assert "hash" in cover_metadata, "Cover metadata missing 'hash'"
        assert "url" in cover_metadata, "Cover metadata missing 'url'"
        assert "dim" in cover_metadata, "Cover metadata missing 'dim'"
        assert "mime" in cover_metadata, "Cover metadata missing 'mime'"

        # Verify hash is valid SHA256
        assert len(cover_metadata["hash"]) == 64, f"Invalid hash length: {len(cover_metadata['hash'])}"
        assert all(c in "0123456789abcdef" for c in cover_metadata["hash"]), "Hash contains invalid characters"

        # Verify URL is valid
        assert cover_metadata["url"].startswith("http"), f"Invalid URL: {cover_metadata['url']}"

        # Verify dimensions format
        assert re.match(r"^\d+x\d+$", cover_metadata["dim"]), f"Invalid dim format: {cover_metadata['dim']}"

        # Verify MIME type
        assert cover_metadata["mime"] == "image/jpeg", f"Unexpected MIME: {cover_metadata['mime']}"

        # Verify event_id and pubkey in JSON output
        assert "event_id" in publish_result, "Output missing event_id"
        assert "pubkey" in publish_result, "Output missing pubkey"

    def test_cover_file_upload_produces_imeta_tag(self, docker_services):
        """Test that published event contains imeta tag with cover metadata.

        Property: Event imeta tag contains url, hash, dim, and mime from upload result.
        """
        article_path = FIXTURES_DIR / "article-with-cover-file.md"

        result = run_nostr_publish_with_blossom(
            article_path,
            bunker_uri=docker_services["bunker_uri"],
            relays=[docker_services["relay_url"]],
            blossom_url=docker_services["blossom_url"],
            client_secret=docker_services["client_secret"],
        )

        assert result.returncode == 0, f"CLI failed: {result.stderr}"

        # Parse JSON output
        publish_result = parse_publish_result(result.stdout)
        assert "event_id" in publish_result, "Output missing event_id"
        assert "image" in publish_result, "Output missing image metadata"

        event_id = publish_result["event_id"]
        cover_metadata = publish_result["image"]

        # Wait for relay
        time.sleep(2)

        # Fetch event from relay
        event = fetch_event_from_relay(event_id, docker_services["relay_url"])
        assert event is not None, f"Event {event_id} not found in relay"

        # Find imeta tag
        imeta_tags = [tag for tag in event["tags"] if tag[0] == "imeta"]
        assert len(imeta_tags) == 1, f"Expected 1 imeta tag, found {len(imeta_tags)}"

        imeta_tag = imeta_tags[0]

        # Verify imeta contains uploaded metadata
        url_elem = f"url {cover_metadata['url']}"
        assert url_elem in imeta_tag, f"imeta missing url: {imeta_tag}"

        dim_elem = f"dim {cover_metadata['dim']}"
        assert dim_elem in imeta_tag, f"imeta missing dim: {imeta_tag}"

        mime_elem = f"m {cover_metadata['mime']}"
        assert mime_elem in imeta_tag, f"imeta missing mime: {imeta_tag}"

        # Verify hash (x field in NIP-92)
        hash_elem = f"x {cover_metadata['hash']}"
        assert hash_elem in imeta_tag, f"imeta missing hash: {imeta_tag}"

    def test_cover_file_upload_produces_image_tag(self, docker_services):
        """Test that published event contains NIP-23 image tag for njump compatibility.

        Property: Event includes ["image", url] tag alongside imeta for clients like njump.

        NIP-23 specifies the simple ["image", url] format for article cover images.
        This ensures compatibility with clients that don't support NIP-92 imeta format.
        """
        article_path = FIXTURES_DIR / "article-with-cover-file.md"

        result = run_nostr_publish_with_blossom(
            article_path,
            bunker_uri=docker_services["bunker_uri"],
            relays=[docker_services["relay_url"]],
            blossom_url=docker_services["blossom_url"],
            client_secret=docker_services["client_secret"],
        )

        assert result.returncode == 0, f"CLI failed: {result.stderr}"

        # Parse JSON output
        publish_result = parse_publish_result(result.stdout)
        assert "event_id" in publish_result, "Output missing event_id"
        assert "image" in publish_result, "Output missing image metadata"

        event_id = publish_result["event_id"]
        cover_metadata = publish_result["image"]

        # Wait for relay
        time.sleep(2)

        # Fetch event from relay
        event = fetch_event_from_relay(event_id, docker_services["relay_url"])
        assert event is not None, f"Event {event_id} not found in relay"

        # Find image tag (NIP-23 simple format)
        image_tags = [tag for tag in event["tags"] if tag[0] == "image"]
        assert len(image_tags) == 1, f"Expected 1 image tag, found {len(image_tags)}: {event['tags']}"

        image_tag = image_tags[0]

        # Verify image tag contains the URL directly
        assert len(image_tag) == 2, f"image tag should have exactly 2 elements: {image_tag}"
        assert image_tag[1] == cover_metadata["url"], (
            f"image tag URL mismatch: {image_tag[1]} != {cover_metadata['url']}"
        )

        # Verify image tag comes before imeta tag
        image_index = next(i for i, tag in enumerate(event["tags"]) if tag[0] == "image")
        imeta_index = next(i for i, tag in enumerate(event["tags"]) if tag[0] == "imeta")
        assert image_index < imeta_index, "image tag should come before imeta tag"

    def test_cover_file_blob_accessible(self, docker_services):
        """Test that uploaded blob is accessible from Blossom URL.

        Property: Blob can be fetched from the URL returned by upload.
        """
        article_path = FIXTURES_DIR / "article-with-cover-file.md"

        result = run_nostr_publish_with_blossom(
            article_path,
            bunker_uri=docker_services["bunker_uri"],
            relays=[docker_services["relay_url"]],
            blossom_url=docker_services["blossom_url"],
            client_secret=docker_services["client_secret"],
        )

        assert result.returncode == 0, f"CLI failed: {result.stderr}"

        # Parse JSON output and extract image URL
        publish_result = parse_publish_result(result.stdout)
        assert "image" in publish_result, "Output missing image metadata"

        blob_url = publish_result["image"]["url"]

        # Fetch blob
        blob_content = fetch_blob_from_blossom(blob_url)
        assert blob_content is not None, f"Failed to fetch blob from {blob_url}"
        assert len(blob_content) > 0, "Blob content is empty"

        # Verify it's a JPEG (starts with FFD8)
        assert blob_content[:2] == b"\xff\xd8", "Blob is not a valid JPEG"

    def test_cover_file_exif_stripped(self, docker_services):
        """Test that EXIF metadata is stripped from uploaded image.

        Property: Uploaded image has no GPS or camera metadata.
        """
        article_path = FIXTURES_DIR / "article-with-cover-file.md"

        result = run_nostr_publish_with_blossom(
            article_path,
            bunker_uri=docker_services["bunker_uri"],
            relays=[docker_services["relay_url"]],
            blossom_url=docker_services["blossom_url"],
            client_secret=docker_services["client_secret"],
        )

        assert result.returncode == 0, f"CLI failed: {result.stderr}"

        # Parse JSON output and extract image URL
        publish_result = parse_publish_result(result.stdout)
        assert "image" in publish_result, "Output missing image metadata"

        cover_metadata = publish_result["image"]
        blob_url = cover_metadata["url"]

        # Fetch blob
        blob_content = fetch_blob_from_blossom(blob_url)
        assert blob_content is not None, f"Failed to fetch blob from {blob_url}"

        # Check for EXIF markers in JPEG
        # EXIF data is stored in APP1 marker (FFE1)
        # If EXIF is stripped, there should be minimal or no APP1 segment
        # A properly stripped image will have only essential JPEG segments

        # Simple check: the blob should not contain GPS-related strings
        # (GPS data would include coordinate values that survive in raw bytes)
        content_str = blob_content.decode("latin-1", errors="ignore")
        assert "GPS" not in content_str, "EXIF GPS data not stripped"
        assert "TestCamera" not in content_str, "EXIF camera Make not stripped"
        assert "TestModel" not in content_str, "EXIF camera Model not stripped"

    def test_cover_file_dimensions_respected(self, docker_services):
        """Test that cover image is resized to target dimensions.

        Property: Returned dimensions match or fit within --cover-size.
        """
        article_path = FIXTURES_DIR / "article-with-cover-file.md"

        # Request specific dimensions
        cover_size = "640x480"

        result = run_nostr_publish_with_blossom(
            article_path,
            bunker_uri=docker_services["bunker_uri"],
            relays=[docker_services["relay_url"]],
            blossom_url=docker_services["blossom_url"],
            cover_size=cover_size,
            client_secret=docker_services["client_secret"],
        )

        assert result.returncode == 0, f"CLI failed: {result.stderr}"

        # Parse JSON output and extract dimensions
        publish_result = parse_publish_result(result.stdout)
        assert "image" in publish_result, "Output missing image metadata"

        cover_metadata = publish_result["image"]
        returned_dim = cover_metadata["dim"]

        # Parse dimensions
        width, height = map(int, returned_dim.split("x"))

        # Dimensions should fit within requested size (aspect ratio preserved)
        assert width <= 640, f"Width {width} exceeds requested 640"
        assert height <= 480, f"Height {height} exceeds requested 480"

        # At least one dimension should equal the target (image fills one axis)
        assert width == 640 or height == 480, f"Image not properly resized: {returned_dim}"

    def test_cover_file_missing_blossom_url_fails(self, docker_services):
        """Test that missing --blossom URL fails when cover.file is present.

        Property: CLI requires --blossom when frontmatter has cover.file.
        """
        article_path = FIXTURES_DIR / "article-with-cover-file.md"

        result = run_nostr_publish_with_blossom(
            article_path,
            bunker_uri=docker_services["bunker_uri"],
            relays=[docker_services["relay_url"]],
            blossom_url=None,  # Explicitly no Blossom URL
            client_secret=docker_services["client_secret"],
        )

        assert result.returncode != 0, "CLI should fail without --blossom when image.file is present"
        assert "blossom" in result.stderr.lower() or "image" in result.stderr.lower(), (
            f"Error should mention blossom or image requirement: {result.stderr}"
        )
