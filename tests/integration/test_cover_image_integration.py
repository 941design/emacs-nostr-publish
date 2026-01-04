"""Integration tests for cover image feature (NIP-92 imeta tags).

Tests the complete workflow: frontmatter parsing → validation → imeta tag emission.
"""

from hypothesis import given
from hypothesis import strategies as st

from nostr_publish.event import build_tags, construct_event
from nostr_publish.frontmatter import dict_to_frontmatter, parse_frontmatter
from nostr_publish.models import ImageMetadata
from nostr_publish.validator import validate_frontmatter


# Strategies
@st.composite
def valid_image_url(draw):
    """Generate valid HTTP(S) image URLs."""
    protocol = draw(st.sampled_from(["http://", "https://"]))
    domain = draw(st.text(alphabet=st.characters(whitelist_categories=("Ll", "Nd")), min_size=1, max_size=20))
    path = draw(st.text(alphabet=st.characters(whitelist_categories=("Ll", "Nd", "Pd")), min_size=1, max_size=30))
    extension = draw(st.sampled_from(["jpg", "jpeg", "png", "gif", "webp", "svg"]))
    return f"{protocol}{domain}.com/{path}.{extension}"


@st.composite
def valid_mime_type(draw):
    """Generate valid MIME types for images."""
    return draw(st.sampled_from(["image/jpeg", "image/png", "image/gif", "image/webp", "image/svg+xml"]))


@st.composite
def valid_dimensions(draw):
    """Generate valid dimension strings in WIDTHxHEIGHT format."""
    width = draw(st.integers(min_value=1, max_value=10000))
    height = draw(st.integers(min_value=1, max_value=10000))
    return f"{width}x{height}"


@st.composite
def simple_image_frontmatter(draw):
    """Generate frontmatter dict with image as simple URL string."""
    url = draw(valid_image_url())
    return {"title": "Test Article", "slug": "test-article", "image": url}


@st.composite
def extended_image_frontmatter(draw):
    """Generate frontmatter dict with image as extended dict format."""
    url = draw(valid_image_url())
    mime = draw(st.one_of(st.none(), valid_mime_type()))
    alt = draw(
        st.one_of(
            st.none(),
            st.text(alphabet=st.characters(min_codepoint=32, max_codepoint=126), min_size=1, max_size=100).filter(
                lambda s: s.strip() and not any(char in s for char in ["\n", "\r", "[", "]", "{", "}"])
            ),
        )
    )
    dim = draw(st.one_of(st.none(), valid_dimensions()))

    image_dict = {"url": url}
    if mime is not None:
        image_dict["mime"] = mime
    if alt is not None:
        image_dict["alt"] = alt
    if dim is not None:
        image_dict["dim"] = dim

    return {"title": "Test Article", "slug": "test-article", "image": image_dict}


@st.composite
def image_frontmatter(draw):
    """Generate frontmatter using image field."""
    url = draw(valid_image_url())
    return {"title": "Test Article", "slug": "test-article", "image": url}


class TestCoverImageIntegration:
    """Integration tests for complete cover image workflow."""

    @given(simple_image_frontmatter())
    def test_simple_format_end_to_end(self, fm_dict):
        """Simple string URL format flows through entire pipeline."""
        # Parse
        fm = dict_to_frontmatter(fm_dict)
        assert fm.image is not None
        assert isinstance(fm.image, ImageMetadata)
        assert fm.image.url == fm_dict["image"]

        # Validate
        validated_fm = validate_frontmatter(fm)
        assert validated_fm.image is not None
        assert validated_fm.image.url.startswith("http")

        # Build tags
        tags = build_tags(validated_fm)
        imeta_tags = [tag for tag in tags if tag[0] == "imeta"]
        assert len(imeta_tags) == 1

        # Verify imeta structure
        imeta_tag = imeta_tags[0]
        assert imeta_tag[0] == "imeta"
        assert imeta_tag[1].startswith("url ")
        assert fm_dict["image"] in imeta_tag[1]

    @given(extended_image_frontmatter())
    def test_extended_format_end_to_end(self, fm_dict):
        """Extended dict format with optional fields flows through pipeline."""
        # Parse
        fm = dict_to_frontmatter(fm_dict)
        assert fm.image is not None
        assert isinstance(fm.image, ImageMetadata)
        assert fm.image.url == fm_dict["image"]["url"]

        # Validate
        validated_fm = validate_frontmatter(fm)
        assert validated_fm.image is not None

        # Build tags
        tags = build_tags(validated_fm)
        imeta_tags = [tag for tag in tags if tag[0] == "imeta"]
        assert len(imeta_tags) == 1

        # Verify imeta structure matches input
        imeta_tag = imeta_tags[0]
        assert imeta_tag[0] == "imeta"
        assert any(fm_dict["image"]["url"] in elem for elem in imeta_tag)

        # Verify optional fields present if provided (use validated/trimmed values)
        if validated_fm.image.mime is not None:
            assert any(f"m {validated_fm.image.mime}" == elem for elem in imeta_tag)
        if validated_fm.image.alt is not None:
            assert any(f"alt {validated_fm.image.alt}" == elem for elem in imeta_tag)
        if validated_fm.image.dim is not None:
            assert any(f"dim {validated_fm.image.dim}" == elem for elem in imeta_tag)

    @given(image_frontmatter())
    def test_image_field_produces_imeta_tag(self, fm_dict):
        """Image field produces valid imeta tags."""
        # Parse with image
        fm = dict_to_frontmatter(fm_dict)
        assert fm.image is not None

        # Validate
        validated_fm = validate_frontmatter(fm)
        assert validated_fm.image is not None

        # Build tags
        tags = build_tags(validated_fm)
        imeta_tags = [tag for tag in tags if tag[0] == "imeta"]
        assert len(imeta_tags) == 1

    @given(st.sampled_from(["title", "slug", "summary", "published_at", "tags", "relays"]))
    def test_backwards_compatibility_no_image(self, extra_field):
        """Articles without image field continue to work (no imeta tag emitted)."""
        fm_dict = {"title": "Test Article", "slug": "test-article"}

        # Parse
        fm = dict_to_frontmatter(fm_dict)
        assert fm.image is None

        # Validate
        validated_fm = validate_frontmatter(fm)
        assert validated_fm.image is None

        # Build tags
        tags = build_tags(validated_fm)
        imeta_tags = [tag for tag in tags if tag[0] == "imeta"]
        assert len(imeta_tags) == 0

    def test_tag_ordering_preservation_with_imeta(self):
        """When imeta tag is added, other tags are present alongside it."""
        fm_dict = {
            "title": "Test Article",
            "slug": "test-article",
            "tags": ["tech", "programming"],
            "image": "https://example.com/cover.jpg",
        }

        # Parse and validate
        fm = dict_to_frontmatter(fm_dict)
        validated_fm = validate_frontmatter(fm)

        # Build tags
        tags = build_tags(validated_fm)

        # Extract tag types in order
        tag_types = [tag[0] for tag in tags]

        # Verify imeta appears exactly once
        assert tag_types.count("imeta") == 1

        # Verify t tags are present (both tags)
        t_tags = [tag for tag in tags if tag[0] == "t"]
        assert len(t_tags) == 2
        t_tag_values = {tag[1] for tag in t_tags}
        assert t_tag_values == {"tech", "programming"}

    @given(valid_image_url())
    def test_mime_type_inference_when_absent(self, url):
        """MIME type is inferred from URL extension when not explicitly provided."""
        fm_dict = {"title": "Test Article", "slug": "test-article", "image": url}

        # Parse and validate
        fm = dict_to_frontmatter(fm_dict)
        validated_fm = validate_frontmatter(fm)

        # Build tags
        tags = build_tags(validated_fm)
        imeta_tag = next(tag for tag in tags if tag[0] == "imeta")

        # If URL has known extension, MIME should be inferred and present
        extension = url.split(".")[-1].lower()
        if extension in ["jpg", "jpeg", "png", "gif", "webp", "svg"]:
            mime_elements = [elem for elem in imeta_tag if elem.startswith("m ")]
            assert len(mime_elements) == 1

    def test_yaml_frontmatter_with_image_integration(self):
        """Complete YAML frontmatter with image field can be parsed and converted to event."""
        markdown = """---
title: My Blog Post
slug: my-blog-post
image: https://example.com/header.jpg
---

Blog content here.
"""
        # Parse YAML frontmatter
        fm_dict, content = parse_frontmatter(markdown)
        assert "image" in fm_dict

        # Convert to Frontmatter model
        fm = dict_to_frontmatter(fm_dict)
        assert fm.image is not None
        assert fm.image.url == "https://example.com/header.jpg"

        # Validate
        validated_fm = validate_frontmatter(fm)

        # Construct event
        event = construct_event(validated_fm, content)

        # Verify event has imeta tag
        imeta_tags = [tag for tag in event.tags if tag[0] == "imeta"]
        assert len(imeta_tags) == 1
        assert "https://example.com/header.jpg" in imeta_tags[0][1]

    def test_yaml_frontmatter_with_extended_image_integration(self):
        """YAML frontmatter with extended image dict format works end-to-end."""
        markdown = """---
title: My Blog Post
slug: my-blog-post
image:
  url: https://example.com/header.jpg
  mime: image/jpeg
  alt: Blog header image
  dim: 1200x630
---

Blog content here.
"""
        # Parse YAML frontmatter
        fm_dict, content = parse_frontmatter(markdown)
        assert "image" in fm_dict
        assert isinstance(fm_dict["image"], dict)

        # Convert to Frontmatter model
        fm = dict_to_frontmatter(fm_dict)
        assert fm.image is not None
        assert fm.image.url == "https://example.com/header.jpg"
        assert fm.image.mime == "image/jpeg"
        assert fm.image.alt == "Blog header image"
        assert fm.image.dim == "1200x630"

        # Validate
        validated_fm = validate_frontmatter(fm)

        # Construct event
        event = construct_event(validated_fm, content)

        # Verify event has complete imeta tag
        imeta_tags = [tag for tag in event.tags if tag[0] == "imeta"]
        assert len(imeta_tags) == 1
        assert "url https://example.com/header.jpg" in imeta_tags[0]
        assert "m image/jpeg" in imeta_tags[0]
        assert "alt Blog header image" in imeta_tags[0]
        assert "dim 1200x630" in imeta_tags[0]


class TestIdempotentWorkflow:
    """Integration tests for idempotent publish workflow (hash-based upload skip)."""

    def test_idempotent_publish_skips_upload_when_hash_matches(self):
        """When image hash matches, second publish skips upload and reuses URL.

        This test verifies the complete idempotent workflow:
        1. First publish: image.file → process → upload → get hash/url → write to frontmatter
        2. Second publish: image.file + existing hash → process → compare hash → skip upload → reuse url

        The orchestrate_cover_upload function implements this logic at lines 129-148.
        """
        import hashlib
        from pathlib import Path
        from tempfile import NamedTemporaryFile
        from unittest.mock import MagicMock, patch

        from nostr_publish.cli_cover_upload import orchestrate_cover_upload
        from nostr_publish.models import Frontmatter, ImageMetadata

        # Create a temporary test image file
        with NamedTemporaryFile(suffix=".jpg", delete=False) as temp_image:
            # Write minimal JPEG data
            temp_image.write(b"\xff\xd8\xff\xe0\x00\x10JFIF")
            temp_image_path = temp_image.name

        try:
            # Mock markdown file path
            markdown_path = Path("/tmp/test-article.md")

            # Mock Blossom upload to capture calls
            mock_upload = MagicMock(
                return_value={
                    "hash": "abc123" * 10 + "abcd",  # 64-char hex
                    "url": "https://cdn.example.com/uploaded.jpg",
                }
            )

            # Mock image processing to return predictable output
            mock_process = MagicMock(
                return_value=(
                    temp_image_path,  # processed path (reuse temp file for simplicity)
                    1200,  # width
                    630,  # height
                )
            )

            with (
                patch("nostr_publish.cli_cover_upload.upload_to_blossom", mock_upload),
                patch("nostr_publish.cli_cover_upload.process_cover_image", mock_process),
            ):
                # FIRST PUBLISH: No hash in frontmatter
                frontmatter_first = Frontmatter(
                    title="Test Article", slug="test-article", image=ImageMetadata(file=temp_image_path)
                )

                result_first = orchestrate_cover_upload(
                    frontmatter=frontmatter_first,
                    markdown_file_path=markdown_path,
                    blossom_url="https://blossom.example.com",
                    bunker_uri="bunker://test",
                    blossom_timeout=30,
                    cover_size="1200x630",
                )

                # Verify first publish uploaded to Blossom
                assert mock_upload.call_count == 1
                assert result_first is not None
                assert result_first["hash"] == "abc123" * 10 + "abcd"
                assert result_first["url"] == "https://cdn.example.com/uploaded.jpg"
                assert result_first["dim"] == "1200x630"
                assert result_first["mime"] == "image/jpeg"

                # Compute actual hash of processed file (for second publish)
                with open(temp_image_path, "rb") as f:
                    actual_hash = hashlib.sha256(f.read()).hexdigest()

                # SECOND PUBLISH: Hash and URL now in frontmatter (as if Emacs wrote them back)
                frontmatter_second = Frontmatter(
                    title="Test Article",
                    slug="test-article",
                    image=ImageMetadata(
                        file=temp_image_path,
                        hash=actual_hash,  # Hash from first publish
                        url="https://cdn.example.com/uploaded.jpg",  # URL from first publish
                    ),
                )

                result_second = orchestrate_cover_upload(
                    frontmatter=frontmatter_second,
                    markdown_file_path=markdown_path,
                    blossom_url="https://blossom.example.com",
                    bunker_uri="bunker://test",
                    blossom_timeout=30,
                    cover_size="1200x630",
                )

                # Verify second publish did NOT upload again (upload count unchanged)
                assert mock_upload.call_count == 1  # Still 1, not 2

                # Verify second publish reused existing URL
                assert result_second is not None
                assert result_second["hash"] == actual_hash
                assert result_second["url"] == "https://cdn.example.com/uploaded.jpg"  # Same URL
                assert result_second["dim"] == "1200x630"
                assert result_second["mime"] == "image/jpeg"

        finally:
            # Cleanup temp file
            Path(temp_image_path).unlink(missing_ok=True)

    def test_upload_when_hash_differs(self):
        """When image hash differs, publish uploads again even if hash exists.

        This verifies that changing the image file triggers re-upload.
        """
        from pathlib import Path
        from tempfile import NamedTemporaryFile
        from unittest.mock import MagicMock, patch

        from nostr_publish.cli_cover_upload import orchestrate_cover_upload
        from nostr_publish.models import Frontmatter, ImageMetadata

        # Create a temporary test image file
        with NamedTemporaryFile(suffix=".jpg", delete=False) as temp_image:
            temp_image.write(b"\xff\xd8\xff\xe0\x00\x10JFIF")
            temp_image_path = temp_image.name

        try:
            markdown_path = Path("/tmp/test-article.md")

            # Mock Blossom upload
            mock_upload = MagicMock(
                return_value={"hash": "newHash" * 10 + "abcd", "url": "https://cdn.example.com/new-upload.jpg"}
            )

            # Mock image processing
            mock_process = MagicMock(return_value=(temp_image_path, 1200, 630))

            with (
                patch("nostr_publish.cli_cover_upload.upload_to_blossom", mock_upload),
                patch("nostr_publish.cli_cover_upload.process_cover_image", mock_process),
            ):
                # Frontmatter with OLD hash (different from actual file hash)
                frontmatter = Frontmatter(
                    title="Test Article",
                    slug="test-article",
                    image=ImageMetadata(
                        file=temp_image_path,
                        hash="oldHash" * 10 + "abcd",  # Different from actual
                        url="https://cdn.example.com/old-url.jpg",
                    ),
                )

                result = orchestrate_cover_upload(
                    frontmatter=frontmatter,
                    markdown_file_path=markdown_path,
                    blossom_url="https://blossom.example.com",
                    bunker_uri="bunker://test",
                    blossom_timeout=30,
                    cover_size="1200x630",
                )

                # Verify upload WAS called (hash mismatch triggers upload)
                assert mock_upload.call_count == 1

                # Verify result uses NEW hash and URL from upload
                assert result is not None
                assert result["hash"] == "newHash" * 10 + "abcd"
                assert result["url"] == "https://cdn.example.com/new-upload.jpg"

        finally:
            Path(temp_image_path).unlink(missing_ok=True)

    def test_upload_when_no_hash_in_frontmatter(self):
        """When no hash exists in frontmatter, always upload (first-time publish)."""
        from pathlib import Path
        from tempfile import NamedTemporaryFile
        from unittest.mock import MagicMock, patch

        from nostr_publish.cli_cover_upload import orchestrate_cover_upload
        from nostr_publish.models import Frontmatter, ImageMetadata

        with NamedTemporaryFile(suffix=".jpg", delete=False) as temp_image:
            temp_image.write(b"\xff\xd8\xff\xe0\x00\x10JFIF")
            temp_image_path = temp_image.name

        try:
            markdown_path = Path("/tmp/test-article.md")

            mock_upload = MagicMock(
                return_value={"hash": "firstUpload" * 6 + "ab", "url": "https://cdn.example.com/first.jpg"}
            )

            mock_process = MagicMock(return_value=(temp_image_path, 1200, 630))

            with (
                patch("nostr_publish.cli_cover_upload.upload_to_blossom", mock_upload),
                patch("nostr_publish.cli_cover_upload.process_cover_image", mock_process),
            ):
                # No hash in frontmatter
                frontmatter = Frontmatter(
                    title="Test Article",
                    slug="test-article",
                    image=ImageMetadata(file=temp_image_path),  # No hash field
                )

                result = orchestrate_cover_upload(
                    frontmatter=frontmatter,
                    markdown_file_path=markdown_path,
                    blossom_url="https://blossom.example.com",
                    bunker_uri="bunker://test",
                    blossom_timeout=30,
                    cover_size="1200x630",
                )

                # Verify upload was called
                assert mock_upload.call_count == 1
                assert result is not None
                assert result["hash"] == "firstUpload" * 6 + "ab"

        finally:
            Path(temp_image_path).unlink(missing_ok=True)
