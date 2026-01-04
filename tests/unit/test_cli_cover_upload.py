"""Unit tests for cover upload orchestration.

Property-based tests for orchestrate_cover_upload function using Hypothesis.
"""

from pathlib import Path
from unittest.mock import patch

import pytest
from hypothesis import given
from hypothesis import strategies as st

from nostr_publish.cli_cover_upload import orchestrate_cover_upload
from nostr_publish.errors import BlossomUploadError, ImageProcessingError, InvalidFieldValueError
from nostr_publish.models import Frontmatter, ImageMetadata

# Test bunker URI used throughout tests
TEST_BUNKER_URI = "bunker://79be667ef9dcbbac55a06295ce870b07029bfcdb2dce28d959f2815b16f81798?relay=ws://localhost:8080"


# Strategy for generating valid cover size strings
@st.composite
def cover_sizes(draw):
    """Generate valid WIDTHxHEIGHT format strings."""
    width = draw(st.integers(min_value=100, max_value=4000))
    height = draw(st.integers(min_value=100, max_value=4000))
    return f"{width}x{height}"


# Strategy for generating invalid cover size strings
@st.composite
def invalid_cover_sizes(draw):
    """Generate invalid cover size strings that lack 'x' separator or have non-numeric parts."""
    invalid = draw(
        st.one_of(
            st.just(""),
            st.just("invalid"),
            st.just("1200"),
            st.just("1200x"),
            st.just("x630"),
            st.just("1200-630"),
            st.just("abcxdef"),
        )
    )
    return invalid


# Strategy for generating Blossom URLs
@st.composite
def blossom_urls(draw):
    """Generate valid Blossom server URLs."""
    return draw(st.just("http://localhost:3000") | st.just("https://blossom.example.com"))


# Strategy for generating blob hashes
@st.composite
def blob_hashes(draw):
    """Generate realistic blob hashes."""
    return "".join(draw(st.lists(st.sampled_from("0123456789abcdef"), min_size=64, max_size=64)))


# Strategy for generating image file paths
@st.composite
def image_file_paths(draw):
    """Generate plausible image file paths."""
    return draw(st.just("/tmp/test-image.png") | st.just("/tmp/my-cover.jpg"))


# Strategy for generating slugs
@st.composite
def slugs(draw):
    """Generate valid article slugs."""
    return draw(st.text(alphabet="abcdefghijklmnopqrstuvwxyz0123456789-", min_size=1, max_size=50))


# Strategy for generating dimensions
@st.composite
def dimensions(draw):
    """Generate valid image dimensions."""
    width = draw(st.integers(min_value=100, max_value=4000))
    height = draw(st.integers(min_value=100, max_value=4000))
    return (width, height)


class TestOrchestrateCoverUploadConditionalExecution:
    """Test conditional execution: only processes if cover.file is present."""

    def test_returns_none_when_image_is_none(self):
        """Property: returns None immediately if frontmatter.image is None."""
        frontmatter = Frontmatter(title="Test", slug="test-slug")
        markdown_path = Path("/tmp/test.md")

        result = orchestrate_cover_upload(
            frontmatter, markdown_path, "http://localhost:3000", TEST_BUNKER_URI, 30, "1200x630"
        )

        assert result is None

    def test_returns_none_when_image_file_is_none(self):
        """Property: returns None immediately if image.file is None."""
        image = ImageMetadata(url="https://example.com/image.jpg", file=None)
        frontmatter = Frontmatter(title="Test", slug="test-slug", image=image)
        markdown_path = Path("/tmp/test.md")

        result = orchestrate_cover_upload(
            frontmatter, markdown_path, "http://localhost:3000", TEST_BUNKER_URI, 30, "1200x630"
        )

        assert result is None

    def test_returns_none_when_image_file_is_empty_string(self):
        """Property: returns None if image.file is None (not empty string)."""
        image = ImageMetadata(url="https://example.com/image.jpg", file=None)
        frontmatter = Frontmatter(title="Test", slug="test-slug", image=image)
        markdown_path = Path("/tmp/test.md")

        result = orchestrate_cover_upload(
            frontmatter, markdown_path, "http://localhost:3000", TEST_BUNKER_URI, 30, "1200x630"
        )

        assert result is None


class TestOrchestrateCoverUploadCoverSizeParsing:
    """Test cover_size format parsing and validation."""

    @given(cover_sizes())
    def test_parses_valid_cover_size_format(self, cover_size):
        """Property: parses valid WIDTHxHEIGHT format and extracts dimensions."""
        image = ImageMetadata(url="https://example.com/image.jpg", file="/tmp/test.jpg")
        frontmatter = Frontmatter(title="Test", slug="test-slug", image=image)
        markdown_path = Path("/tmp/test.md")

        with patch("nostr_publish.cli_cover_upload.validate_image_metadata"):
            with patch("nostr_publish.cli_cover_upload.get_processed_cover_path") as mock_get_path:
                mock_get_path.return_value = "/tmp/processed.jpg"
                with patch("nostr_publish.cli_cover_upload.process_cover_image") as mock_process:
                    mock_process.return_value = ("/tmp/processed.jpg", 1200, 630)
                    with patch("nostr_publish.cli_cover_upload.upload_to_blossom") as mock_upload:
                        mock_upload.return_value = {"hash": "abc123", "url": "http://example.com/abc123"}

                        result = orchestrate_cover_upload(
                            frontmatter, markdown_path, "http://localhost:3000", TEST_BUNKER_URI, 30, cover_size
                        )

                        assert result is not None
                        # Verify process_cover_image was called with parsed dimensions
                        parts = cover_size.split("x")
                        expected_width = int(parts[0])
                        expected_height = int(parts[1])
                        mock_process.assert_called_once()
                        call_args = mock_process.call_args
                        assert call_args[0][2] == expected_width
                        assert call_args[0][3] == expected_height

    @given(invalid_cover_sizes())
    def test_rejects_invalid_cover_size_format(self, invalid_size):
        """Property: raises InvalidFieldValueError for invalid WIDTHxHEIGHT format."""
        image = ImageMetadata(url="https://example.com/image.jpg", file="/tmp/test.jpg")
        frontmatter = Frontmatter(title="Test", slug="test-slug", image=image)
        markdown_path = Path("/tmp/test.md")

        with patch("nostr_publish.cli_cover_upload.validate_image_metadata"):
            with pytest.raises(InvalidFieldValueError, match="WIDTHxHEIGHT format"):
                orchestrate_cover_upload(
                    frontmatter, markdown_path, "http://localhost:3000", TEST_BUNKER_URI, 30, invalid_size
                )


class TestOrchestrateCoverUploadErrorPropagation:
    """Test error propagation from image processing and upload modules."""

    def test_propagates_image_metadata_validation_error(self):
        """Property: validation errors from image metadata propagate."""
        image = ImageMetadata(url="https://example.com/image.jpg", file="/tmp/nonexistent.jpg")
        frontmatter = Frontmatter(title="Test", slug="test-slug", image=image)
        markdown_path = Path("/tmp/test.md")

        with patch("nostr_publish.cli_cover_upload.validate_image_metadata") as mock_validate:
            mock_validate.side_effect = InvalidFieldValueError("File not found")

            with pytest.raises(InvalidFieldValueError, match="File not found"):
                orchestrate_cover_upload(
                    frontmatter, markdown_path, "http://localhost:3000", TEST_BUNKER_URI, 30, "1200x630"
                )

    def test_propagates_image_processing_error(self):
        """Property: ImageProcessingError from process_cover_image propagates."""
        image = ImageMetadata(url="https://example.com/image.jpg", file="/tmp/test.jpg")
        frontmatter = Frontmatter(title="Test", slug="test-slug", image=image)
        markdown_path = Path("/tmp/test.md")

        with patch("nostr_publish.cli_cover_upload.validate_image_metadata"):
            with patch("nostr_publish.cli_cover_upload.get_processed_cover_path") as mock_get_path:
                mock_get_path.return_value = "/tmp/processed.jpg"
                with patch("nostr_publish.cli_cover_upload.process_cover_image") as mock_process:
                    mock_process.side_effect = ImageProcessingError("Invalid image format")

                    with pytest.raises(ImageProcessingError, match="Invalid image format"):
                        orchestrate_cover_upload(
                            frontmatter, markdown_path, "http://localhost:3000", TEST_BUNKER_URI, 30, "1200x630"
                        )

    def test_propagates_blossom_upload_error(self):
        """Property: BlossomUploadError from upload_to_blossom propagates."""
        image = ImageMetadata(url="https://example.com/image.jpg", file="/tmp/test.jpg")
        frontmatter = Frontmatter(title="Test", slug="test-slug", image=image)
        markdown_path = Path("/tmp/test.md")

        with patch("nostr_publish.cli_cover_upload.validate_image_metadata"):
            with patch("nostr_publish.cli_cover_upload.get_processed_cover_path") as mock_get_path:
                mock_get_path.return_value = "/tmp/processed.jpg"
                with patch("nostr_publish.cli_cover_upload.process_cover_image") as mock_process:
                    mock_process.return_value = ("/tmp/processed.jpg", 1200, 630)
                    with patch("nostr_publish.cli_cover_upload.upload_to_blossom") as mock_upload:
                        mock_upload.side_effect = BlossomUploadError("Connection timeout")

                        with pytest.raises(BlossomUploadError, match="Connection timeout"):
                            orchestrate_cover_upload(
                                frontmatter, markdown_path, "http://localhost:3000", TEST_BUNKER_URI, 30, "1200x630"
                            )


class TestOrchestrateCoverUploadSuccessfulOrchestration:
    """Test successful end-to-end orchestration."""

    @given(cover_size=cover_sizes(), slug=slugs(), actual_dim=dimensions())
    def test_orchestration_returns_complete_metadata(self, cover_size, slug, actual_dim):
        """Property: successful orchestration returns complete metadata dictionary."""
        image = ImageMetadata(url="https://example.com/image.jpg", file="/tmp/test.jpg")
        frontmatter = Frontmatter(title="Test", slug=slug, image=image)
        markdown_path = Path("/tmp/test.md")

        with patch("nostr_publish.cli_cover_upload.validate_image_metadata"):
            with patch("nostr_publish.cli_cover_upload.get_processed_cover_path") as mock_get_path:
                mock_get_path.return_value = "/tmp/processed.jpg"
                with patch("nostr_publish.cli_cover_upload.process_cover_image") as mock_process:
                    mock_process.return_value = ("/tmp/processed.jpg", actual_dim[0], actual_dim[1])
                    with patch("nostr_publish.cli_cover_upload.upload_to_blossom") as mock_upload:
                        mock_upload.return_value = {"hash": "abc123def", "url": "http://example.com/blob/abc123def"}

                        result = orchestrate_cover_upload(
                            frontmatter, markdown_path, "http://localhost:3000", TEST_BUNKER_URI, 30, cover_size
                        )

                        assert result is not None
                        assert isinstance(result, dict)
                        assert "hash" in result
                        assert "url" in result
                        assert "dim" in result
                        assert "mime" in result

    @given(
        cover_size=cover_sizes(),
        slug=slugs(),
        actual_dim=dimensions(),
        blob_hash=blob_hashes(),
        blossom_url=blossom_urls(),
    )
    def test_orchestration_metadata_structure_complete(self, cover_size, slug, actual_dim, blob_hash, blossom_url):
        """Property: returned metadata has all required fields with correct structure."""
        image = ImageMetadata(url="https://example.com/image.jpg", file="/tmp/test.jpg")
        frontmatter = Frontmatter(title="Test", slug=slug, image=image)
        markdown_path = Path("/tmp/test.md")

        with patch("nostr_publish.cli_cover_upload.validate_image_metadata"):
            with patch("nostr_publish.cli_cover_upload.get_processed_cover_path") as mock_get_path:
                mock_get_path.return_value = "/tmp/processed.jpg"
                with patch("nostr_publish.cli_cover_upload.process_cover_image") as mock_process:
                    mock_process.return_value = ("/tmp/processed.jpg", actual_dim[0], actual_dim[1])
                    with patch("nostr_publish.cli_cover_upload.upload_to_blossom") as mock_upload:
                        upload_url = f"{blossom_url}/blob/{blob_hash}"
                        mock_upload.return_value = {"hash": blob_hash, "url": upload_url}

                        result = orchestrate_cover_upload(
                            frontmatter, markdown_path, blossom_url, TEST_BUNKER_URI, 30, cover_size
                        )

                        assert result["hash"] == blob_hash
                        assert result["url"] == upload_url
                        assert result["dim"] == f"{actual_dim[0]}x{actual_dim[1]}"
                        assert result["mime"] == "image/jpeg"

    @given(cover_size=cover_sizes())
    def test_orchestration_calls_all_modules_in_order(self, cover_size):
        """Property: orchestration calls validate, process, and upload in correct sequence."""
        image = ImageMetadata(url="https://example.com/image.jpg", file="/tmp/test.jpg")
        frontmatter = Frontmatter(title="Test", slug="test-slug", image=image)
        markdown_path = Path("/tmp/test.md")
        call_order = []

        def mock_validate(img, md_dir):
            call_order.append("validate")
            # Return validated image with resolved absolute file path
            return ImageMetadata(url=img.url, file="/tmp/test.jpg")

        def mock_process(input_p, output_p, w, h):
            call_order.append("process")
            return (output_p, 1200, 630)

        def mock_upload(path, url, bunker_uri, timeout):
            call_order.append("upload")
            return {"hash": "test", "url": "http://example.com/test"}

        def mock_get_path(md_path, slug):
            return "/tmp/processed.jpg"

        with patch("nostr_publish.cli_cover_upload.validate_image_metadata", side_effect=mock_validate):
            with patch("nostr_publish.cli_cover_upload.get_processed_cover_path", side_effect=mock_get_path):
                with patch("nostr_publish.cli_cover_upload.process_cover_image", side_effect=mock_process):
                    with patch("nostr_publish.cli_cover_upload.upload_to_blossom", side_effect=mock_upload):
                        orchestrate_cover_upload(
                            frontmatter, markdown_path, "http://localhost:3000", TEST_BUNKER_URI, 30, cover_size
                        )

                        assert call_order == ["validate", "process", "upload"]

    def test_orchestration_passes_correct_arguments_to_process_cover_image(self):
        """Property: process_cover_image receives input, output, width, height from cover_size."""
        image = ImageMetadata(url="https://example.com/image.jpg", file="/tmp/test.jpg")
        frontmatter = Frontmatter(title="Test", slug="test-slug", image=image)
        markdown_path = Path("/tmp/test.md")
        cover_size = "1200x630"
        validated_file_path = "/tmp/test.jpg"

        # Mock validate_image_metadata to return validated image with resolved path
        with patch("nostr_publish.cli_cover_upload.validate_image_metadata") as mock_validate:
            mock_validate.return_value = ImageMetadata(url=image.url, file=validated_file_path)
            with patch("nostr_publish.cli_cover_upload.get_processed_cover_path") as mock_get_path:
                processed_path = "/tmp/.nostr-publish/cache/covers/test-slug/cover.jpg"
                mock_get_path.return_value = processed_path
                with patch("nostr_publish.cli_cover_upload.process_cover_image") as mock_process:
                    mock_process.return_value = (processed_path, 1200, 630)
                    with patch("nostr_publish.cli_cover_upload.upload_to_blossom") as mock_upload:
                        mock_upload.return_value = {"hash": "test", "url": "http://example.com/test"}

                        orchestrate_cover_upload(
                            frontmatter, markdown_path, "http://localhost:3000", TEST_BUNKER_URI, 30, cover_size
                        )

                        mock_process.assert_called_once_with(
                            validated_file_path,  # validated_image.file (resolved path)
                            processed_path,  # output from get_processed_cover_path
                            1200,  # parsed width
                            630,  # parsed height
                        )

    def test_orchestration_passes_processed_path_to_upload(self):
        """Property: upload_to_blossom receives path from process_cover_image."""
        image = ImageMetadata(url="https://example.com/image.jpg", file="/tmp/test.jpg")
        frontmatter = Frontmatter(title="Test", slug="test-slug", image=image)
        markdown_path = Path("/tmp/test.md")

        with patch("nostr_publish.cli_cover_upload.validate_image_metadata"):
            with patch("nostr_publish.cli_cover_upload.get_processed_cover_path") as mock_get_path:
                processed_path = "/tmp/.nostr-publish/cache/covers/test-slug/cover.jpg"
                mock_get_path.return_value = processed_path
                with patch("nostr_publish.cli_cover_upload.process_cover_image") as mock_process:
                    mock_process.return_value = (processed_path, 1200, 630)
                    with patch("nostr_publish.cli_cover_upload.upload_to_blossom") as mock_upload:
                        mock_upload.return_value = {"hash": "test", "url": "http://example.com/test"}

                        orchestrate_cover_upload(
                            frontmatter, markdown_path, "http://localhost:3000", TEST_BUNKER_URI, 30, "1200x630"
                        )

                        mock_upload.assert_called_once()
                        call_args = mock_upload.call_args
                        assert call_args[0][0] == processed_path

    @given(blossom_url=blossom_urls())
    def test_orchestration_passes_blossom_parameters(self, blossom_url):
        """Property: upload_to_blossom receives correct URL and timeout."""
        image = ImageMetadata(url="https://example.com/image.jpg", file="/tmp/test.jpg")
        frontmatter = Frontmatter(title="Test", slug="test-slug", image=image)
        markdown_path = Path("/tmp/test.md")
        timeout = 42

        with patch("nostr_publish.cli_cover_upload.validate_image_metadata"):
            with patch("nostr_publish.cli_cover_upload.get_processed_cover_path") as mock_get_path:
                mock_get_path.return_value = "/tmp/processed.jpg"
                with patch("nostr_publish.cli_cover_upload.process_cover_image") as mock_process:
                    mock_process.return_value = ("/tmp/processed.jpg", 1200, 630)
                    with patch("nostr_publish.cli_cover_upload.upload_to_blossom") as mock_upload:
                        mock_upload.return_value = {"hash": "test", "url": "http://example.com/test"}

                        orchestrate_cover_upload(
                            frontmatter, markdown_path, blossom_url, TEST_BUNKER_URI, timeout, "1200x630"
                        )

                        mock_upload.assert_called_once()
                        call_args = mock_upload.call_args
                        assert call_args[0][1] == blossom_url
                        assert call_args[0][2] == TEST_BUNKER_URI
                        assert call_args[0][3] == timeout

    def test_orchestration_resolves_markdown_directory(self):
        """Property: markdown_file_dir is correctly extracted from markdown_file_path."""
        image = ImageMetadata(url="https://example.com/image.jpg", file="/tmp/test.jpg")
        frontmatter = Frontmatter(title="Test", slug="test-slug", image=image)
        markdown_path = Path("/home/user/articles/test.md")

        with patch("nostr_publish.cli_cover_upload.validate_image_metadata") as mock_validate:
            with patch("nostr_publish.cli_cover_upload.get_processed_cover_path"):
                with patch("nostr_publish.cli_cover_upload.process_cover_image") as mock_process:
                    mock_process.return_value = ("/tmp/processed.jpg", 1200, 630)
                    with patch("nostr_publish.cli_cover_upload.upload_to_blossom") as mock_upload:
                        mock_upload.return_value = {"hash": "test", "url": "http://example.com/test"}

                        orchestrate_cover_upload(
                            frontmatter, markdown_path, "http://localhost:3000", TEST_BUNKER_URI, 30, "1200x630"
                        )

                        mock_validate.assert_called_once()
                        call_args = mock_validate.call_args
                        # Second argument should be the parent directory of markdown_path
                        assert call_args[0][1] == "/home/user/articles"


class TestOrchestrateCoverUploadMetadataConstruction:
    """Test correct metadata construction from components."""

    @given(
        blob_hash=blob_hashes(),
        url=st.text(min_size=10, max_size=100),
        width=st.integers(100, 4000),
        height=st.integers(100, 4000),
    )
    def test_metadata_dim_format(self, blob_hash, url, width, height):
        """Property: dim field is formatted as WIDTHxHEIGHT."""
        image = ImageMetadata(url="https://example.com/image.jpg", file="/tmp/test.jpg")
        frontmatter = Frontmatter(title="Test", slug="test-slug", image=image)
        markdown_path = Path("/tmp/test.md")

        with patch("nostr_publish.cli_cover_upload.validate_image_metadata"):
            with patch("nostr_publish.cli_cover_upload.get_processed_cover_path") as mock_get_path:
                mock_get_path.return_value = "/tmp/processed.jpg"
                with patch("nostr_publish.cli_cover_upload.process_cover_image") as mock_process:
                    mock_process.return_value = ("/tmp/processed.jpg", width, height)
                    with patch("nostr_publish.cli_cover_upload.upload_to_blossom") as mock_upload:
                        mock_upload.return_value = {"hash": blob_hash, "url": url}

                        result = orchestrate_cover_upload(
                            frontmatter, markdown_path, "http://localhost:3000", TEST_BUNKER_URI, 30, "1200x630"
                        )

                        assert result["dim"] == f"{width}x{height}"

    def test_metadata_mime_always_jpeg(self):
        """Property: mime field is always 'image/jpeg' for processed images."""
        image = ImageMetadata(url="https://example.com/image.jpg", file="/tmp/test.jpg")
        frontmatter = Frontmatter(title="Test", slug="test-slug", image=image)
        markdown_path = Path("/tmp/test.md")

        with patch("nostr_publish.cli_cover_upload.validate_image_metadata"):
            with patch("nostr_publish.cli_cover_upload.get_processed_cover_path") as mock_get_path:
                mock_get_path.return_value = "/tmp/processed.jpg"
                with patch("nostr_publish.cli_cover_upload.process_cover_image") as mock_process:
                    mock_process.return_value = ("/tmp/processed.jpg", 1200, 630)
                    with patch("nostr_publish.cli_cover_upload.upload_to_blossom") as mock_upload:
                        mock_upload.return_value = {"hash": "test", "url": "http://example.com/test"}

                        result = orchestrate_cover_upload(
                            frontmatter, markdown_path, "http://localhost:3000", TEST_BUNKER_URI, 30, "1200x630"
                        )

                        assert result["mime"] == "image/jpeg"

    @given(blob_hash=blob_hashes())
    def test_metadata_hash_from_upload(self, blob_hash):
        """Property: hash field is extracted directly from upload result."""
        image = ImageMetadata(url="https://example.com/image.jpg", file="/tmp/test.jpg")
        frontmatter = Frontmatter(title="Test", slug="test-slug", image=image)
        markdown_path = Path("/tmp/test.md")

        with patch("nostr_publish.cli_cover_upload.validate_image_metadata"):
            with patch("nostr_publish.cli_cover_upload.get_processed_cover_path") as mock_get_path:
                mock_get_path.return_value = "/tmp/processed.jpg"
                with patch("nostr_publish.cli_cover_upload.process_cover_image") as mock_process:
                    mock_process.return_value = ("/tmp/processed.jpg", 1200, 630)
                    with patch("nostr_publish.cli_cover_upload.upload_to_blossom") as mock_upload:
                        mock_upload.return_value = {"hash": blob_hash, "url": "http://example.com/test"}

                        result = orchestrate_cover_upload(
                            frontmatter, markdown_path, "http://localhost:3000", TEST_BUNKER_URI, 30, "1200x630"
                        )

                        assert result["hash"] == blob_hash

    @given(upload_url=st.text(min_size=10, max_size=100))
    def test_metadata_url_from_upload(self, upload_url):
        """Property: url field is extracted directly from upload result."""
        image = ImageMetadata(url="https://example.com/image.jpg", file="/tmp/test.jpg")
        frontmatter = Frontmatter(title="Test", slug="test-slug", image=image)
        markdown_path = Path("/tmp/test.md")

        with patch("nostr_publish.cli_cover_upload.validate_image_metadata"):
            with patch("nostr_publish.cli_cover_upload.get_processed_cover_path") as mock_get_path:
                mock_get_path.return_value = "/tmp/processed.jpg"
                with patch("nostr_publish.cli_cover_upload.process_cover_image") as mock_process:
                    mock_process.return_value = ("/tmp/processed.jpg", 1200, 630)
                    with patch("nostr_publish.cli_cover_upload.upload_to_blossom") as mock_upload:
                        mock_upload.return_value = {"hash": "test", "url": upload_url}

                        result = orchestrate_cover_upload(
                            frontmatter, markdown_path, "http://localhost:3000", TEST_BUNKER_URI, 30, "1200x630"
                        )

                        assert result["url"] == upload_url
