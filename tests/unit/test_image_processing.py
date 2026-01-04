"""Property-based tests for image processing and cover image handling.

Comprehensive tests for process_cover_image() and get_processed_cover_path()
using Hypothesis for property-based testing.
"""

import tempfile
from pathlib import Path

import pytest
from hypothesis import given, settings
from hypothesis import strategies as st

from nostr_publish.errors import ImageProcessingError
from nostr_publish.image_processing import (
    MAX_ASPECT_RATIO,
    MIN_ASPECT_RATIO,
    MIN_DIMENSION,
    get_processed_cover_path,
    process_cover_image,
    validate_image_dimensions,
)


class TestValidateImageDimensions:
    """Tests for image dimension validation."""

    def test_valid_dimensions_pass(self):
        """Normal dimensions pass validation."""
        is_valid, error = validate_image_dimensions(1200, 630)
        assert is_valid is True
        assert error is None

    def test_valid_square_dimensions_pass(self):
        """Square images pass validation."""
        is_valid, error = validate_image_dimensions(800, 800)
        assert is_valid is True
        assert error is None

    def test_width_too_small_fails(self):
        """Image too narrow fails validation."""
        is_valid, error = validate_image_dimensions(100, 800)
        assert is_valid is False
        assert "too narrow" in error
        assert "100px" in error

    def test_height_too_small_fails(self):
        """Image too short fails validation."""
        is_valid, error = validate_image_dimensions(800, 100)
        assert is_valid is False
        assert "too short" in error
        assert "100px" in error

    def test_aspect_ratio_too_tall_fails(self):
        """Very tall images (narrow aspect ratio) fail validation."""
        # Width/Height < 1/4, so very tall image
        is_valid, error = validate_image_dimensions(200, 1000)  # 1:5 ratio
        assert is_valid is False
        assert "too tall" in error

    def test_aspect_ratio_too_wide_fails(self):
        """Very wide images (wide aspect ratio) fail validation."""
        # Width/Height > 4/1, so very wide image
        is_valid, error = validate_image_dimensions(1000, 200)  # 5:1 ratio
        assert is_valid is False
        assert "too wide" in error

    def test_edge_case_exactly_min_aspect_ratio_passes(self):
        """Aspect ratio exactly at MIN_ASPECT_RATIO passes."""
        # 1:4 ratio exactly
        is_valid, error = validate_image_dimensions(250, 1000)
        assert is_valid is True
        assert error is None

    def test_edge_case_exactly_max_aspect_ratio_passes(self):
        """Aspect ratio exactly at MAX_ASPECT_RATIO passes."""
        # 4:1 ratio exactly
        is_valid, error = validate_image_dimensions(1000, 250)
        assert is_valid is True
        assert error is None

    def test_edge_case_exactly_min_dimension_passes(self):
        """Dimensions exactly at MIN_DIMENSION pass."""
        is_valid, error = validate_image_dimensions(MIN_DIMENSION, MIN_DIMENSION)
        assert is_valid is True
        assert error is None

    def test_custom_min_dimension(self):
        """Custom min_dimension is respected."""
        # 100x100 would fail with default (200), but pass with min=50
        is_valid, error = validate_image_dimensions(100, 100, min_dimension=50)
        assert is_valid is True
        assert error is None

        # 100x100 fails with min=150
        is_valid, error = validate_image_dimensions(100, 100, min_dimension=150)
        assert is_valid is False
        assert "100px" in error

    @given(st.integers(min_value=MIN_DIMENSION, max_value=5000), st.integers(min_value=MIN_DIMENSION, max_value=5000))
    @settings(max_examples=50)
    def test_property_valid_dimensions_within_ratio(self, width, height):
        """Property: dimensions >= MIN_DIMENSION with reasonable aspect ratio pass."""
        aspect_ratio = width / height
        expected_valid = MIN_ASPECT_RATIO <= aspect_ratio <= MAX_ASPECT_RATIO

        is_valid, error = validate_image_dimensions(width, height)
        assert is_valid == expected_valid


class TestProcessCoverImageInputValidation:
    """Property-based tests for input validation and error handling."""

    def test_missing_input_file_raises_error(self):
        """Non-existent input file raises ImageProcessingError."""
        with tempfile.TemporaryDirectory() as tmpdir:
            output_path = str(Path(tmpdir) / "output.jpg")
            with pytest.raises(ImageProcessingError, match="input file not found"):
                process_cover_image("/nonexistent/path/image.jpg", output_path)

    def test_image_too_small_raises_error(self):
        """Image with dimensions below minimum raises ImageProcessingError."""
        with tempfile.TemporaryDirectory() as tmpdir:
            input_path = str(Path(tmpdir) / "tiny.jpg")
            output_path = str(Path(tmpdir) / "output.jpg")

            from PIL import Image

            tiny_image = Image.new("RGB", (100, 100), color="red")
            tiny_image.save(input_path, format="JPEG")

            with pytest.raises(ImageProcessingError, match="too narrow|too short"):
                process_cover_image(input_path, output_path)

    def test_image_too_tall_raises_error(self):
        """Image with extreme tall aspect ratio raises ImageProcessingError."""
        with tempfile.TemporaryDirectory() as tmpdir:
            input_path = str(Path(tmpdir) / "tall.jpg")
            output_path = str(Path(tmpdir) / "output.jpg")

            from PIL import Image

            # Very tall image: 200x1000 (1:5 ratio, exceeds 1:4 limit)
            tall_image = Image.new("RGB", (200, 1000), color="blue")
            tall_image.save(input_path, format="JPEG")

            with pytest.raises(ImageProcessingError, match="too tall"):
                process_cover_image(input_path, output_path)

    def test_image_too_wide_raises_error(self):
        """Image with extreme wide aspect ratio raises ImageProcessingError."""
        with tempfile.TemporaryDirectory() as tmpdir:
            input_path = str(Path(tmpdir) / "wide.jpg")
            output_path = str(Path(tmpdir) / "output.jpg")

            from PIL import Image

            # Very wide image: 1000x200 (5:1 ratio, exceeds 4:1 limit)
            wide_image = Image.new("RGB", (1000, 200), color="green")
            wide_image.save(input_path, format="JPEG")

            with pytest.raises(ImageProcessingError, match="too wide"):
                process_cover_image(input_path, output_path)

    @given(st.sampled_from([".bmp", ".gif", ".tiff", ".ico"]))
    def test_unsupported_format_raises_error(self, extension):
        """Unsupported image formats raise ImageProcessingError."""
        with tempfile.TemporaryDirectory() as tmpdir:
            input_path = str(Path(tmpdir) / f"image{extension}")
            output_path = str(Path(tmpdir) / "output.jpg")

            from PIL import Image

            unsupported_image = Image.new("RGB", (100, 100), color="red")
            unsupported_image.save(input_path, format="BMP" if extension == ".bmp" else "GIF")

            with pytest.raises(ImageProcessingError, match="unsupported image format"):
                process_cover_image(input_path, output_path)


class TestProcessCoverImageDimensions:
    """Property-based tests for dimension handling and resizing."""

    def test_small_image_no_upscale(self):
        """Images smaller than target are not upscaled."""
        with tempfile.TemporaryDirectory() as tmpdir:
            input_path = str(Path(tmpdir) / "small.jpg")
            output_path = str(Path(tmpdir) / "output.jpg")

            from PIL import Image

            small_image = Image.new("RGB", (400, 200), color="blue")
            small_image.save(input_path, format="JPEG")

            path, width, height = process_cover_image(input_path, output_path)

            assert path == output_path
            assert width == 400
            assert height == 200

    def test_large_image_resized_to_target(self):
        """Images larger than target are resized to target dimensions."""
        with tempfile.TemporaryDirectory() as tmpdir:
            input_path = str(Path(tmpdir) / "large.jpg")
            output_path = str(Path(tmpdir) / "output.jpg")

            from PIL import Image

            large_image = Image.new("RGB", (3000, 2000), color="green")
            large_image.save(input_path, format="JPEG")

            path, width, height = process_cover_image(input_path, output_path, target_width=1200, target_height=630)

            assert path == output_path
            assert width == 1200
            assert height == 630

    def test_custom_target_dimensions(self):
        """Custom target dimensions are respected."""
        with tempfile.TemporaryDirectory() as tmpdir:
            input_path = str(Path(tmpdir) / "image.jpg")
            output_path = str(Path(tmpdir) / "output.jpg")

            from PIL import Image

            image = Image.new("RGB", (2000, 1000), color="cyan")
            image.save(input_path, format="JPEG")

            path, width, height = process_cover_image(input_path, output_path, target_width=800, target_height=400)

            assert path == output_path
            assert width == 800
            assert height == 400

    @given(
        st.integers(min_value=MIN_DIMENSION, max_value=2000),
        st.integers(min_value=MIN_DIMENSION, max_value=2000),
        st.integers(min_value=MIN_DIMENSION, max_value=2000),
        st.integers(min_value=MIN_DIMENSION, max_value=2000),
    )
    @settings(max_examples=20, deadline=500)  # Increased deadline due to cache cleanup I/O
    def test_output_dimensions_respect_no_upscale(self, img_width, img_height, target_w, target_h):
        """Output dimensions never exceed input dimensions when input is small."""
        # Skip invalid aspect ratios that would be rejected by validation
        aspect_ratio = img_width / img_height
        if aspect_ratio < MIN_ASPECT_RATIO or aspect_ratio > MAX_ASPECT_RATIO:
            return  # Skip this test case

        with tempfile.TemporaryDirectory() as tmpdir:
            input_path = str(Path(tmpdir) / "image.jpg")
            output_path = str(Path(tmpdir) / "output.jpg")

            from PIL import Image

            image = Image.new("RGB", (img_width, img_height), color="magenta")
            image.save(input_path, format="JPEG")

            path, width, height = process_cover_image(
                input_path, output_path, target_width=target_w, target_height=target_h
            )

            if img_width < target_w and img_height < target_h:
                assert width == img_width
                assert height == img_height
            else:
                assert width == target_w
                assert height == target_h

    @given(st.integers(min_value=500, max_value=2000), st.integers(min_value=500, max_value=2000))
    @settings(max_examples=15)
    def test_aspect_ratio_preserved_via_center_crop(self, img_width, img_height):
        """Aspect ratio is preserved through center-crop for aspect ratio mismatch."""
        # Skip invalid aspect ratios that would be rejected by validation
        aspect_ratio = img_width / img_height
        if aspect_ratio < MIN_ASPECT_RATIO or aspect_ratio > MAX_ASPECT_RATIO:
            return  # Skip this test case

        with tempfile.TemporaryDirectory() as tmpdir:
            input_path = str(Path(tmpdir) / "image.jpg")
            output_path = str(Path(tmpdir) / "output.jpg")

            from PIL import Image

            image = Image.new("RGB", (img_width, img_height), color="yellow")
            image.save(input_path, format="JPEG")

            target_w, target_h = 1200, 630
            path, width, height = process_cover_image(
                input_path, output_path, target_width=target_w, target_height=target_h
            )

            if img_width < target_w and img_height < target_h:
                assert (width, height) == (img_width, img_height)
            else:
                assert (width, height) == (target_w, target_h)


class TestProcessCoverImageExifStripping:
    """Property-based tests for EXIF metadata stripping."""

    def test_exif_stripped_from_output(self):
        """EXIF metadata is removed from processed image."""
        with tempfile.TemporaryDirectory() as tmpdir:
            input_path = str(Path(tmpdir) / "image.jpg")
            output_path = str(Path(tmpdir) / "output.jpg")

            from PIL import Image

            image = Image.new("RGB", (1500, 800), color="red")
            image.save(input_path, format="JPEG")

            process_cover_image(input_path, output_path)

            from PIL import Image as PILImage

            output_image = PILImage.open(output_path)
            exif_data = output_image.getexif()

            assert len(exif_data) == 0, "EXIF data should be stripped"

    def test_exif_stripping_idempotent(self):
        """Processing an already-processed image produces consistent result."""
        with tempfile.TemporaryDirectory() as tmpdir:
            input_path = str(Path(tmpdir) / "image.jpg")
            first_output = str(Path(tmpdir) / "first_output.jpg")
            second_output = str(Path(tmpdir) / "second_output.jpg")

            from PIL import Image

            image = Image.new("RGB", (2000, 1200), color="orange")
            image.save(input_path, format="JPEG")

            path1, w1, h1 = process_cover_image(input_path, first_output)
            path2, w2, h2 = process_cover_image(first_output, second_output)

            assert (w1, h1) == (w2, h2), "Dimensions should be consistent on re-processing"

            with open(first_output, "rb") as f1, open(second_output, "rb") as f2:
                bytes1 = f1.read()
                bytes2 = f2.read()

            assert bytes1 == bytes2, "Re-processing should produce identical output"


class TestProcessCoverImageFormats:
    """Property-based tests for supported image formats."""

    @given(st.sampled_from(["JPEG", "PNG", "WEBP"]))
    @settings(deadline=None)
    def test_supported_formats_process_successfully(self, format_name):
        """All supported formats (JPEG, PNG, WebP) are processed successfully."""
        with tempfile.TemporaryDirectory() as tmpdir:
            input_path = str(Path(tmpdir) / f"image.{format_name.lower()}")
            output_path = str(Path(tmpdir) / "output.jpg")

            from PIL import Image

            image = Image.new("RGB", (1500, 900), color="purple")
            image.save(input_path, format=format_name)

            path, width, height = process_cover_image(input_path, output_path)

            assert path == output_path
            assert width > 0
            assert height > 0

    def test_output_always_jpeg_format(self):
        """Output is always JPEG format regardless of input format."""
        with tempfile.TemporaryDirectory() as tmpdir:
            for input_format in ["PNG", "WEBP"]:
                input_path = str(Path(tmpdir) / f"image_{input_format}.{input_format.lower()}")
                output_path = str(Path(tmpdir) / f"output_{input_format}.jpg")

                from PIL import Image as PILImage

                image = PILImage.new("RGB", (1200, 800), color="teal")
                image.save(input_path, format=input_format)

                process_cover_image(input_path, output_path)

                output_image = PILImage.open(output_path)
                assert output_image.format == "JPEG", f"Output should be JPEG, got {output_image.format}"


class TestProcessCoverImageDeterminism:
    """Property-based tests for deterministic output."""

    def test_same_input_produces_identical_output_bytes(self):
        """Same input image produces identical output bytes (deterministic)."""
        with tempfile.TemporaryDirectory() as tmpdir:
            input_path = str(Path(tmpdir) / "input.jpg")
            output_path_1 = str(Path(tmpdir) / "output_1.jpg")
            output_path_2 = str(Path(tmpdir) / "output_2.jpg")

            from PIL import Image

            image = Image.new("RGB", (2400, 1440), color="navy")
            image.save(input_path, format="JPEG")

            process_cover_image(input_path, output_path_1)
            process_cover_image(input_path, output_path_2)

            with open(output_path_1, "rb") as f1, open(output_path_2, "rb") as f2:
                bytes_1 = f1.read()
                bytes_2 = f2.read()

            assert bytes_1 == bytes_2, "Same input should produce identical output bytes"

    def test_return_value_matches_output_path(self):
        """Return value's first element matches the provided output_path."""
        with tempfile.TemporaryDirectory() as tmpdir:
            input_path = str(Path(tmpdir) / "input.jpg")
            output_path = str(Path(tmpdir) / "subdir" / "output.jpg")

            from PIL import Image

            image = Image.new("RGB", (1800, 1200), color="olive")
            image.save(input_path, format="JPEG")

            returned_path, width, height = process_cover_image(input_path, output_path)

            assert returned_path == output_path, "Returned path must match output_path"
            assert Path(output_path).exists(), "Output file must exist at output_path"


class TestProcessCoverImageOutputDirectory:
    """Property-based tests for output directory creation."""

    def test_output_directory_created_if_missing(self):
        """Output directory is created if it doesn't exist."""
        with tempfile.TemporaryDirectory() as tmpdir:
            input_path = str(Path(tmpdir) / "input.jpg")
            output_path = str(Path(tmpdir) / "deep" / "nested" / "dirs" / "output.jpg")

            from PIL import Image

            image = Image.new("RGB", (1000, 800), color="maroon")
            image.save(input_path, format="JPEG")

            assert not Path(output_path).parent.exists()

            process_cover_image(input_path, output_path)

            assert Path(output_path).parent.exists(), "Output directory should be created"
            assert Path(output_path).exists(), "Output file should exist"

    def test_output_directory_not_created_if_exists(self):
        """Output directory creation is idempotent."""
        with tempfile.TemporaryDirectory() as tmpdir:
            input_path = str(Path(tmpdir) / "input.jpg")
            output_dir = Path(tmpdir) / "output" / "dir"
            output_dir.mkdir(parents=True, exist_ok=True)
            output_path = str(output_dir / "output.jpg")

            from PIL import Image

            image = Image.new("RGB", (1000, 800), color="navy")
            image.save(input_path, format="JPEG")

            process_cover_image(input_path, output_path)

            assert output_dir.exists()
            assert Path(output_path).exists()


class TestProcessCoverImageQuality:
    """Property-based tests for output quality and format settings."""

    def test_output_is_valid_jpeg(self):
        """Output is a valid JPEG file."""
        with tempfile.TemporaryDirectory() as tmpdir:
            input_path = str(Path(tmpdir) / "input.jpg")
            output_path = str(Path(tmpdir) / "output.jpg")

            from PIL import Image

            image = Image.new("RGB", (1200, 630), color="pink")
            image.save(input_path, format="JPEG")

            process_cover_image(input_path, output_path)

            from PIL import Image as PILImage

            output_image = PILImage.open(output_path)
            assert output_image.format == "JPEG"
            assert output_image.mode == "RGB"

    def test_output_has_valid_dimensions_and_content(self):
        """Output image has valid dimensions and readable content."""
        with tempfile.TemporaryDirectory() as tmpdir:
            input_path = str(Path(tmpdir) / "input.png")
            output_path = str(Path(tmpdir) / "output.jpg")

            from PIL import Image

            image = Image.new("RGB", (1600, 900), color="lime")
            image.save(input_path, format="PNG")

            returned_path, width, height = process_cover_image(
                input_path, output_path, target_width=1200, target_height=630
            )

            from PIL import Image as PILImage

            output_image = PILImage.open(output_path)
            assert output_image.size == (width, height)


class TestGetProcessedCoverPath:
    """Property-based tests for processed cover path generation."""

    def test_path_structure_correct(self):
        """Generated path has correct structure and is absolute."""
        md_path = "/some/path/article.md"
        slug = "my-article"

        result = get_processed_cover_path(md_path, slug)

        assert result.startswith("/"), "Path should be absolute"
        assert ".nostr-publish/cache/covers" in result, "Path should contain cache structure"
        assert slug in result, "Path should contain slug"
        assert result.endswith("cover.jpg"), "Path should end with cover.jpg"

    def test_path_deterministic(self):
        """Same inputs always produce same path."""
        md_path = "/var/articles/post.md"
        slug = "test-article"

        result1 = get_processed_cover_path(md_path, slug)
        result2 = get_processed_cover_path(md_path, slug)

        assert result1 == result2, "Path generation should be deterministic"

    def test_path_includes_markdown_directory(self):
        """Generated path is relative to the markdown file's directory."""
        md_path = "/home/user/docs/articles/my-article.md"
        slug = "article-slug"

        result = get_processed_cover_path(md_path, slug)

        assert result.startswith("/home/user/docs/articles"), "Path should be in markdown directory"

    @given(
        st.just("/home/author/documents/"),
        st.text(alphabet="abcdefghijklmnopqrstuvwxyz-_0123456789", min_size=1, max_size=50),
    )
    @settings(max_examples=20)
    def test_path_generation_with_various_slugs(self, base_path, slug):
        """Path generation works with various slug formats."""
        md_path = base_path + "article.md"

        result = get_processed_cover_path(md_path, slug)

        assert isinstance(result, str)
        assert ".nostr-publish/cache/covers" in result
        assert slug in result
        assert result.endswith("cover.jpg")

    def test_path_contains_all_slug_characters(self):
        """Generated path includes all slug characters."""
        md_path = "/docs/article.md"
        slug = "my-special_article.v2"

        result = get_processed_cover_path(md_path, slug)

        assert slug in result, f"Slug '{slug}' should be fully included in path"

    def test_path_with_trailing_slash_in_markdown_path(self):
        """Path generation handles markdown path with trailing slash."""
        md_path = "/home/user/"
        slug = "test"

        result = get_processed_cover_path(md_path, slug)

        assert isinstance(result, str)
        assert ".nostr-publish/cache/covers" in result


class TestProcessCoverImageIntegration:
    """Integration tests combining multiple properties."""

    def test_full_pipeline_png_to_jpeg_large_image(self):
        """Full pipeline: PNG input, large image, resized to target, EXIF stripped."""
        with tempfile.TemporaryDirectory() as tmpdir:
            input_path = str(Path(tmpdir) / "input.png")
            output_path = str(Path(tmpdir) / "output.jpg")

            from PIL import Image

            image = Image.new("RGB", (3000, 2000), color="red")
            image.save(input_path, format="PNG")

            path, width, height = process_cover_image(input_path, output_path)

            assert path == output_path
            assert width == 1200
            assert height == 630

            from PIL import Image as PILImage

            output_image = PILImage.open(output_path)
            assert output_image.format == "JPEG"
            assert len(output_image.getexif()) == 0

    def test_full_pipeline_webp_to_jpeg_small_image(self):
        """Full pipeline: WebP input, small image, no upscale, EXIF stripped."""
        with tempfile.TemporaryDirectory() as tmpdir:
            input_path = str(Path(tmpdir) / "input.webp")
            output_path = str(Path(tmpdir) / "output.jpg")

            from PIL import Image

            image = Image.new("RGB", (600, 400), color="blue")
            image.save(input_path, format="WEBP")

            path, width, height = process_cover_image(input_path, output_path)

            assert path == output_path
            assert width == 600
            assert height == 400

            from PIL import Image as PILImage

            output_image = PILImage.open(output_path)
            assert output_image.format == "JPEG"

    def test_deterministic_path_and_processing(self):
        """Integration: path generation and image processing are both deterministic."""
        with tempfile.TemporaryDirectory() as tmpdir:
            md_path = str(Path(tmpdir) / "article.md")
            slug = "test-article"

            path1 = get_processed_cover_path(md_path, slug)
            path2 = get_processed_cover_path(md_path, slug)

            assert path1 == path2

            input_path = str(Path(tmpdir) / "input.jpg")

            from PIL import Image

            image = Image.new("RGB", (2000, 1200), color="green")
            image.save(input_path, format="JPEG")

            output1, w1, h1 = process_cover_image(input_path, path1)
            output2, w2, h2 = process_cover_image(input_path, path2)

            assert (w1, h1) == (w2, h2)

            with open(output1, "rb") as f1, open(output2, "rb") as f2:
                assert f1.read() == f2.read()
