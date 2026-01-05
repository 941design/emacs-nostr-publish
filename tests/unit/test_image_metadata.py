"""Unit tests for image metadata parsing and validation.

Property-based tests for parse_image_field, infer_mime_type, and validate_image_metadata functions.
"""

import pytest
from hypothesis import assume, given
from hypothesis import strategies as st

from nostr_publish.errors import InvalidFieldTypeError, InvalidFieldValueError
from nostr_publish.image_metadata import infer_mime_type, parse_image_field, validate_image_metadata
from nostr_publish.models import ImageMetadata


class TestParseImageField:
    """Property-based tests for parse_image_field function."""

    def test_none_input_returns_none(self):
        """None input returns None."""
        result = parse_image_field(None)
        assert result is None

    def test_string_input_creates_image_with_url_only(self):
        """String input creates ImageMetadata with url set, other fields None."""
        url = "https://example.com/image.jpg"
        result = parse_image_field(url)
        assert result is not None
        assert result.url == url
        assert result.mime is None
        assert result.alt is None
        assert result.dim is None

    def test_dict_with_url_only(self):
        """Dict with only url key creates ImageMetadata."""
        data = {"url": "https://example.com/image.png"}
        result = parse_image_field(data)
        assert result is not None
        assert result.url == "https://example.com/image.png"
        assert result.mime is None
        assert result.alt is None
        assert result.dim is None

    def test_dict_with_all_fields(self):
        """Dict with all fields populates ImageMetadata completely."""
        data = {"url": "https://example.com/image.jpg", "mime": "image/jpeg", "alt": "Test image", "dim": "1920x1080"}
        result = parse_image_field(data)
        assert result is not None
        assert result.url == "https://example.com/image.jpg"
        assert result.mime == "image/jpeg"
        assert result.alt == "Test image"
        assert result.dim == "1920x1080"

    def test_dict_with_optional_fields_missing(self):
        """Dict with some optional fields omitted returns None for missing fields."""
        data = {"url": "https://example.com/image.gif", "alt": "An image"}
        result = parse_image_field(data)
        assert result is not None
        assert result.url == "https://example.com/image.gif"
        assert result.mime is None
        assert result.alt == "An image"
        assert result.dim is None

    def test_dict_with_extra_keys_raises_error(self):
        """Dict with unknown keys raises InvalidFieldValueError (strict mode per spec 5.5)."""
        data = {
            "url": "https://example.com/image.webp",
            "mime": "image/webp",
            "extra_key": "should cause error",
            "another_extra": 123,
        }
        with pytest.raises(InvalidFieldValueError) as exc_info:
            parse_image_field(data)
        assert "unknown keys in image object" in str(exc_info.value)
        assert "another_extra" in str(exc_info.value)
        assert "extra_key" in str(exc_info.value)

    def test_dict_with_single_unknown_key_raises_error(self):
        """Dict with single unknown key raises InvalidFieldValueError."""
        data = {"url": "https://example.com/image.jpg", "unknown_field": "value"}
        with pytest.raises(InvalidFieldValueError) as exc_info:
            parse_image_field(data)
        assert "unknown keys in image object" in str(exc_info.value)
        assert "unknown_field" in str(exc_info.value)

    def test_integer_input_raises_error(self):
        """Integer input raises InvalidFieldTypeError."""
        with pytest.raises(InvalidFieldTypeError):
            parse_image_field(123)

    def test_list_input_raises_error(self):
        """List input raises InvalidFieldTypeError."""
        with pytest.raises(InvalidFieldTypeError):
            parse_image_field(["https://example.com/image.jpg"])

    def test_boolean_input_raises_error(self):
        """Boolean input raises InvalidFieldTypeError."""
        with pytest.raises(InvalidFieldTypeError):
            parse_image_field(True)

    def test_float_input_raises_error(self):
        """Float input raises InvalidFieldTypeError."""
        with pytest.raises(InvalidFieldTypeError):
            parse_image_field(3.14)

    def test_deterministic_same_input_same_output(self):
        """Same input always yields same output."""
        url = "https://example.com/image.jpg"
        result1 = parse_image_field(url)
        result2 = parse_image_field(url)
        assert result1.url == result2.url
        assert result1.mime == result2.mime
        assert result1.alt == result2.alt
        assert result1.dim == result2.dim

    def test_deterministic_dict_same_input_same_output(self):
        """Same dict input always yields same output."""
        data = {"url": "https://example.com/image.png", "mime": "image/png", "alt": "Test", "dim": "800x600"}
        result1 = parse_image_field(data)
        result2 = parse_image_field(data)
        assert result1.url == result2.url
        assert result1.mime == result2.mime
        assert result1.alt == result2.alt
        assert result1.dim == result2.dim

    def test_type_safe_returns_image_metadata_or_none(self):
        """Result is always ImageMetadata or None."""
        assert parse_image_field(None) is None
        assert isinstance(parse_image_field("https://example.com/img.jpg"), ImageMetadata)
        assert isinstance(parse_image_field({"url": "https://example.com/img.jpg"}), ImageMetadata)

    def test_normalized_string_and_dict_produce_compatible_results(self):
        """String URL and dict with same URL produce structurally similar results."""
        url = "https://example.com/image.jpg"
        result_string = parse_image_field(url)
        result_dict = parse_image_field({"url": url})
        assert result_string.url == result_dict.url
        assert result_string.mime == result_dict.mime
        assert result_string.alt == result_dict.alt
        assert result_string.dim == result_dict.dim

    @given(st.text(min_size=1, max_size=200))
    def test_string_url_preserved_exactly(self, url_text):
        """String input is preserved exactly as url field."""
        assume("---" not in url_text)
        result = parse_image_field(url_text)
        assert result.url == url_text

    @given(st.fixed_dictionaries({"url": st.text(min_size=1, max_size=200)}).filter(lambda d: "---" not in d["url"]))
    def test_dict_url_extracted_exactly(self, data):
        """URL from dict is extracted exactly."""
        result = parse_image_field(data)
        assert result.url == data["url"]

    def test_dict_none_values_in_optional_fields(self):
        """Dict with explicit None values for optional fields."""
        data = {"url": "https://example.com/image.jpg", "mime": None, "alt": None, "dim": None}
        result = parse_image_field(data)
        assert result.url == "https://example.com/image.jpg"
        assert result.mime is None
        assert result.alt is None
        assert result.dim is None


class TestInferMimeType:
    """Property-based tests for infer_mime_type function."""

    def test_jpg_extension_returns_image_jpeg(self):
        """URL with .jpg extension returns image/jpeg."""
        assert infer_mime_type("https://example.com/image.jpg") == "image/jpeg"

    def test_jpeg_extension_returns_image_jpeg(self):
        """URL with .jpeg extension returns image/jpeg."""
        assert infer_mime_type("https://example.com/image.jpeg") == "image/jpeg"

    def test_png_extension_returns_image_png(self):
        """URL with .png extension returns image/png."""
        assert infer_mime_type("https://example.com/image.png") == "image/png"

    def test_gif_extension_returns_image_gif(self):
        """URL with .gif extension returns image/gif."""
        assert infer_mime_type("https://example.com/image.gif") == "image/gif"

    def test_webp_extension_returns_image_webp(self):
        """URL with .webp extension returns image/webp."""
        assert infer_mime_type("https://example.com/image.webp") == "image/webp"

    def test_svg_extension_returns_image_svg(self):
        """URL with .svg extension returns image/svg+xml."""
        assert infer_mime_type("https://example.com/image.svg") == "image/svg+xml"

    def test_uppercase_jpg_extension_case_insensitive(self):
        """URL with .JPG (uppercase) returns image/jpeg."""
        assert infer_mime_type("https://example.com/image.JPG") == "image/jpeg"

    def test_mixed_case_png_extension_case_insensitive(self):
        """URL with .PNG (uppercase) returns image/png."""
        assert infer_mime_type("https://example.com/image.PNG") == "image/png"

    def test_mixed_case_jpeg_extension_case_insensitive(self):
        """URL with .JpEg (mixed case) returns image/jpeg."""
        assert infer_mime_type("https://example.com/image.JpEg") == "image/jpeg"

    def test_unknown_extension_returns_none(self):
        """URL with unknown extension returns None."""
        assert infer_mime_type("https://example.com/image.bmp") is None

    def test_txt_extension_returns_none(self):
        """URL with .txt extension returns None."""
        assert infer_mime_type("https://example.com/file.txt") is None

    def test_no_extension_returns_none(self):
        """URL without extension returns None."""
        assert infer_mime_type("https://example.com/image") is None

    def test_url_ending_with_dot_returns_none(self):
        """URL ending with dot (no extension) returns None."""
        assert infer_mime_type("https://example.com/image.") is None

    def test_multiple_dots_uses_last_extension(self):
        """URL with multiple dots uses last extension only."""
        assert infer_mime_type("https://example.com/archive.tar.jpg") == "image/jpeg"

    def test_query_parameters_ignored(self):
        """Query parameters after extension are included in extension."""
        result = infer_mime_type("https://example.com/image.jpg?size=large")
        assert result is None

    def test_url_fragment_ignored(self):
        """Fragment after extension affects extension parsing."""
        result = infer_mime_type("https://example.com/image.jpg#anchor")
        assert result is None

    def test_deterministic_same_url_same_mime(self):
        """Same URL always yields same MIME type."""
        url = "https://example.com/image.png"
        result1 = infer_mime_type(url)
        result2 = infer_mime_type(url)
        assert result1 == result2

    def test_case_insensitive_same_result(self):
        """Different case extensions yield same MIME type."""
        url_lower = "https://example.com/image.jpg"
        url_upper = "https://example.com/image.JPG"
        assert infer_mime_type(url_lower) == infer_mime_type(url_upper)

    @given(st.sampled_from(["jpg", "jpeg", "png", "gif", "webp", "svg"]))
    def test_all_supported_extensions_return_mime(self, extension):
        """All documented extensions return a MIME type."""
        url = f"https://example.com/image.{extension}"
        result = infer_mime_type(url)
        assert result is not None
        assert "/" in result

    @given(
        st.text(min_size=1, max_size=50).filter(
            lambda x: x.lower().lstrip(".") not in ["jpg", "jpeg", "png", "gif", "webp", "svg"]
            and not any(x.lower().endswith(f".{ext}") for ext in ["jpg", "jpeg", "png", "gif", "webp", "svg"])
        )
    )
    def test_unsupported_extensions_return_none(self, extension):
        """Unsupported extensions return None."""
        url = f"https://example.com/image.{extension}"
        result = infer_mime_type(url)
        assert result is None


class TestValidateImageMetadata:
    """Property-based tests for validate_image_metadata function."""

    def test_valid_url_with_http_passes(self):
        """Valid URL with http:// passes validation."""
        image = ImageMetadata(url="http://example.com/image.jpg")
        result = validate_image_metadata(image)
        assert result.url == "http://example.com/image.jpg"

    def test_valid_url_with_https_passes(self):
        """Valid URL with https:// passes validation."""
        image = ImageMetadata(url="https://example.com/image.jpg")
        result = validate_image_metadata(image)
        assert result.url == "https://example.com/image.jpg"

    def test_url_with_leading_whitespace_trimmed(self):
        """URL with leading whitespace is trimmed."""
        image = ImageMetadata(url="  https://example.com/image.jpg")
        result = validate_image_metadata(image)
        assert result.url == "https://example.com/image.jpg"

    def test_url_with_trailing_whitespace_trimmed(self):
        """URL with trailing whitespace is trimmed."""
        image = ImageMetadata(url="https://example.com/image.jpg  ")
        result = validate_image_metadata(image)
        assert result.url == "https://example.com/image.jpg"

    def test_url_with_both_leading_and_trailing_whitespace_trimmed(self):
        """URL with both leading and trailing whitespace is trimmed."""
        image = ImageMetadata(url="  https://example.com/image.jpg  ")
        result = validate_image_metadata(image)
        assert result.url == "https://example.com/image.jpg"

    def test_empty_url_raises_error(self):
        """Empty URL raises InvalidFieldValueError."""
        image = ImageMetadata(url="")
        with pytest.raises(InvalidFieldValueError):
            validate_image_metadata(image)

    def test_whitespace_only_url_raises_error(self):
        """Whitespace-only URL raises InvalidFieldValueError."""
        image = ImageMetadata(url="   \t  ")
        with pytest.raises(InvalidFieldValueError):
            validate_image_metadata(image)

    def test_url_without_http_prefix_raises_error(self):
        """URL without http:// or https:// raises InvalidFieldValueError."""
        image = ImageMetadata(url="ftp://example.com/image.jpg")
        with pytest.raises(InvalidFieldValueError):
            validate_image_metadata(image)

    def test_url_starting_with_www_raises_error(self):
        """URL starting with www (no protocol) raises InvalidFieldValueError."""
        image = ImageMetadata(url="www.example.com/image.jpg")
        with pytest.raises(InvalidFieldValueError):
            validate_image_metadata(image)

    def test_non_string_url_raises_type_error(self):
        """Non-string URL raises InvalidFieldTypeError."""
        image = ImageMetadata(url=123)
        with pytest.raises(InvalidFieldTypeError):
            validate_image_metadata(image)

    def test_none_mime_inferred_from_known_extension(self):
        """None mime is inferred from URL with known extension."""
        image = ImageMetadata(url="https://example.com/image.jpg", mime=None)
        result = validate_image_metadata(image)
        assert result.mime == "image/jpeg"

    def test_none_mime_with_unknown_extension_stays_none(self):
        """None mime stays None if URL has unknown extension."""
        image = ImageMetadata(url="https://example.com/image.bmp", mime=None)
        result = validate_image_metadata(image)
        assert result.mime is None

    def test_none_mime_with_no_extension_stays_none(self):
        """None mime stays None if URL has no extension."""
        image = ImageMetadata(url="https://example.com/image", mime=None)
        result = validate_image_metadata(image)
        assert result.mime is None

    def test_valid_mime_type_preserved(self):
        """Valid MIME type is preserved."""
        image = ImageMetadata(url="https://example.com/image.jpg", mime="image/jpeg")
        result = validate_image_metadata(image)
        assert result.mime == "image/jpeg"

    def test_mime_with_whitespace_trimmed(self):
        """MIME type with whitespace is trimmed."""
        image = ImageMetadata(url="https://example.com/image.jpg", mime="  image/jpeg  ")
        result = validate_image_metadata(image)
        assert result.mime == "image/jpeg"

    def test_mime_without_slash_raises_error(self):
        """MIME type without slash raises InvalidFieldValueError."""
        image = ImageMetadata(url="https://example.com/image.jpg", mime="imagejpeg")
        with pytest.raises(InvalidFieldValueError):
            validate_image_metadata(image)

    def test_mime_only_whitespace_raises_error(self):
        """MIME type with only whitespace raises InvalidFieldValueError."""
        image = ImageMetadata(url="https://example.com/image.jpg", mime="   \t  ")
        with pytest.raises(InvalidFieldValueError):
            validate_image_metadata(image)

    def test_non_string_mime_raises_type_error(self):
        """Non-string MIME type raises InvalidFieldTypeError."""
        image = ImageMetadata(url="https://example.com/image.jpg", mime=123)
        with pytest.raises(InvalidFieldTypeError):
            validate_image_metadata(image)

    def test_valid_alt_text_preserved(self):
        """Valid alt text is preserved."""
        image = ImageMetadata(url="https://example.com/image.jpg", alt="A test image")
        result = validate_image_metadata(image)
        assert result.alt == "A test image"

    def test_alt_with_whitespace_trimmed(self):
        """Alt text with whitespace is trimmed."""
        image = ImageMetadata(url="https://example.com/image.jpg", alt="  A test image  ")
        result = validate_image_metadata(image)
        assert result.alt == "A test image"

    def test_alt_only_whitespace_raises_error(self):
        """Alt text with only whitespace raises InvalidFieldValueError."""
        image = ImageMetadata(url="https://example.com/image.jpg", alt="   \t  ")
        with pytest.raises(InvalidFieldValueError):
            validate_image_metadata(image)

    def test_non_string_alt_raises_type_error(self):
        """Non-string alt text raises InvalidFieldTypeError."""
        image = ImageMetadata(url="https://example.com/image.jpg", alt=123)
        with pytest.raises(InvalidFieldTypeError):
            validate_image_metadata(image)

    def test_none_alt_stays_none(self):
        """None alt text stays None."""
        image = ImageMetadata(url="https://example.com/image.jpg", alt=None)
        result = validate_image_metadata(image)
        assert result.alt is None

    def test_valid_dimension_format_preserved(self):
        """Valid dimension format is preserved."""
        image = ImageMetadata(url="https://example.com/image.jpg", dim="1920x1080")
        result = validate_image_metadata(image)
        assert result.dim == "1920x1080"

    def test_single_digit_dimensions_valid(self):
        """Single digit dimensions are valid."""
        image = ImageMetadata(url="https://example.com/image.jpg", dim="1x1")
        result = validate_image_metadata(image)
        assert result.dim == "1x1"

    def test_large_dimension_values_valid(self):
        """Large dimension values are valid."""
        image = ImageMetadata(url="https://example.com/image.jpg", dim="8192x4096")
        result = validate_image_metadata(image)
        assert result.dim == "8192x4096"

    def test_dimension_with_whitespace_trimmed(self):
        """Dimension with whitespace is trimmed."""
        image = ImageMetadata(url="https://example.com/image.jpg", dim="  1920x1080  ")
        result = validate_image_metadata(image)
        assert result.dim == "1920x1080"

    def test_dimension_only_whitespace_raises_error(self):
        """Dimension with only whitespace raises InvalidFieldValueError."""
        image = ImageMetadata(url="https://example.com/image.jpg", dim="   \t  ")
        with pytest.raises(InvalidFieldValueError):
            validate_image_metadata(image)

    def test_dimension_with_non_numeric_raises_error(self):
        """Dimension with non-numeric values raises InvalidFieldValueError."""
        image = ImageMetadata(url="https://example.com/image.jpg", dim="1920xABC")
        with pytest.raises(InvalidFieldValueError):
            validate_image_metadata(image)

    def test_dimension_without_x_raises_error(self):
        """Dimension without 'x' separator raises InvalidFieldValueError."""
        image = ImageMetadata(url="https://example.com/image.jpg", dim="1920-1080")
        with pytest.raises(InvalidFieldValueError):
            validate_image_metadata(image)

    def test_dimension_with_single_number_raises_error(self):
        """Dimension with single number raises InvalidFieldValueError."""
        image = ImageMetadata(url="https://example.com/image.jpg", dim="1920")
        with pytest.raises(InvalidFieldValueError):
            validate_image_metadata(image)

    def test_non_string_dimension_raises_type_error(self):
        """Non-string dimension raises InvalidFieldTypeError."""
        image = ImageMetadata(url="https://example.com/image.jpg", dim=123)
        with pytest.raises(InvalidFieldTypeError):
            validate_image_metadata(image)

    def test_none_dimension_stays_none(self):
        """None dimension stays None."""
        image = ImageMetadata(url="https://example.com/image.jpg", dim=None)
        result = validate_image_metadata(image)
        assert result.dim is None

    def test_deterministic_same_input_same_output(self):
        """Same input always yields same output."""
        image = ImageMetadata(url="https://example.com/image.jpg", mime="image/jpeg", alt="Test", dim="800x600")
        result1 = validate_image_metadata(image)
        result2 = validate_image_metadata(image)
        assert result1.url == result2.url
        assert result1.mime == result2.mime
        assert result1.alt == result2.alt
        assert result1.dim == result2.dim

    def test_normalization_idempotent_url(self):
        """URL normalization is idempotent."""
        image = ImageMetadata(url="https://example.com/image.jpg")
        result1 = validate_image_metadata(image)
        result2 = validate_image_metadata(result1)
        assert result1.url == result2.url

    def test_normalization_idempotent_alt(self):
        """Alt text normalization is idempotent."""
        image = ImageMetadata(url="https://example.com/image.jpg", alt="Test text")
        result1 = validate_image_metadata(image)
        result2 = validate_image_metadata(result1)
        assert result1.alt == result2.alt

    def test_normalization_idempotent_dim(self):
        """Dimension normalization is idempotent."""
        image = ImageMetadata(url="https://example.com/image.jpg", dim="1920x1080")
        result1 = validate_image_metadata(image)
        result2 = validate_image_metadata(result1)
        assert result1.dim == result2.dim

    def test_fail_fast_url_validation_before_mime(self):
        """Invalid URL raises before any MIME inference."""
        image = ImageMetadata(url="invalid", mime=None)
        with pytest.raises(InvalidFieldValueError):
            validate_image_metadata(image)

    def test_returns_new_image_metadata_instance(self):
        """Returns a new ImageMetadata instance, not the original."""
        original = ImageMetadata(url="https://example.com/image.jpg", alt="Test")
        result = validate_image_metadata(original)
        assert result is not original
        assert result.url == original.url
        assert result.alt == original.alt

    @given(
        st.fixed_dictionaries(
            {"url": st.one_of(st.just("https://example.com/image.jpg"), st.just("http://example.com/image.png"))}
        )
    )
    def test_validate_valid_urls(self, data):
        """Valid URLs from generated data pass validation."""
        image = ImageMetadata(url=data["url"])
        result = validate_image_metadata(image)
        assert result.url == data["url"]

    @given(
        st.fixed_dictionaries(
            {"url": st.just("https://example.com/image.jpg"), "dim": st.from_regex(r"\d{1,4}x\d{1,4}", fullmatch=True)}
        )
    )
    def test_validate_generated_dimensions(self, data):
        """Generated valid dimension formats pass validation."""
        image = ImageMetadata(url=data["url"], dim=data["dim"])
        result = validate_image_metadata(image)
        assert result.dim == data["dim"]


class TestYAMLInjectionSecurity:
    """Security tests for YAML injection prevention."""

    def test_url_with_newline_rejected(self):
        """URL containing newline is rejected."""
        image = ImageMetadata(url="https://example.com/image.jpg\nmalicious: value")
        with pytest.raises(InvalidFieldValueError, match="invalid characters"):
            validate_image_metadata(image)

    def test_url_with_carriage_return_rejected(self):
        """URL containing carriage return is rejected."""
        image = ImageMetadata(url="https://example.com/image.jpg\rmalicious: value")
        with pytest.raises(InvalidFieldValueError, match="invalid characters"):
            validate_image_metadata(image)

    def test_url_with_opening_bracket_rejected(self):
        """URL containing opening bracket is rejected."""
        image = ImageMetadata(url="https://example.com/image.jpg[malicious]")
        with pytest.raises(InvalidFieldValueError, match="invalid characters"):
            validate_image_metadata(image)

    def test_url_with_closing_bracket_rejected(self):
        """URL containing closing bracket is rejected."""
        image = ImageMetadata(url="https://example.com/image.jpg]malicious")
        with pytest.raises(InvalidFieldValueError, match="invalid characters"):
            validate_image_metadata(image)

    def test_url_with_opening_brace_rejected(self):
        """URL containing opening brace is rejected."""
        image = ImageMetadata(url="https://example.com/image.jpg{malicious}")
        with pytest.raises(InvalidFieldValueError, match="invalid characters"):
            validate_image_metadata(image)

    def test_url_with_closing_brace_rejected(self):
        """URL containing closing brace is rejected."""
        image = ImageMetadata(url="https://example.com/image.jpg}malicious")
        with pytest.raises(InvalidFieldValueError, match="invalid characters"):
            validate_image_metadata(image)

    def test_mime_with_newline_rejected(self):
        """MIME type containing newline is rejected."""
        image = ImageMetadata(url="https://example.com/image.jpg", mime="image/jpeg\nmalicious: value")
        with pytest.raises(InvalidFieldValueError, match="invalid characters"):
            validate_image_metadata(image)

    def test_mime_with_yaml_structural_chars_rejected(self):
        """MIME type containing YAML structural characters is rejected."""
        image = ImageMetadata(url="https://example.com/image.jpg", mime="image/jpeg[malicious]")
        with pytest.raises(InvalidFieldValueError, match="invalid characters"):
            validate_image_metadata(image)

    def test_alt_with_newline_rejected(self):
        """Alt text containing newline is rejected."""
        image = ImageMetadata(url="https://example.com/image.jpg", alt="Description\nmalicious: value")
        with pytest.raises(InvalidFieldValueError, match="invalid characters"):
            validate_image_metadata(image)

    def test_alt_with_yaml_structural_chars_rejected(self):
        """Alt text containing YAML structural characters is rejected."""
        image = ImageMetadata(url="https://example.com/image.jpg", alt="Description {malicious}")
        with pytest.raises(InvalidFieldValueError, match="invalid characters"):
            validate_image_metadata(image)

    def test_dim_with_newline_rejected(self):
        """Dimension containing newline is rejected."""
        image = ImageMetadata(url="https://example.com/image.jpg", dim="1920x1080\nmalicious: value")
        with pytest.raises(InvalidFieldValueError, match="invalid characters"):
            validate_image_metadata(image)

    def test_dim_with_yaml_structural_chars_rejected(self):
        """Dimension containing YAML structural characters is rejected."""
        image = ImageMetadata(url="https://example.com/image.jpg", dim="1920x1080]malicious")
        with pytest.raises(InvalidFieldValueError, match="invalid characters"):
            validate_image_metadata(image)

    def test_hash_with_newline_rejected(self):
        """Hash containing newline is rejected."""
        image = ImageMetadata(url="https://example.com/image.jpg", hash="abc123\nmalicious: value")
        with pytest.raises(InvalidFieldValueError, match="invalid characters"):
            validate_image_metadata(image)

    def test_hash_with_yaml_structural_chars_rejected(self):
        """Hash containing YAML structural characters is rejected."""
        image = ImageMetadata(url="https://example.com/image.jpg", hash="abc123{malicious}")
        with pytest.raises(InvalidFieldValueError, match="invalid characters"):
            validate_image_metadata(image)


class TestRoundTripProperties:
    """Tests for properties relating parse and validate together."""

    def test_parse_then_validate_string_input(self):
        """Parse string then validate produces valid ImageMetadata."""
        url = "https://example.com/image.jpg"
        parsed = parse_image_field(url)
        validated = validate_image_metadata(parsed)
        assert validated.url == url

    def test_parse_then_validate_dict_input(self):
        """Parse dict then validate produces valid ImageMetadata."""
        data = {"url": "https://example.com/image.png", "mime": "image/png", "alt": "Test image"}
        parsed = parse_image_field(data)
        validated = validate_image_metadata(parsed)
        assert validated.url == data["url"]
        assert validated.mime == data["mime"]
        assert validated.alt == data["alt"]

    def test_infer_mime_used_by_validate(self):
        """Validate uses infer_mime_type for None mime values."""
        image = ImageMetadata(url="https://example.com/image.webp", mime=None)
        result = validate_image_metadata(image)
        assert result.mime == infer_mime_type("https://example.com/image.webp")

    def test_parse_then_validate_with_whitespace(self):
        """Parse handles raw dict values, validate normalizes them."""
        data = {"url": "https://example.com/image.jpg", "alt": "  Test image  ", "dim": "  1920x1080  "}
        parsed = parse_image_field(data)
        validated = validate_image_metadata(parsed)
        assert validated.alt == "Test image"
        assert validated.dim == "1920x1080"
