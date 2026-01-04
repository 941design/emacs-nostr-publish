"""Unit tests for frontmatter validation.

Property-based tests for validation functions.
To be implemented by pbt-dev.
"""

import pytest
from hypothesis import assume, given
from hypothesis import strategies as st

from nostr_publish.errors import InvalidFieldTypeError, InvalidFieldValueError, MissingFieldError, UnknownFieldError
from nostr_publish.models import Frontmatter, ImageMetadata
from nostr_publish.utils import deduplicate_preserving_order
from nostr_publish.validator import ALLOWED_FIELDS, REQUIRED_FIELDS, validate_frontmatter, validate_frontmatter_dict

# ============================================================================
# validate_frontmatter_dict Tests
# ============================================================================


class TestValidateFrontmatterDict:
    """Tests for validate_frontmatter_dict function."""

    def test_validate_frontmatter_dict_valid_minimal(self):
        """Example-based: Valid dict with only required fields."""
        fm_dict = {"title": "Test", "slug": "test-slug"}
        validate_frontmatter_dict(fm_dict)

    def test_validate_frontmatter_dict_valid_all_fields(self):
        """Example-based: Valid dict with all allowed fields."""
        fm_dict = {
            "title": "Test",
            "slug": "test-slug",
            "summary": "A summary",
            "published_at": 1700000000,
            "tags": ["tag1", "tag2"],
            "relays": ["wss://relay1.example.com"],
        }
        validate_frontmatter_dict(fm_dict)

    @given(st.sets(st.sampled_from(list(ALLOWED_FIELDS))).filter(lambda s: REQUIRED_FIELDS.issubset(s)))
    def test_validate_frontmatter_dict_any_valid_field_combination(self, valid_fields):
        """Property: Any combination including required fields is valid."""
        fm_dict = dict.fromkeys(valid_fields, "value")
        validate_frontmatter_dict(fm_dict)

    def test_validate_frontmatter_dict_unknown_field_single(self):
        """Example-based: Unknown field causes error."""
        fm_dict = {"title": "Test", "slug": "test", "unknown_field": "value"}
        with pytest.raises(UnknownFieldError) as exc_info:
            validate_frontmatter_dict(fm_dict)
        assert "unknown_field" in str(exc_info.value)

    @given(st.text(min_size=1).filter(lambda s: s not in ALLOWED_FIELDS))
    def test_validate_frontmatter_dict_rejects_unknown_field(self, unknown_field):
        """Property: Any field not in ALLOWED_FIELDS raises UnknownFieldError."""
        fm_dict = {"title": "Test", "slug": "test", unknown_field: "value"}
        with pytest.raises(UnknownFieldError):
            validate_frontmatter_dict(fm_dict)

    def test_validate_frontmatter_dict_missing_title(self):
        """Example-based: Missing title causes error."""
        fm_dict = {"slug": "test"}
        with pytest.raises(MissingFieldError) as exc_info:
            validate_frontmatter_dict(fm_dict)
        assert "title" in str(exc_info.value)

    def test_validate_frontmatter_dict_missing_slug(self):
        """Example-based: Missing slug causes error."""
        fm_dict = {"title": "Test"}
        with pytest.raises(MissingFieldError) as exc_info:
            validate_frontmatter_dict(fm_dict)
        assert "slug" in str(exc_info.value)

    @given(st.sets(st.sampled_from(list(REQUIRED_FIELDS))).filter(lambda s: len(s) < len(REQUIRED_FIELDS)))
    def test_validate_frontmatter_dict_rejects_missing_required(self, partial_required):
        """Property: Missing any required field raises MissingFieldError."""
        optional = ALLOWED_FIELDS - REQUIRED_FIELDS
        fm_dict = dict.fromkeys(partial_required, "value")
        fm_dict.update({field: "value" for field in optional if field in ["summary"]})
        with pytest.raises(MissingFieldError):
            validate_frontmatter_dict(fm_dict)

    def test_validate_frontmatter_dict_empty_dict(self):
        """Example-based: Empty dict is missing required fields."""
        with pytest.raises(MissingFieldError):
            validate_frontmatter_dict({})

    @given(st.just({"title": "Test", "slug": "test"}))
    def test_validate_frontmatter_dict_deterministic_minimal(self, fm_dict):
        """Property: Minimal valid dict passes consistently."""
        validate_frontmatter_dict(fm_dict)
        validate_frontmatter_dict(fm_dict)

    def test_validate_frontmatter_dict_unknown_before_missing_check(self):
        """Example-based: Unknown fields checked before missing fields."""
        fm_dict = {"title": "Test", "unknown": "value"}
        with pytest.raises(UnknownFieldError):
            validate_frontmatter_dict(fm_dict)


# ============================================================================
# deduplicate_preserving_order Tests
# ============================================================================


class TestDeduplicateTags:
    """Tests for deduplicate_preserving_order function."""

    def test_deduplicate_preserving_order_no_duplicates(self):
        """Example-based: List with no duplicates is unchanged."""
        tags = ["tag1", "tag2", "tag3"]
        result = deduplicate_preserving_order(tags)
        assert result == ["tag1", "tag2", "tag3"]

    def test_deduplicate_preserving_order_with_duplicates(self):
        """Example-based: Duplicates are removed, order preserved."""
        tags = ["tag1", "tag2", "tag1", "tag3", "tag2"]
        result = deduplicate_preserving_order(tags)
        assert result == ["tag1", "tag2", "tag3"]

    def test_deduplicate_preserving_order_all_same(self):
        """Example-based: All identical tags becomes single tag."""
        tags = ["tag1", "tag1", "tag1"]
        result = deduplicate_preserving_order(tags)
        assert result == ["tag1"]

    def test_deduplicate_preserving_order_empty_list(self):
        """Example-based: Empty list returns empty list."""
        result = deduplicate_preserving_order([])
        assert result == []

    @given(st.lists(st.text(min_size=1), unique=True, max_size=10))
    def test_deduplicate_preserving_order_no_duplicates_property(self, tags):
        """Property: Unique tags list is unchanged."""
        result = deduplicate_preserving_order(tags)
        assert result == tags

    @given(
        st.lists(st.text(min_size=1), max_size=5).flatmap(
            lambda unique_tags: st.just(unique_tags).map(lambda t: t + t) if unique_tags else st.just([])
        )
    )
    def test_deduplicate_preserving_order_idempotent(self, tags):
        """Property: Deduplicating twice yields same result."""
        result1 = deduplicate_preserving_order(tags)
        result2 = deduplicate_preserving_order(result1)
        assert result1 == result2

    @given(st.lists(st.text(min_size=1), max_size=10))
    def test_deduplicate_preserving_order_preserves_first_occurrence_order(self, tags):
        """Property: Result preserves order of first occurrences."""
        result = deduplicate_preserving_order(tags)
        seen = set()
        for tag in tags:
            if tag not in seen:
                seen.add(tag)
        assert set(result) == seen

    @given(st.lists(st.text(min_size=1), max_size=10))
    def test_deduplicate_preserving_order_no_additional_elements(self, tags):
        """Property: Result contains only elements from input."""
        result = deduplicate_preserving_order(tags)
        assert all(tag in tags for tag in result)

    @given(st.lists(st.text(min_size=1), max_size=10))
    def test_deduplicate_preserving_order_case_sensitive(self, tags):
        """Property: Deduplication is case-sensitive."""
        if not tags:
            return
        case_varied = [tags[0].upper() if i == 1 and len(tags) > 1 else tags[i] for i in range(len(tags))]
        result = deduplicate_preserving_order(case_varied)
        expected_length = len(set(case_varied))
        assert len(result) == expected_length

    @given(st.just([]))
    def test_deduplicate_preserving_order_empty_deterministic(self, tags):
        """Property: Empty list always produces empty list."""
        result = deduplicate_preserving_order(tags)
        assert result == []

    def test_deduplicate_preserving_order_single_element(self):
        """Example-based: Single element list is returned unchanged."""
        result = deduplicate_preserving_order(["tag1"])
        assert result == ["tag1"]

    def test_deduplicate_preserving_order_case_sensitivity(self):
        """Example-based: Case variations are treated as different tags."""
        tags = ["Tag", "tag", "TAG"]
        result = deduplicate_preserving_order(tags)
        assert result == ["Tag", "tag", "TAG"]


# ============================================================================
# validate_frontmatter Tests
# ============================================================================


class TestValidateFrontmatter:
    """Tests for validate_frontmatter function."""

    def test_validate_frontmatter_valid_minimal(self):
        """Example-based: Valid frontmatter with only required fields."""
        fm = Frontmatter(title="Test Title", slug="test-slug")
        result = validate_frontmatter(fm)
        assert result.title == "Test Title"
        assert result.slug == "test-slug"

    def test_validate_frontmatter_valid_with_all_fields(self):
        """Example-based: Valid frontmatter with all fields."""
        fm = Frontmatter(
            title="Test Title",
            slug="test-slug",
            summary="A summary",
            published_at=1700000000,
            tags=["tag1", "tag2"],
            relays=["wss://relay1.example.com"],
        )
        result = validate_frontmatter(fm)
        assert result.title == "Test Title"
        assert result.slug == "test-slug"
        assert result.summary == "A summary"
        assert result.published_at == 1700000000
        assert result.tags == ["tag1", "tag2"]
        assert result.relays == ["wss://relay1.example.com"]

    @given(st.text(min_size=1).map(lambda s: s.strip()).filter(lambda s: s))
    def test_validate_frontmatter_title_non_empty_string(self, title):
        """Property: Non-empty title strings pass validation."""
        fm = Frontmatter(title=title, slug="slug")
        result = validate_frontmatter(fm)
        assert result.title == title

    @given(st.from_regex(r"[a-z0-9]+(-[a-z0-9]+)*", fullmatch=True))
    def test_validate_frontmatter_slug_valid_format(self, slug):
        """Property: Valid slug format (lowercase, numbers, hyphens) passes validation."""
        fm = Frontmatter(title="title", slug=slug)
        result = validate_frontmatter(fm)
        assert result.slug == slug

    def test_validate_frontmatter_title_trimmed(self):
        """Example-based: Title whitespace is trimmed."""
        fm = Frontmatter(title="  Title  ", slug="slug")
        result = validate_frontmatter(fm)
        assert result.title == "Title"

    def test_validate_frontmatter_slug_trimmed(self):
        """Example-based: Slug whitespace is trimmed."""
        fm = Frontmatter(title="Title", slug="  slug  ")
        result = validate_frontmatter(fm)
        assert result.slug == "slug"

    def test_validate_frontmatter_title_empty_string_fails(self):
        """Example-based: Empty title fails validation."""
        fm = Frontmatter(title="", slug="slug")
        with pytest.raises(InvalidFieldValueError):
            validate_frontmatter(fm)

    def test_validate_frontmatter_title_whitespace_only_fails(self):
        """Example-based: Whitespace-only title fails validation."""
        fm = Frontmatter(title="   ", slug="slug")
        with pytest.raises(InvalidFieldValueError):
            validate_frontmatter(fm)

    def test_validate_frontmatter_slug_empty_string_fails(self):
        """Example-based: Empty slug fails validation."""
        fm = Frontmatter(title="Title", slug="")
        with pytest.raises(InvalidFieldValueError):
            validate_frontmatter(fm)

    def test_validate_frontmatter_slug_whitespace_only_fails(self):
        """Example-based: Whitespace-only slug fails validation."""
        fm = Frontmatter(title="Title", slug="   ")
        with pytest.raises(InvalidFieldValueError):
            validate_frontmatter(fm)

    def test_validate_frontmatter_slug_uppercase_fails(self):
        """Example-based: Uppercase letters in slug fail validation."""
        fm = Frontmatter(title="Title", slug="My-Article")
        with pytest.raises(InvalidFieldValueError) as exc_info:
            validate_frontmatter(fm)
        assert "lowercase" in str(exc_info.value)

    def test_validate_frontmatter_slug_spaces_fails(self):
        """Example-based: Spaces in slug fail validation."""
        fm = Frontmatter(title="Title", slug="my article")
        with pytest.raises(InvalidFieldValueError) as exc_info:
            validate_frontmatter(fm)
        assert "lowercase" in str(exc_info.value)

    def test_validate_frontmatter_slug_special_chars_fails(self):
        """Example-based: Special characters in slug fail validation."""
        fm = Frontmatter(title="Title", slug="my_article!")
        with pytest.raises(InvalidFieldValueError) as exc_info:
            validate_frontmatter(fm)
        assert "lowercase" in str(exc_info.value)

    def test_validate_frontmatter_slug_underscore_fails(self):
        """Example-based: Underscore in slug fails validation."""
        fm = Frontmatter(title="Title", slug="my_article")
        with pytest.raises(InvalidFieldValueError):
            validate_frontmatter(fm)

    def test_validate_frontmatter_slug_valid_examples(self):
        """Example-based: Valid slug formats pass validation."""
        valid_slugs = ["my-article", "article-2024", "123-test", "a", "article123", "my-long-article-title-2024"]
        for slug in valid_slugs:
            fm = Frontmatter(title="Title", slug=slug)
            result = validate_frontmatter(fm)
            assert result.slug == slug

    @given(
        st.text(min_size=1).filter(
            lambda s: s.strip() and not all(c in "abcdefghijklmnopqrstuvwxyz0123456789-" for c in s.strip())
        )
    )
    def test_validate_frontmatter_slug_rejects_invalid_chars(self, slug):
        """Property: Slugs with invalid characters fail validation."""
        fm = Frontmatter(title="Title", slug=slug)
        with pytest.raises(InvalidFieldValueError):
            validate_frontmatter(fm)

    def test_validate_frontmatter_title_not_string_fails(self):
        """Example-based: Non-string title fails validation."""
        fm = Frontmatter(title=123, slug="slug")
        with pytest.raises(InvalidFieldTypeError):
            validate_frontmatter(fm)

    def test_validate_frontmatter_slug_not_string_fails(self):
        """Example-based: Non-string slug fails validation."""
        fm = Frontmatter(title="Title", slug=123)
        with pytest.raises(InvalidFieldTypeError):
            validate_frontmatter(fm)

    def test_validate_frontmatter_summary_trimmed(self):
        """Example-based: Summary whitespace is trimmed."""
        fm = Frontmatter(title="Title", slug="slug", summary="  Summary  ")
        result = validate_frontmatter(fm)
        assert result.summary == "Summary"

    def test_validate_frontmatter_summary_empty_string_fails(self):
        """Example-based: Empty string summary fails validation (spec 5.2, 5.3)."""
        fm = Frontmatter(title="Title", slug="slug", summary="")
        with pytest.raises(InvalidFieldValueError):
            validate_frontmatter(fm)

    def test_validate_frontmatter_summary_whitespace_only_fails(self):
        """Example-based: Whitespace-only summary fails validation (spec 5.3)."""
        fm = Frontmatter(title="Title", slug="slug", summary="   ")
        with pytest.raises(InvalidFieldValueError):
            validate_frontmatter(fm)

    def test_validate_frontmatter_summary_none_preserved(self):
        """Example-based: None summary is preserved."""
        fm = Frontmatter(title="Title", slug="slug", summary=None)
        result = validate_frontmatter(fm)
        assert result.summary is None

    def test_validate_frontmatter_summary_not_string_fails(self):
        """Example-based: Non-string summary fails validation."""
        fm = Frontmatter(title="Title", slug="slug", summary=123)
        with pytest.raises(InvalidFieldTypeError):
            validate_frontmatter(fm)

    def test_validate_frontmatter_published_at_valid(self):
        """Example-based: Valid published_at passes."""
        fm = Frontmatter(title="Title", slug="slug", published_at=1700000000)
        result = validate_frontmatter(fm)
        assert result.published_at == 1700000000

    def test_validate_frontmatter_published_at_zero(self):
        """Example-based: Zero is valid published_at."""
        fm = Frontmatter(title="Title", slug="slug", published_at=0)
        result = validate_frontmatter(fm)
        assert result.published_at == 0

    @given(st.integers(min_value=0))
    def test_validate_frontmatter_published_at_non_negative(self, timestamp):
        """Property: Non-negative published_at values pass."""
        fm = Frontmatter(title="Title", slug="slug", published_at=timestamp)
        result = validate_frontmatter(fm)
        assert result.published_at == timestamp

    def test_validate_frontmatter_published_at_negative_fails(self):
        """Example-based: Negative published_at fails validation."""
        fm = Frontmatter(title="Title", slug="slug", published_at=-1)
        with pytest.raises(InvalidFieldValueError):
            validate_frontmatter(fm)

    @given(st.integers(max_value=-1))
    def test_validate_frontmatter_published_at_rejects_negative(self, timestamp):
        """Property: Negative published_at values fail."""
        fm = Frontmatter(title="Title", slug="slug", published_at=timestamp)
        with pytest.raises(InvalidFieldValueError):
            validate_frontmatter(fm)

    def test_validate_frontmatter_published_at_not_integer_fails(self):
        """Example-based: Non-integer published_at fails validation."""
        fm = Frontmatter(title="Title", slug="slug", published_at="123")
        with pytest.raises(InvalidFieldTypeError):
            validate_frontmatter(fm)

    def test_validate_frontmatter_published_at_boolean_fails(self):
        """Example-based: Boolean published_at fails validation (bool is subclass of int)."""
        fm = Frontmatter(title="Title", slug="slug", published_at=True)
        with pytest.raises(InvalidFieldTypeError):
            validate_frontmatter(fm)

    def test_validate_frontmatter_published_at_none_preserved(self):
        """Example-based: None published_at is preserved."""
        fm = Frontmatter(title="Title", slug="slug", published_at=None)
        result = validate_frontmatter(fm)
        assert result.published_at is None

    def test_validate_frontmatter_tags_valid(self):
        """Example-based: Valid tags pass validation."""
        fm = Frontmatter(title="Title", slug="slug", tags=["tag1", "tag2"])
        result = validate_frontmatter(fm)
        assert result.tags == ["tag1", "tag2"]

    def test_validate_frontmatter_tags_empty_list(self):
        """Example-based: Empty tags list is valid."""
        fm = Frontmatter(title="Title", slug="slug", tags=[])
        result = validate_frontmatter(fm)
        assert result.tags == []

    @given(st.lists(st.text(min_size=1).map(lambda s: s.strip()).filter(lambda s: s), max_size=10, unique=True))
    def test_validate_frontmatter_tags_non_empty_strings(self, tags):
        """Property: Tags with non-empty strings pass validation."""
        fm = Frontmatter(title="Title", slug="slug", tags=tags)
        result = validate_frontmatter(fm)
        assert result.tags == tags

    def test_validate_frontmatter_tags_trimmed(self):
        """Example-based: Tag whitespace is trimmed."""
        fm = Frontmatter(title="Title", slug="slug", tags=["  tag1  ", "tag2  "])
        result = validate_frontmatter(fm)
        assert result.tags == ["tag1", "tag2"]

    def test_validate_frontmatter_tags_deduplicated(self):
        """Example-based: Duplicate tags are removed."""
        fm = Frontmatter(title="Title", slug="slug", tags=["tag1", "tag2", "tag1"])
        result = validate_frontmatter(fm)
        assert result.tags == ["tag1", "tag2"]

    def test_validate_frontmatter_tags_empty_string_fails(self):
        """Example-based: Empty tag string fails validation."""
        fm = Frontmatter(title="Title", slug="slug", tags=["tag1", ""])
        with pytest.raises(InvalidFieldValueError):
            validate_frontmatter(fm)

    def test_validate_frontmatter_tags_whitespace_only_fails(self):
        """Example-based: Whitespace-only tag fails validation."""
        fm = Frontmatter(title="Title", slug="slug", tags=["tag1", "   "])
        with pytest.raises(InvalidFieldValueError):
            validate_frontmatter(fm)

    def test_validate_frontmatter_tags_not_string_fails(self):
        """Example-based: Non-string tag fails validation."""
        fm = Frontmatter(title="Title", slug="slug", tags=["tag1", 123])
        with pytest.raises(InvalidFieldTypeError):
            validate_frontmatter(fm)

    def test_validate_frontmatter_tags_not_list_fails(self):
        """Example-based: Non-list tags fails validation."""
        fm = Frontmatter(title="Title", slug="slug", tags="tag1")
        with pytest.raises(InvalidFieldTypeError):
            validate_frontmatter(fm)

    def test_validate_frontmatter_relays_valid(self):
        """Example-based: Valid wss:// relays pass validation."""
        fm = Frontmatter(title="Title", slug="slug", relays=["wss://relay1.example.com", "wss://relay2.example.com"])
        result = validate_frontmatter(fm)
        assert result.relays == ["wss://relay1.example.com", "wss://relay2.example.com"]

    def test_validate_frontmatter_relays_empty_list(self):
        """Example-based: Empty relays list is valid."""
        fm = Frontmatter(title="Title", slug="slug", relays=[])
        result = validate_frontmatter(fm)
        assert result.relays == []

    def test_validate_frontmatter_relay_not_wss_fails(self):
        """Example-based: Non-wss:// relay fails validation."""
        fm = Frontmatter(title="Title", slug="slug", relays=["http://relay.example.com"])
        with pytest.raises(InvalidFieldValueError):
            validate_frontmatter(fm)

    def test_validate_frontmatter_relay_ws_passes(self):
        """Example-based: ws:// relay passes validation (for local testing)."""
        fm = Frontmatter(title="Title", slug="slug", relays=["ws://relay.example.com"])
        result = validate_frontmatter(fm)
        assert result.relays == ["ws://relay.example.com"]

    def test_validate_frontmatter_relay_wildcard_passes(self):
        """Example-based: Wildcard '*' relay passes validation (spec section 8.2)."""
        fm = Frontmatter(title="Title", slug="slug", relays=["*"])
        result = validate_frontmatter(fm)
        assert result.relays == ["*"]

    def test_validate_frontmatter_relay_plain_url_fails(self):
        """Example-based: Plain URL without protocol fails validation."""
        fm = Frontmatter(title="Title", slug="slug", relays=["relay.example.com"])
        with pytest.raises(InvalidFieldValueError):
            validate_frontmatter(fm)

    @given(st.text(min_size=1).filter(lambda s: not s.startswith("wss://") and not s.startswith("ws://") and s != "*"))
    def test_validate_frontmatter_relays_rejects_non_ws(self, relay):
        """Property: Non-WebSocket relay strings fail validation (except wildcard '*')."""
        fm = Frontmatter(title="Title", slug="slug", relays=[relay])
        with pytest.raises(InvalidFieldValueError):
            validate_frontmatter(fm)

    def test_validate_frontmatter_relay_not_string_fails(self):
        """Example-based: Non-string relay fails validation."""
        fm = Frontmatter(title="Title", slug="slug", relays=["wss://relay.example.com", 123])
        with pytest.raises(InvalidFieldTypeError):
            validate_frontmatter(fm)

    def test_validate_frontmatter_relays_not_list_fails(self):
        """Example-based: Non-list relays fails validation."""
        fm = Frontmatter(title="Title", slug="slug", relays="wss://relay.example.com")
        with pytest.raises(InvalidFieldTypeError):
            validate_frontmatter(fm)

    def test_validate_frontmatter_normalization_idempotent(self):
        """Example-based: Validating twice yields same result."""
        fm = Frontmatter(title="  Title  ", slug="  slug  ", tags=["  tag1  "])
        result1 = validate_frontmatter(fm)
        result2 = validate_frontmatter(result1)
        assert result1.title == result2.title
        assert result1.slug == result2.slug
        assert result1.tags == result2.tags

    def test_validate_frontmatter_returns_new_instance(self):
        """Example-based: validate_frontmatter returns new Frontmatter instance."""
        fm = Frontmatter(title="Title", slug="slug")
        result = validate_frontmatter(fm)
        assert isinstance(result, Frontmatter)
        assert result.title == "Title"
        assert result.slug == "slug"

    def test_validate_frontmatter_tags_deduplication_preserves_order(self):
        """Example-based: Tag deduplication preserves first occurrence order."""
        fm = Frontmatter(title="Title", slug="slug", tags=["a", "b", "a", "c", "b"])
        result = validate_frontmatter(fm)
        assert result.tags == ["a", "b", "c"]

    @given(st.lists(st.text(min_size=1, max_size=10).map(lambda s: s.strip()).filter(lambda s: s), max_size=5))
    def test_validate_frontmatter_with_varied_tags(self, tags):
        """Property: Various tag lists are processed correctly."""
        assume(len(tags) > 0)
        fm = Frontmatter(title="Title", slug="slug", tags=tags)
        result = validate_frontmatter(fm)
        assert all(tag in tags for tag in result.tags)

    def test_validate_frontmatter_image_none_preserved(self):
        """Example-based: None image is preserved."""
        fm = Frontmatter(title="Title", slug="slug", image=None)
        result = validate_frontmatter(fm)
        assert result.image is None

    def test_validate_frontmatter_image_valid_url(self):
        """Example-based: Valid image with https URL passes validation."""
        image = ImageMetadata(url="https://example.com/image.jpg")
        fm = Frontmatter(title="Title", slug="slug", image=image)
        result = validate_frontmatter(fm)
        assert result.image is not None
        assert result.image.url == "https://example.com/image.jpg"

    def test_validate_frontmatter_image_ws_url(self):
        """Example-based: Valid image with http URL passes validation."""
        image = ImageMetadata(url="http://example.com/image.png")
        fm = Frontmatter(title="Title", slug="slug", image=image)
        result = validate_frontmatter(fm)
        assert result.image is not None
        assert result.image.url == "http://example.com/image.png"

    def test_validate_frontmatter_image_trimmed_url(self):
        """Example-based: Image URL whitespace is trimmed."""
        image = ImageMetadata(url="  https://example.com/image.jpg  ")
        fm = Frontmatter(title="Title", slug="slug", image=image)
        result = validate_frontmatter(fm)
        assert result.image.url == "https://example.com/image.jpg"

    def test_validate_frontmatter_image_with_mime(self):
        """Example-based: Image with explicit mime type passes."""
        image = ImageMetadata(url="https://example.com/image.jpg", mime="image/jpeg")
        fm = Frontmatter(title="Title", slug="slug", image=image)
        result = validate_frontmatter(fm)
        assert result.image.mime == "image/jpeg"

    def test_validate_frontmatter_image_with_alt(self):
        """Example-based: Image with alt text passes."""
        image = ImageMetadata(url="https://example.com/image.jpg", alt="A test image")
        fm = Frontmatter(title="Title", slug="slug", image=image)
        result = validate_frontmatter(fm)
        assert result.image.alt == "A test image"

    def test_validate_frontmatter_image_with_dim(self):
        """Example-based: Image with dimensions passes."""
        image = ImageMetadata(url="https://example.com/image.jpg", dim="1920x1080")
        fm = Frontmatter(title="Title", slug="slug", image=image)
        result = validate_frontmatter(fm)
        assert result.image.dim == "1920x1080"

    def test_validate_frontmatter_image_all_fields(self):
        """Example-based: Image with all fields passes validation."""
        image = ImageMetadata(
            url="https://example.com/image.jpg", mime="image/jpeg", alt="A test image", dim="1920x1080"
        )
        fm = Frontmatter(title="Title", slug="slug", image=image)
        result = validate_frontmatter(fm)
        assert result.image.url == "https://example.com/image.jpg"
        assert result.image.mime == "image/jpeg"
        assert result.image.alt == "A test image"
        assert result.image.dim == "1920x1080"

    def test_validate_frontmatter_image_empty_url_fails(self):
        """Example-based: Image with empty URL fails validation."""
        image = ImageMetadata(url="")
        fm = Frontmatter(title="Title", slug="slug", image=image)
        with pytest.raises(InvalidFieldValueError):
            validate_frontmatter(fm)

    def test_validate_frontmatter_image_whitespace_only_url_fails(self):
        """Example-based: Image with whitespace-only URL fails validation."""
        image = ImageMetadata(url="   ")
        fm = Frontmatter(title="Title", slug="slug", image=image)
        with pytest.raises(InvalidFieldValueError):
            validate_frontmatter(fm)

    def test_validate_frontmatter_image_non_http_url_fails(self):
        """Example-based: Image with non-HTTP URL fails validation."""
        image = ImageMetadata(url="ftp://example.com/image.jpg")
        fm = Frontmatter(title="Title", slug="slug", image=image)
        with pytest.raises(InvalidFieldValueError):
            validate_frontmatter(fm)

    def test_validate_frontmatter_image_relative_url_fails(self):
        """Example-based: Image with relative URL fails validation."""
        image = ImageMetadata(url="/image.jpg")
        fm = Frontmatter(title="Title", slug="slug", image=image)
        with pytest.raises(InvalidFieldValueError):
            validate_frontmatter(fm)

    @given(st.from_regex(r"^https?://[a-z0-9-]+\.[a-z0-9-]+\.(jpg|png|gif|webp|svg)$", fullmatch=True))
    def test_validate_frontmatter_image_valid_http_urls(self, url):
        """Property: Valid HTTP/HTTPS URLs pass validation."""
        image = ImageMetadata(url=url)
        fm = Frontmatter(title="Title", slug="slug", image=image)
        result = validate_frontmatter(fm)
        assert result.image.url == url

    @given(st.text(min_size=1).filter(lambda s: not s.startswith("http://") and not s.startswith("https://")))
    def test_validate_frontmatter_image_rejects_non_http_urls(self, url):
        """Property: Non-HTTP/HTTPS URLs fail validation."""
        image = ImageMetadata(url=url)
        fm = Frontmatter(title="Title", slug="slug", image=image)
        with pytest.raises(InvalidFieldValueError):
            validate_frontmatter(fm)

    def test_validate_frontmatter_image_invalid_mime_format_fails(self):
        """Example-based: Image with invalid MIME format fails validation."""
        image = ImageMetadata(url="https://example.com/image.jpg", mime="notavalidmime")
        fm = Frontmatter(title="Title", slug="slug", image=image)
        with pytest.raises(InvalidFieldValueError):
            validate_frontmatter(fm)

    def test_validate_frontmatter_image_empty_mime_fails(self):
        """Example-based: Image with empty MIME fails validation."""
        image = ImageMetadata(url="https://example.com/image.jpg", mime="")
        fm = Frontmatter(title="Title", slug="slug", image=image)
        with pytest.raises(InvalidFieldValueError):
            validate_frontmatter(fm)

    def test_validate_frontmatter_image_empty_alt_fails(self):
        """Example-based: Image with empty alt text fails validation."""
        image = ImageMetadata(url="https://example.com/image.jpg", alt="")
        fm = Frontmatter(title="Title", slug="slug", image=image)
        with pytest.raises(InvalidFieldValueError):
            validate_frontmatter(fm)

    def test_validate_frontmatter_image_invalid_dim_format_fails(self):
        """Example-based: Image with invalid dimension format fails validation."""
        image = ImageMetadata(url="https://example.com/image.jpg", dim="1920-1080")
        fm = Frontmatter(title="Title", slug="slug", image=image)
        with pytest.raises(InvalidFieldValueError):
            validate_frontmatter(fm)

    def test_validate_frontmatter_image_empty_dim_fails(self):
        """Example-based: Image with empty dimensions fails validation."""
        image = ImageMetadata(url="https://example.com/image.jpg", dim="")
        fm = Frontmatter(title="Title", slug="slug", image=image)
        with pytest.raises(InvalidFieldValueError):
            validate_frontmatter(fm)

    @given(st.from_regex(r"^\d+x\d+$", fullmatch=True))
    def test_validate_frontmatter_image_valid_dimensions(self, dim):
        """Property: Valid dimension formats pass validation."""
        image = ImageMetadata(url="https://example.com/image.jpg", dim=dim)
        fm = Frontmatter(title="Title", slug="slug", image=image)
        result = validate_frontmatter(fm)
        assert result.image.dim == dim

    @given(st.text(min_size=1).filter(lambda s: not __import__("re").match(r"^\d+x\d+$", s)))
    def test_validate_frontmatter_image_rejects_invalid_dimensions(self, dim):
        """Property: Invalid dimension formats fail validation."""
        image = ImageMetadata(url="https://example.com/image.jpg", dim=dim)
        fm = Frontmatter(title="Title", slug="slug", image=image)
        with pytest.raises(InvalidFieldValueError):
            validate_frontmatter(fm)

    def test_validate_frontmatter_image_inferred_mime_jpg(self):
        """Example-based: MIME type inferred from .jpg extension."""
        image = ImageMetadata(url="https://example.com/image.jpg")
        fm = Frontmatter(title="Title", slug="slug", image=image)
        result = validate_frontmatter(fm)
        assert result.image.mime == "image/jpeg"

    def test_validate_frontmatter_image_inferred_mime_png(self):
        """Example-based: MIME type inferred from .png extension."""
        image = ImageMetadata(url="https://example.com/image.png")
        fm = Frontmatter(title="Title", slug="slug", image=image)
        result = validate_frontmatter(fm)
        assert result.image.mime == "image/png"

    def test_validate_frontmatter_image_inferred_mime_gif(self):
        """Example-based: MIME type inferred from .gif extension."""
        image = ImageMetadata(url="https://example.com/image.gif")
        fm = Frontmatter(title="Title", slug="slug", image=image)
        result = validate_frontmatter(fm)
        assert result.image.mime == "image/gif"

    def test_validate_frontmatter_image_mime_trimmed(self):
        """Example-based: MIME type whitespace is trimmed."""
        image = ImageMetadata(url="https://example.com/image.jpg", mime="  image/jpeg  ")
        fm = Frontmatter(title="Title", slug="slug", image=image)
        result = validate_frontmatter(fm)
        assert result.image.mime == "image/jpeg"

    def test_validate_frontmatter_image_alt_trimmed(self):
        """Example-based: Alt text whitespace is trimmed."""
        image = ImageMetadata(url="https://example.com/image.jpg", alt="  Alt text  ")
        fm = Frontmatter(title="Title", slug="slug", image=image)
        result = validate_frontmatter(fm)
        assert result.image.alt == "Alt text"

    def test_validate_frontmatter_image_dim_trimmed(self):
        """Example-based: Dimension string whitespace is trimmed."""
        image = ImageMetadata(url="https://example.com/image.jpg", dim="  1920x1080  ")
        fm = Frontmatter(title="Title", slug="slug", image=image)
        result = validate_frontmatter(fm)
        assert result.image.dim == "1920x1080"

    @given(st.from_regex(r"^[a-z0-9]+/[a-z0-9+.-]+$", fullmatch=True))
    def test_validate_frontmatter_image_valid_mime_formats(self, mime):
        """Property: Valid MIME format with type/subtype passes."""
        image = ImageMetadata(url="https://example.com/image.jpg", mime=mime)
        fm = Frontmatter(title="Title", slug="slug", image=image)
        result = validate_frontmatter(fm)
        assert result.image.mime == mime

    def test_validate_frontmatter_image_normalization_idempotent(self):
        """Example-based: Image validation is idempotent."""
        image = ImageMetadata(
            url="  https://example.com/image.jpg  ", mime="  image/jpeg  ", alt="  Alt text  ", dim="  1920x1080  "
        )
        fm = Frontmatter(title="Title", slug="slug", image=image)
        result1 = validate_frontmatter(fm)

        fm2 = Frontmatter(title="Title", slug="slug", image=result1.image)
        result2 = validate_frontmatter(fm2)

        assert result1.image.url == result2.image.url
        assert result1.image.mime == result2.image.mime
        assert result1.image.alt == result2.image.alt
        assert result1.image.dim == result2.image.dim
