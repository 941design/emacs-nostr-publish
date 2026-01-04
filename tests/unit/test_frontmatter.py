"""Unit tests for frontmatter parsing.

Property-based tests for parse_frontmatter and dict_to_frontmatter functions.
"""

import pytest
from hypothesis import assume, given
from hypothesis import strategies as st

from nostr_publish.errors import FrontmatterParseError
from nostr_publish.frontmatter import dict_to_frontmatter, parse_frontmatter
from nostr_publish.models import Frontmatter


class TestParseFrontmatter:
    """Property-based tests for parse_frontmatter function."""

    def test_no_frontmatter_returns_none_and_original_content(self):
        """Content without frontmatter returns (None, original_content)."""
        content = "# Hello World\n\nSome markdown here."
        frontmatter, body = parse_frontmatter(content)
        assert frontmatter is None
        assert body == content

    def test_no_frontmatter_with_dashes_mid_content(self):
        """Dashes not at start are treated as body content."""
        content = "# Hello\n---\nSome dashes\n---\nEnd"
        frontmatter, body = parse_frontmatter(content)
        assert frontmatter is None
        assert body == content

    def test_empty_body_after_frontmatter(self):
        """Frontmatter followed by empty body."""
        content = "---\ntitle: test\n---\n"
        frontmatter, body = parse_frontmatter(content)
        assert frontmatter == {"title": "test"}
        assert body == ""

    def test_body_with_leading_newline_trimmed(self):
        """Single leading newline after closing delimiter is trimmed."""
        content = "---\ntitle: test\n---\nContent here"
        frontmatter, body = parse_frontmatter(content)
        assert frontmatter == {"title": "test"}
        assert body == "Content here"

    def test_body_preserves_content_starting_with_newline(self):
        """Body that starts with newline has only first newline trimmed."""
        content = "---\ntitle: test\n---\n\nContent"
        frontmatter, body = parse_frontmatter(content)
        assert frontmatter == {"title": "test"}
        assert body == "Content"

    def test_valid_yaml_with_multiple_fields(self):
        """Valid YAML with multiple fields parsed correctly."""
        content = """---
title: My Article
slug: my-article
summary: A summary
published_at: 1700000000
tags:
  - python
  - testing
relays:
  - wss://relay1
  - wss://relay2
---
# Body content
"""
        frontmatter, body = parse_frontmatter(content)
        assert frontmatter["title"] == "My Article"
        assert frontmatter["slug"] == "my-article"
        assert frontmatter["summary"] == "A summary"
        assert frontmatter["published_at"] == 1700000000
        assert frontmatter["tags"] == ["python", "testing"]
        assert frontmatter["relays"] == ["wss://relay1", "wss://relay2"]
        assert body == "# Body content\n"

    def test_empty_frontmatter_raises_error(self):
        """Empty frontmatter (---\\n---) raises FrontmatterParseError."""
        content = "---\n---\nBody"
        with pytest.raises(FrontmatterParseError):
            parse_frontmatter(content)

    def test_whitespace_only_frontmatter_raises_error(self):
        """Frontmatter with only whitespace raises FrontmatterParseError."""
        content = "---\n   \n\n---\nBody"
        with pytest.raises(FrontmatterParseError):
            parse_frontmatter(content)

    def test_invalid_yaml_raises_error(self):
        """Invalid YAML syntax raises FrontmatterParseError."""
        content = '---\ntitle: unclosed quote\nvalue: "unclosed\n---\nBody'
        with pytest.raises(FrontmatterParseError):
            parse_frontmatter(content)

    def test_yaml_parses_to_null_raises_error(self):
        """YAML that parses to null raises FrontmatterParseError."""
        content = "---\nnull\n---\nBody"
        with pytest.raises(FrontmatterParseError):
            parse_frontmatter(content)

    def test_yaml_parses_to_list_raises_error(self):
        """YAML that parses to non-dict raises FrontmatterParseError."""
        content = "---\n- item1\n- item2\n---\nBody"
        with pytest.raises(FrontmatterParseError):
            parse_frontmatter(content)

    def test_yaml_parses_to_scalar_raises_error(self):
        """YAML that parses to scalar raises FrontmatterParseError."""
        content = "---\njust a string\n---\nBody"
        with pytest.raises(FrontmatterParseError):
            parse_frontmatter(content)

    def test_missing_closing_delimiter_raises_error(self):
        """Frontmatter without closing delimiter raises FrontmatterParseError."""
        content = "---\ntitle: test\nNo closing delimiter here"
        with pytest.raises(FrontmatterParseError):
            parse_frontmatter(content)

    def test_deterministic_same_input_same_output(self):
        """Same input always produces same output."""
        content = "---\ntitle: test\nslug: test-slug\n---\nBody content"
        result1 = parse_frontmatter(content)
        result2 = parse_frontmatter(content)
        assert result1 == result2

    def test_idempotent_no_frontmatter_body(self):
        """Parsing body without frontmatter is idempotent."""
        body = "# Hello\n\nContent"
        _, body1 = parse_frontmatter(body)
        _, body2 = parse_frontmatter(body1)
        assert body1 == body2

    def test_parsed_dict_is_mutable_independent_copy(self):
        """Returned dict is independent copy."""
        content = "---\ntitle: test\n---\nBody"
        frontmatter1, _ = parse_frontmatter(content)
        frontmatter1["title"] = "modified"
        frontmatter2, _ = parse_frontmatter(content)
        assert frontmatter2["title"] == "test"

    def test_yaml_with_special_characters(self):
        """YAML containing special characters parses correctly."""
        content = '---\ntitle: "Test: Article & More"\nslug: test-article\n---\nBody'
        frontmatter, body = parse_frontmatter(content)
        assert frontmatter["title"] == "Test: Article & More"
        assert body == "Body"

    def test_yaml_with_nested_structures(self):
        """YAML with nested objects parses correctly."""
        content = """---
title: Test
nested:
  key1: value1
  key2: value2
---
Body"""
        frontmatter, body = parse_frontmatter(content)
        assert frontmatter["title"] == "Test"
        assert frontmatter["nested"]["key1"] == "value1"
        assert body == "Body"

    def test_body_with_frontmatter_like_content(self):
        """Body content that looks like frontmatter is preserved."""
        content = "---\ntitle: test\n---\n---\ntitle: not frontmatter\n---"
        frontmatter, body = parse_frontmatter(content)
        assert frontmatter == {"title": "test"}
        assert body == "---\ntitle: not frontmatter\n---"

    def test_body_no_trailing_newline(self):
        """Body content without trailing newline preserved."""
        content = "---\ntitle: test\n---\nContent"
        frontmatter, body = parse_frontmatter(content)
        assert body == "Content"
        assert not body.endswith("\n")

    @given(st.text(min_size=1))
    def test_parse_preserves_body_content(self, body_text):
        """Parsed body is never modified from original input."""
        assume("---\n" not in body_text)
        content = f"---\ntitle: test\n---\n{body_text}"
        _, parsed_body = parse_frontmatter(content)
        expected = body_text if not body_text.startswith("\n") else body_text[1:]
        assert parsed_body == expected

    @given(
        st.fixed_dictionaries(
            {"title": st.text(min_size=1, max_size=50), "slug": st.text(min_size=1, max_size=50)}
        ).filter(lambda d: "---\n" not in d["title"] and "---\n" not in d["slug"])
    )
    def test_parse_with_minimal_valid_frontmatter(self, minimal_dict):
        """Minimal valid frontmatter parses without error."""
        import yaml

        frontmatter_yaml = yaml.dump(minimal_dict)
        content = f"---\n{frontmatter_yaml}---\nBody"
        fm, body = parse_frontmatter(content)
        assert fm["title"] == minimal_dict["title"]
        assert fm["slug"] == minimal_dict["slug"]
        assert body == "Body"


class TestDictToFrontmatter:
    """Property-based tests for dict_to_frontmatter function."""

    def test_converts_dict_with_all_fields(self):
        """Dictionary with all fields converts to Frontmatter."""
        data = {
            "title": "Test Article",
            "slug": "test-article",
            "summary": "A test summary",
            "published_at": 1700000000,
            "tags": ["tag1", "tag2"],
            "relays": ["wss://relay1"],
        }
        fm = dict_to_frontmatter(data)
        assert fm.title == "Test Article"
        assert fm.slug == "test-article"
        assert fm.summary == "A test summary"
        assert fm.published_at == 1700000000
        assert fm.tags == ["tag1", "tag2"]
        assert fm.relays == ["wss://relay1"]

    def test_converts_dict_with_required_fields_only(self):
        """Dictionary with only required fields (title, slug)."""
        data = {"title": "Test", "slug": "test"}
        fm = dict_to_frontmatter(data)
        assert fm.title == "Test"
        assert fm.slug == "test"
        assert fm.summary is None
        assert fm.published_at is None
        assert fm.tags == []
        assert fm.relays == []

    def test_missing_fields_default_to_none_or_empty_list(self):
        """Missing optional fields default to None or empty list."""
        data = {"title": "Test", "slug": "test"}
        fm = dict_to_frontmatter(data)
        assert fm.summary is None
        assert fm.published_at is None
        assert fm.tags == []
        assert fm.relays == []

    def test_none_values_for_optional_fields(self):
        """Explicit None values for optional fields handled correctly."""
        data = {"title": "Test", "slug": "test", "summary": None, "published_at": None, "tags": None, "relays": None}
        fm = dict_to_frontmatter(data)
        assert fm.summary is None
        assert fm.published_at is None
        assert fm.tags == []
        assert fm.relays == []

    def test_empty_list_fields(self):
        """Empty lists for tags and relays handled correctly."""
        data = {"title": "Test", "slug": "test", "tags": [], "relays": []}
        fm = dict_to_frontmatter(data)
        assert fm.tags == []
        assert fm.relays == []

    def test_deterministic_same_dict_same_frontmatter(self):
        """Same dictionary always produces same Frontmatter."""
        data = {
            "title": "Test",
            "slug": "test",
            "summary": "Summary",
            "published_at": 1700000000,
            "tags": ["a", "b"],
            "relays": ["wss://relay"],
        }
        fm1 = dict_to_frontmatter(data)
        fm2 = dict_to_frontmatter(data)
        assert fm1.title == fm2.title
        assert fm1.slug == fm2.slug
        assert fm1.summary == fm2.summary
        assert fm1.published_at == fm2.published_at
        assert fm1.tags == fm2.tags
        assert fm1.relays == fm2.relays

    def test_tags_list_preserved(self):
        """Tags list is preserved exactly."""
        data = {"title": "Test", "slug": "test", "tags": ["python", "testing", "hypothesis"]}
        fm = dict_to_frontmatter(data)
        assert fm.tags == ["python", "testing", "hypothesis"]

    def test_relays_list_preserved(self):
        """Relays list is preserved exactly."""
        data = {"title": "Test", "slug": "test", "relays": ["wss://relay1.com", "wss://relay2.com"]}
        fm = dict_to_frontmatter(data)
        assert fm.relays == ["wss://relay1.com", "wss://relay2.com"]

    def test_integer_published_at_preserved(self):
        """Integer published_at value preserved exactly."""
        data = {"title": "Test", "slug": "test", "published_at": 1700000000}
        fm = dict_to_frontmatter(data)
        assert fm.published_at == 1700000000

    def test_zero_published_at_preserved(self):
        """Zero is valid published_at value."""
        data = {"title": "Test", "slug": "test", "published_at": 0}
        fm = dict_to_frontmatter(data)
        assert fm.published_at == 0

    def test_string_values_preserved(self):
        """String values preserved with exact content."""
        data = {
            "title": "Test Title: With Special Chars & Symbols!",
            "slug": "test-slug-123",
            "summary": "Summary with\nmultiple\nlines",
        }
        fm = dict_to_frontmatter(data)
        assert fm.title == "Test Title: With Special Chars & Symbols!"
        assert fm.slug == "test-slug-123"
        assert fm.summary == "Summary with\nmultiple\nlines"

    def test_extra_keys_in_dict_ignored_not_passed_to_frontmatter(self):
        """Extra keys in dictionary are ignored (not passed to Frontmatter)."""
        data = {"title": "Test", "slug": "test", "extra_key": "should be ignored", "another_extra": 123}
        fm = dict_to_frontmatter(data)
        assert not hasattr(fm, "extra_key")
        assert not hasattr(fm, "another_extra")

    @given(st.fixed_dictionaries({"title": st.text(min_size=1), "slug": st.text(min_size=1)}))
    def test_converts_minimal_dicts(self, minimal):
        """Minimal dictionaries with title and slug convert successfully."""
        fm = dict_to_frontmatter(minimal)
        assert fm.title == minimal["title"]
        assert fm.slug == minimal["slug"]
        assert isinstance(fm, Frontmatter)

    def test_mutable_lists_are_independent(self):
        """Modifying returned lists doesn't affect original dict."""
        data = {"title": "Test", "slug": "test", "tags": ["tag1"], "relays": ["wss://relay"]}
        fm = dict_to_frontmatter(data)
        fm.tags.append("tag2")
        fm.relays.append("wss://relay2")

        fm2 = dict_to_frontmatter(data)
        assert fm2.tags == ["tag1"]
        assert fm2.relays == ["wss://relay"]

    def test_converts_dict_from_parse_frontmatter(self):
        """Dictionary from parse_frontmatter converts to Frontmatter."""
        content = """---
title: Test
slug: test
summary: Summary
published_at: 1700000000
tags:
  - tag1
relays:
  - wss://relay
---
Body"""
        parsed_dict, _ = parse_frontmatter(content)
        fm = dict_to_frontmatter(parsed_dict)
        assert fm.title == "Test"
        assert fm.slug == "test"
        assert fm.summary == "Summary"
        assert fm.published_at == 1700000000
        assert fm.tags == ["tag1"]
        assert fm.relays == ["wss://relay"]


class TestRoundTripProperties:
    """Tests for properties relating both functions together."""

    def test_parse_then_convert_preserves_all_fields(self):
        """parse_frontmatter -> dict_to_frontmatter preserves all fields."""
        content = """---
title: My Article
slug: my-article
summary: Article summary
published_at: 1700000000
tags:
  - python
  - testing
relays:
  - wss://relay1
  - wss://relay2
---
Article body here."""
        parsed_dict, body = parse_frontmatter(content)
        fm = dict_to_frontmatter(parsed_dict)

        assert fm.title == "My Article"
        assert fm.slug == "my-article"
        assert fm.summary == "Article summary"
        assert fm.published_at == 1700000000
        assert fm.tags == ["python", "testing"]
        assert fm.relays == ["wss://relay1", "wss://relay2"]

    def test_parse_idempotency_with_no_frontmatter(self):
        """Parsing body with no frontmatter is idempotent."""
        content = "# Article\n\nSome content"
        _, body1 = parse_frontmatter(content)
        _, body2 = parse_frontmatter(body1)
        assert body1 == body2 == content

    def test_conversion_of_minimal_parse_result(self):
        """Minimal parse result converts correctly."""
        content = "---\ntitle: Test\nslug: test\n---\nBody"
        parsed_dict, _ = parse_frontmatter(content)
        fm = dict_to_frontmatter(parsed_dict)
        assert isinstance(fm, Frontmatter)
        assert fm.title == "Test"
        assert fm.slug == "test"

    @given(st.text(min_size=1, max_size=100))
    def test_body_preserved_through_parse(self, body_content):
        """Body content preserved exactly through parse_frontmatter."""
        assume("---\n" not in body_content)
        content = f"---\ntitle: Test\nslug: test\n---\n{body_content}"
        _, parsed_body = parse_frontmatter(content)
        expected = body_content if not body_content.startswith("\n") else body_content[1:]
        assert parsed_body == expected


class TestDictToFrontmatterImageField:
    """Property-based tests for cover field handling in dict_to_frontmatter."""

    def test_dict_without_cover_field_results_in_none_image(self):
        """Dictionary without cover field results in image=None."""
        data = {"title": "Test", "slug": "test"}
        fm = dict_to_frontmatter(data)
        assert fm.image is None

    def test_dict_with_cover_string_field(self):
        """Dictionary with 'cover' string field parses to ImageMetadata."""
        data = {"title": "Test", "slug": "test", "image": "https://example.com/cover.jpg"}
        fm = dict_to_frontmatter(data)
        assert fm.image is not None
        assert fm.image.url == "https://example.com/cover.jpg"
        assert fm.image.mime is None
        assert fm.image.alt is None
        assert fm.image.dim is None

    def test_dict_with_cover_dict_field_all_properties(self):
        """Dictionary with 'cover' dict field containing all properties."""
        data = {
            "title": "Test",
            "slug": "test",
            "image": {
                "url": "https://example.com/cover.jpg",
                "mime": "image/jpeg",
                "alt": "Test image",
                "dim": "1920x1080",
            },
        }
        fm = dict_to_frontmatter(data)
        assert fm.image is not None
        assert fm.image.url == "https://example.com/cover.jpg"
        assert fm.image.mime == "image/jpeg"
        assert fm.image.alt == "Test image"
        assert fm.image.dim == "1920x1080"

    def test_dict_with_cover_dict_field_url_only(self):
        """Dictionary with 'cover' dict containing only url."""
        data = {"title": "Test", "slug": "test", "image": {"url": "https://example.com/cover.png"}}
        fm = dict_to_frontmatter(data)
        assert fm.image is not None
        assert fm.image.url == "https://example.com/cover.png"
        assert fm.image.mime is None
        assert fm.image.alt is None
        assert fm.image.dim is None

    def test_backwards_compatibility_documents_without_cover_work_identically(self):
        """Documents without cover fields work identically to before."""
        data_old = {"title": "Test", "slug": "test", "summary": "Summary"}
        fm = dict_to_frontmatter(data_old)
        assert fm.title == "Test"
        assert fm.slug == "test"
        assert fm.summary == "Summary"
        assert fm.image is None
        assert fm.tags == []
        assert fm.relays == []

    def test_deterministic_same_dict_with_cover_produces_same_frontmatter(self):
        """Same dictionary with cover always produces same Frontmatter."""
        data = {"title": "Test", "slug": "test", "image": "https://example.com/cover.jpg"}
        fm1 = dict_to_frontmatter(data)
        fm2 = dict_to_frontmatter(data)
        assert fm1.image.url == fm2.image.url
        assert fm1.image.mime == fm2.image.mime
        assert fm1.image.alt == fm2.image.alt
        assert fm1.image.dim == fm2.image.dim

    def test_deterministic_dict_with_cover_dict(self):
        """Same dictionary with cover dict always produces same result."""
        data = {
            "title": "Test",
            "slug": "test",
            "image": {"url": "https://example.com/cover.jpg", "mime": "image/jpeg", "alt": "Test"},
        }
        fm1 = dict_to_frontmatter(data)
        fm2 = dict_to_frontmatter(data)
        assert fm1.image.url == fm2.image.url
        assert fm1.image.mime == fm2.image.mime
        assert fm1.image.alt == fm2.image.alt

    def test_none_cover_field_explicitly_set(self):
        """Explicit None for cover field results in None image."""
        data = {"title": "Test", "slug": "test", "image": None}
        fm = dict_to_frontmatter(data)
        assert fm.image is None

    @given(st.text(min_size=1, max_size=200))
    def test_cover_string_field_preserved_exactly(self, url_text):
        """Cover string field value preserved exactly as URL."""
        assume("---" not in url_text)
        data = {"title": "Test", "slug": "test", "image": url_text}
        fm = dict_to_frontmatter(data)
        assert fm.image is not None
        assert fm.image.url == url_text

    @given(st.fixed_dictionaries({"url": st.text(min_size=1, max_size=200)}).filter(lambda d: "---" not in d["url"]))
    def test_cover_dict_url_extracted_exactly(self, cover_dict):
        """URL from cover dict extracted exactly."""
        data = {"title": "Test", "slug": "test", "image": cover_dict}
        fm = dict_to_frontmatter(data)
        assert fm.image is not None
        assert fm.image.url == cover_dict["url"]

    def test_cover_dict_with_extra_keys_ignored(self):
        """Extra keys in cover dict are ignored."""
        data = {
            "title": "Test",
            "slug": "test",
            "image": {"url": "https://example.com/cover.jpg", "extra_key": "should be ignored"},
        }
        fm = dict_to_frontmatter(data)
        assert fm.image is not None
        assert fm.image.url == "https://example.com/cover.jpg"
        assert not hasattr(fm.image, "extra_key")

    def test_full_frontmatter_with_all_fields_including_cover_string(self):
        """Full frontmatter with all fields including cover string."""
        data = {
            "title": "Test Article",
            "slug": "test-article",
            "summary": "Summary",
            "published_at": 1700000000,
            "tags": ["tag1", "tag2"],
            "relays": ["wss://relay1"],
            "image": "https://example.com/cover.jpg",
        }
        fm = dict_to_frontmatter(data)
        assert fm.title == "Test Article"
        assert fm.slug == "test-article"
        assert fm.summary == "Summary"
        assert fm.published_at == 1700000000
        assert fm.tags == ["tag1", "tag2"]
        assert fm.relays == ["wss://relay1"]
        assert fm.image is not None
        assert fm.image.url == "https://example.com/cover.jpg"

    def test_full_frontmatter_with_all_fields_including_cover_dict(self):
        """Full frontmatter with all fields including cover dict."""
        data = {
            "title": "Test Article",
            "slug": "test-article",
            "summary": "Summary",
            "published_at": 1700000000,
            "tags": ["tag1", "tag2"],
            "relays": ["wss://relay1"],
            "image": {"url": "https://example.com/cover.jpg", "mime": "image/jpeg", "alt": "Test", "dim": "1920x1080"},
        }
        fm = dict_to_frontmatter(data)
        assert fm.title == "Test Article"
        assert fm.image is not None
        assert fm.image.url == "https://example.com/cover.jpg"
        assert fm.image.mime == "image/jpeg"
        assert fm.image.alt == "Test"
        assert fm.image.dim == "1920x1080"

    def test_parse_then_convert_with_cover_string(self):
        """parse_frontmatter -> dict_to_frontmatter preserves image string."""
        content = """---
title: Test
slug: test
image: https://example.com/cover.jpg
---
Body"""
        parsed_dict, _ = parse_frontmatter(content)
        fm = dict_to_frontmatter(parsed_dict)
        assert fm.image is not None
        assert fm.image.url == "https://example.com/cover.jpg"

    def test_parse_then_convert_with_cover_dict(self):
        """parse_frontmatter -> dict_to_frontmatter preserves image dict."""
        content = """---
title: Test
slug: test
image:
  url: https://example.com/cover.jpg
  mime: image/jpeg
  alt: Test image
  dim: 1920x1080
---
Body"""
        parsed_dict, _ = parse_frontmatter(content)
        fm = dict_to_frontmatter(parsed_dict)
        assert fm.image is not None
        assert fm.image.url == "https://example.com/cover.jpg"
        assert fm.image.mime == "image/jpeg"
        assert fm.image.alt == "Test image"
        assert fm.image.dim == "1920x1080"
