"""Property-based tests for arbitrary tag injection feature.

Tests cover:
- CLI argument parsing for --tag
- Tag positioning in event tag array
- Integration with existing tag building logic
"""

import pytest
from hypothesis import given
from hypothesis import strategies as st

from nostr_publish.cli import parse_arguments
from nostr_publish.event import build_tags, construct_event
from nostr_publish.models import Frontmatter, ImageMetadata

# === Strategies ===


@st.composite
def tag_key_values(draw):
    """Generate valid tag key-value pairs.

    Filters out:
    - Core tag keys (d, title, summary, published_at, imeta, t)
    - Values starting with '-' to avoid argparse interpreting them as flags
    - Control characters in values (now rejected by validation)
    """
    core_tag_keys = {"d", "title", "summary", "published_at", "imeta", "t"}
    key = draw(
        st.text(min_size=1, max_size=50, alphabet=st.characters(blacklist_categories=("Cc", "Cs"))).filter(
            lambda x: not x.startswith("-") and x not in core_tag_keys
        )
    )
    value = draw(
        st.text(min_size=1, max_size=100, alphabet=st.characters(blacklist_categories=("Cc", "Cs"))).filter(
            lambda x: not x.startswith("-")
        )
    )
    return (key, value)


# === CLI Argument Parsing Tests ===


class TestParseArgumentsTagInjection:
    """Tag injection via --tag CLI argument."""

    def test_no_tags_defaults_empty_list(self):
        """Without --tag, extra_tags is empty list."""
        argv = ["test.md", "--relay", "wss://relay.example.com"]
        result = parse_arguments(argv)
        assert result["extra_tags"] == []

    def test_single_tag_parsed(self):
        """Single --tag argument produces one tag array."""
        argv = ["test.md", "--relay", "wss://relay.example.com", "--tag", "key", "value"]
        result = parse_arguments(argv)
        assert result["extra_tags"] == [["key", "value"]]

    def test_multiple_tags_parsed(self):
        """Multiple --tag arguments accumulate in order."""
        argv = [
            "test.md",
            "--relay",
            "wss://relay.example.com",
            "--tag",
            "key1",
            "value1",
            "--tag",
            "key2",
            "value2",
            "--tag",
            "key3",
            "value3",
        ]
        result = parse_arguments(argv)
        assert result["extra_tags"] == [["key1", "value1"], ["key2", "value2"], ["key3", "value3"]]

    def test_tag_order_preserved(self):
        """Tags appear in extra_tags in the order specified."""
        argv = [
            "test.md",
            "--relay",
            "wss://relay.example.com",
            "--tag",
            "z",
            "last",
            "--tag",
            "a",
            "first",
            "--tag",
            "m",
            "middle",
        ]
        result = parse_arguments(argv)
        # Order preserved as specified, not sorted
        assert result["extra_tags"] == [["z", "last"], ["a", "first"], ["m", "middle"]]

    def test_preview_tag_example(self):
        """Preview mode tag (x-emacs-nostr-publish preview) parsed correctly."""
        argv = ["test.md", "--relay", "wss://relay.example.com", "--tag", "x-emacs-nostr-publish", "preview"]
        result = parse_arguments(argv)
        assert result["extra_tags"] == [["x-emacs-nostr-publish", "preview"]]

    @given(key_value=tag_key_values())
    def test_arbitrary_key_value_accepted(self, key_value):
        """Any non-empty key-value pair is accepted."""
        key, value = key_value
        argv = ["test.md", "--relay", "wss://relay.example.com", "--tag", key, value]
        result = parse_arguments(argv)
        assert result["extra_tags"] == [[key, value]]

    @given(tags=st.lists(tag_key_values(), min_size=0, max_size=10))
    def test_deterministic_tag_parsing(self, tags):
        """Parsing --tag arguments is deterministic."""
        argv = ["test.md", "--relay", "wss://relay.example.com"]
        for key, value in tags:
            argv.extend(["--tag", key, value])

        result1 = parse_arguments(argv)
        result2 = parse_arguments(argv)

        assert result1["extra_tags"] == result2["extra_tags"]
        assert result1["extra_tags"] == [[key, value] for key, value in tags]


# === Tag Building Tests ===


class TestBuildTagsWithExtraTags:
    """build_tags() correctly positions extra tags."""

    def test_no_extra_tags_backwards_compatible(self):
        """build_tags without extra_tags behaves identically to before."""
        fm = Frontmatter(title="Test", slug="test-slug", tags=["tag1", "tag2"])
        tags = build_tags(fm)

        assert tags[0] == ["d", "test-slug"]
        assert tags[1] == ["title", "Test"]
        assert tags[2] == ["t", "tag1"]
        assert tags[3] == ["t", "tag2"]

    def test_extra_tags_after_metadata_before_content(self):
        """Extra tags appear after metadata, before content tags."""
        fm = Frontmatter(title="Test", slug="test-slug", tags=["tag1"])
        extra_tags = [["client", "emacs"], ["version", "1.0"]]

        tags = build_tags(fm, extra_tags)

        assert tags[0] == ["d", "test-slug"]
        assert tags[1] == ["title", "Test"]
        assert tags[2] == ["client", "emacs"]
        assert tags[3] == ["version", "1.0"]
        assert tags[4] == ["t", "tag1"]

    def test_extra_tags_after_imeta(self):
        """Extra tags appear after imeta tag."""
        image = ImageMetadata(url="https://example.com/image.jpg", mime="image/jpeg")
        fm = Frontmatter(title="Test", slug="test-slug", image=image, tags=["tag1"])
        extra_tags = [["x-emacs-nostr-publish", "preview"]]

        tags = build_tags(fm, extra_tags)

        # Find positions
        d_index = next(i for i, tag in enumerate(tags) if tag[0] == "d")
        title_index = next(i for i, tag in enumerate(tags) if tag[0] == "title")
        imeta_index = next(i for i, tag in enumerate(tags) if tag[0] == "imeta")
        extra_tag_index = next(i for i, tag in enumerate(tags) if tag[0] == "x-emacs-nostr-publish")
        content_tag_index = next(i for i, tag in enumerate(tags) if tag[0] == "t")

        # Verify ordering
        assert d_index < title_index < imeta_index < extra_tag_index < content_tag_index

    def test_extra_tags_order_preserved(self):
        """Extra tags appear in the order provided."""
        fm = Frontmatter(title="Test", slug="test-slug")
        extra_tags = [["z", "last"], ["a", "first"], ["m", "middle"]]

        tags = build_tags(fm, extra_tags)

        # Extra tags should appear in specified order, not sorted
        assert tags[2] == ["z", "last"]
        assert tags[3] == ["a", "first"]
        assert tags[4] == ["m", "middle"]

    def test_empty_extra_tags_list(self):
        """Empty extra_tags list behaves same as None."""
        fm = Frontmatter(title="Test", slug="test-slug", tags=["tag1"])

        tags_none = build_tags(fm, None)
        tags_empty = build_tags(fm, [])

        assert tags_none == tags_empty

    @given(
        title=st.text(min_size=1, max_size=50),
        slug=st.text(min_size=1, max_size=30),
        extra_tags=st.lists(st.lists(st.text(min_size=1, max_size=20), min_size=2, max_size=2), max_size=5),
    )
    def test_deterministic_with_extra_tags(self, title, slug, extra_tags):
        """build_tags with extra_tags is deterministic."""
        fm = Frontmatter(title=title, slug=slug)

        tags1 = build_tags(fm, extra_tags)
        tags2 = build_tags(fm, extra_tags)

        assert tags1 == tags2


# === Event Construction Tests ===


class TestConstructEventWithExtraTags:
    """construct_event() passes extra_tags to build_tags."""

    def test_construct_event_no_extra_tags(self):
        """construct_event without extra_tags works as before."""
        fm = Frontmatter(title="Test", slug="test-slug", tags=["tag1"])
        body = "# Test Article\n\nContent here."

        event = construct_event(fm, body)

        assert event.kind == 30023
        assert event.content == body
        assert event.tags[0] == ["d", "test-slug"]
        assert event.tags[1] == ["title", "Test"]
        assert ["t", "tag1"] in event.tags

    def test_construct_event_with_extra_tags(self):
        """construct_event includes extra_tags in event."""
        fm = Frontmatter(title="Test", slug="test-slug", tags=["tag1"])
        body = "# Test Article"
        extra_tags = [["x-emacs-nostr-publish", "preview"]]

        event = construct_event(fm, body, extra_tags)

        assert event.kind == 30023
        assert event.content == body
        assert ["x-emacs-nostr-publish", "preview"] in event.tags

    def test_preview_mode_event_structure(self):
        """Preview mode event has correct tag structure."""
        fm = Frontmatter(title="Preview Test", slug="preview-article", tags=["nostr"])
        body = "Preview content"
        extra_tags = [["x-emacs-nostr-publish", "preview"]]

        event = construct_event(fm, body, extra_tags)

        # Verify tag ordering: d, title, extra tags, content tags
        assert event.tags[0] == ["d", "preview-article"]
        assert event.tags[1] == ["title", "Preview Test"]
        assert event.tags[2] == ["x-emacs-nostr-publish", "preview"]
        assert event.tags[3] == ["t", "nostr"]

    @given(
        title=st.text(min_size=1, max_size=50),
        slug=st.text(min_size=1, max_size=30),
        body=st.text(min_size=0, max_size=200),
        extra_tags=st.lists(st.lists(st.text(min_size=1, max_size=20), min_size=2, max_size=2), max_size=3),
    )
    def test_construct_event_deterministic_with_extra_tags(self, title, slug, body, extra_tags):
        """construct_event with extra_tags is deterministic."""
        fm = Frontmatter(title=title, slug=slug)

        event1 = construct_event(fm, body, extra_tags)
        event2 = construct_event(fm, body, extra_tags)

        assert event1.kind == event2.kind
        assert event1.content == event2.content
        assert event1.tags == event2.tags


# === Integration Tests ===


class TestTagInjectionIntegration:
    """End-to-end tag injection from CLI to event."""

    def test_cli_to_event_integration(self):
        """Tags flow from CLI parsing through to event construction."""
        # Simulate CLI invocation with preview tag
        argv = ["test.md", "--relay", "wss://relay.example.com", "--tag", "x-emacs-nostr-publish", "preview"]
        args = parse_arguments(argv)

        # Use parsed extra_tags in event construction
        fm = Frontmatter(title="Test", slug="test-slug")
        body = "Test content"
        event = construct_event(fm, body, args["extra_tags"])

        # Verify tag appears in event
        assert ["x-emacs-nostr-publish", "preview"] in event.tags

    def test_multiple_tags_integration(self):
        """Multiple CLI tags all appear in event."""
        argv = [
            "test.md",
            "--relay",
            "wss://relay.example.com",
            "--tag",
            "client",
            "emacs",
            "--tag",
            "version",
            "0.1.0",
            "--tag",
            "x-preview",
            "true",
        ]
        args = parse_arguments(argv)

        fm = Frontmatter(title="Test", slug="test-slug")
        event = construct_event(fm, "Body", args["extra_tags"])

        assert ["client", "emacs"] in event.tags
        assert ["version", "0.1.0"] in event.tags
        assert ["x-preview", "true"] in event.tags


# === Tag Validation Tests ===


class TestTagValidation:
    """Tests for tag key and value validation."""

    def test_reject_core_tag_d(self):
        """Cannot override core tag 'd' via --tag."""
        argv = ["test.md", "--relay", "wss://relay.example.com", "--tag", "d", "alternative-slug"]
        with pytest.raises(SystemExit):
            parse_arguments(argv)

    def test_reject_core_tag_title(self):
        """Cannot override core tag 'title' via --tag."""
        argv = ["test.md", "--relay", "wss://relay.example.com", "--tag", "title", "Alt Title"]
        with pytest.raises(SystemExit):
            parse_arguments(argv)

    def test_reject_core_tag_summary(self):
        """Cannot override core tag 'summary' via --tag."""
        argv = ["test.md", "--relay", "wss://relay.example.com", "--tag", "summary", "Alt Summary"]
        with pytest.raises(SystemExit):
            parse_arguments(argv)

    def test_reject_core_tag_published_at(self):
        """Cannot override core tag 'published_at' via --tag."""
        argv = ["test.md", "--relay", "wss://relay.example.com", "--tag", "published_at", "1234567890"]
        with pytest.raises(SystemExit):
            parse_arguments(argv)

    def test_reject_core_tag_imeta(self):
        """Cannot override core tag 'imeta' via --tag."""
        argv = ["test.md", "--relay", "wss://relay.example.com", "--tag", "imeta", "url https://..."]
        with pytest.raises(SystemExit):
            parse_arguments(argv)

    def test_reject_core_tag_t(self):
        """Cannot override core tag 't' via --tag."""
        argv = ["test.md", "--relay", "wss://relay.example.com", "--tag", "t", "injected-tag"]
        with pytest.raises(SystemExit):
            parse_arguments(argv)

    def test_reject_newline_in_value(self):
        """Tag value with newline is rejected."""
        argv = ["test.md", "--relay", "wss://relay.example.com", "--tag", "key", "value\nwith\nnewlines"]
        with pytest.raises(SystemExit):
            parse_arguments(argv)

    def test_reject_tab_in_value(self):
        """Tag value with tab is rejected."""
        argv = ["test.md", "--relay", "wss://relay.example.com", "--tag", "key", "value\twith\ttabs"]
        with pytest.raises(SystemExit):
            parse_arguments(argv)

    def test_reject_control_char_in_value(self):
        """Tag value with control character is rejected."""
        argv = ["test.md", "--relay", "wss://relay.example.com", "--tag", "key", "value\x00null"]
        with pytest.raises(SystemExit):
            parse_arguments(argv)

    def test_allow_non_core_tag_keys(self):
        """Non-core tag keys are allowed."""
        argv = [
            "test.md",
            "--relay",
            "wss://relay.example.com",
            "--tag",
            "x-emacs-nostr-publish",
            "preview",
            "--tag",
            "client",
            "emacs",
            "--tag",
            "client-version",
            "1.0",
        ]
        result = parse_arguments(argv)
        assert result["extra_tags"] == [
            ["x-emacs-nostr-publish", "preview"],
            ["client", "emacs"],
            ["client-version", "1.0"],
        ]

    def test_allow_special_chars_in_value(self):
        """Special characters (except control chars) are allowed in values."""
        argv = [
            "test.md",
            "--relay",
            "wss://relay.example.com",
            "--tag",
            "url",
            "https://example.com/path?query=value&foo=bar",
        ]
        result = parse_arguments(argv)
        assert result["extra_tags"] == [["url", "https://example.com/path?query=value&foo=bar"]]

    def test_allow_unicode_in_value(self):
        """Unicode characters are allowed in tag values."""
        argv = ["test.md", "--relay", "wss://relay.example.com", "--tag", "greeting", "Hello 世界 🌍"]
        result = parse_arguments(argv)
        assert result["extra_tags"] == [["greeting", "Hello 世界 🌍"]]

    def test_allow_spaces_in_value(self):
        """Spaces are allowed in tag values."""
        argv = [
            "test.md",
            "--relay",
            "wss://relay.example.com",
            "--tag",
            "description",
            "This is a description with spaces",
        ]
        result = parse_arguments(argv)
        assert result["extra_tags"] == [["description", "This is a description with spaces"]]
