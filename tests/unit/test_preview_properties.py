"""Property-based tests for Preview Mode feature.

Tests properties that secure the preview feature:
- Preview tag semantics (advisory only, no protocol impact)
- Tag ordering determinism
- Event structure invariants with preview tags
- naddr encoding with relay hints
"""

from unittest.mock import MagicMock, patch

from hypothesis import given, settings
from hypothesis import strategies as st

from nostr_publish.event import build_tags, construct_event
from nostr_publish.models import Frontmatter, ImageMetadata
from nostr_publish.naddr_encoder import encode_naddr

# === Strategies ===


@st.composite
def valid_frontmatter(draw):
    """Generate valid Frontmatter with optional fields."""
    title = draw(st.text(min_size=1, max_size=100, alphabet=st.characters(blacklist_categories=("Cc", "Cs"))))
    slug = draw(
        st.text(
            min_size=1,
            max_size=50,
            alphabet=st.characters(whitelist_categories=("Ll", "Lu", "Nd"), whitelist_characters="-_"),
        )
    )
    summary = draw(
        st.none() | st.text(min_size=1, max_size=200, alphabet=st.characters(blacklist_categories=("Cc", "Cs")))
    )
    tags = draw(
        st.lists(
            st.text(min_size=1, max_size=30, alphabet=st.characters(blacklist_categories=("Cc", "Cs"))), max_size=5
        )
    )
    published_at = draw(st.none() | st.integers(min_value=0, max_value=2000000000))

    return Frontmatter(title=title, slug=slug, summary=summary, tags=tags, published_at=published_at)


@st.composite
def preview_extra_tags(draw):
    """Generate extra tags list that includes preview tag."""
    preview_tag = ["x-emacs-nostr-publish", "preview"]

    # Generate 0-3 additional arbitrary tags
    other_tags = draw(
        st.lists(
            st.lists(
                st.text(min_size=1, max_size=20, alphabet=st.characters(blacklist_categories=("Cc", "Cs"))),
                min_size=2,
                max_size=2,
            ),
            max_size=3,
        )
    )

    # Preview tag can appear at any position
    position = draw(st.integers(min_value=0, max_value=len(other_tags)))
    result = other_tags.copy()
    result.insert(position, preview_tag)

    return result


@st.composite
def non_preview_extra_tags(draw):
    """Generate extra tags list without preview tag."""
    return draw(
        st.lists(
            st.lists(
                st.text(min_size=1, max_size=20, alphabet=st.characters(blacklist_categories=("Cc", "Cs"))),
                min_size=2,
                max_size=2,
            ),
            max_size=5,
        )
    )


# === Preview Tag Semantic Tests ===


class TestPreviewTagSemantics:
    """Preview tag is advisory only - doesn't affect protocol semantics."""

    def test_preview_tag_does_not_change_event_kind(self):
        """Event kind remains 30023 regardless of preview tag."""
        fm = Frontmatter(title="Test", slug="test-slug")
        body = "Test content"

        event_without = construct_event(fm, body, extra_tags=None)
        event_with = construct_event(fm, body, extra_tags=[["x-emacs-nostr-publish", "preview"]])

        assert event_without.kind == 30023
        assert event_with.kind == 30023
        assert event_without.kind == event_with.kind

    def test_preview_tag_does_not_change_d_tag(self):
        """The 'd' (identifier) tag is identical with or without preview tag."""
        fm = Frontmatter(title="Test", slug="my-unique-slug")
        body = "Content"

        event_without = construct_event(fm, body, extra_tags=None)
        event_with = construct_event(fm, body, extra_tags=[["x-emacs-nostr-publish", "preview"]])

        d_tag_without = next(tag for tag in event_without.tags if tag[0] == "d")
        d_tag_with = next(tag for tag in event_with.tags if tag[0] == "d")

        assert d_tag_without == d_tag_with
        assert d_tag_without == ["d", "my-unique-slug"]

    def test_preview_tag_does_not_change_content(self):
        """Event content is identical with or without preview tag."""
        fm = Frontmatter(title="Test", slug="test-slug")
        body = "# Article\n\nThis is my content."

        event_without = construct_event(fm, body, extra_tags=None)
        event_with = construct_event(fm, body, extra_tags=[["x-emacs-nostr-publish", "preview"]])

        assert event_without.content == event_with.content
        assert event_without.content == body

    @given(fm=valid_frontmatter(), body=st.text(min_size=0, max_size=500))
    @settings(max_examples=50)
    def test_preview_tag_preserves_structural_tags(self, fm, body):
        """Structural tags (d, title, summary, published_at) are preserved with preview."""
        event_without = construct_event(fm, body, extra_tags=None)
        event_with = construct_event(fm, body, extra_tags=[["x-emacs-nostr-publish", "preview"]])

        def get_structural_tags(event):
            """Extract structural tags from event."""
            structural_keys = {"d", "title", "summary", "published_at"}
            return [tag for tag in event.tags if tag[0] in structural_keys]

        assert get_structural_tags(event_without) == get_structural_tags(event_with)

    @given(fm=valid_frontmatter(), body=st.text(min_size=0, max_size=200))
    @settings(max_examples=50)
    def test_preview_tag_preserves_content_tags(self, fm, body):
        """Content tags ('t' tags) are preserved with preview tag."""
        event_without = construct_event(fm, body, extra_tags=None)
        event_with = construct_event(fm, body, extra_tags=[["x-emacs-nostr-publish", "preview"]])

        t_tags_without = [tag for tag in event_without.tags if tag[0] == "t"]
        t_tags_with = [tag for tag in event_with.tags if tag[0] == "t"]

        assert t_tags_without == t_tags_with


# === Tag Ordering Determinism ===


class TestPreviewTagOrdering:
    """Preview tag ordering is deterministic and follows spec."""

    def test_preview_tag_after_d_tag(self):
        """Preview tag appears after 'd' tag (spec section 9.3)."""
        fm = Frontmatter(title="Test", slug="test-slug", tags=["nostr"])
        extra_tags = [["x-emacs-nostr-publish", "preview"]]

        tags = build_tags(fm, extra_tags)

        d_index = next(i for i, tag in enumerate(tags) if tag[0] == "d")
        preview_index = next(i for i, tag in enumerate(tags) if tag[0] == "x-emacs-nostr-publish")

        assert d_index < preview_index

    def test_preview_tag_after_title_tag(self):
        """Preview tag appears after 'title' tag."""
        fm = Frontmatter(title="Test", slug="test-slug", tags=["nostr"])
        extra_tags = [["x-emacs-nostr-publish", "preview"]]

        tags = build_tags(fm, extra_tags)

        title_index = next(i for i, tag in enumerate(tags) if tag[0] == "title")
        preview_index = next(i for i, tag in enumerate(tags) if tag[0] == "x-emacs-nostr-publish")

        assert title_index < preview_index

    def test_preview_tag_after_summary_tag(self):
        """Preview tag appears after 'summary' tag when present."""
        fm = Frontmatter(title="Test", slug="test-slug", summary="A summary", tags=["nostr"])
        extra_tags = [["x-emacs-nostr-publish", "preview"]]

        tags = build_tags(fm, extra_tags)

        summary_index = next(i for i, tag in enumerate(tags) if tag[0] == "summary")
        preview_index = next(i for i, tag in enumerate(tags) if tag[0] == "x-emacs-nostr-publish")

        assert summary_index < preview_index

    def test_preview_tag_after_imeta_tag(self):
        """Preview tag appears after 'imeta' tag when image present (spec section 9.3)."""
        image = ImageMetadata(url="https://example.com/img.jpg", mime="image/jpeg")
        fm = Frontmatter(title="Test", slug="test-slug", image=image, tags=["nostr"])
        extra_tags = [["x-emacs-nostr-publish", "preview"]]

        tags = build_tags(fm, extra_tags)

        imeta_index = next(i for i, tag in enumerate(tags) if tag[0] == "imeta")
        preview_index = next(i for i, tag in enumerate(tags) if tag[0] == "x-emacs-nostr-publish")

        assert imeta_index < preview_index

    def test_preview_tag_before_content_tags(self):
        """Preview tag appears before content 't' tags (spec section 9.3)."""
        fm = Frontmatter(title="Test", slug="test-slug", tags=["nostr", "article"])
        extra_tags = [["x-emacs-nostr-publish", "preview"]]

        tags = build_tags(fm, extra_tags)

        preview_index = next(i for i, tag in enumerate(tags) if tag[0] == "x-emacs-nostr-publish")
        first_t_index = next(i for i, tag in enumerate(tags) if tag[0] == "t")

        assert preview_index < first_t_index

    @given(fm=valid_frontmatter(), extra_tags=preview_extra_tags())
    @settings(max_examples=100)
    def test_tag_ordering_deterministic(self, fm, extra_tags):
        """Tag ordering is deterministic for same input."""
        tags1 = build_tags(fm, extra_tags)
        tags2 = build_tags(fm, extra_tags)

        assert tags1 == tags2

    @given(fm=valid_frontmatter(), body=st.text(min_size=0, max_size=200), extra_tags=preview_extra_tags())
    @settings(max_examples=50)
    def test_event_construction_deterministic_with_preview(self, fm, body, extra_tags):
        """Event construction with preview tag is deterministic."""
        event1 = construct_event(fm, body, extra_tags)
        event2 = construct_event(fm, body, extra_tags)

        assert event1.kind == event2.kind
        assert event1.content == event2.content
        assert event1.tags == event2.tags


# === Preview with Image Tests ===


class TestPreviewWithImage:
    """Preview mode works correctly with image uploads."""

    def test_preview_and_imeta_both_present(self):
        """Both preview tag and imeta tag can coexist."""
        image = ImageMetadata(
            url="https://cdn.example.com/cover.jpg", mime="image/jpeg", dim="1200x630", alt="Cover image"
        )
        fm = Frontmatter(title="Article with Image", slug="image-article", image=image, tags=["test"])
        extra_tags = [["x-emacs-nostr-publish", "preview"]]

        event = construct_event(fm, "Content", extra_tags)

        # Both tags present
        tag_keys = [tag[0] for tag in event.tags]
        assert "imeta" in tag_keys
        assert "x-emacs-nostr-publish" in tag_keys

    def test_imeta_content_unchanged_by_preview_tag(self):
        """Image metadata in imeta tag is identical with or without preview."""
        image = ImageMetadata(
            url="https://cdn.example.com/cover.jpg", mime="image/jpeg", dim="1200x630", alt="Cover image"
        )
        fm = Frontmatter(title="Test", slug="test", image=image)

        event_without = construct_event(fm, "Content", extra_tags=None)
        event_with = construct_event(fm, "Content", extra_tags=[["x-emacs-nostr-publish", "preview"]])

        imeta_without = next(tag for tag in event_without.tags if tag[0] == "imeta")
        imeta_with = next(tag for tag in event_with.tags if tag[0] == "imeta")

        assert imeta_without == imeta_with

    @given(
        url=st.from_regex(r"https://[a-z]+\.example\.com/[a-z0-9]+\.(jpg|png|gif)", fullmatch=True),
        mime=st.sampled_from(["image/jpeg", "image/png", "image/gif", "image/webp"]),
    )
    @settings(max_examples=20)
    def test_preview_tag_with_various_image_types(self, url, mime):
        """Preview tag works with various image types."""
        image = ImageMetadata(url=url, mime=mime)
        fm = Frontmatter(title="Test", slug="test", image=image)
        extra_tags = [["x-emacs-nostr-publish", "preview"]]

        event = construct_event(fm, "Content", extra_tags)

        # Both present and correctly ordered
        imeta_index = next(i for i, tag in enumerate(event.tags) if tag[0] == "imeta")
        preview_index = next(i for i, tag in enumerate(event.tags) if tag[0] == "x-emacs-nostr-publish")
        assert imeta_index < preview_index


# === naddr Encoding Tests ===


class TestNaddrEncodingWithPreview:
    """naddr encoding for preview events.

    Note: The encode_naddr function produces naddr without relay hints.
    Relay hints are added at the Emacs layer when constructing the reader URL.
    """

    @staticmethod
    def _mock_popen():
        """Create a patch for subprocess.Popen that returns a fake naddr."""
        mock_process = MagicMock()
        mock_process.communicate.return_value = ("naddr1qqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqq", "")
        mock_process.returncode = 0
        return patch("subprocess.Popen", return_value=mock_process)

    @staticmethod
    def _assert_nak_command(mock_popen, pubkey, slug, call_count):
        """Assert subprocess was invoked with expected nak encode command args."""
        assert mock_popen.call_count == call_count

        for call in mock_popen.call_args_list:
            cmd = call.args[0]
            kwargs = call.kwargs

            assert cmd == ["nak", "encode", "naddr", "--kind", "30023", "--identifier", slug, "--pubkey", pubkey]
            assert kwargs["text"] is True

    def test_naddr_valid_format(self):
        """naddr encoding produces valid NIP-19 format."""
        pubkey = "79be667ef9dcbbac55a06295ce870b07029bfcdb2dce28d959f2815b16f81798"
        slug = "preview-test-article"

        with self._mock_popen() as mock_popen:
            naddr = encode_naddr(pubkey=pubkey, slug=slug)

        self._assert_nak_command(mock_popen, pubkey, slug, call_count=1)

        # The naddr should have correct prefix and reasonable length
        assert naddr.startswith("naddr1")
        assert len(naddr) > 10

    def test_naddr_deterministic_for_preview(self):
        """naddr is deterministic - same slug/pubkey always produces same naddr."""
        pubkey = "79be667ef9dcbbac55a06295ce870b07029bfcdb2dce28d959f2815b16f81798"
        slug = "preview-test"

        with self._mock_popen() as mock_popen:
            naddr1 = encode_naddr(pubkey=pubkey, slug=slug)
            naddr2 = encode_naddr(pubkey=pubkey, slug=slug)

        self._assert_nak_command(mock_popen, pubkey, slug, call_count=2)

        assert naddr1 == naddr2

    @given(
        slug=st.text(
            min_size=1,
            max_size=30,
            alphabet=st.characters(whitelist_categories=("Ll", "Lu", "Nd"), whitelist_characters="-_"),
        )
    )
    @settings(max_examples=30)
    def test_naddr_encoding_deterministic_property(self, slug):
        """naddr encoding is deterministic for any valid slug."""
        pubkey = "79be667ef9dcbbac55a06295ce870b07029bfcdb2dce28d959f2815b16f81798"

        with self._mock_popen() as mock_popen:
            naddr1 = encode_naddr(pubkey=pubkey, slug=slug)
            naddr2 = encode_naddr(pubkey=pubkey, slug=slug)

        self._assert_nak_command(mock_popen, pubkey, slug, call_count=2)

        assert naddr1 == naddr2
        assert naddr1.startswith("naddr1")


# === Event Validity Tests ===


class TestPreviewEventValidity:
    """Preview events are valid, signed events (spec section 10)."""

    def test_preview_event_is_valid_nostr_event(self):
        """Preview event has all required fields for a valid Nostr event."""
        fm = Frontmatter(title="Preview Article", slug="preview-article", tags=["test"])
        extra_tags = [["x-emacs-nostr-publish", "preview"]]

        event = construct_event(fm, "Content", extra_tags)

        # Valid NIP-23 event structure
        assert event.kind == 30023
        assert isinstance(event.content, str)
        assert isinstance(event.tags, list)

        # Has required structural tags
        tag_keys = [tag[0] for tag in event.tags]
        assert "d" in tag_keys
        assert "title" in tag_keys

    def test_preview_event_replaceable(self):
        """Preview event kind 30023 is replaceable (NIP-23)."""
        fm = Frontmatter(title="Test", slug="test")
        extra_tags = [["x-emacs-nostr-publish", "preview"]]

        event = construct_event(fm, "Content", extra_tags)

        # Kind 30023 is a parameterized replaceable event (30000-39999 range)
        assert 30000 <= event.kind < 40000


# === Multiple Extra Tags Tests ===


class TestMultipleExtraTags:
    """Multiple extra tags work correctly together."""

    def test_preview_with_client_tags(self):
        """Preview tag works alongside client identification tags."""
        fm = Frontmatter(title="Test", slug="test", tags=["nostr"])
        extra_tags = [["x-emacs-nostr-publish", "preview"], ["client", "emacs"], ["client-version", "0.1.0"]]

        event = construct_event(fm, "Content", extra_tags)

        # All extra tags present
        assert ["x-emacs-nostr-publish", "preview"] in event.tags
        assert ["client", "emacs"] in event.tags
        assert ["client-version", "0.1.0"] in event.tags

    def test_extra_tags_order_preserved(self):
        """Extra tags appear in the order specified."""
        fm = Frontmatter(title="Test", slug="test")
        extra_tags = [["first", "1"], ["second", "2"], ["third", "3"]]

        tags = build_tags(fm, extra_tags)

        # Find indices of extra tags
        first_idx = next(i for i, tag in enumerate(tags) if tag == ["first", "1"])
        second_idx = next(i for i, tag in enumerate(tags) if tag == ["second", "2"])
        third_idx = next(i for i, tag in enumerate(tags) if tag == ["third", "3"])

        assert first_idx < second_idx < third_idx

    @given(
        extra_tags=st.lists(st.lists(st.text(min_size=1, max_size=10), min_size=2, max_size=2), min_size=1, max_size=5)
    )
    @settings(max_examples=50)
    def test_all_extra_tags_included(self, extra_tags):
        """All provided extra tags are included in the event."""
        fm = Frontmatter(title="Test", slug="test")

        result_tags = build_tags(fm, extra_tags)

        for extra_tag in extra_tags:
            assert extra_tag in result_tags


# === Backwards Compatibility ===


class TestBackwardsCompatibility:
    """Preview feature maintains backwards compatibility."""

    def test_no_extra_tags_unchanged_behavior(self):
        """Events without extra_tags behave identically to before."""
        fm = Frontmatter(
            title="Test", slug="test-slug", summary="A summary", tags=["tag1", "tag2"], published_at=1700000000
        )

        # Build tags with None and empty list should be identical
        tags_none = build_tags(fm, None)
        tags_empty = build_tags(fm, [])

        assert tags_none == tags_empty

    def test_event_without_preview_has_no_preview_tag(self):
        """Events without preview extra_tag don't contain preview tag."""
        fm = Frontmatter(title="Test", slug="test")

        event = construct_event(fm, "Content", extra_tags=None)

        tag_keys = [tag[0] for tag in event.tags]
        assert "x-emacs-nostr-publish" not in tag_keys

    @given(fm=valid_frontmatter(), body=st.text(min_size=0, max_size=200))
    @settings(max_examples=30)
    def test_none_and_empty_extra_tags_equivalent(self, fm, body):
        """None and [] for extra_tags produce identical events."""
        event_none = construct_event(fm, body, extra_tags=None)
        event_empty = construct_event(fm, body, extra_tags=[])

        assert event_none.kind == event_empty.kind
        assert event_none.content == event_empty.content
        assert event_none.tags == event_empty.tags
