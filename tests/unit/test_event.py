"""Unit tests for event construction.

Property-based tests for construct_event and build_tags functions.
"""

from hypothesis import HealthCheck, assume, given, settings
from hypothesis import strategies as st

from nostr_publish.event import build_imeta_tag, build_tags, construct_event
from nostr_publish.models import Frontmatter, ImageMetadata, UnsignedEvent


@st.composite
def image_metadata_strategy(draw):
    """Generate valid ImageMetadata instances."""
    return ImageMetadata(
        url=draw(st.text(min_size=1)),
        mime=draw(st.one_of(st.none(), st.text(min_size=1))),
        alt=draw(st.one_of(st.none(), st.text())),
        dim=draw(st.one_of(st.none(), st.text(min_size=1))),
    )


@st.composite
def frontmatter_strategy(draw, require_image: bool = False, require_published_at: bool = False):
    """Generate valid Frontmatter instances.

    Args:
        require_image: If True, always generate a non-None image.
        require_published_at: If True, always generate a non-None published_at.
    """
    if require_published_at:
        published_at = draw(st.integers(min_value=0))
    else:
        published_at = draw(st.one_of(st.none(), st.integers(min_value=0)))

    if require_image:
        image = draw(image_metadata_strategy())
    else:
        image = draw(st.one_of(st.none(), image_metadata_strategy()))

    return Frontmatter(
        title=draw(st.text(min_size=1)),
        slug=draw(st.text(min_size=1)),
        summary=draw(st.one_of(st.none(), st.text())),
        published_at=published_at,
        tags=draw(st.lists(st.text(min_size=1), unique=True)),
        relays=[],
        image=image,
    )


class TestBuildTagsOrdering:
    """Property tests for tag ordering invariants."""

    @given(frontmatter_strategy())
    def test_first_tag_is_d_slug(self, frontmatter):
        """First tag must be ["d", slug]."""
        tags = build_tags(frontmatter)
        assert len(tags) >= 1
        assert tags[0] == ["d", frontmatter.slug]

    @given(frontmatter_strategy())
    def test_second_tag_is_title(self, frontmatter):
        """Second tag must be ["title", title]."""
        tags = build_tags(frontmatter)
        assert len(tags) >= 2
        assert tags[1] == ["title", frontmatter.title]

    @given(frontmatter_strategy())
    def test_summary_tag_position(self, frontmatter):
        """Summary tag (when present) appears after title, before published_at."""
        assume(frontmatter.summary is not None)
        tags = build_tags(frontmatter)
        summary_idx = next(i for i, tag in enumerate(tags) if tag[0] == "summary")
        assert summary_idx > 1
        if any(tag[0] == "published_at" for tag in tags):
            published_idx = next(i for i, tag in enumerate(tags) if tag[0] == "published_at")
            assert summary_idx < published_idx

    @given(frontmatter_strategy())
    def test_published_at_before_content_tags(self, frontmatter):
        """published_at tag (when present) appears before any t tags."""
        assume(frontmatter.published_at is not None and len(frontmatter.tags) > 0)
        tags = build_tags(frontmatter)
        published_idx = next(i for i, tag in enumerate(tags) if tag[0] == "published_at")
        content_indices = [i for i, tag in enumerate(tags) if tag[0] == "t"]
        assert all(published_idx < ci for ci in content_indices)

    @given(frontmatter_strategy())
    def test_content_tags_sorted_lexicographically(self, frontmatter):
        """Content tags (["t", ...]) must be sorted lexicographically."""
        tags = build_tags(frontmatter)
        content_tags = [tag[1] for tag in tags if tag[0] == "t"]
        assert content_tags == sorted(content_tags)

    @given(frontmatter_strategy())
    def test_no_extra_tags(self, frontmatter):
        """Only d, title, summary, published_at, imeta, and t tags are allowed."""
        tags = build_tags(frontmatter)
        allowed_names = {"d", "title", "summary", "published_at", "imeta", "t"}
        for tag in tags:
            assert tag[0] in allowed_names


class TestBuildTagsDeterminism:
    """Property tests for deterministic behavior."""

    @given(frontmatter_strategy())
    def test_same_frontmatter_same_tags(self, frontmatter):
        """Same frontmatter always produces same tags."""
        tags1 = build_tags(frontmatter)
        tags2 = build_tags(frontmatter)
        assert tags1 == tags2

    @given(st.lists(st.text(min_size=1, max_size=50), unique=True, min_size=0, max_size=10))
    def test_tag_list_order_does_not_affect_output(self, tag_list):
        """Different input tag orderings produce identical output."""
        if len(tag_list) < 2:
            return
        frontmatter1 = Frontmatter(title="Test", slug="test-slug", tags=tag_list)
        frontmatter2 = Frontmatter(title="Test", slug="test-slug", tags=list(reversed(tag_list)))
        tags1 = build_tags(frontmatter1)
        tags2 = build_tags(frontmatter2)
        assert tags1 == tags2


class TestBuildTagsCompleteness:
    """Property tests for complete representation of frontmatter."""

    @given(frontmatter_strategy())
    def test_slug_in_tags(self, frontmatter):
        """Slug must be represented in tags."""
        tags = build_tags(frontmatter)
        assert any(tag == ["d", frontmatter.slug] for tag in tags)

    @given(frontmatter_strategy())
    def test_title_in_tags(self, frontmatter):
        """Title must be represented in tags."""
        tags = build_tags(frontmatter)
        assert any(tag == ["title", frontmatter.title] for tag in tags)

    @given(frontmatter_strategy())
    def test_summary_in_tags_when_present(self, frontmatter):
        """Summary must be in tags if present."""
        assume(frontmatter.summary is not None)
        tags = build_tags(frontmatter)
        assert any(tag == ["summary", frontmatter.summary] for tag in tags)

    @given(frontmatter_strategy())
    def test_summary_not_in_tags_when_absent(self, frontmatter):
        """Summary must not be in tags if absent."""
        assume(frontmatter.summary is None)
        tags = build_tags(frontmatter)
        assert not any(tag[0] == "summary" for tag in tags)

    @given(frontmatter_strategy())
    def test_published_at_in_tags_when_present(self, frontmatter):
        """published_at must be in tags if present."""
        assume(frontmatter.published_at is not None)
        tags = build_tags(frontmatter)
        assert any(tag == ["published_at", str(frontmatter.published_at)] for tag in tags)

    @given(frontmatter_strategy())
    def test_published_at_not_in_tags_when_absent(self, frontmatter):
        """published_at must not be in tags if absent."""
        assume(frontmatter.published_at is None)
        tags = build_tags(frontmatter)
        assert not any(tag[0] == "published_at" for tag in tags)

    @given(frontmatter_strategy())
    def test_all_content_tags_present(self, frontmatter):
        """All frontmatter tags must be represented as ["t", tag]."""
        tags = build_tags(frontmatter)
        content_tags = {tag[1] for tag in tags if tag[0] == "t"}
        assert content_tags == set(frontmatter.tags)

    @given(frontmatter_strategy())
    def test_tag_count_correct(self, frontmatter):
        """Tag count must match formula: 2 + (summary?1:0) + (published_at?1:0) + (image?1:0) + len(tags)."""
        tags = build_tags(frontmatter)
        expected_count = 2
        if frontmatter.summary is not None:
            expected_count += 1
        if frontmatter.published_at is not None:
            expected_count += 1
        if frontmatter.image is not None:
            expected_count += 1
        expected_count += len(frontmatter.tags)
        assert len(tags) == expected_count


class TestBuildImetaTag:
    """Property tests for build_imeta_tag function."""

    @given(image_metadata_strategy())
    def test_imeta_first_element(self, image):
        """First element of imeta tag must be 'imeta'."""
        tag = build_imeta_tag(image)
        assert len(tag) >= 1
        assert tag[0] == "imeta"

    @given(image_metadata_strategy())
    def test_imeta_second_element_is_url(self, image):
        """Second element must be 'url <url_value>'."""
        tag = build_imeta_tag(image)
        assert len(tag) >= 2
        assert tag[1] == f"url {image.url}"

    @given(image_metadata_strategy())
    def test_imeta_format_key_value(self, image):
        """All elements after first must follow 'key value' format."""
        tag = build_imeta_tag(image)
        for element in tag[1:]:
            parts = element.split(" ", 1)
            assert len(parts) == 2, f"Element '{element}' doesn't follow 'key value' format"

    @given(image_metadata_strategy())
    def test_imeta_mime_field_when_present(self, image):
        """Mime field 'm <mime>' must be present when image.mime is not None."""
        assume(image.mime is not None)
        tag = build_imeta_tag(image)
        mime_elements = [e for e in tag if e.startswith("m ")]
        assert len(mime_elements) == 1
        assert mime_elements[0] == f"m {image.mime}"

    @given(image_metadata_strategy())
    def test_imeta_mime_field_when_absent(self, image):
        """Mime field must not be present when image.mime is None."""
        assume(image.mime is None)
        tag = build_imeta_tag(image)
        mime_elements = [e for e in tag if e.startswith("m ")]
        assert len(mime_elements) == 0

    @given(image_metadata_strategy())
    def test_imeta_alt_field_when_present(self, image):
        """Alt field 'alt <alt>' must be present when image.alt is not None."""
        assume(image.alt is not None)
        tag = build_imeta_tag(image)
        alt_elements = [e for e in tag if e.startswith("alt ")]
        assert len(alt_elements) == 1
        assert alt_elements[0] == f"alt {image.alt}"

    @given(image_metadata_strategy())
    def test_imeta_alt_field_when_absent(self, image):
        """Alt field must not be present when image.alt is None."""
        assume(image.alt is None)
        tag = build_imeta_tag(image)
        alt_elements = [e for e in tag if e.startswith("alt ")]
        assert len(alt_elements) == 0

    @given(image_metadata_strategy())
    def test_imeta_dim_field_when_present(self, image):
        """Dim field 'dim <dim>' must be present when image.dim is not None."""
        assume(image.dim is not None)
        tag = build_imeta_tag(image)
        dim_elements = [e for e in tag if e.startswith("dim ")]
        assert len(dim_elements) == 1
        assert dim_elements[0] == f"dim {image.dim}"

    @given(image_metadata_strategy())
    def test_imeta_dim_field_when_absent(self, image):
        """Dim field must not be present when image.dim is None."""
        assume(image.dim is None)
        tag = build_imeta_tag(image)
        dim_elements = [e for e in tag if e.startswith("dim ")]
        assert len(dim_elements) == 0

    @given(image_metadata_strategy())
    def test_imeta_field_order(self, image):
        """Fields must appear in order: url, m, alt, dim."""
        tag = build_imeta_tag(image)
        key_order = [e.split(" ", 1)[0] for e in tag[1:]]
        expected_keys = []
        expected_keys.append("url")
        if image.mime is not None:
            expected_keys.append("m")
        if image.alt is not None:
            expected_keys.append("alt")
        if image.dim is not None:
            expected_keys.append("dim")
        assert key_order == expected_keys

    @given(image_metadata_strategy())
    def test_imeta_deterministic(self, image):
        """Same ImageMetadata always produces same tag."""
        tag1 = build_imeta_tag(image)
        tag2 = build_imeta_tag(image)
        assert tag1 == tag2

    @given(image_metadata_strategy())
    def test_imeta_tag_length(self, image):
        """Tag length must match: 1 (imeta) + 1 (url) + optional fields."""
        tag = build_imeta_tag(image)
        expected_length = 2
        if image.mime is not None:
            expected_length += 1
        if image.alt is not None:
            expected_length += 1
        if image.dim is not None:
            expected_length += 1
        assert len(tag) == expected_length


class TestBuildTagsImeta:
    """Property tests for imeta tag integration in build_tags."""

    @given(frontmatter_strategy())
    def test_imeta_in_tags_when_image_present(self, frontmatter):
        """imeta tag must be present when frontmatter.image is not None."""
        assume(frontmatter.image is not None)
        tags = build_tags(frontmatter)
        imeta_tags = [tag for tag in tags if tag[0] == "imeta"]
        assert len(imeta_tags) == 1

    @given(frontmatter_strategy())
    def test_imeta_not_in_tags_when_image_absent(self, frontmatter):
        """imeta tag must not be present when frontmatter.image is None."""
        assume(frontmatter.image is None)
        tags = build_tags(frontmatter)
        imeta_tags = [tag for tag in tags if tag[0] == "imeta"]
        assert len(imeta_tags) == 0

    @given(frontmatter_strategy(require_image=True, require_published_at=True))
    def test_imeta_position_after_published_at(self, frontmatter):
        """imeta tag (when present) must appear after published_at."""
        tags = build_tags(frontmatter)
        published_idx = next(i for i, tag in enumerate(tags) if tag[0] == "published_at")
        imeta_idx = next(i for i, tag in enumerate(tags) if tag[0] == "imeta")
        assert imeta_idx > published_idx

    @given(frontmatter_strategy())
    def test_imeta_position_before_content_tags(self, frontmatter):
        """imeta tag (when present) must appear before t tags."""
        assume(frontmatter.image is not None and len(frontmatter.tags) > 0)
        tags = build_tags(frontmatter)
        imeta_idx = next(i for i, tag in enumerate(tags) if tag[0] == "imeta")
        content_indices = [i for i, tag in enumerate(tags) if tag[0] == "t"]
        assert all(imeta_idx < ci for ci in content_indices)

    @given(frontmatter_strategy())
    def test_imeta_content_matches_image(self, frontmatter):
        """imeta tag content must match build_imeta_tag output."""
        assume(frontmatter.image is not None)
        tags = build_tags(frontmatter)
        imeta_tag = next(tag for tag in tags if tag[0] == "imeta")
        expected_tag = build_imeta_tag(frontmatter.image)
        assert imeta_tag == expected_tag


class TestConstructEvent:
    """Property tests for construct_event function."""

    @given(frontmatter_strategy(), st.text())
    def test_kind_is_30023(self, frontmatter, body):
        """Event kind must always be 30023."""
        event = construct_event(frontmatter, body)
        assert event.kind == 30023

    @given(frontmatter_strategy(), st.text())
    def test_content_equals_body(self, frontmatter, body):
        """Event content must equal the provided body exactly."""
        event = construct_event(frontmatter, body)
        assert event.content == body

    @given(frontmatter_strategy(), st.text())
    def test_tags_from_build_tags(self, frontmatter, body):
        """Event tags must match build_tags output."""
        event = construct_event(frontmatter, body)
        expected_tags = build_tags(frontmatter)
        assert event.tags == expected_tags

    @given(frontmatter_strategy(), st.text())
    def test_event_is_unsigned_event(self, frontmatter, body):
        """construct_event must return UnsignedEvent instance."""
        event = construct_event(frontmatter, body)
        assert isinstance(event, UnsignedEvent)

    @given(frontmatter_strategy(), st.text())
    def test_event_deterministic(self, frontmatter, body):
        """Same inputs always produce identical events."""
        event1 = construct_event(frontmatter, body)
        event2 = construct_event(frontmatter, body)
        assert event1.kind == event2.kind
        assert event1.content == event2.content
        assert event1.tags == event2.tags

    @given(frontmatter_strategy(), st.text())
    def test_event_tags_are_lists(self, frontmatter, body):
        """Each tag in event must be a list."""
        event = construct_event(frontmatter, body)
        for tag in event.tags:
            assert isinstance(tag, list)

    @given(frontmatter_strategy(), st.text())
    def test_event_tags_have_at_least_two_elements(self, frontmatter, body):
        """Each tag must have at least [tag_name, tag_value]."""
        event = construct_event(frontmatter, body)
        for tag in event.tags:
            assert len(tag) >= 2


class TestEdgeCases:
    """Example-based tests for edge cases and boundary conditions."""

    @settings(suppress_health_check=[HealthCheck.filter_too_much])
    @given(frontmatter_strategy())
    def test_empty_tags_list(self, frontmatter):
        """Frontmatter with no tags should have correct tag count."""
        assume(len(frontmatter.tags) == 0)
        tags = build_tags(frontmatter)
        expected_count = 2
        if frontmatter.summary is not None:
            expected_count += 1
        if frontmatter.published_at is not None:
            expected_count += 1
        if frontmatter.image is not None:
            expected_count += 1
        assert len(tags) == expected_count

    def test_zero_published_at(self):
        """published_at of 0 (Unix epoch) should be represented correctly."""
        frontmatter = Frontmatter(title="Test", slug="test", published_at=0)
        tags = build_tags(frontmatter)
        assert ["published_at", "0"] in tags

    @given(st.integers(min_value=0, max_value=10**10))
    def test_large_published_at(self, timestamp):
        """Large timestamps should be converted to string correctly."""
        frontmatter = Frontmatter(title="Test", slug="test", published_at=timestamp)
        tags = build_tags(frontmatter)
        assert ["published_at", str(timestamp)] in tags

    def test_minimal_frontmatter(self):
        """Minimal frontmatter (only required fields) should work."""
        frontmatter = Frontmatter(title="Test Title", slug="test-slug")
        tags = build_tags(frontmatter)
        assert len(tags) == 2
        assert tags[0] == ["d", "test-slug"]
        assert tags[1] == ["title", "Test Title"]

    def test_full_frontmatter(self):
        """Frontmatter with all fields should include all tags."""
        frontmatter = Frontmatter(
            title="Test Title", slug="test-slug", summary="A summary", published_at=1700000000, tags=["tag1", "tag2"]
        )
        tags = build_tags(frontmatter)
        assert len(tags) == 6
        assert tags[0] == ["d", "test-slug"]
        assert tags[1] == ["title", "Test Title"]
        assert tags[2] == ["summary", "A summary"]
        assert tags[3] == ["published_at", "1700000000"]
        assert tags[4] == ["t", "tag1"]
        assert tags[5] == ["t", "tag2"]

    def test_body_with_special_characters(self):
        """Body with special characters should be preserved exactly."""
        special_body = "# Heading\n\nContent with 🎉 emoji and special chars: <>&\"'"
        frontmatter = Frontmatter(title="Test", slug="test")
        event = construct_event(frontmatter, special_body)
        assert event.content == special_body

    def test_empty_body(self):
        """Empty body should be preserved."""
        frontmatter = Frontmatter(title="Test", slug="test")
        event = construct_event(frontmatter, "")
        assert event.content == ""

    def test_single_tag(self):
        """Single tag should be preserved."""
        frontmatter = Frontmatter(title="Test", slug="test", tags=["single"])
        tags = build_tags(frontmatter)
        assert ["t", "single"] in tags

    def test_case_sensitive_tag_sorting(self):
        """Tags should be sorted case-sensitively (ASCII order)."""
        frontmatter = Frontmatter(title="Test", slug="test", tags=["Zebra", "apple", "Banana"])
        tags = build_tags(frontmatter)
        content_tags = [tag[1] for tag in tags if tag[0] == "t"]
        assert content_tags == ["Banana", "Zebra", "apple"]

    def test_minimal_imeta_tag(self):
        """imeta tag with only required url field."""
        image = ImageMetadata(url="https://example.com/image.jpg")
        tag = build_imeta_tag(image)
        assert tag == ["imeta", "url https://example.com/image.jpg"]

    def test_full_imeta_tag(self):
        """imeta tag with all fields present."""
        image = ImageMetadata(
            url="https://example.com/image.jpg", mime="image/jpeg", alt="A description", dim="1200x800"
        )
        tag = build_imeta_tag(image)
        assert tag == [
            "imeta",
            "url https://example.com/image.jpg",
            "m image/jpeg",
            "alt A description",
            "dim 1200x800",
        ]

    def test_imeta_with_only_mime(self):
        """imeta tag with url and mime only."""
        image = ImageMetadata(url="https://example.com/image.jpg", mime="image/png")
        tag = build_imeta_tag(image)
        assert tag == ["imeta", "url https://example.com/image.jpg", "m image/png"]

    def test_imeta_with_only_alt(self):
        """imeta tag with url and alt only."""
        image = ImageMetadata(url="https://example.com/image.jpg", alt="Description")
        tag = build_imeta_tag(image)
        assert tag == ["imeta", "url https://example.com/image.jpg", "alt Description"]

    def test_imeta_with_only_dim(self):
        """imeta tag with url and dim only."""
        image = ImageMetadata(url="https://example.com/image.jpg", dim="800x600")
        tag = build_imeta_tag(image)
        assert tag == ["imeta", "url https://example.com/image.jpg", "dim 800x600"]

    def test_imeta_preserves_spaces_in_alt(self):
        """imeta alt field preserves whitespace."""
        image = ImageMetadata(url="https://example.com/image.jpg", alt="Image with  multiple  spaces")
        tag = build_imeta_tag(image)
        assert tag[2] == "alt Image with  multiple  spaces"

    def test_full_event_with_imeta(self):
        """Full event construction with image metadata."""
        image = ImageMetadata(url="https://example.com/cover.jpg", mime="image/jpeg", alt="Cover image", dim="1200x800")
        frontmatter = Frontmatter(
            title="Test Article",
            slug="test-article",
            summary="A test summary",
            published_at=1700000000,
            tags=["test", "example"],
            image=image,
        )
        event = construct_event(frontmatter, "# Content")
        assert event.kind == 30023
        assert event.content == "# Content"
        assert len(event.tags) == 7
        assert event.tags[0] == ["d", "test-article"]
        assert event.tags[1] == ["title", "Test Article"]
        assert event.tags[2] == ["summary", "A test summary"]
        assert event.tags[3] == ["published_at", "1700000000"]
        assert event.tags[4][0] == "imeta"
        assert event.tags[5] == ["t", "example"]
        assert event.tags[6] == ["t", "test"]

    def test_imeta_position_with_optional_fields(self):
        """imeta tag positioned correctly even when optional fields are missing."""
        image = ImageMetadata(url="https://example.com/image.jpg")
        frontmatter = Frontmatter(title="Test", slug="test", published_at=1700000000, tags=["tag1"], image=image)
        tags = build_tags(frontmatter)
        assert tags[0][0] == "d"
        assert tags[1][0] == "title"
        assert tags[2][0] == "published_at"
        assert tags[3][0] == "imeta"
        assert tags[4][0] == "t"

    def test_backwards_compatibility_no_image(self):
        """Existing frontmatter without image still works correctly."""
        frontmatter = Frontmatter(title="Test", slug="test", summary="Summary", published_at=1700000000, tags=["tag1"])
        tags = build_tags(frontmatter)
        assert len(tags) == 5
        assert not any(tag[0] == "imeta" for tag in tags)
