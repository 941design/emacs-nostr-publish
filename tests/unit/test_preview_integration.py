"""Integration tests for Preview Mode feature.

Tests verify the complete preview workflow:
- CLI invocation with preview tag
- Event construction with preview tag
- Tag positioning in the final event
"""

import json
from pathlib import Path
from unittest.mock import Mock, patch

from nostr_publish.cli import main, parse_arguments
from nostr_publish.event import construct_event
from nostr_publish.models import Frontmatter, ImageMetadata


class TestPreviewModeEndToEnd:
    """Preview mode complete workflow tests."""

    def test_preview_cli_invocation_with_tag(self):
        """Preview mode CLI invocation includes preview tag."""
        argv = [
            "test.md",
            "--relay",
            "wss://preview.relay.example.com",
            "--bunker",
            "bunker://example",
            "--tag",
            "x-emacs-nostr-publish",
            "preview",
        ]
        args = parse_arguments(argv)

        assert args["relays"] == ["wss://preview.relay.example.com"]
        assert args["bunker_uri"] == "bunker://example"
        assert args["extra_tags"] == [["x-emacs-nostr-publish", "preview"]]

    def test_preview_event_structure(self):
        """Preview event has correct structure with preview tag."""
        fm = Frontmatter(
            title="Preview Article", slug="preview-test", summary="Testing preview mode", tags=["nostr", "preview"]
        )
        body = "# Preview Article\n\nThis is a preview."
        extra_tags = [["x-emacs-nostr-publish", "preview"]]

        event = construct_event(fm, body, extra_tags)

        # Verify event structure
        assert event.kind == 30023
        assert event.content == body

        # Verify tag structure and ordering
        assert event.tags[0] == ["d", "preview-test"]
        assert event.tags[1] == ["title", "Preview Article"]
        assert event.tags[2] == ["summary", "Testing preview mode"]
        assert event.tags[3] == ["x-emacs-nostr-publish", "preview"]
        assert event.tags[4] == ["t", "nostr"]
        assert event.tags[5] == ["t", "preview"]

    def test_preview_with_image_metadata(self):
        """Preview event with image metadata positions tags correctly."""
        image = ImageMetadata(
            url="https://preview-blossom.example.com/image.jpg", mime="image/jpeg", hash="abc123", dim="1200x630"
        )
        fm = Frontmatter(title="Preview with Image", slug="preview-image", image=image, tags=["media"])
        body = "Preview with cover image."
        extra_tags = [["x-emacs-nostr-publish", "preview"]]

        event = construct_event(fm, body, extra_tags)

        # Find tag positions
        tag_types = [tag[0] for tag in event.tags]

        # Verify ordering: d, title, imeta, extra tags, content tags
        assert tag_types.index("d") < tag_types.index("title")
        assert tag_types.index("title") < tag_types.index("imeta")
        assert tag_types.index("imeta") < tag_types.index("x-emacs-nostr-publish")
        assert tag_types.index("x-emacs-nostr-publish") < tag_types.index("t")

        # Verify preview tag present
        assert ["x-emacs-nostr-publish", "preview"] in event.tags

    def test_preview_multiple_extra_tags(self):
        """Preview can include multiple arbitrary tags."""
        fm = Frontmatter(title="Test", slug="test-slug")
        body = "Content"
        extra_tags = [["x-emacs-nostr-publish", "preview"], ["client", "emacs"], ["client-version", "0.1.0"]]

        event = construct_event(fm, body, extra_tags)

        # All extra tags present in event
        assert ["x-emacs-nostr-publish", "preview"] in event.tags
        assert ["client", "emacs"] in event.tags
        assert ["client-version", "0.1.0"] in event.tags

        # Extra tags appear in specified order
        extra_tag_indices = [
            next(i for i, tag in enumerate(event.tags) if tag[0] == "x-emacs-nostr-publish"),
            next(i for i, tag in enumerate(event.tags) if tag[0] == "client" and tag[1] == "emacs"),
            next(i for i, tag in enumerate(event.tags) if tag[0] == "client-version"),
        ]
        assert extra_tag_indices == sorted(extra_tag_indices)

    @patch("nostr_publish.cli.parse_arguments")
    @patch("nostr_publish.cli.read_markdown_file")
    @patch("nostr_publish.cli.parse_frontmatter")
    @patch("nostr_publish.cli.validate_frontmatter_dict")
    @patch("nostr_publish.cli.dict_to_frontmatter")
    @patch("nostr_publish.cli.validate_frontmatter")
    @patch("nostr_publish.cli.construct_event")
    @patch("nostr_publish.cli.resolve_relays")
    @patch("nostr_publish.cli.invoke_nak")
    def test_preview_publish_workflow(
        self,
        mock_nak,
        mock_resolve_relays,
        mock_construct_event,
        mock_validate_fm,
        mock_dict_to_fm,
        mock_validate_dict,
        mock_parse_fm,
        mock_read_file,
        mock_parse_args,
    ):
        """Preview publish workflow passes extra_tags through to event construction."""
        # Setup mocks
        mock_parse_args.return_value = {
            "file": Path("preview.md"),
            "bunker_uri": "bunker://preview",
            "relays": ["wss://preview.relay.example"],
            "dry_run": False,
            "timeout": 30,
            "blossom_url": None,
            "blossom_timeout": 30,
            "cover_size": "1200x630",
            "extra_tags": [["x-emacs-nostr-publish", "preview"]],
        }
        mock_read_file.return_value = "---\ntitle: Test\nslug: test\n---\nBody"
        mock_parse_fm.return_value = ({"title": "Test", "slug": "test"}, "Body")

        fm_obj = Mock()
        fm_obj.relays = []
        fm_obj.image = None
        fm_obj.slug = "test"
        mock_dict_to_fm.return_value = fm_obj
        mock_validate_fm.return_value = fm_obj

        event_obj = Mock()
        event_obj.to_dict.return_value = {"kind": 30023, "content": "Body", "tags": []}
        mock_construct_event.return_value = event_obj

        mock_resolve_relays.return_value = ["wss://preview.relay.example"]

        result_obj = Mock()
        result_obj.event_id = "preview123"
        result_obj.pubkey = "0" * 64
        result_obj.naddr = None
        mock_nak.return_value = result_obj

        with patch("builtins.print"):
            result = main([])

        # Verify construct_event was called with extra_tags
        mock_construct_event.assert_called_once()
        call_args = mock_construct_event.call_args
        assert call_args[0][2] == [["x-emacs-nostr-publish", "preview"]]  # extra_tags parameter

        # Verify success
        assert result == 0

    def test_production_publish_no_extra_tags(self):
        """Production publish (without --tag) has empty extra_tags."""
        argv = ["article.md", "--relay", "wss://relay.example.com", "--bunker", "bunker://production"]
        args = parse_arguments(argv)

        assert args["extra_tags"] == []

        # Verify event construction works without extra_tags
        fm = Frontmatter(title="Production", slug="prod-article", tags=["nostr"])
        event = construct_event(fm, "Content", args["extra_tags"])

        # No preview tag in production
        assert ["x-emacs-nostr-publish", "preview"] not in event.tags

        # Only structural and content tags
        tag_types = [tag[0] for tag in event.tags]
        assert set(tag_types) == {"d", "title", "t"}

    def test_preview_tag_not_in_production_event(self):
        """Production event does not include preview tag even if frontmatter has preview-like tags."""
        fm = Frontmatter(title="Article", slug="article", tags=["preview", "draft"])
        event = construct_event(fm, "Content", extra_tags=[])

        # Content tags include "preview" but no client-specific preview tag
        assert ["t", "draft"] in event.tags
        assert ["t", "preview"] in event.tags
        assert ["x-emacs-nostr-publish", "preview"] not in event.tags


class TestPreviewModeProperties:
    """Property-based tests for preview mode invariants."""

    def test_preview_tag_always_before_content_tags(self):
        """Preview tag always appears before content tags regardless of frontmatter."""
        fm = Frontmatter(
            title="Test",
            slug="test-slug",
            summary="Summary",
            tags=["aaa", "zzz", "mmm"],  # Various ordering
        )
        extra_tags = [["x-emacs-nostr-publish", "preview"]]

        event = construct_event(fm, "Body", extra_tags)

        # Find indices
        preview_index = next(i for i, tag in enumerate(event.tags) if tag[0] == "x-emacs-nostr-publish")
        content_tag_indices = [i for i, tag in enumerate(event.tags) if tag[0] == "t"]

        # Preview tag before all content tags
        assert all(preview_index < idx for idx in content_tag_indices)

    def test_preview_tag_after_structural_metadata(self):
        """Preview tag always appears after structural metadata tags."""
        fm = Frontmatter(title="Test", slug="test-slug", summary="Summary", published_at=1234567890)
        extra_tags = [["x-emacs-nostr-publish", "preview"]]

        event = construct_event(fm, "Body", extra_tags)

        # Find indices
        d_index = next(i for i, tag in enumerate(event.tags) if tag[0] == "d")
        title_index = next(i for i, tag in enumerate(event.tags) if tag[0] == "title")
        summary_index = next(i for i, tag in enumerate(event.tags) if tag[0] == "summary")
        published_index = next(i for i, tag in enumerate(event.tags) if tag[0] == "published_at")
        preview_index = next(i for i, tag in enumerate(event.tags) if tag[0] == "x-emacs-nostr-publish")

        # All structural metadata before preview tag
        assert d_index < preview_index
        assert title_index < preview_index
        assert summary_index < preview_index
        assert published_index < preview_index

    def test_event_serialization_includes_preview_tag(self):
        """Serialized event includes preview tag in JSON output."""
        fm = Frontmatter(title="Test", slug="test-slug")
        extra_tags = [["x-emacs-nostr-publish", "preview"]]

        event = construct_event(fm, "Body", extra_tags)
        event_dict = event.to_dict()

        # Verify JSON structure
        assert "tags" in event_dict
        assert ["x-emacs-nostr-publish", "preview"] in event_dict["tags"]

        # Verify JSON serialization works
        json_str = json.dumps(event_dict)
        parsed = json.loads(json_str)
        assert ["x-emacs-nostr-publish", "preview"] in parsed["tags"]


class TestPreviewModeBackwardsCompatibility:
    """Ensure preview mode doesn't break existing functionality."""

    def test_existing_events_unaffected(self):
        """Events without extra_tags are identical to pre-preview implementation."""
        fm = Frontmatter(title="Test", slug="test-slug", summary="Summary", tags=["tag1", "tag2"])
        body = "Content"

        # Construct without extra_tags (backwards compatibility)
        event_no_tags = construct_event(fm, body)
        event_empty_tags = construct_event(fm, body, [])
        event_none_tags = construct_event(fm, body, None)

        # All three should be identical
        assert event_no_tags.tags == event_empty_tags.tags
        assert event_no_tags.tags == event_none_tags.tags

    def test_cli_without_tag_argument_unchanged(self):
        """CLI invocation without --tag produces same result as before."""
        argv_before = ["test.md", "--relay", "wss://relay.example.com", "--bunker", "bunker://example"]
        args = parse_arguments(argv_before)

        # extra_tags present but empty
        assert "extra_tags" in args
        assert args["extra_tags"] == []

    def test_dry_run_with_preview_tag(self):
        """Dry-run mode works with preview tag."""
        argv = [
            "test.md",
            "--relay",
            "wss://relay.example.com",
            "--dry-run",
            "--tag",
            "x-emacs-nostr-publish",
            "preview",
        ]
        args = parse_arguments(argv)

        assert args["dry_run"] is True
        assert args["extra_tags"] == [["x-emacs-nostr-publish", "preview"]]


class TestPreviewModeFileImmutability:
    """Tests verifying file immutability in preview mode."""

    def test_preview_event_construction_no_file_io(self):
        """Preview event construction does not perform file I/O."""
        fm = Frontmatter(title="Test", slug="test-slug", summary="Summary")
        body = "Test content"
        extra_tags = [["x-emacs-nostr-publish", "preview"]]

        # Mock file operations to verify they're not called
        with patch("builtins.open") as mock_open:
            event = construct_event(fm, body, extra_tags)

            # Verify event was constructed successfully
            assert event.kind == 30023
            assert ["x-emacs-nostr-publish", "preview"] in event.tags

            # Verify no file operations occurred
            mock_open.assert_not_called()

    def test_preview_workflow_no_file_writes(self):
        """Preview workflow via Python CLI does not write to files.

        Note: Frontmatter update responsibility is Emacs-side. Python CLI
        outputs JSON result, and Emacs decides whether to update frontmatter.
        In preview mode, Emacs suppresses frontmatter updates.
        """
        with (
            patch("nostr_publish.cli.parse_arguments") as mock_parse_args,
            patch("nostr_publish.cli.read_markdown_file") as mock_read_file,
            patch("nostr_publish.cli.parse_frontmatter") as mock_parse_fm,
            patch("nostr_publish.cli.validate_frontmatter_dict"),
            patch("nostr_publish.cli.dict_to_frontmatter") as mock_dict_to_fm,
            patch("nostr_publish.cli.validate_frontmatter") as mock_validate_fm,
            patch("nostr_publish.cli.construct_event") as mock_construct_event,
            patch("nostr_publish.cli.resolve_relays") as mock_resolve_relays,
            patch("nostr_publish.cli.invoke_nak") as mock_nak,
            patch("builtins.open") as mock_open_file,
        ):
            # Setup mocks
            mock_parse_args.return_value = {
                "file": Path("preview.md"),
                "bunker_uri": "bunker://preview",
                "relays": ["wss://preview.relay.example"],
                "dry_run": False,
                "timeout": 30,
                "blossom_url": None,
                "blossom_timeout": 30,
                "cover_size": "1200x630",
                "allow_dry_run_without_upload": False,
                "extra_tags": [["x-emacs-nostr-publish", "preview"]],
            }
            mock_read_file.return_value = "---\ntitle: Test\nslug: test\n---\nBody"
            mock_parse_fm.return_value = ({"title": "Test", "slug": "test"}, "Body")

            fm_obj = Mock()
            fm_obj.relays = []
            fm_obj.image = None
            fm_obj.slug = "test"
            mock_dict_to_fm.return_value = fm_obj
            mock_validate_fm.return_value = fm_obj

            event_obj = Mock()
            event_obj.to_dict.return_value = {"kind": 30023, "content": "Body", "tags": []}
            mock_construct_event.return_value = event_obj

            mock_resolve_relays.return_value = ["wss://preview.relay.example"]

            result_obj = Mock()
            result_obj.event_id = "preview123"
            result_obj.pubkey = "0" * 64
            result_obj.naddr = "naddr1test"
            result_obj.image = None
            mock_nak.return_value = result_obj

            # Run main workflow
            with patch("builtins.print"):
                result = main([])

            # Verify no file writes occurred (Python CLI never writes to markdown file)
            for call_args in mock_open_file.call_args_list:
                args = call_args[0]
                kwargs = call_args[1]
                mode = args[1] if len(args) > 1 else kwargs.get("mode", "r")
                # Python CLI should only read files, never write
                assert "w" not in mode and "a" not in mode, f"File opened in write mode: {call_args}"

            # Verify success
            assert result == 0

    def test_preview_dry_run_no_file_writes(self):
        """Preview dry-run mode performs no file writes."""
        argv = [
            "test.md",
            "--relay",
            "wss://relay.example.com",
            "--dry-run",
            "--tag",
            "x-emacs-nostr-publish",
            "preview",
        ]

        with (
            patch("nostr_publish.cli.read_markdown_file") as mock_read,
            patch("nostr_publish.cli.parse_frontmatter") as mock_parse,
            patch("nostr_publish.cli.validate_frontmatter_dict"),
            patch("nostr_publish.cli.dict_to_frontmatter") as mock_dict_to_fm,
            patch("nostr_publish.cli.validate_frontmatter") as mock_validate_fm,
            patch("nostr_publish.cli.construct_event") as mock_construct,
            patch("nostr_publish.cli.resolve_relays") as mock_resolve,
            patch("builtins.open") as mock_open_file,
        ):
            # Setup mocks
            mock_read.return_value = "---\ntitle: Test\nslug: test\n---\nContent"
            mock_parse.return_value = ({"title": "Test", "slug": "test"}, "Content")

            fm_obj = Mock()
            fm_obj.relays = []
            fm_obj.image = None
            mock_dict_to_fm.return_value = fm_obj
            mock_validate_fm.return_value = fm_obj

            event_obj = Mock()
            event_obj.to_dict.return_value = {"kind": 30023, "tags": [], "content": "Content"}
            mock_construct.return_value = event_obj

            mock_resolve.return_value = ["wss://relay.example.com"]

            # Run main
            with patch("builtins.print"):
                result = main(argv)

            # Verify no file writes (open not called with write mode)
            for call_args in mock_open_file.call_args_list:
                mode = call_args[0][1] if len(call_args[0]) > 1 else "r"
                assert "w" not in mode, f"File opened in write mode: {call_args}"

            assert result == 0
