"""Integration tests for naddr frontmatter feature.

Tests the complete workflow:
- Publish → naddr encoding → JSON output → Emacs frontmatter update
"""

import json

from hypothesis import given, settings
from hypothesis import strategies as st

from nostr_publish.cli_output import format_publish_result
from nostr_publish.models import PublishResult
from nostr_publish.naddr_encoder import encode_naddr


class TestJsonOutputIntegration:
    """Test JSON output format correctness."""

    @given(
        event_id=st.text(alphabet="0123456789abcdef", min_size=64, max_size=64),
        pubkey=st.text(alphabet="0123456789abcdef", min_size=64, max_size=64),
        naddr=st.just("naddr1testvalue"),  # Fixed naddr, don't invoke nak
    )
    @settings(max_examples=50)
    def test_json_output_parseable(self, event_id, pubkey, naddr):
        """JSON output is valid and parseable."""
        result = PublishResult(event_id=event_id, pubkey=pubkey, naddr=naddr)

        output = format_publish_result(result, cover_metadata=None)

        # Should parse as JSON
        parsed = json.loads(output)

        assert parsed["event_id"] == event_id
        assert parsed["pubkey"] == pubkey
        assert parsed["naddr"] == naddr

    @given(
        event_id=st.text(alphabet="0123456789abcdef", min_size=64, max_size=64),
        pubkey=st.text(alphabet="0123456789abcdef", min_size=64, max_size=64),
        naddr=st.just("naddr1testvalue"),  # Fixed naddr, don't invoke nak
    )
    @settings(max_examples=50)
    def test_json_output_single_line(self, event_id, pubkey, naddr):
        """JSON output is single line (no embedded newlines)."""
        result = PublishResult(event_id=event_id, pubkey=pubkey, naddr=naddr)

        output = format_publish_result(result, cover_metadata=None)

        assert "\n" not in output
        assert "\r" not in output

    @given(
        event_id=st.text(alphabet="0123456789abcdef", min_size=64, max_size=64),
        pubkey=st.text(alphabet="0123456789abcdef", min_size=64, max_size=64),
        naddr=st.just("naddr1testvalue"),  # Fixed naddr, don't invoke nak
    )
    @settings(max_examples=50)
    def test_json_output_keys_sorted(self, event_id, pubkey, naddr):
        """JSON output has keys sorted alphabetically."""
        result = PublishResult(event_id=event_id, pubkey=pubkey, naddr=naddr)

        output = format_publish_result(result, cover_metadata=None)
        parsed = json.loads(output)

        keys = list(parsed.keys())
        assert keys == sorted(keys)

    def test_json_output_with_cover_metadata(self):
        """JSON output includes image metadata when present."""
        result = PublishResult(event_id="abc123" + "0" * 58, pubkey="def456" + "0" * 58, naddr="naddr1test")
        cover = {"hash": "c" * 64, "url": "https://example.com/image.jpg", "dim": "1200x630", "mime": "image/jpeg"}

        output = format_publish_result(result, cover_metadata=cover)
        parsed = json.loads(output)

        assert "image" in parsed
        assert parsed["image"]["hash"] == cover["hash"]
        assert parsed["image"]["url"] == cover["url"]
        assert parsed["image"]["dim"] == cover["dim"]
        assert parsed["image"]["mime"] == cover["mime"]

    def test_json_output_without_cover_metadata(self):
        """JSON output omits image when not present."""
        result = PublishResult(event_id="abc123" + "0" * 58, pubkey="def456" + "0" * 58, naddr="naddr1test")

        output = format_publish_result(result, cover_metadata=None)
        parsed = json.loads(output)

        assert "image" not in parsed


class TestEmacsIntegrationWorkflow:
    """Test end-to-end Emacs integration workflow.

    Note: Emacs integration is tested via Emacs test suite (nostr-publish-tests.el).
    These tests verify Python side produces compatible output."""

    def test_json_output_compatible_with_emacs(self):
        """JSON output format is compatible with Emacs json-parse-string."""
        result = PublishResult(event_id="abc123" + "0" * 58, pubkey="def456" + "0" * 58, naddr="naddr1test")
        output = format_publish_result(result, cover_metadata=None)

        # Python's json.loads should successfully parse it
        # This verifies it's valid JSON that Emacs can also parse
        parsed = json.loads(output)

        assert parsed["event_id"] == result.event_id
        assert parsed["pubkey"] == result.pubkey
        assert parsed["naddr"] == result.naddr

    def test_json_output_keys_use_underscore_not_dash(self):
        """JSON keys use underscores (event_id) not dashes (event-id) for Emacs alist compatibility."""
        result = PublishResult(event_id="abc123" + "0" * 58, pubkey="def456" + "0" * 58, naddr="naddr1test")
        output = format_publish_result(result, cover_metadata=None)
        parsed = json.loads(output)

        # Keys must be underscore-separated (Python/JSON convention)
        # Emacs converts these to symbols when parsing
        assert "event_id" in parsed
        assert "event-id" not in parsed  # Emacs-style dashes not used


class TestSystemLevelProperties:
    """Test system-level properties spanning multiple components."""

    @given(
        event_id=st.text(alphabet="0123456789abcdef", min_size=64, max_size=64),
        pubkey=st.text(alphabet="0123456789abcdef", min_size=64, max_size=64),
        naddr=st.from_regex(r"naddr1[a-z0-9]{10,190}", fullmatch=True),  # Valid naddr pattern
    )
    @settings(max_examples=50)
    def test_roundtrip_format_parse_naddr(self, event_id, pubkey, naddr):
        """Roundtrip: format JSON with naddr → parse JSON → extract naddr."""
        # Format
        result = PublishResult(event_id=event_id, pubkey=pubkey, naddr=naddr)
        json_output = format_publish_result(result, cover_metadata=None)

        # Parse
        parsed = json.loads(json_output)

        # Verify roundtrip preserves naddr
        assert parsed["naddr"] == naddr

    def test_complete_workflow_with_cover_and_naddr(self):
        """Complete workflow: publish result with both cover and naddr."""
        pubkey = "abc123" + "0" * 58
        slug = "test-article"
        event_id = "def456" + "0" * 58
        naddr = encode_naddr(pubkey, slug)

        cover_metadata = {
            "hash": "3" * 64,
            "url": "https://cdn.example.com/cover.jpg",
            "dim": "1200x630",
            "mime": "image/jpeg",
        }

        result = PublishResult(event_id=event_id, pubkey=pubkey, naddr=naddr)
        json_output = format_publish_result(result, cover_metadata=cover_metadata)

        # Parse and verify all fields
        parsed = json.loads(json_output)

        assert parsed["event_id"] == event_id
        assert parsed["pubkey"] == pubkey
        assert parsed["naddr"] == naddr
        assert "image" in parsed
        assert parsed["image"]["hash"] == cover_metadata["hash"]
        assert parsed["image"]["url"] == cover_metadata["url"]
        assert parsed["image"]["dim"] == cover_metadata["dim"]
        assert parsed["image"]["mime"] == cover_metadata["mime"]

        # Verify keys are sorted
        keys = list(parsed.keys())
        assert keys == sorted(keys)

        # Verify single-line output
        assert "\n" not in json_output
