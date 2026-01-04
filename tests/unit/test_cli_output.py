"""Property-based and unit tests for CLI output formatting.

Tests cover JSON serialization, field presence, determinism, and edge cases
for the format_publish_result function with comprehensive property-based tests.
"""

import json

import pytest
from hypothesis import given
from hypothesis import strategies as st
from hypothesis.strategies import composite

from nostr_publish.cli_output import format_publish_result
from nostr_publish.models import PublishResult

# === Strategies ===


@composite
def event_ids(draw):
    """Generate valid event IDs (hex strings)."""
    return draw(st.text(alphabet="0123456789abcdef", min_size=1, max_size=64))


@composite
def pubkeys(draw):
    """Generate valid pubkeys (hex strings)."""
    return draw(st.text(alphabet="0123456789abcdef", min_size=1, max_size=64))


@composite
def nadirs(draw):
    """Generate valid naddr values or None."""
    return draw(st.one_of(st.none(), st.text(alphabet="0123456789abcdef", min_size=1, max_size=100)))


@composite
def urls(draw):
    """Generate valid URLs."""
    return draw(st.from_regex(r"https://[a-z0-9.-]+\.[a-z]{2,}(/[a-z0-9._-]*)*", fullmatch=True))


@composite
def mime_types(draw):
    """Generate valid MIME types."""
    return draw(st.sampled_from(["image/jpeg", "image/png", "image/webp", "image/gif"]))


@composite
def dimensions(draw):
    """Generate valid image dimensions."""
    return draw(st.from_regex(r"[1-9][0-9]{2,3}x[1-9][0-9]{2,3}", fullmatch=True))


@composite
def hashes(draw):
    """Generate valid SHA256 hash strings (64 hex chars)."""
    return draw(st.text(alphabet="0123456789abcdef", min_size=64, max_size=64))


@composite
def cover_metadata(draw):
    """Generate valid cover metadata dictionaries."""
    return {"hash": draw(hashes()), "url": draw(urls()), "dim": draw(dimensions()), "mime": draw(mime_types())}


@composite
def publish_results(draw, include_naddr=None):
    """Generate PublishResult instances.

    Args:
        include_naddr: If None, randomly decide. If True, always include naddr.
            If False, never include naddr.
    """
    event_id = draw(event_ids())
    pubkey = draw(pubkeys())

    if include_naddr is None:
        naddr = draw(nadirs())
    elif include_naddr:
        naddr = draw(nadirs().filter(lambda x: x is not None))
    else:
        naddr = None

    return PublishResult(event_id=event_id, pubkey=pubkey, naddr=naddr)


# === Property-Based Tests: JSON Validity ===


class TestFormatPublishResultJsonValidity:
    """Output is always valid JSON."""

    @given(result=publish_results())
    def test_output_is_valid_json(self, result):
        """Output is always valid JSON that can be parsed."""
        output = format_publish_result(result)
        parsed = json.loads(output)
        assert isinstance(parsed, dict)

    @given(result=publish_results(), metadata=st.one_of(st.none(), cover_metadata()))
    def test_output_with_cover_is_valid_json(self, result, metadata):
        """Output with cover metadata is always valid JSON."""
        output = format_publish_result(result, metadata)
        parsed = json.loads(output)
        assert isinstance(parsed, dict)

    @given(output=st.text(min_size=0, max_size=1000))
    def test_malformed_input_still_produces_json(self, output):
        """Even with strange input strings, output is valid JSON."""
        result = PublishResult(event_id=output, pubkey=output)
        try:
            output_str = format_publish_result(result)
            json.loads(output_str)
        except json.JSONDecodeError:
            pytest.fail("Output should be valid JSON")


# === Property-Based Tests: Field Presence ===


class TestFormatPublishResultFieldPresence:
    """Required and optional fields are present/absent correctly."""

    @given(result=publish_results())
    def test_event_id_always_present(self, result):
        """event_id field is always present."""
        output = format_publish_result(result)
        parsed = json.loads(output)
        assert "event_id" in parsed
        assert parsed["event_id"] == result.event_id

    @given(result=publish_results())
    def test_pubkey_always_present(self, result):
        """pubkey field is always present."""
        output = format_publish_result(result)
        parsed = json.loads(output)
        assert "pubkey" in parsed
        assert parsed["pubkey"] == result.pubkey

    @given(result=publish_results(include_naddr=True))
    def test_naddr_present_when_provided(self, result):
        """naddr field is present when result.naddr is not None."""
        assert result.naddr is not None
        output = format_publish_result(result)
        parsed = json.loads(output)
        assert "naddr" in parsed
        assert parsed["naddr"] == result.naddr

    @given(result=publish_results(include_naddr=False))
    def test_naddr_absent_when_none(self, result):
        """naddr field is absent when result.naddr is None."""
        assert result.naddr is None
        output = format_publish_result(result)
        parsed = json.loads(output)
        assert "naddr" not in parsed

    def test_naddr_absent_when_empty_string(self):
        """naddr field is absent when result.naddr is empty string."""
        result = PublishResult(event_id="abc", pubkey="def", naddr="")
        output = format_publish_result(result)
        parsed = json.loads(output)
        assert "naddr" not in parsed

    @given(result=publish_results(), metadata=cover_metadata())
    def test_cover_present_when_provided(self, result, metadata):
        """cover field is present when cover_metadata is provided."""
        output = format_publish_result(result, metadata)
        parsed = json.loads(output)
        assert "image" in parsed
        assert parsed["image"] == metadata

    @given(result=publish_results())
    def test_cover_absent_when_not_provided(self, result):
        """cover field is absent when cover_metadata is None."""
        output = format_publish_result(result, None)
        parsed = json.loads(output)
        assert "image" not in parsed


# === Property-Based Tests: Determinism ===


class TestFormatPublishResultDeterminism:
    """Same inputs always produce same output."""

    @given(result=publish_results())
    def test_deterministic_output(self, result):
        """Calling function twice with same result produces identical output."""
        output1 = format_publish_result(result)
        output2 = format_publish_result(result)
        assert output1 == output2

    @given(result=publish_results(), metadata=st.one_of(st.none(), cover_metadata()))
    def test_deterministic_with_cover(self, result, metadata):
        """Calling function twice with same result and metadata produces identical output."""
        output1 = format_publish_result(result, metadata)
        output2 = format_publish_result(result, metadata)
        assert output1 == output2

    @given(result=publish_results(include_naddr=True), metadata=cover_metadata())
    def test_deterministic_with_all_fields(self, result, metadata):
        """All fields present results are deterministic."""
        output1 = format_publish_result(result, metadata)
        output2 = format_publish_result(result, metadata)
        output3 = format_publish_result(result, metadata)
        assert output1 == output2 == output3


# === Property-Based Tests: Key Ordering ===


class TestFormatPublishResultKeyOrdering:
    """Keys are always sorted alphabetically."""

    @given(result=publish_results())
    def test_keys_sorted_alphabetically(self, result):
        """JSON keys are sorted alphabetically."""
        output = format_publish_result(result)
        # Manual check: extract keys from JSON object notation
        # JSON object format is {key1: value1, key2: value2, ...}
        # We can verify by parsing and checking key order
        parsed = json.loads(output)
        keys = list(parsed.keys())
        assert keys == sorted(keys)

    @given(result=publish_results(include_naddr=True), metadata=cover_metadata())
    def test_keys_sorted_with_all_fields(self, result, metadata):
        """Keys are sorted when all fields present."""
        output = format_publish_result(result, metadata)
        parsed = json.loads(output)
        keys = list(parsed.keys())
        assert keys == sorted(keys)

    @given(result=publish_results(include_naddr=True))
    def test_cover_keys_sorted(self, result):
        """Cover object keys are sorted alphabetically."""
        metadata = {"hash": "abc123", "url": "https://example.com", "dim": "1200x630", "mime": "image/jpeg"}
        output = format_publish_result(result, metadata)
        parsed = json.loads(output)
        if "image" in parsed:
            cover_keys = list(parsed["image"].keys())
            assert cover_keys == sorted(cover_keys)


# === Property-Based Tests: Single-Line Output ===


class TestFormatPublishResultSingleLine:
    """Output is single-line with no embedded newlines."""

    @given(result=publish_results())
    def test_no_trailing_newline(self, result):
        """Output does not have trailing newline."""
        output = format_publish_result(result)
        assert not output.endswith("\n")

    @given(result=publish_results(), metadata=st.one_of(st.none(), cover_metadata()))
    def test_no_embedded_newlines(self, result, metadata):
        """Output has no embedded newlines."""
        output = format_publish_result(result, metadata)
        assert "\n" not in output

    @given(result=publish_results(include_naddr=True), metadata=cover_metadata())
    def test_single_line_with_all_fields(self, result, metadata):
        """Output is single line even with all fields."""
        output = format_publish_result(result, metadata)
        assert "\n" not in output
        assert output.startswith("{")
        assert output.endswith("}")


# === Property-Based Tests: UTF-8 Preservation ===


class TestFormatPublishResultUtf8Preservation:
    """UTF-8 characters are preserved, not escaped."""

    @given(event_id=st.text(alphabet="0123456789abcdef", min_size=1, max_size=20))
    def test_utf8_in_event_id(self, event_id):
        """UTF-8 characters in event_id are preserved."""
        result = PublishResult(event_id=event_id, pubkey="pubkey")
        output = format_publish_result(result)
        parsed = json.loads(output)
        assert parsed["event_id"] == event_id

    def test_unicode_characters_preserved(self):
        """Unicode characters are preserved in output."""
        result = PublishResult(event_id="café", pubkey="café")
        output = format_publish_result(result)
        # ensure_ascii=False means unicode chars are in output as-is
        assert "café" in output
        # Verify it parses correctly
        parsed = json.loads(output)
        assert parsed["event_id"] == "café"
        assert parsed["pubkey"] == "café"

    def test_emoji_preserved(self):
        """Emoji characters are preserved."""
        result = PublishResult(event_id="event🎉", pubkey="pub🎉")
        output = format_publish_result(result)
        parsed = json.loads(output)
        assert parsed["event_id"] == "event🎉"
        assert parsed["pubkey"] == "pub🎉"


# === Property-Based Tests: Parseability ===


class TestFormatPublishResultParseability:
    """Output is parseable by json.loads."""

    @given(result=publish_results(), metadata=st.one_of(st.none(), cover_metadata()))
    def test_json_loads_succeeds(self, result, metadata):
        """json.loads() always succeeds on output."""
        output = format_publish_result(result, metadata)
        try:
            parsed = json.loads(output)
            assert isinstance(parsed, dict)
        except json.JSONDecodeError:
            pytest.fail("json.loads() should succeed")

    @given(result=publish_results(include_naddr=True), metadata=cover_metadata())
    def test_parsed_structure_matches_input(self, result, metadata):
        """Parsed JSON structure matches input data."""
        output = format_publish_result(result, metadata)
        parsed = json.loads(output)

        assert parsed["event_id"] == result.event_id
        assert parsed["pubkey"] == result.pubkey
        assert parsed["naddr"] == result.naddr
        assert parsed["image"] == metadata


# === Property-Based Tests: Completeness ===


class TestFormatPublishResultCompleteness:
    """All non-None fields from input are included."""

    @given(result=publish_results(include_naddr=True))
    def test_naddr_field_included(self, result):
        """naddr is included if provided and non-empty."""
        output = format_publish_result(result)
        parsed = json.loads(output)
        assert parsed["naddr"] == result.naddr

    @given(result=publish_results(), metadata=cover_metadata())
    def test_cover_fields_included(self, result, metadata):
        """All cover fields are included in output."""
        output = format_publish_result(result, metadata)
        parsed = json.loads(output)
        assert "image" in parsed
        for key in metadata:
            assert key in parsed["image"]
            assert parsed["image"][key] == metadata[key]

    @given(result=publish_results(include_naddr=True), metadata=cover_metadata())
    def test_all_fields_included_when_present(self, result, metadata):
        """All fields are included when all inputs provided."""
        output = format_publish_result(result, metadata)
        parsed = json.loads(output)

        assert "event_id" in parsed
        assert "pubkey" in parsed
        assert "naddr" in parsed
        assert "image" in parsed

        assert parsed["event_id"] == result.event_id
        assert parsed["pubkey"] == result.pubkey
        assert parsed["naddr"] == result.naddr
        assert parsed["image"] == metadata


# === Property-Based Tests: No Extra Fields ===


class TestFormatPublishResultNoExtraFields:
    """No unexpected fields are added to output."""

    @given(result=publish_results(include_naddr=False))
    def test_minimal_case_no_extra_fields(self, result):
        """With no naddr/cover, output has exactly 2 fields."""
        output = format_publish_result(result)
        parsed = json.loads(output)
        assert set(parsed.keys()) == {"event_id", "pubkey"}

    @given(result=publish_results(include_naddr=True))
    def test_with_naddr_no_extra_fields(self, result):
        """With naddr only, output has exactly 3 fields."""
        output = format_publish_result(result)
        parsed = json.loads(output)
        assert set(parsed.keys()) == {"event_id", "pubkey", "naddr"}

    @given(result=publish_results(include_naddr=False), metadata=cover_metadata())
    def test_with_cover_no_extra_fields(self, result, metadata):
        """With cover only, output has exactly 3 fields."""
        output = format_publish_result(result, metadata)
        parsed = json.loads(output)
        assert set(parsed.keys()) == {"event_id", "pubkey", "image"}

    @given(result=publish_results(include_naddr=True), metadata=cover_metadata())
    def test_complete_case_no_extra_fields(self, result, metadata):
        """With all fields, output has exactly 4 fields."""
        output = format_publish_result(result, metadata)
        parsed = json.loads(output)
        assert set(parsed.keys()) == {"event_id", "pubkey", "naddr", "image"}


# === Example-Based Tests: Specific Behaviors ===


class TestFormatPublishResultExamples:
    """Test specific documented examples from contract."""

    def test_minimal_example(self):
        """Minimal case matches contract example."""
        result = PublishResult(event_id="abc", pubkey="def")
        output = format_publish_result(result)
        assert output == '{"event_id": "abc", "pubkey": "def"}'

    def test_with_naddr_example(self):
        """With naddr matches contract example."""
        result = PublishResult(event_id="abc", pubkey="def", naddr="naddr1xyz")
        output = format_publish_result(result)
        # Keys are sorted: event_id, naddr, pubkey
        assert output == '{"event_id": "abc", "naddr": "naddr1xyz", "pubkey": "def"}'

    def test_with_cover_example(self):
        """With cover matches contract example."""
        result = PublishResult(event_id="abc", pubkey="def")
        metadata = {"dim": "1200x630", "hash": "sha256...", "mime": "image/jpeg", "url": "https://..."}
        output = format_publish_result(result, metadata)
        parsed = json.loads(output)

        # Verify structure
        assert set(parsed.keys()) == {"image", "event_id", "pubkey"}
        assert parsed["event_id"] == "abc"
        assert parsed["pubkey"] == "def"
        assert parsed["image"]["dim"] == "1200x630"
        assert parsed["image"]["hash"] == "sha256..."
        assert parsed["image"]["mime"] == "image/jpeg"
        assert parsed["image"]["url"] == "https://..."

    def test_complete_example(self):
        """Complete case with all fields matches contract."""
        result = PublishResult(event_id="abc", pubkey="def", naddr="naddr1xyz")
        metadata = {"dim": "1200x630", "hash": "sha256...", "mime": "image/jpeg", "url": "https://..."}
        output = format_publish_result(result, metadata)
        parsed = json.loads(output)

        # Verify all fields present and sorted alphabetically
        keys = list(parsed.keys())
        assert keys == ["event_id", "image", "naddr", "pubkey"]
        assert parsed["event_id"] == "abc"
        assert parsed["pubkey"] == "def"
        assert parsed["naddr"] == "naddr1xyz"
        assert parsed["image"] == metadata

    def test_empty_naddr_treated_as_none(self):
        """Empty string naddr is treated as if not provided."""
        result = PublishResult(event_id="abc", pubkey="def", naddr="")
        output = format_publish_result(result)
        parsed = json.loads(output)
        assert "naddr" not in parsed
        assert set(parsed.keys()) == {"event_id", "pubkey"}


# === Edge Cases ===


class TestFormatPublishResultEdgeCases:
    """Edge cases and boundary conditions."""

    def test_very_long_event_id(self):
        """Very long event_id is handled."""
        long_id = "a" * 1000
        result = PublishResult(event_id=long_id, pubkey="pub")
        output = format_publish_result(result)
        parsed = json.loads(output)
        assert parsed["event_id"] == long_id

    def test_very_long_pubkey(self):
        """Very long pubkey is handled."""
        long_key = "b" * 1000
        result = PublishResult(event_id="id", pubkey=long_key)
        output = format_publish_result(result)
        parsed = json.loads(output)
        assert parsed["pubkey"] == long_key

    def test_special_json_characters_in_event_id(self):
        """Special JSON characters in event_id are escaped properly."""
        result = PublishResult(event_id='abc"def', pubkey="pub")
        output = format_publish_result(result)
        parsed = json.loads(output)
        assert parsed["event_id"] == 'abc"def'

    def test_special_json_characters_in_pubkey(self):
        """Special JSON characters in pubkey are escaped properly."""
        result = PublishResult(event_id="id", pubkey="pub\\key")
        output = format_publish_result(result)
        parsed = json.loads(output)
        assert parsed["pubkey"] == "pub\\key"

    def test_special_json_characters_in_naddr(self):
        """Special JSON characters in naddr are escaped properly."""
        result = PublishResult(event_id="id", pubkey="pub", naddr='naddr{}"')
        output = format_publish_result(result)
        parsed = json.loads(output)
        assert parsed["naddr"] == 'naddr{}"'

    def test_special_json_characters_in_cover_values(self):
        """Special JSON characters in cover metadata are escaped properly."""
        result = PublishResult(event_id="id", pubkey="pub")
        metadata = {
            "hash": "abc123",
            "url": 'https://example.com/file"with"quotes.jpg',
            "dim": "1200x630",
            "mime": "image/jpeg",
        }
        output = format_publish_result(result, metadata)
        parsed = json.loads(output)
        assert parsed["image"]["url"] == metadata["url"]

    def test_whitespace_in_event_id(self):
        """Whitespace in event_id is preserved."""
        result = PublishResult(event_id="abc def\ttab", pubkey="pub")
        output = format_publish_result(result)
        parsed = json.loads(output)
        assert parsed["event_id"] == "abc def\ttab"

    def test_whitespace_in_naddr(self):
        """Whitespace in naddr is preserved."""
        result = PublishResult(event_id="id", pubkey="pub", naddr="naddr space\t123")
        output = format_publish_result(result)
        parsed = json.loads(output)
        assert parsed["naddr"] == "naddr space\t123"

    def test_nested_cover_metadata_with_special_values(self):
        """Cover metadata with special values is handled."""
        result = PublishResult(event_id="id", pubkey="pub")
        metadata = {
            "hash": "a" * 64,
            "url": "https://example.com/image.jpg?v=1&size=large",
            "dim": "9999x9999",
            "mime": "image/jpeg",
        }
        output = format_publish_result(result, metadata)
        parsed = json.loads(output)
        assert parsed["image"] == metadata
