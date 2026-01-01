"""Unit tests for nak subprocess invocation.

Property-based tests for invoke_nak and parse_nak_output functions.
"""

import json
import subprocess
from unittest.mock import MagicMock, patch

import pytest
from hypothesis import assume, given
from hypothesis import strategies as st

from nostr_publish.errors import NakInvocationError, PublishTimeoutError, SigningError
from nostr_publish.models import PublishResult, UnsignedEvent
from nostr_publish.nak import invoke_nak, parse_nak_output


# Strategy for generating valid event IDs (64-character hex strings)
@st.composite
def event_ids(draw):
    return "".join(draw(st.lists(st.sampled_from("0123456789abcdef"), min_size=64, max_size=64)))


# Strategy for generating valid pubkeys (66-character hex strings with prefix)
@st.composite
def pubkeys(draw):
    return "0" + "".join(draw(st.lists(st.sampled_from("0123456789abcdef"), min_size=65, max_size=65)))


# Strategy for generating UnsignedEvent instances
@st.composite
def unsigned_events(draw):
    return UnsignedEvent(
        kind=30023,
        content=draw(st.text(min_size=1, max_size=1000)),
        tags=draw(st.lists(st.lists(st.text(min_size=1, max_size=100), min_size=1, max_size=3), max_size=50)),
    )


# Strategy for generating realistic nak stdout (status lines + JSON)
@st.composite
def nak_stdout(draw, event_id=None, pubkey=None):
    """Generate realistic nak stdout with status lines and event JSON."""
    if event_id is None:
        event_id = draw(event_ids())
    if pubkey is None:
        pubkey = draw(pubkeys())

    # Generate random relay URLs for status messages
    relay_count = draw(st.integers(min_value=1, max_value=3))
    relays = [f"wss://relay{i}.example.com" for i in range(relay_count)]

    lines = []
    # Connection status lines
    for relay in relays:
        lines.append(f"connecting to {relay}... ok.")

    # Event JSON line (this is what nak actually outputs)
    event_json = {
        "kind": 30023,
        "id": event_id,
        "pubkey": pubkey,
        "created_at": draw(st.integers(min_value=1000000000, max_value=2000000000)),
        "tags": [],
        "content": "test content",
        "sig": "a" * 128,
    }
    lines.append(json.dumps(event_json))

    # Publishing status lines
    for relay in relays:
        status = draw(st.sampled_from(["success.", "failed: connection refused"]))
        lines.append(f"publishing to {relay.replace('wss://', '')}... {status}")

    return "\n".join(lines)


class TestParseNakOutput:
    """Property-based tests for parse_nak_output function."""

    @given(event_id=event_ids(), pubkey=pubkeys())
    def test_parse_valid_output_deterministic(self, event_id, pubkey):
        """Property: parse_nak_output is deterministic for valid input."""
        # Simulate realistic nak output with status lines and event JSON
        stdout = f"""connecting to wss://relay.example.com... ok.
{{"kind":30023,"id":"{event_id}","pubkey":"{pubkey}","created_at":1234567890,"tags":[],"content":"test","sig":"{"a" * 128}"}}
publishing to relay.example.com... success."""

        result1 = parse_nak_output(stdout)
        result2 = parse_nak_output(stdout)

        assert result1 == result2
        assert result1.event_id == event_id
        assert result1.pubkey == pubkey

    @given(event_id=event_ids(), pubkey=pubkeys())
    def test_parse_output_extracts_json_from_mixed_output(self, event_id, pubkey):
        """Property: correctly extracts JSON from mixed status/JSON output."""
        stdout = f"""connecting to wss://relay1.example.com... ok.
connecting to wss://relay2.example.com... ok.
{{"kind":1,"id":"{event_id}","pubkey":"{pubkey}","created_at":1234567890,"tags":[],"content":"hello","sig":"{"b" * 128}"}}
publishing to relay1.example.com... success.
publishing to relay2.example.com... success."""

        result = parse_nak_output(stdout)

        assert result.event_id == event_id
        assert result.pubkey == pubkey

    @given(event_id=event_ids(), pubkey=pubkeys())
    def test_parse_output_handles_json_only(self, event_id, pubkey):
        """Property: handles output with only JSON (no relay args)."""
        stdout = json.dumps(
            {
                "kind": 1,
                "id": event_id,
                "pubkey": pubkey,
                "created_at": 1234567890,
                "tags": [],
                "content": "test",
                "sig": "a" * 128,
            }
        )

        result = parse_nak_output(stdout)

        assert result.event_id == event_id
        assert result.pubkey == pubkey

    @given(event_id=event_ids(), pubkey=pubkeys(), extra_field=st.text())
    def test_parse_output_ignores_extra_fields(self, event_id, pubkey, extra_field):
        """Property: extra unknown fields in JSON are ignored."""
        assume(extra_field not in ("id", "pubkey", "kind", "created_at", "tags", "content", "sig"))

        stdout = json.dumps(
            {
                "id": event_id,
                "pubkey": pubkey,
                "kind": 1,
                "created_at": 1234567890,
                "tags": [],
                "content": "test",
                "sig": "a" * 128,
                "unknown_field": extra_field,
            }
        )

        result = parse_nak_output(stdout)

        assert result.event_id == event_id
        assert result.pubkey == pubkey

    @given(st.text().filter(lambda x: not x.strip()))
    def test_parse_output_empty_event_id_fails(self, empty_id):
        """Property: empty or missing event ID raises NakInvocationError."""
        stdout = json.dumps({"id": empty_id, "pubkey": "02" + "a" * 64})

        with pytest.raises(NakInvocationError, match="missing required field: id"):
            parse_nak_output(stdout)

    def test_parse_output_missing_event_id_fails(self):
        """Property: missing event ID raises NakInvocationError."""
        stdout = json.dumps({"pubkey": "02" + "a" * 64})

        with pytest.raises(NakInvocationError, match="missing required field: id"):
            parse_nak_output(stdout)

    @given(st.text().filter(lambda x: not x.strip()))
    def test_parse_output_empty_pubkey_fails(self, empty_pubkey):
        """Property: empty or missing pubkey raises NakInvocationError."""
        stdout = json.dumps({"id": "a" * 64, "pubkey": empty_pubkey})

        with pytest.raises(NakInvocationError, match="missing required field: pubkey"):
            parse_nak_output(stdout)

    def test_parse_output_missing_pubkey_fails(self):
        """Property: missing pubkey raises NakInvocationError."""
        stdout = json.dumps({"id": "a" * 64})

        with pytest.raises(NakInvocationError, match="missing required field: pubkey"):
            parse_nak_output(stdout)

    def test_parse_output_no_json_line_fails(self):
        """Property: output with no JSON line raises NakInvocationError."""
        stdout = """connecting to wss://relay.example.com... ok.
publishing to relay.example.com... success."""

        with pytest.raises(NakInvocationError, match="No JSON event found"):
            parse_nak_output(stdout)

    def test_parse_output_invalid_json_fails(self):
        """Property: invalid JSON raises NakInvocationError."""
        invalid_json = "{this is not json}"

        with pytest.raises(NakInvocationError, match="Failed to parse nak output as JSON"):
            parse_nak_output(invalid_json)

    def test_parse_output_non_object_json_fails(self):
        """Property: non-object JSON output raises NakInvocationError."""
        stdout = json.dumps(["id", "pubkey"])

        # Arrays don't match the JSON object detection (starts with { ends with })
        with pytest.raises(NakInvocationError, match="No JSON event found"):
            parse_nak_output(stdout)

    @given(event_id=event_ids(), pubkey=pubkeys())
    def test_parse_output_returns_publish_result(self, event_id, pubkey):
        """Property: returns correct PublishResult instance."""
        stdout = json.dumps(
            {
                "kind": 1,
                "id": event_id,
                "pubkey": pubkey,
                "created_at": 1234567890,
                "tags": [],
                "content": "test",
                "sig": "a" * 128,
            }
        )

        result = parse_nak_output(stdout)

        assert isinstance(result, PublishResult)
        assert result.event_id == event_id
        assert result.pubkey == pubkey


class TestInvokeNak:
    """Property-based tests for invoke_nak function."""

    @given(event=unsigned_events(), event_id=event_ids(), pubkey=pubkeys())
    def test_invoke_nak_success_with_valid_nak(self, event, event_id, pubkey):
        """Property: successful nak invocation returns correct PublishResult."""
        bunker_uri = "bunker://test.uri"
        relays = ["wss://relay1.example.com", "wss://relay2.example.com"]
        timeout = 30

        # Simulate realistic nak output
        nak_output = f"""connecting to wss://relay1.example.com... ok.
connecting to wss://relay2.example.com... ok.
{{"kind":30023,"id":"{event_id}","pubkey":"{pubkey}","created_at":1234567890,"tags":[],"content":"test","sig":"{"a" * 128}"}}
publishing to relay1.example.com... success.
publishing to relay2.example.com... success."""

        with patch("subprocess.Popen") as mock_popen:
            mock_process = MagicMock()
            mock_process.communicate.return_value = (nak_output, "")
            mock_process.returncode = 0
            mock_popen.return_value = mock_process

            result = invoke_nak(event, bunker_uri, relays, timeout)

            assert isinstance(result, PublishResult)
            assert result.event_id == event_id
            assert result.pubkey == pubkey

    @given(event=unsigned_events())
    def test_invoke_nak_constructs_correct_command(self, event):
        """Property: invoke_nak constructs command with correct structure."""
        bunker_uri = "bunker://test.uri"
        relays = ["wss://relay1.example.com", "wss://relay2.example.com"]
        timeout = 30

        nak_output = json.dumps(
            {
                "id": "a" * 64,
                "pubkey": "02" + "b" * 64,
                "kind": 1,
                "created_at": 1234567890,
                "tags": [],
                "content": "test",
                "sig": "c" * 128,
            }
        )

        with patch("subprocess.Popen") as mock_popen:
            mock_process = MagicMock()
            mock_process.communicate.return_value = (nak_output, "")
            mock_process.returncode = 0
            mock_popen.return_value = mock_process

            invoke_nak(event, bunker_uri, relays, timeout)

            called_cmd = mock_popen.call_args[0][0]

            assert "nak" in called_cmd
            assert "event" in called_cmd
            assert "--sec" in called_cmd
            assert bunker_uri in called_cmd
            # Relays are positional arguments at the end (not with --relay flag)
            for relay in relays:
                assert relay in called_cmd

    @given(event=unsigned_events())
    def test_invoke_nak_passes_event_json_to_stdin(self, event):
        """Property: invoke_nak passes event as JSON to subprocess stdin."""
        bunker_uri = "bunker://test.uri"
        relays = ["wss://relay1.example.com"]
        timeout = 30

        nak_output = json.dumps(
            {
                "id": "a" * 64,
                "pubkey": "02" + "b" * 64,
                "kind": 1,
                "created_at": 1234567890,
                "tags": [],
                "content": "test",
                "sig": "c" * 128,
            }
        )

        with patch("subprocess.Popen") as mock_popen:
            mock_process = MagicMock()
            mock_process.communicate.return_value = (nak_output, "")
            mock_process.returncode = 0
            mock_popen.return_value = mock_process

            invoke_nak(event, bunker_uri, relays, timeout)

            communicate_input = mock_process.communicate.call_args[1]["input"]
            parsed_input = json.loads(communicate_input)

            assert parsed_input["kind"] == event.kind
            assert parsed_input["content"] == event.content
            assert parsed_input["tags"] == event.tags

    @given(event=unsigned_events())
    def test_invoke_nak_timeout_raises_timeout_error(self, event):
        """Property: subprocess timeout raises PublishTimeoutError."""
        bunker_uri = "bunker://test.uri"
        relays = ["wss://relay1.example.com"]
        timeout = 1

        with patch("subprocess.Popen") as mock_popen:
            mock_process = MagicMock()
            mock_process.communicate.side_effect = subprocess.TimeoutExpired("nak", timeout)
            mock_popen.return_value = mock_process

            with pytest.raises(PublishTimeoutError, match="timed out"):
                invoke_nak(event, bunker_uri, relays, timeout)

            mock_process.kill.assert_called_once()
            mock_process.wait.assert_called_once()

    @given(event=unsigned_events())
    def test_invoke_nak_nak_not_found_raises_error(self, event):
        """Property: nak binary not found raises NakInvocationError."""
        bunker_uri = "bunker://test.uri"
        relays = ["wss://relay1.example.com"]
        timeout = 30

        with patch("subprocess.Popen") as mock_popen:
            mock_popen.side_effect = FileNotFoundError("nak not found")

            with pytest.raises(NakInvocationError, match="nak binary not found"):
                invoke_nak(event, bunker_uri, relays, timeout)

    @given(event=unsigned_events())
    def test_invoke_nak_process_error_raises_error(self, event):
        """Property: other process errors raise NakInvocationError."""
        bunker_uri = "bunker://test.uri"
        relays = ["wss://relay1.example.com"]
        timeout = 30

        with patch("subprocess.Popen") as mock_popen:
            mock_popen.side_effect = OSError("Permission denied")

            with pytest.raises(NakInvocationError, match="Failed to start nak"):
                invoke_nak(event, bunker_uri, relays, timeout)

    @given(event=unsigned_events())
    def test_invoke_nak_non_zero_exit_raises_error(self, event):
        """Property: non-zero exit code raises NakInvocationError."""
        bunker_uri = "bunker://test.uri"
        relays = ["wss://relay1.example.com"]
        timeout = 30

        with patch("subprocess.Popen") as mock_popen:
            mock_process = MagicMock()
            mock_process.communicate.return_value = ("", "Generic error")
            mock_process.returncode = 1
            mock_popen.return_value = mock_process

            with pytest.raises(NakInvocationError, match="Generic error"):
                invoke_nak(event, bunker_uri, relays, timeout)

    @given(event=unsigned_events())
    def test_invoke_nak_signing_rejected_raises_signing_error(self, event):
        """Property: signing-related errors raise SigningError."""
        bunker_uri = "bunker://test.uri"
        relays = ["wss://relay1.example.com"]
        timeout = 30

        with patch("subprocess.Popen") as mock_popen:
            mock_process = MagicMock()
            mock_process.communicate.return_value = ("", "User rejected signing request")
            mock_process.returncode = 1
            mock_popen.return_value = mock_process

            with pytest.raises(SigningError, match="rejected"):
                invoke_nak(event, bunker_uri, relays, timeout)

    @given(event=unsigned_events())
    def test_invoke_nak_signing_denied_raises_signing_error(self, event):
        """Property: 'denied' in error message raises SigningError."""
        bunker_uri = "bunker://test.uri"
        relays = ["wss://relay1.example.com"]
        timeout = 30

        with patch("subprocess.Popen") as mock_popen:
            mock_process = MagicMock()
            mock_process.communicate.return_value = ("", "Request denied by signer")
            mock_process.returncode = 1
            mock_popen.return_value = mock_process

            with pytest.raises(SigningError):
                invoke_nak(event, bunker_uri, relays, timeout)

    @given(event=unsigned_events())
    def test_invoke_nak_empty_stderr_uses_exit_code_message(self, event):
        """Property: empty stderr falls back to exit code message."""
        bunker_uri = "bunker://test.uri"
        relays = ["wss://relay1.example.com"]
        timeout = 30

        with patch("subprocess.Popen") as mock_popen:
            mock_process = MagicMock()
            mock_process.communicate.return_value = ("", "")
            mock_process.returncode = 42
            mock_popen.return_value = mock_process

            with pytest.raises(NakInvocationError, match="exited with code 42"):
                invoke_nak(event, bunker_uri, relays, timeout)

    @given(st.lists(st.just("wss://relay.example.com"), min_size=0, max_size=10))
    def test_invoke_nak_multiple_relays(self, relays):
        """Property: all relay arguments are passed to nak as positional args."""
        event = UnsignedEvent(kind=30023, content="test", tags=[])
        bunker_uri = "bunker://test.uri"
        timeout = 30

        nak_output = json.dumps(
            {
                "id": "a" * 64,
                "pubkey": "02" + "b" * 64,
                "kind": 1,
                "created_at": 1234567890,
                "tags": [],
                "content": "test",
                "sig": "c" * 128,
            }
        )

        with patch("subprocess.Popen") as mock_popen:
            mock_process = MagicMock()
            mock_process.communicate.return_value = (nak_output, "")
            mock_process.returncode = 0
            mock_popen.return_value = mock_process

            invoke_nak(event, bunker_uri, relays, timeout)

            called_cmd = mock_popen.call_args[0][0]

            # Relays are positional arguments at the end (after nak, event, --sec, uri)
            for relay in relays:
                assert relay in called_cmd

    @given(event=unsigned_events())
    def test_invoke_nak_uses_text_mode(self, event):
        """Property: subprocess is created with text=True."""
        bunker_uri = "bunker://test.uri"
        relays = ["wss://relay1.example.com"]
        timeout = 30

        nak_output = json.dumps(
            {
                "id": "a" * 64,
                "pubkey": "02" + "b" * 64,
                "kind": 1,
                "created_at": 1234567890,
                "tags": [],
                "content": "test",
                "sig": "c" * 128,
            }
        )

        with patch("subprocess.Popen") as mock_popen:
            mock_process = MagicMock()
            mock_process.communicate.return_value = (nak_output, "")
            mock_process.returncode = 0
            mock_popen.return_value = mock_process

            invoke_nak(event, bunker_uri, relays, timeout)

            kwargs = mock_popen.call_args[1]
            assert kwargs.get("text") is True
            assert kwargs.get("stdin") == subprocess.PIPE
            assert kwargs.get("stdout") == subprocess.PIPE
            assert kwargs.get("stderr") == subprocess.PIPE

    @given(event=unsigned_events(), event_id=event_ids(), pubkey=pubkeys())
    def test_invoke_nak_passes_timeout_to_communicate(self, event, event_id, pubkey):
        """Property: timeout parameter is passed to communicate."""
        bunker_uri = "bunker://test.uri"
        relays = ["wss://relay1.example.com"]
        timeout = 42

        nak_output = json.dumps(
            {
                "id": event_id,
                "pubkey": pubkey,
                "kind": 1,
                "created_at": 1234567890,
                "tags": [],
                "content": "test",
                "sig": "c" * 128,
            }
        )

        with patch("subprocess.Popen") as mock_popen:
            mock_process = MagicMock()
            mock_process.communicate.return_value = (nak_output, "")
            mock_process.returncode = 0
            mock_popen.return_value = mock_process

            invoke_nak(event, bunker_uri, relays, timeout)

            kwargs = mock_process.communicate.call_args[1]
            assert kwargs.get("timeout") == timeout
