"""Unit tests for NIP-19 naddr encoding.

Property-based tests for encode_naddr and validate_naddr functions.
"""

import subprocess
from unittest.mock import MagicMock, patch

import pytest
from hypothesis import given
from hypothesis import strategies as st

from nostr_publish.errors import NakInvocationError
from nostr_publish.naddr_encoder import encode_naddr, validate_naddr


@st.composite
def valid_pubkeys(draw):
    """Generate valid 64-character hex pubkeys."""
    return "".join(draw(st.lists(st.sampled_from("0123456789abcdef"), min_size=64, max_size=64)))


@st.composite
def valid_slugs(draw):
    """Generate valid article slugs (non-empty alphanumeric with hyphens/underscores)."""
    return draw(st.text(alphabet="abcdefghijklmnopqrstuvwxyz0123456789-_", min_size=1, max_size=100))


@st.composite
def valid_nadrs(draw):
    """Generate valid naddr format strings."""
    prefix = "naddr1"
    bech32_chars = "023456789acdefghjklmnpqrstuvwxyz"
    data = "".join(draw(st.lists(st.sampled_from(bech32_chars), min_size=40, max_size=100)))
    return prefix + data


@st.composite
def invalid_pubkey_formats(draw):
    """Generate invalid pubkey formats."""
    choice = draw(st.integers(min_value=0, max_value=4))
    if choice == 0:
        return ""
    elif choice == 1:
        return "a" * 63
    elif choice == 2:
        return "a" * 65
    elif choice == 3:
        return "g" * 64
    else:
        return "zz" + "a" * 62


class TestValidateNaddr:
    """Property-based tests for validate_naddr function."""

    @given(naddr=valid_nadrs())
    def test_validate_valid_naddr_succeeds(self, naddr):
        """Property: valid naddr passes validation without raising."""
        validate_naddr(naddr)

    @given(naddr=valid_nadrs())
    def test_validate_same_naddr_deterministic(self, naddr):
        """Property: validate_naddr is deterministic for same input."""
        validate_naddr(naddr)
        validate_naddr(naddr)

    def test_validate_empty_string_fails(self):
        """Empty string fails validation."""
        with pytest.raises(NakInvocationError, match="naddr must be non-empty string"):
            validate_naddr("")

    def test_validate_none_fails(self):
        """None input fails validation."""
        with pytest.raises(NakInvocationError, match="naddr must be non-empty string"):
            validate_naddr(None)

    @given(whitespace=st.sampled_from([" ", "\t", "\n", "\r"]))
    def test_validate_naddr_with_leading_whitespace_fails(self, whitespace):
        """Property: naddr with leading whitespace fails validation."""
        naddr = whitespace + "naddr1qqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqq"
        with pytest.raises(NakInvocationError, match="naddr must not contain whitespace"):
            validate_naddr(naddr)

    @given(whitespace=st.sampled_from([" ", "\t", "\n", "\r"]))
    def test_validate_naddr_with_trailing_whitespace_fails(self, whitespace):
        """Property: naddr with trailing whitespace fails validation."""
        naddr = "naddr1qqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqq" + whitespace
        with pytest.raises(NakInvocationError, match="naddr must not contain whitespace"):
            validate_naddr(naddr)

    @given(whitespace=st.sampled_from([" ", "\t", "\n", "\r"]))
    def test_validate_naddr_with_internal_whitespace_fails(self, whitespace):
        """Property: naddr with internal whitespace fails validation."""
        naddr = "naddr1qqqqqqqq" + whitespace + "qqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqq"
        with pytest.raises(NakInvocationError, match="naddr must not contain whitespace"):
            validate_naddr(naddr)

    def test_validate_too_short_naddr_fails(self):
        """naddr shorter than 7 characters fails validation."""
        with pytest.raises(NakInvocationError, match="naddr must be at least 7 characters"):
            validate_naddr("naddr1")

    def test_validate_naddr_with_short_data_part_fails(self):
        """naddr shorter than 7 characters fails validation."""
        with pytest.raises(NakInvocationError, match="naddr must be at least 7 characters"):
            validate_naddr("naddr")

    def test_validate_wrong_prefix_fails(self):
        """naddr with wrong prefix fails validation."""
        with pytest.raises(NakInvocationError, match="naddr must start with 'naddr1'"):
            validate_naddr("naddr0qqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqq")

    def test_validate_no_prefix_fails(self):
        """naddr without prefix fails validation."""
        with pytest.raises(NakInvocationError, match="naddr must start with 'naddr1'"):
            validate_naddr("qqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqq")

    @given(invalid_char=st.sampled_from("1bio"))
    def test_validate_naddr_with_invalid_bech32_char_fails(self, invalid_char):
        """Property: naddr with invalid bech32 characters fails validation."""
        naddr = "naddr1" + invalid_char + "qqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqq"
        with pytest.raises(NakInvocationError, match="naddr contains invalid bech32 character"):
            validate_naddr(naddr)

    @given(invalid_char=st.sampled_from("ABCDEFGHIJKLMNOPQRSTUVWXYZ"))
    def test_validate_naddr_with_uppercase_fails(self, invalid_char):
        """Property: naddr with uppercase letters fails validation."""
        naddr = f"naddr1{invalid_char}qqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqq"
        with pytest.raises(NakInvocationError, match="naddr contains invalid bech32 character"):
            validate_naddr(naddr)

    @given(naddr=valid_nadrs())
    def test_validate_naddr_starts_with_naddr1(self, naddr):
        """Property: all generated valid nadrs start with naddr1."""
        assert naddr.startswith("naddr1")

    @given(naddr=valid_nadrs())
    def test_validate_naddr_minimum_length(self, naddr):
        """Property: all generated valid nadrs are at least 7 characters."""
        assert len(naddr) >= 7

    def test_validate_naddr_with_special_characters_fails(self):
        """naddr with special characters fails validation."""
        with pytest.raises(NakInvocationError, match="naddr contains invalid bech32 character"):
            validate_naddr("naddr1!@#$%^&*()")

    def test_validate_naddr_with_spaces_in_data_fails(self):
        """naddr with spaces in data part fails validation."""
        with pytest.raises(NakInvocationError, match="naddr must not contain whitespace"):
            validate_naddr("naddr1 qqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqq")


class TestEncodeNaddr:
    """Property-based tests for encode_naddr function."""

    @given(pubkey=valid_pubkeys(), slug=valid_slugs())
    def test_encode_valid_inputs_returns_naddr(self, pubkey, slug):
        """Property: encoding valid inputs returns valid naddr."""
        with patch("subprocess.Popen") as mock_popen:
            mock_process = MagicMock()
            mock_process.communicate.return_value = ("naddr1qqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqq", "")
            mock_process.returncode = 0
            mock_popen.return_value = mock_process

            result = encode_naddr(pubkey, slug)

            assert result.startswith("naddr1")
            assert isinstance(result, str)

    @given(pubkey=valid_pubkeys(), slug=valid_slugs())
    def test_encode_deterministic(self, pubkey, slug):
        """Property: same inputs always produce same output (deterministic)."""
        naddr_output = "naddr1qqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqq"

        with patch("subprocess.Popen") as mock_popen:
            mock_process = MagicMock()
            mock_process.communicate.return_value = (naddr_output, "")
            mock_process.returncode = 0
            mock_popen.return_value = mock_process

            result1 = encode_naddr(pubkey, slug)
            result2 = encode_naddr(pubkey, slug)

            assert result1 == result2

    @given(pubkey=valid_pubkeys(), slug=valid_slugs())
    def test_encode_constructs_correct_nak_command(self, pubkey, slug):
        """Property: encode_naddr constructs correct nak command with all parameters."""
        with patch("subprocess.Popen") as mock_popen:
            mock_process = MagicMock()
            mock_process.communicate.return_value = ("naddr1qqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqq", "")
            mock_process.returncode = 0
            mock_popen.return_value = mock_process

            encode_naddr(pubkey, slug)

            called_cmd = mock_popen.call_args[0][0]

            assert "nak" in called_cmd
            assert "encode" in called_cmd
            assert "naddr" in called_cmd
            assert "--kind" in called_cmd
            assert "30023" in called_cmd
            assert "--identifier" in called_cmd
            assert slug in called_cmd
            assert "--pubkey" in called_cmd
            assert pubkey in called_cmd

    @given(pubkey=valid_pubkeys(), slug=valid_slugs())
    def test_encode_uses_text_mode_for_subprocess(self, pubkey, slug):
        """Property: subprocess is created with text=True."""
        with patch("subprocess.Popen") as mock_popen:
            mock_process = MagicMock()
            mock_process.communicate.return_value = ("naddr1qqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqq", "")
            mock_process.returncode = 0
            mock_popen.return_value = mock_process

            encode_naddr(pubkey, slug)

            kwargs = mock_popen.call_args[1]
            assert kwargs.get("text") is True
            assert kwargs.get("stdin") == subprocess.PIPE
            assert kwargs.get("stdout") == subprocess.PIPE
            assert kwargs.get("stderr") == subprocess.PIPE

    @given(pubkey=valid_pubkeys(), slug=valid_slugs())
    def test_encode_naddr_timeout_raises_error(self, pubkey, slug):
        """Property: subprocess timeout raises NakInvocationError."""
        with patch("subprocess.Popen") as mock_popen:
            mock_process = MagicMock()
            mock_process.communicate.side_effect = subprocess.TimeoutExpired("nak", 30)
            mock_popen.return_value = mock_process

            with pytest.raises(NakInvocationError, match="timed out"):
                encode_naddr(pubkey, slug)

            mock_process.kill.assert_called_once()
            mock_process.wait.assert_called_once()

    @given(pubkey=valid_pubkeys(), slug=valid_slugs())
    def test_encode_nak_not_found_raises_error(self, pubkey, slug):
        """Property: nak binary not found raises NakInvocationError."""
        with patch("subprocess.Popen") as mock_popen:
            mock_popen.side_effect = FileNotFoundError("nak not found")

            with pytest.raises(NakInvocationError, match="nak binary not found"):
                encode_naddr(pubkey, slug)

    @given(pubkey=valid_pubkeys(), slug=valid_slugs())
    def test_encode_process_error_raises_error(self, pubkey, slug):
        """Property: process creation errors raise NakInvocationError."""
        with patch("subprocess.Popen") as mock_popen:
            mock_popen.side_effect = OSError("Permission denied")

            with pytest.raises(NakInvocationError, match="Failed to start nak"):
                encode_naddr(pubkey, slug)

    @given(pubkey=valid_pubkeys(), slug=valid_slugs())
    def test_encode_non_zero_exit_raises_error(self, pubkey, slug):
        """Property: non-zero exit code raises NakInvocationError."""
        with patch("subprocess.Popen") as mock_popen:
            mock_process = MagicMock()
            mock_process.communicate.return_value = ("", "Invalid pubkey")
            mock_process.returncode = 1
            mock_popen.return_value = mock_process

            with pytest.raises(NakInvocationError, match="Invalid pubkey"):
                encode_naddr(pubkey, slug)

    @given(pubkey=valid_pubkeys(), slug=valid_slugs())
    def test_encode_empty_output_raises_error(self, pubkey, slug):
        """Property: empty nak output raises NakInvocationError."""
        with patch("subprocess.Popen") as mock_popen:
            mock_process = MagicMock()
            mock_process.communicate.return_value = ("", "")
            mock_process.returncode = 0
            mock_popen.return_value = mock_process

            with pytest.raises(NakInvocationError, match="empty output"):
                encode_naddr(pubkey, slug)

    @given(pubkey=valid_pubkeys(), slug=valid_slugs())
    def test_encode_invalid_prefix_raises_error(self, pubkey, slug):
        """Property: naddr without naddr1 prefix raises NakInvocationError."""
        with patch("subprocess.Popen") as mock_popen:
            mock_process = MagicMock()
            mock_process.communicate.return_value = ("notaddr1qqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqq", "")
            mock_process.returncode = 0
            mock_popen.return_value = mock_process

            with pytest.raises(NakInvocationError, match="must start with 'naddr1'"):
                encode_naddr(pubkey, slug)

    def test_encode_empty_pubkey_raises_error(self):
        """Empty pubkey raises NakInvocationError."""
        with pytest.raises(NakInvocationError, match="pubkey must be non-empty string"):
            encode_naddr("", "my-slug")

    def test_encode_none_pubkey_raises_error(self):
        """None pubkey raises NakInvocationError."""
        with pytest.raises(NakInvocationError, match="pubkey must be non-empty string"):
            encode_naddr(None, "my-slug")

    def test_encode_empty_slug_raises_error(self):
        """Empty slug raises NakInvocationError."""
        with pytest.raises(NakInvocationError, match="slug must be non-empty string"):
            encode_naddr("a" * 64, "")

    def test_encode_none_slug_raises_error(self):
        """None slug raises NakInvocationError."""
        with pytest.raises(NakInvocationError, match="slug must be non-empty string"):
            encode_naddr("a" * 64, None)

    @given(pubkey=invalid_pubkey_formats())
    def test_encode_invalid_pubkey_format_raises_error(self, pubkey):
        """Property: invalid pubkey format raises NakInvocationError."""
        with pytest.raises(NakInvocationError):
            encode_naddr(pubkey, "my-slug")

    def test_encode_pubkey_too_short_raises_error(self):
        """Pubkey shorter than 64 hex chars raises NakInvocationError."""
        with pytest.raises(NakInvocationError, match="must be 64 hex characters"):
            encode_naddr("a" * 63, "my-slug")

    def test_encode_pubkey_too_long_raises_error(self):
        """Pubkey longer than 64 hex chars raises NakInvocationError."""
        with pytest.raises(NakInvocationError, match="must be 64 hex characters"):
            encode_naddr("a" * 65, "my-slug")

    def test_encode_pubkey_invalid_hex_raises_error(self):
        """Pubkey with non-hex characters raises NakInvocationError."""
        with pytest.raises(NakInvocationError, match="valid hexadecimal"):
            encode_naddr("g" * 64, "my-slug")

    def test_encode_pubkey_uppercase_hex_is_valid(self):
        """Pubkey with uppercase hex chars is valid."""
        with patch("subprocess.Popen") as mock_popen:
            mock_process = MagicMock()
            mock_process.communicate.return_value = ("naddr1qqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqq", "")
            mock_process.returncode = 0
            mock_popen.return_value = mock_process

            result = encode_naddr("A" * 64, "my-slug")
            assert result.startswith("naddr1")

    @given(pubkey=valid_pubkeys(), slug=valid_slugs())
    def test_encode_output_is_validated(self, pubkey, slug):
        """Property: encoded output is validated before returning."""
        with patch("subprocess.Popen") as mock_popen:
            mock_process = MagicMock()
            naddr_output = "naddr1qqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqq"
            mock_process.communicate.return_value = (naddr_output, "")
            mock_process.returncode = 0
            mock_popen.return_value = mock_process

            result = encode_naddr(pubkey, slug)

            validate_naddr(result)

    @given(pubkey=valid_pubkeys(), slug=valid_slugs())
    def test_encode_output_has_correct_invariants(self, pubkey, slug):
        """Property: all encoded nadrs satisfy invariants."""
        with patch("subprocess.Popen") as mock_popen:
            mock_process = MagicMock()
            mock_process.communicate.return_value = ("naddr1qqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqq", "")
            mock_process.returncode = 0
            mock_popen.return_value = mock_process

            result = encode_naddr(pubkey, slug)

            assert result.startswith("naddr1")
            assert len(result) >= 7
            assert isinstance(result, str)
            assert not any(c.isspace() for c in result)

    def test_encode_with_special_characters_in_slug(self):
        """Encoding slug with special characters is handled by nak."""
        with patch("subprocess.Popen") as mock_popen:
            mock_process = MagicMock()
            mock_process.communicate.return_value = ("naddr1qqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqq", "")
            mock_process.returncode = 0
            mock_popen.return_value = mock_process

            result = encode_naddr("a" * 64, "my-special-slug!@#")
            assert result.startswith("naddr1")

    def test_encode_with_unicode_in_slug(self):
        """Encoding slug with unicode characters is handled by nak."""
        with patch("subprocess.Popen") as mock_popen:
            mock_process = MagicMock()
            mock_process.communicate.return_value = ("naddr1qqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqq", "")
            mock_process.returncode = 0
            mock_popen.return_value = mock_process

            result = encode_naddr("a" * 64, "my-article-ñ")
            assert result.startswith("naddr1")

    @given(pubkey=valid_pubkeys(), slug=valid_slugs())
    def test_encode_output_stripped_of_whitespace(self, pubkey, slug):
        """Property: encode_naddr strips whitespace from nak output."""
        with patch("subprocess.Popen") as mock_popen:
            mock_process = MagicMock()
            naddr_with_whitespace = "  naddr1qqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqq  \n"
            mock_process.communicate.return_value = (naddr_with_whitespace, "")
            mock_process.returncode = 0
            mock_popen.return_value = mock_process

            result = encode_naddr(pubkey, slug)

            assert result == "naddr1qqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqq"
            assert not result.startswith(" ")
            assert not result.endswith(" ")


class TestIntegrationEncodeValidate:
    """Integration tests between encode_naddr and validate_naddr."""

    @given(pubkey=valid_pubkeys(), slug=valid_slugs())
    def test_encoded_naddr_passes_validation(self, pubkey, slug):
        """Property: all encoded nadrs pass validate_naddr."""
        with patch("subprocess.Popen") as mock_popen:
            mock_process = MagicMock()
            mock_process.communicate.return_value = ("naddr1qqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqq", "")
            mock_process.returncode = 0
            mock_popen.return_value = mock_process

            naddr = encode_naddr(pubkey, slug)
            validate_naddr(naddr)

    @given(naddr=valid_nadrs())
    def test_valid_naddr_validation_idempotent(self, naddr):
        """Property: validating the same naddr multiple times succeeds."""
        validate_naddr(naddr)
        validate_naddr(naddr)
        validate_naddr(naddr)

    @given(pubkey=valid_pubkeys(), slug=valid_slugs())
    def test_encode_and_validate_round_trip(self, pubkey, slug):
        """Property: encoded naddr can be validated successfully."""
        with patch("subprocess.Popen") as mock_popen:
            mock_process = MagicMock()
            naddr_value = "naddr1qqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqq"
            mock_process.communicate.return_value = (naddr_value, "")
            mock_process.returncode = 0
            mock_popen.return_value = mock_process

            encoded = encode_naddr(pubkey, slug)
            validate_naddr(encoded)
            assert encoded == naddr_value
