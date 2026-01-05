"""Property-based and unit tests for CLI module.

Tests cover argument parsing, file reading, and workflow orchestration
with determinism, error handling, and fail-fast semantics.
"""

import json
import tempfile
from pathlib import Path
from unittest.mock import Mock, patch

import pytest
from hypothesis import given
from hypothesis import strategies as st
from hypothesis.strategies import composite

from nostr_publish.cli import main, parse_arguments, read_markdown_file
from nostr_publish.errors import FrontmatterParseError

# === Strategies ===


@composite
def valid_relay_urls(draw):
    """Generate valid wss:// relay URLs."""
    domain = draw(
        st.text(
            alphabet=st.characters(blacklist_categories=("Cc", "Cs"), blacklist_characters=" \t\n\r"),
            min_size=1,
            max_size=50,
        )
    )
    path = draw(
        st.text(
            alphabet=st.characters(blacklist_categories=("Cc", "Cs"), blacklist_characters=" \t\n\r/"),
            min_size=0,
            max_size=30,
        )
    )
    if path:
        return f"wss://{domain}/{path}"
    return f"wss://{domain}"


@composite
def valid_file_paths(draw):
    """Generate valid file path strings (alphanumeric + .md)."""
    return draw(st.from_regex(r"[a-z0-9_]{1,20}\.md", fullmatch=True))


@composite
def positive_integers(draw, min_val=1, max_val=3600):
    """Generate positive integers."""
    return draw(st.integers(min_value=min_val, max_value=max_val))


@composite
def valid_bunker_uris(draw):
    """Generate valid NIP-46 bunker URIs.

    Format: bunker://pubkey@relay?secret=...
    Bunker URIs must start with 'bunker://' per NIP-46 spec.
    """
    # Generate a hex pubkey (64 chars)
    pubkey = draw(st.text(alphabet="0123456789abcdef", min_size=64, max_size=64))
    # Generate a relay domain
    domain = draw(
        st.text(
            alphabet=st.characters(whitelist_categories=("Ll", "Lu", "Nd"), whitelist_characters=".-"),
            min_size=3,
            max_size=30,
        ).filter(lambda x: x and x[0].isalnum() and x[-1].isalnum())
    )
    # Optional secret
    has_secret = draw(st.booleans())
    if has_secret:
        secret = draw(st.text(alphabet="0123456789abcdef", min_size=16, max_size=32))
        return f"bunker://{pubkey}@wss://{domain}?secret={secret}"
    return f"bunker://{pubkey}@wss://{domain}"


@composite
def text_without_line_ending_normalization(draw):
    """Generate text that won't have line endings normalized by Python text mode.

    Python's text mode on Windows/Mac converts \\r to \\n, so we exclude bare \\r.
    """
    return draw(
        st.text(
            alphabet=st.characters(blacklist_categories=("Cc", "Cs"), blacklist_characters="\r"),
            min_size=0,
            max_size=1000,
        )
    )


# === Property-Based Tests: parse_arguments ===


class TestParseArgumentsDeterminism:
    """Same arguments always produce same parsed result."""

    @given(file=valid_file_paths(), bunker=st.one_of(st.none(), valid_bunker_uris()), timeout=positive_integers())
    def test_deterministic_output(self, file, bunker, timeout):
        """Parsing same argv twice produces identical results."""
        argv = [file, "--relay", "wss://relay.example.com", "--timeout", str(timeout)]
        if bunker:
            argv.extend(["--bunker", bunker])

        result1 = parse_arguments(argv)
        result2 = parse_arguments(argv)

        assert result1 == result2

    @given(
        file=valid_file_paths(),
        bunker=st.one_of(st.none(), valid_bunker_uris()),
        relays=st.lists(valid_relay_urls(), min_size=1, max_size=5),
        timeout=positive_integers(),
    )
    def test_idempotent_parsing(self, file, bunker, relays, timeout):
        """Parsing arguments produces idempotent results."""
        argv = [file, "--timeout", str(timeout)]
        if bunker:
            argv.extend(["--bunker", bunker])
        for relay in relays:
            argv.extend(["--relay", relay])

        result = parse_arguments(argv)

        # Parse again to verify no side effects
        result2 = parse_arguments(argv)
        assert result == result2


class TestParseArgumentsFileHandling:
    """File argument parsing with various input formats."""

    @given(file=valid_file_paths())
    def test_file_argument_required(self, file):
        """File path is required and captured as Path object."""
        argv = [file, "--relay", "wss://relay.example.com"]
        result = parse_arguments(argv)

        assert "file" in result
        assert isinstance(result["file"], Path)
        assert str(result["file"]) == file

    def test_missing_file_argument_fails(self):
        """Missing file argument causes parse error."""
        argv = ["--relay", "wss://relay.example.com"]
        with pytest.raises(SystemExit):
            parse_arguments(argv)

    @given(file=valid_file_paths(), relays=st.lists(valid_relay_urls(), min_size=1, max_size=5))
    def test_relay_flags_accumulate(self, file, relays):
        """Multiple --relay flags accumulate into list."""
        argv = [file]
        for relay in relays:
            argv.extend(["--relay", relay])

        result = parse_arguments(argv)

        assert result["relays"] == relays
        assert len(result["relays"]) == len(relays)

    def test_relay_flags_required(self):
        """At least one --relay flag is required."""
        argv = ["test.md"]
        with pytest.raises(SystemExit):
            parse_arguments(argv)


class TestParseArgumentsBunkerHandling:
    """--bunker argument parsing and defaults."""

    def test_bunker_optional_defaults_none(self):
        """Without --bunker, bunker_uri is None."""
        argv = ["test.md", "--relay", "wss://relay.example.com"]
        result = parse_arguments(argv)

        assert result["bunker_uri"] is None

    @given(bunker_uri=st.from_regex(r"[a-zA-Z0-9:/_\.]{1,100}", fullmatch=True))
    def test_bunker_captured_when_provided(self, bunker_uri):
        """--bunker argument is captured as string."""
        argv = ["test.md", "--relay", "wss://relay.example.com", "--bunker", bunker_uri]
        result = parse_arguments(argv)

        assert result["bunker_uri"] == bunker_uri

    @given(bunker_uri=st.from_regex(r"[a-zA-Z0-9:/_\.]{1,100}", fullmatch=True))
    def test_bunker_long_form(self, bunker_uri):
        """--bunker long form works correctly."""
        argv = ["test.md", "--relay", "wss://relay.example.com", "--bunker", bunker_uri]
        result = parse_arguments(argv)

        assert result["bunker_uri"] == bunker_uri


class TestParseArgumentsDryRun:
    """--dry-run flag handling."""

    def test_dry_run_defaults_false(self):
        """Without --dry-run flag, dry_run is False."""
        argv = ["test.md", "--relay", "wss://relay.example.com"]
        result = parse_arguments(argv)

        assert result["dry_run"] is False
        assert isinstance(result["dry_run"], bool)

    def test_dry_run_flag_sets_true(self):
        """--dry-run flag sets dry_run to True."""
        argv = ["test.md", "--relay", "wss://relay.example.com", "--dry-run"]
        result = parse_arguments(argv)

        assert result["dry_run"] is True

    @given(file=valid_file_paths())
    def test_dry_run_with_other_options(self, file):
        """--dry-run works with other options."""
        argv = [
            file,
            "--relay",
            "wss://relay.example.com",
            "--dry-run",
            "--bunker",
            "nip46://example",
            "--timeout",
            "60",
        ]
        result = parse_arguments(argv)

        assert result["dry_run"] is True
        assert result["bunker_uri"] == "nip46://example"
        assert result["timeout"] == 60


class TestParseArgumentsTimeout:
    """--timeout argument validation and defaults."""

    def test_timeout_defaults_30(self):
        """Without --timeout, timeout defaults to 30."""
        argv = ["test.md", "--relay", "wss://relay.example.com"]
        result = parse_arguments(argv)

        assert result["timeout"] == 30
        assert isinstance(result["timeout"], int)

    @given(timeout=positive_integers())
    def test_timeout_positive_integer_accepted(self, timeout):
        """Positive integer timeout values are accepted."""
        argv = ["test.md", "--relay", "wss://relay.example.com", "--timeout", str(timeout)]
        result = parse_arguments(argv)

        assert result["timeout"] == timeout
        assert result["timeout"] > 0

    def test_timeout_zero_rejected(self):
        """Timeout of 0 is rejected."""
        argv = ["test.md", "--relay", "wss://relay.example.com", "--timeout", "0"]
        with pytest.raises(SystemExit):
            parse_arguments(argv)

    def test_timeout_negative_rejected(self):
        """Negative timeout is rejected."""
        argv = ["test.md", "--relay", "wss://relay.example.com", "--timeout", "-5"]
        with pytest.raises(SystemExit):
            parse_arguments(argv)

    def test_timeout_non_integer_rejected(self):
        """Non-integer timeout is rejected."""
        argv = ["test.md", "--relay", "wss://relay.example.com", "--timeout", "not_a_number"]
        with pytest.raises(SystemExit):
            parse_arguments(argv)


class TestParseArgumentsComprehensive:
    """Full argument set parsing with various combinations."""

    @given(
        file=valid_file_paths(),
        bunker=st.one_of(st.none(), valid_bunker_uris()),
        relays=st.lists(valid_relay_urls(), min_size=1, max_size=3),
        dry_run=st.booleans(),
        timeout=positive_integers(),
    )
    def test_all_arguments_together(self, file, bunker, relays, dry_run, timeout):
        """All arguments can be combined with valid NIP-46 bunker URIs."""
        argv = [file, "--timeout", str(timeout)]
        if bunker:
            argv.extend(["--bunker", bunker])
        for relay in relays:
            argv.extend(["--relay", relay])
        if dry_run:
            argv.append("--dry-run")

        result = parse_arguments(argv)

        assert result["file"] == Path(file)
        assert result["bunker_uri"] == bunker
        assert result["relays"] == relays
        assert result["dry_run"] == dry_run
        assert result["timeout"] == timeout

    def test_result_keys_present(self):
        """Parsed result always has required keys."""
        argv = ["test.md", "--relay", "wss://relay.example.com"]
        result = parse_arguments(argv)

        required_keys = {
            "file",
            "bunker_uri",
            "relays",
            "dry_run",
            "timeout",
            "blossom_url",
            "blossom_timeout",
            "cover_size",
            "allow_dry_run_without_upload",
            "extra_tags",
        }
        assert set(result.keys()) == required_keys


# === Property-Based Tests: read_markdown_file ===


class TestReadMarkdownFileDeterminism:
    """File reading is deterministic."""

    @given(content=text_without_line_ending_normalization())
    def test_deterministic_file_reading(self, content):
        """Reading same file twice yields identical content."""
        with tempfile.NamedTemporaryFile(mode="w", suffix=".md", delete=False, encoding="utf-8", newline="") as f:
            f.write(content)
            temp_path = Path(f.name)

        try:
            result1 = read_markdown_file(temp_path)
            result2 = read_markdown_file(temp_path)

            assert result1 == result2
            assert result1 == content
        finally:
            temp_path.unlink()

    @given(content=text_without_line_ending_normalization())
    def test_file_content_preserved_exactly(self, content):
        """File content is read exactly without modification."""
        with tempfile.NamedTemporaryFile(mode="w", suffix=".md", delete=False, encoding="utf-8", newline="") as f:
            f.write(content)
            temp_path = Path(f.name)

        try:
            result = read_markdown_file(temp_path)
            assert result == content
        finally:
            temp_path.unlink()


class TestReadMarkdownFileErrorHandling:
    """File reading error cases."""

    def test_nonexistent_file_raises_filenotfound(self):
        """Reading nonexistent file raises FileNotFoundError."""
        path = Path("/nonexistent/file/path/test.md")
        with pytest.raises(FileNotFoundError):
            read_markdown_file(path)

    def test_filenotfound_error_mentions_path(self):
        """FileNotFoundError message includes file path."""
        path = Path("/nonexistent/file/path/test.md")
        try:
            read_markdown_file(path)
        except FileNotFoundError as e:
            assert str(path) in str(e)

    @given(content=text_without_line_ending_normalization())
    def test_utf8_file_reading(self, content):
        """UTF-8 encoded files are read correctly."""
        with tempfile.NamedTemporaryFile(mode="w", suffix=".md", delete=False, encoding="utf-8", newline="") as f:
            f.write(content)
            temp_path = Path(f.name)

        try:
            result = read_markdown_file(temp_path)
            assert result == content
        finally:
            temp_path.unlink()


# === Property-Based Tests: main function ===


class TestMainReturnCodes:
    """main() returns correct exit codes."""

    @patch("nostr_publish.cli.parse_arguments")
    @patch("nostr_publish.cli.read_markdown_file")
    @patch("nostr_publish.cli.parse_frontmatter")
    @patch("nostr_publish.cli.validate_frontmatter_dict")
    @patch("nostr_publish.cli.dict_to_frontmatter")
    @patch("nostr_publish.cli.validate_frontmatter")
    @patch("nostr_publish.cli.construct_event")
    @patch("nostr_publish.cli.resolve_relays")
    def test_success_returns_zero(
        self,
        mock_resolve_relays,
        mock_construct_event,
        mock_validate_fm,
        mock_dict_to_fm,
        mock_validate_dict,
        mock_parse_fm,
        mock_read_file,
        mock_parse_args,
    ):
        """Successful publish returns exit code 0."""
        mock_parse_args.return_value = {
            "file": Path("test.md"),
            "bunker_uri": "nip46://example",
            "relays": ["wss://relay.example"],
            "dry_run": False,
            "timeout": 30,
            "blossom_url": None,
            "blossom_timeout": 30,
            "cover_size": "1200x630",
            "extra_tags": [],
        }
        mock_read_file.return_value = "---\ntitle: test\nslug: test\n---\nBody"
        mock_parse_fm.return_value = ({"title": "test", "slug": "test"}, "Body")

        fm_obj = Mock()
        fm_obj.relays = []
        fm_obj.image = None  # No cover image
        mock_dict_to_fm.return_value = fm_obj
        mock_validate_fm.return_value = fm_obj

        event_obj = Mock()
        event_obj.to_dict.return_value = {}
        mock_construct_event.return_value = event_obj

        mock_resolve_relays.return_value = ["wss://relay.example"]

        fm_obj.slug = "test"  # Add slug for naddr encoding

        with patch("nostr_publish.cli.invoke_nak") as mock_nak:
            result_obj = Mock()
            result_obj.event_id = "123"
            result_obj.pubkey = "0" * 64  # Valid 64-char hex pubkey for naddr encoding
            result_obj.naddr = None  # Will be set by encode_naddr
            mock_nak.return_value = result_obj
            with patch("builtins.print"):
                result = main([])

        assert result == 0

    def test_dry_run_returns_zero(self):
        """Dry-run mode returns exit code 0."""
        with tempfile.NamedTemporaryFile(mode="w", suffix=".md", delete=False, encoding="utf-8") as f:
            f.write("---\ntitle: Test\nslug: test-slug\n---\n# Body\n")
            temp_path = f.name

        try:
            with patch("nostr_publish.cli.parse_frontmatter") as mock_parse:
                with patch("nostr_publish.cli.validate_frontmatter_dict"):
                    with patch("nostr_publish.cli.dict_to_frontmatter") as mock_dict_to_fm:
                        with patch("nostr_publish.cli.validate_frontmatter") as mock_validate:
                            with patch("nostr_publish.cli.construct_event") as mock_construct:
                                with patch("nostr_publish.cli.resolve_relays") as mock_resolve:
                                    mock_parse.return_value = ({"title": "Test", "slug": "test-slug"}, "# Body\n")
                                    mock_fm = Mock(relays=[], image=None)  # No cover image
                                    mock_dict_to_fm.return_value = mock_fm
                                    mock_validate.return_value = mock_fm
                                    mock_construct.return_value = Mock(to_dict=Mock(return_value={}))
                                    mock_resolve.return_value = ["wss://relay.example"]

                                    with patch("builtins.print"):
                                        result = main([temp_path, "--dry-run", "--relay", "wss://relay.example"])

            assert result == 0
        finally:
            Path(temp_path).unlink()

    @patch("nostr_publish.cli.parse_arguments")
    @patch("nostr_publish.cli.read_markdown_file")
    def test_file_not_found_returns_nonzero(self, mock_read, mock_parse_args):
        """FileNotFoundError returns non-zero exit code."""
        mock_parse_args.return_value = {
            "file": Path("test.md"),
            "bunker_uri": None,
            "relays": [],
            "dry_run": False,
            "timeout": 30,
        }
        mock_read.side_effect = FileNotFoundError("File not found")

        with patch("sys.stderr.write"):
            result = main([])

        assert result != 0

    @patch("nostr_publish.cli.parse_arguments")
    @patch("nostr_publish.cli.read_markdown_file")
    @patch("nostr_publish.cli.parse_frontmatter")
    def test_frontmatter_error_returns_nonzero(self, mock_parse_fm, mock_read, mock_parse_args):
        """FrontmatterParseError returns non-zero exit code."""
        mock_parse_args.return_value = {
            "file": Path("test.md"),
            "bunker_uri": None,
            "relays": [],
            "dry_run": False,
            "timeout": 30,
        }
        mock_read.return_value = "invalid"
        mock_parse_fm.side_effect = FrontmatterParseError("Invalid frontmatter")

        with patch("sys.stderr.write"):
            result = main([])

        assert result != 0


class TestMainErrorMessages:
    """main() produces deterministic, human-readable error messages."""

    @patch("nostr_publish.cli.parse_arguments")
    @patch("nostr_publish.cli.read_markdown_file")
    def test_file_not_found_error_message(self, mock_read, mock_parse_args):
        """FileNotFoundError produces appropriate error message."""
        mock_parse_args.return_value = {
            "file": Path("test.md"),
            "bunker_uri": None,
            "relays": [],
            "dry_run": False,
            "timeout": 30,
        }
        mock_read.side_effect = FileNotFoundError("File not found: test.md")

        stderr_messages = []
        with patch("sys.stderr.write", side_effect=lambda x: stderr_messages.append(x)):
            main([])

        assert any("FileNotFoundError" in msg for msg in stderr_messages)
        assert any("ERROR:" in msg for msg in stderr_messages)

    @patch("nostr_publish.cli.parse_arguments")
    @patch("nostr_publish.cli.read_markdown_file")
    @patch("nostr_publish.cli.parse_frontmatter")
    @patch("nostr_publish.cli.validate_frontmatter_dict")
    def test_validation_error_message(self, mock_validate, mock_parse_fm, mock_read, mock_parse_args):
        """Validation error produces appropriate error message."""
        from nostr_publish.errors import MissingFieldError

        mock_parse_args.return_value = {
            "file": Path("test.md"),
            "bunker_uri": None,
            "relays": [],
            "dry_run": False,
            "timeout": 30,
        }
        mock_read.return_value = "---\nslug: test\n---\nBody"
        mock_parse_fm.return_value = ({"slug": "test"}, "Body")
        mock_validate.side_effect = MissingFieldError("Missing required field: title")

        stderr_messages = []
        with patch("sys.stderr.write", side_effect=lambda x: stderr_messages.append(x)):
            main([])

        assert any("MissingFieldError" in msg for msg in stderr_messages)


class TestMainNoBunkerPublishFails:
    """Publishing without --bunker fails appropriately."""

    def test_publish_without_bunker_fails(self):
        """Publishing (non-dry-run) without --bunker returns error."""
        with tempfile.NamedTemporaryFile(mode="w", suffix=".md", delete=False, encoding="utf-8") as f:
            f.write("---\ntitle: Test\nslug: test-slug\n---\n# Body\n")
            temp_path = f.name

        try:
            with patch("nostr_publish.cli.parse_frontmatter") as mock_parse:
                with patch("nostr_publish.cli.validate_frontmatter_dict"):
                    with patch("nostr_publish.cli.dict_to_frontmatter") as mock_dict_to_fm:
                        with patch("nostr_publish.cli.validate_frontmatter") as mock_validate:
                            with patch("nostr_publish.cli.construct_event") as mock_construct:
                                with patch("nostr_publish.cli.resolve_relays") as mock_resolve:
                                    mock_parse.return_value = ({"title": "Test", "slug": "test-slug"}, "# Body\n")
                                    mock_fm = Mock(relays=["wss://relay.example"], image=None)  # No cover image
                                    mock_dict_to_fm.return_value = mock_fm
                                    mock_validate.return_value = mock_fm
                                    mock_construct.return_value = Mock(to_dict=Mock(return_value={}))
                                    mock_resolve.return_value = ["wss://relay.example"]

                                    stderr_messages = []
                                    with patch("sys.stderr.write", side_effect=lambda x: stderr_messages.append(x)):
                                        result = main([temp_path, "--relay", "wss://relay.example"])

            assert result != 0
            assert any("bunker" in msg.lower() for msg in stderr_messages)
        finally:
            Path(temp_path).unlink()


class TestMainNoFrontmatterFails:
    """Publishing without frontmatter fails appropriately."""

    def test_no_frontmatter_fails(self):
        """Publishing without frontmatter returns error."""
        with tempfile.NamedTemporaryFile(mode="w", suffix=".md", delete=False, encoding="utf-8") as f:
            f.write("# No frontmatter, just body\n")
            temp_path = f.name

        try:
            with patch("nostr_publish.cli.parse_frontmatter") as mock_parse:
                mock_parse.return_value = (None, "# No frontmatter, just body\n")

                stderr_messages = []
                with patch("sys.stderr.write", side_effect=lambda x: stderr_messages.append(x)):
                    result = main([temp_path, "--relay", "wss://relay.example.com"])

            assert result != 0
            assert any("frontmatter" in msg.lower() or "FrontmatterParseError" in msg for msg in stderr_messages)
        finally:
            Path(temp_path).unlink()


# === Edge Cases ===


class TestArgumentParsingEdgeCases:
    """Edge cases in argument parsing."""

    def test_empty_argv_fails(self):
        """Empty argv (no file) fails."""
        with pytest.raises(SystemExit):
            parse_arguments([])

    def test_argument_order_flexible(self):
        """Arguments can be in different orders."""
        argv1 = ["test.md", "--timeout", "60", "--bunker", "uri", "--relay", "wss://r"]
        argv2 = ["test.md", "--relay", "wss://r", "--bunker", "uri", "--timeout", "60"]

        result1 = parse_arguments(argv1)
        result2 = parse_arguments(argv2)

        assert result1["timeout"] == result2["timeout"] == 60
        assert result1["bunker_uri"] == result2["bunker_uri"] == "uri"
        assert result1["relays"] == result2["relays"] == ["wss://r"]

    def test_file_with_spaces(self):
        """File paths with spaces are handled."""
        argv = ["my test file.md", "--relay", "wss://relay.example.com"]
        result = parse_arguments(argv)

        assert result["file"] == Path("my test file.md")


class TestReadMarkdownFileEdgeCases:
    """Edge cases in file reading."""

    def test_empty_file(self):
        """Empty files are read correctly."""
        with tempfile.NamedTemporaryFile(mode="w", suffix=".md", delete=False, encoding="utf-8") as f:
            f.write("")
            temp_path = Path(f.name)

        try:
            result = read_markdown_file(temp_path)
            assert result == ""
        finally:
            temp_path.unlink()

    def test_large_file(self):
        """Large files are read completely."""
        content = "x" * 100000
        with tempfile.NamedTemporaryFile(mode="w", suffix=".md", delete=False, encoding="utf-8") as f:
            f.write(content)
            temp_path = Path(f.name)

        try:
            result = read_markdown_file(temp_path)
            assert result == content
            assert len(result) == 100000
        finally:
            temp_path.unlink()

    def test_special_characters_preserved(self):
        """Special characters in files are preserved."""
        content = "Special chars: !@#$%^&*()_+-=[]{}|;':\",./<>?\n\nUnicode: café ñ 中文 🎉"
        with tempfile.NamedTemporaryFile(mode="w", suffix=".md", delete=False, encoding="utf-8") as f:
            f.write(content)
            temp_path = Path(f.name)

        try:
            result = read_markdown_file(temp_path)
            assert result == content
        finally:
            temp_path.unlink()


class TestCoverMetadataOutput:
    """Test CLI cover metadata output to stdout."""

    def test_cli_outputs_cover_when_metadata_present(self):
        """CLI outputs Cover: line when cover_metadata returned from orchestrate_cover_upload."""
        with tempfile.NamedTemporaryFile(mode="w", suffix=".md", delete=False, encoding="utf-8") as f:
            f.write("---\ntitle: Test\nslug: test-slug\nimage:\n  file: /tmp/test.jpg\n---\n# Body\n")
            temp_path = f.name

        try:
            sample_metadata = {
                "hash": "a" * 64,
                "url": "https://blossom.example.com/abc123.jpg",
                "dim": "1200x630",
                "mime": "image/jpeg",
            }

            with patch("nostr_publish.validator.validate_image_metadata") as mock_validate_img:
                # Make validate_image_metadata pass through the ImageMetadata unchanged
                mock_validate_img.side_effect = lambda img, *args, **kwargs: img

                with patch("nostr_publish.cli.validate_cover_arguments"):
                    with patch("nostr_publish.cli.orchestrate_cover_upload") as mock_upload:
                        with patch("nostr_publish.cli.invoke_nak") as mock_nak:
                            mock_upload.return_value = sample_metadata
                            result_obj = Mock()
                            result_obj.event_id = "event123"
                            result_obj.pubkey = "0" * 64  # Valid 64-char hex pubkey
                            result_obj.naddr = None  # Will be set by encode_naddr
                            mock_nak.return_value = result_obj

                            from io import StringIO

                            captured_output = StringIO()
                            with patch("sys.stdout", captured_output):
                                result = main(
                                    [
                                        temp_path,
                                        "--relay",
                                        "wss://relay.example.com",
                                        "--bunker",
                                        "bunker://test@wss://r.example.com",
                                        "--blossom",
                                        "https://blossom.example.com",
                                    ]
                                )

            assert result == 0
            output = captured_output.getvalue().strip()

            # New JSON format - parse and verify
            output_json = json.loads(output)

            # Verify required fields
            assert "event_id" in output_json
            assert "pubkey" in output_json
            assert output_json["event_id"] == "event123"
            assert output_json["pubkey"] == "0" * 64

            # Verify cover metadata included
            assert "image" in output_json
            cover_json = output_json["image"]
            assert set(cover_json.keys()) == {"hash", "url", "dim", "mime"}
            assert cover_json["hash"] == sample_metadata["hash"]
            assert cover_json["url"] == sample_metadata["url"]
            assert cover_json["dim"] == sample_metadata["dim"]
            assert cover_json["mime"] == sample_metadata["mime"]

            # Note: naddr may or may not be present (encoding can fail non-fatally)
            # Just verify output is valid JSON with required fields
        finally:
            Path(temp_path).unlink()

    def test_cli_omits_cover_when_no_metadata(self):
        """CLI does NOT output cover field in JSON when no cover upload occurred."""
        with tempfile.NamedTemporaryFile(mode="w", suffix=".md", delete=False, encoding="utf-8") as f:
            f.write("---\ntitle: Test\nslug: test-slug\n---\n# Body\n")
            temp_path = f.name

        try:
            with patch("nostr_publish.cli.invoke_nak") as mock_nak:
                result_obj = Mock()
                result_obj.event_id = "event123"
                result_obj.pubkey = "0" * 64  # Valid 64-char hex pubkey
                result_obj.naddr = None  # Will be set by encode_naddr
                mock_nak.return_value = result_obj

                from io import StringIO

                captured_output = StringIO()
                with patch("sys.stdout", captured_output):
                    result = main(
                        [
                            temp_path,
                            "--relay",
                            "wss://relay.example.com",
                            "--bunker",
                            "bunker://test@wss://r.example.com",
                        ]
                    )

            assert result == 0
            output = captured_output.getvalue().strip()

            # New JSON format - parse and verify
            output_json = json.loads(output)

            # Verify NO cover field in JSON
            assert "image" not in output_json

            # Verify required fields present
            assert "event_id" in output_json
            assert "pubkey" in output_json
            assert output_json["event_id"] == "event123"
            assert output_json["pubkey"] == "0" * 64

            # Note: naddr may or may not be present (encoding can fail non-fatally)
        finally:
            Path(temp_path).unlink()

    @given(
        hash_val=st.text(alphabet="0123456789abcdef", min_size=64, max_size=64),
        url=st.from_regex(r"https://[a-z0-9.-]+/[a-z0-9]+\.(jpg|png)", fullmatch=True),
        dim=st.from_regex(r"[1-9][0-9]{2,3}x[1-9][0-9]{2,3}", fullmatch=True),
    )
    def test_cover_output_deterministic(self, hash_val: str, url: str, dim: str):
        """Cover output is deterministic for same metadata."""
        with tempfile.NamedTemporaryFile(mode="w", suffix=".md", delete=False, encoding="utf-8") as f:
            f.write("---\ntitle: Test\nslug: test-slug\nimage:\n  file: /tmp/test.jpg\n---\n# Body\n")
            temp_path = f.name

        try:
            metadata = {"hash": hash_val, "url": url, "dim": dim, "mime": "image/jpeg"}

            outputs = []
            for _ in range(2):
                with patch("nostr_publish.validator.validate_image_metadata") as mock_validate_img:
                    mock_validate_img.side_effect = lambda img, *args, **kwargs: img

                    with patch("nostr_publish.cli.validate_cover_arguments"):
                        with patch("nostr_publish.cli.orchestrate_cover_upload") as mock_upload:
                            with patch("nostr_publish.cli.invoke_nak") as mock_nak:
                                mock_upload.return_value = metadata
                                result_obj = Mock()
                                result_obj.event_id = "event123"
                                result_obj.pubkey = "0" * 64  # Valid 64-char hex pubkey
                                result_obj.naddr = None  # Will be set by encode_naddr
                                mock_nak.return_value = result_obj

                                from io import StringIO

                                captured_output = StringIO()
                                with patch("sys.stdout", captured_output):
                                    main(
                                        [
                                            temp_path,
                                            "--relay",
                                            "wss://relay.example.com",
                                            "--bunker",
                                            "bunker://test@wss://r.example.com",
                                            "--blossom",
                                            "https://blossom.example.com",
                                        ]
                                    )

                # New JSON format - capture entire output
                output = captured_output.getvalue().strip()
                outputs.append(output)

            # Verify deterministic output
            assert outputs[0] == outputs[1]

            # Verify JSON is stable and contains cover
            parsed = json.loads(outputs[0])
            assert "image" in parsed
            assert parsed["image"]["hash"] == hash_val
            assert parsed["image"]["url"] == url
            assert parsed["image"]["dim"] == dim
        finally:
            Path(temp_path).unlink()


class TestRelayResolution:
    """Test relay resolution and allowlist logic."""

    def test_frontmatter_subset_of_cli_relays(self):
        """Frontmatter relays must be subset of CLI allowlist."""
        # Frontmatter specifies relay that IS in CLI allowlist
        content = """---
title: Test Article
slug: test-slug
relays:
  - wss://frontmatter.example.com
---
Body content"""
        with tempfile.NamedTemporaryFile(mode="w", suffix=".md", delete=False, encoding="utf-8") as f:
            f.write(content)
            temp_path = Path(f.name)

        try:
            captured_relays = []

            def capture_relays(event, bunker_uri, relays, timeout):
                captured_relays.append(relays)
                # Return proper result object with naddr attribute
                result_obj = type(
                    "NakResult",
                    (),
                    {
                        "event_id": "abc123",
                        "pubkey": "0" * 64,  # Valid 64-char hex pubkey
                        "naddr": None,  # Will be set by encode_naddr
                    },
                )()
                return result_obj

            with patch("nostr_publish.cli.invoke_nak", side_effect=capture_relays):
                # CLI must include frontmatter relay in allowlist
                result = main(
                    [
                        str(temp_path),
                        "--relay",
                        "wss://frontmatter.example.com",
                        "--relay",
                        "wss://other.example.com",
                        "--bunker",
                        "bunker://test?relay=wss://r.example.com",
                    ]
                )
                assert result == 0
                # Only frontmatter relay should be used (subset of allowlist)
                assert captured_relays[0] == ["wss://frontmatter.example.com"]
        finally:
            temp_path.unlink()

    def test_frontmatter_not_in_allowlist_fails(self):
        """Frontmatter relay not in CLI allowlist fails."""
        content = """---
title: Test Article
slug: test-slug
relays:
  - wss://not-in-allowlist.example.com
---
Body content"""
        with tempfile.NamedTemporaryFile(mode="w", suffix=".md", delete=False, encoding="utf-8") as f:
            f.write(content)
            temp_path = Path(f.name)

        try:
            # CLI relay is different from frontmatter relay
            result = main(
                [
                    str(temp_path),
                    "--relay",
                    "wss://allowed.example.com",
                    "--bunker",
                    "bunker://test?relay=wss://r.example.com",
                ]
            )
            assert result != 0  # Should fail due to relay not in allowlist
        finally:
            temp_path.unlink()

    def test_no_cli_relays_fails(self):
        """Publishing fails when no CLI relays provided (now mandatory)."""
        content = """---
title: Test Article
slug: test-slug
---
Body content"""
        with tempfile.NamedTemporaryFile(mode="w", suffix=".md", delete=False, encoding="utf-8") as f:
            f.write(content)
            temp_path = Path(f.name)

        try:
            # No --relay argument - should fail with SystemExit
            with pytest.raises(SystemExit):
                main([str(temp_path), "--bunker", "bunker://test?relay=wss://r.example.com"])
        finally:
            temp_path.unlink()
