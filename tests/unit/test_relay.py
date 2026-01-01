"""Property-based tests for relay selection module.

Tests validate relay allowlist model (spec section 7) and URL validation.
"""

import pytest
from hypothesis import given
from hypothesis import strategies as st

from nostr_publish.errors import InvalidRelayURLError, NoRelaysError, RelayNotInAllowlistError
from nostr_publish.relay import is_localhost_relay, resolve_relays, validate_relay_url, warn_insecure_relays
from nostr_publish.utils import deduplicate_preserving_order

# Strategy for valid WebSocket URLs (both wss:// and ws://)
valid_ws_url = st.one_of(st.just("wss://"), st.just("ws://")).flatmap(
    lambda prefix: st.builds(
        lambda host, port: f"{prefix}{host}:{port}",
        host=st.text(
            alphabet=st.characters(blacklist_categories=("Cc", "Cs"), blacklist_characters=" \n\r\t"), min_size=1
        ),
        port=st.integers(min_value=1, max_value=65535),
    )
)

# Alias for backward compatibility in tests
valid_wss_url = valid_ws_url


class TestValidateRelayUrl:
    @given(valid_ws_url)
    def test_valid_ws_urls_accepted(self, url):
        assert validate_relay_url(url) is True

    def test_valid_ws_urls_specific_examples(self):
        examples = [
            "wss://relay.example.com",
            "wss://relay.example.com:8080",
            "wss://a",
            "wss://",
            "ws://relay.example.com",
            "ws://relay.example.com:8080",
            "ws://localhost:8080",
            "ws://",
        ]
        for url in examples:
            assert validate_relay_url(url) is True

    @given(
        st.text(alphabet=st.characters(blacklist_categories=("Cc", "Cs"), blacklist_characters=" \n\r\t")).filter(
            lambda s: not s.startswith("wss://") and not s.startswith("ws://")
        )
    )
    def test_non_ws_urls_rejected(self, url):
        assert validate_relay_url(url) is False

    def test_non_ws_urls_specific_examples(self):
        examples = [
            "http://relay.example.com",
            "https://relay.example.com",
            "relay.example.com",
            "",
            "WSS://relay.example.com",
            "Wss://relay.example.com",
            "WS://relay.example.com",
            "Ws://relay.example.com",
        ]
        for url in examples:
            assert validate_relay_url(url) is False

    @given(valid_ws_url)
    def test_validation_deterministic(self, url):
        result1 = validate_relay_url(url)
        result2 = validate_relay_url(url)
        assert result1 == result2


class TestDeduplicateRelays:
    @given(st.lists(valid_wss_url))
    def test_all_elements_in_output_exist_in_input(self, relays):
        result = deduplicate_preserving_order(relays)
        for relay in result:
            assert relay in relays

    @given(st.lists(valid_wss_url))
    def test_output_contains_no_duplicates(self, relays):
        result = deduplicate_preserving_order(relays)
        assert len(result) == len(set(result))

    @given(st.lists(valid_wss_url))
    def test_output_length_at_most_input_length(self, relays):
        result = deduplicate_preserving_order(relays)
        assert len(result) <= len(relays)

    @given(st.lists(valid_wss_url))
    def test_order_preserved_for_first_occurrence(self, relays):
        if not relays:
            assert deduplicate_preserving_order(relays) == []
        else:
            result = deduplicate_preserving_order(relays)
            seen = {}
            for i, relay in enumerate(relays):
                if relay not in seen:
                    seen[relay] = i

            for result_relay in result:
                result_index = result.index(result_relay)
                input_first_index = min(i for i, r in enumerate(relays) if r == result_relay)
                assert result_index == sum(
                    1
                    for r in result[:result_index]
                    if min(i for i, x in enumerate(relays) if x == r) < input_first_index
                )

    def test_order_preserved_specific_example(self):
        relays = ["wss://a", "wss://b", "wss://a", "wss://c", "wss://b"]
        result = deduplicate_preserving_order(relays)
        assert result == ["wss://a", "wss://b", "wss://c"]

    @given(st.lists(valid_wss_url))
    def test_idempotent(self, relays):
        result1 = deduplicate_preserving_order(relays)
        result2 = deduplicate_preserving_order(result1)
        assert result1 == result2

    @given(st.lists(valid_wss_url))
    def test_empty_input_yields_empty_output(self, relays):
        if not relays:
            assert deduplicate_preserving_order(relays) == []

    def test_single_element_unchanged(self):
        relay = "wss://relay.example.com"
        assert deduplicate_preserving_order([relay]) == [relay]


class TestResolveRelays:
    """Tests for relay allowlist model per spec section 7.

    CLI relays serve as both allowlist and defaults.
    Frontmatter relays must be subset of CLI allowlist.
    """

    # --- CLI Required Tests ---

    def test_cli_relays_required(self):
        """CLI must provide at least one relay."""
        with pytest.raises(NoRelaysError, match="At least one --relay is required"):
            resolve_relays([], [], None)

    def test_cli_relays_required_even_with_frontmatter(self):
        """CLI required even if frontmatter has relays."""
        with pytest.raises(NoRelaysError, match="At least one --relay is required"):
            resolve_relays([], ["wss://fm.com"], None)

    @given(st.lists(valid_wss_url, min_size=1))
    def test_cli_relays_sufficient_alone(self, cli):
        """CLI relays alone are sufficient."""
        result = resolve_relays(cli, [], None)
        assert result == deduplicate_preserving_order(cli)

    # --- Frontmatter Empty or Wildcard: Use CLI Defaults ---

    def test_empty_frontmatter_uses_cli_defaults(self):
        """Empty frontmatter uses all CLI relays."""
        cli = ["wss://cli1.com", "wss://cli2.com"]
        result = resolve_relays(cli, [], None)
        assert result == cli

    def test_wildcard_frontmatter_uses_cli_defaults(self):
        """Frontmatter with '*' uses all CLI relays."""
        cli = ["wss://cli1.com", "wss://cli2.com"]
        result = resolve_relays(cli, ["*"], None)
        assert result == cli

    def test_wildcard_with_other_relays_uses_cli_defaults(self):
        """Frontmatter containing '*' anywhere uses CLI defaults."""
        cli = ["wss://cli1.com", "wss://cli2.com"]
        result = resolve_relays(cli, ["wss://cli1.com", "*"], None)
        assert result == cli

    # --- Frontmatter Subset of Allowlist ---

    def test_frontmatter_subset_uses_only_frontmatter(self):
        """Frontmatter relays (subset of CLI) are used exclusively."""
        cli = ["wss://cli1.com", "wss://cli2.com", "wss://cli3.com"]
        fm = ["wss://cli2.com"]
        result = resolve_relays(cli, fm, None)
        assert result == ["wss://cli2.com"]

    def test_frontmatter_full_allowlist_uses_frontmatter_order(self):
        """Frontmatter can specify all CLI relays in different order."""
        cli = ["wss://cli1.com", "wss://cli2.com"]
        fm = ["wss://cli2.com", "wss://cli1.com"]
        result = resolve_relays(cli, fm, None)
        assert result == ["wss://cli2.com", "wss://cli1.com"]

    def test_frontmatter_single_relay_from_allowlist(self):
        """Single frontmatter relay from allowlist works."""
        cli = ["wss://cli1.com", "wss://cli2.com", "wss://cli3.com"]
        fm = ["wss://cli3.com"]
        result = resolve_relays(cli, fm, None)
        assert result == ["wss://cli3.com"]

    # --- Frontmatter Not in Allowlist: Error ---

    def test_frontmatter_not_in_allowlist_raises_error(self):
        """Frontmatter relay not in CLI allowlist raises error."""
        cli = ["wss://cli1.com", "wss://cli2.com"]
        fm = ["wss://unknown.com"]
        with pytest.raises(RelayNotInAllowlistError, match="Relay not in allowlist: wss://unknown.com"):
            resolve_relays(cli, fm, None)

    def test_frontmatter_partial_not_in_allowlist_raises_error(self):
        """If any frontmatter relay not in allowlist, raises error."""
        cli = ["wss://cli1.com", "wss://cli2.com"]
        fm = ["wss://cli1.com", "wss://unknown.com"]
        with pytest.raises(RelayNotInAllowlistError, match="Relay not in allowlist: wss://unknown.com"):
            resolve_relays(cli, fm, None)

    # --- URL Validation ---

    def test_invalid_cli_relay_url_raises_error(self):
        """Invalid URL in CLI raises InvalidRelayURLError."""
        with pytest.raises(InvalidRelayURLError):
            resolve_relays(["http://invalid.com"], [], None)

    def test_invalid_frontmatter_relay_url_raises_error(self):
        """Invalid URL in frontmatter raises InvalidRelayURLError."""
        cli = ["wss://cli.com", "http://invalid.com"]
        with pytest.raises(InvalidRelayURLError):
            resolve_relays(cli, [], None)

    def test_invalid_frontmatter_url_checked_before_allowlist(self):
        """Invalid frontmatter URL raises error (URL validation happens)."""
        cli = ["wss://cli.com"]
        fm = ["http://invalid.com"]
        with pytest.raises(InvalidRelayURLError):
            resolve_relays(cli, fm, None)

    # --- Deduplication ---

    def test_cli_duplicates_removed(self):
        """Duplicate CLI relays are deduplicated."""
        cli = ["wss://cli.com", "wss://cli.com", "wss://cli2.com"]
        result = resolve_relays(cli, [], None)
        assert result == ["wss://cli.com", "wss://cli2.com"]

    def test_frontmatter_duplicates_removed(self):
        """Duplicate frontmatter relays are deduplicated."""
        cli = ["wss://cli.com", "wss://cli2.com"]
        fm = ["wss://cli.com", "wss://cli.com"]
        result = resolve_relays(cli, fm, None)
        assert result == ["wss://cli.com"]

    # --- Determinism ---

    @given(cli=st.lists(valid_wss_url, min_size=1))
    def test_result_deterministic_cli_only(self, cli):
        """Same CLI input always produces same output."""
        result1 = resolve_relays(cli, [], None)
        result2 = resolve_relays(cli, [], None)
        assert result1 == result2

    def test_result_deterministic_with_frontmatter(self):
        """Same inputs always produce same output."""
        cli = ["wss://cli1.com", "wss://cli2.com"]
        fm = ["wss://cli1.com"]
        result1 = resolve_relays(cli, fm, None)
        result2 = resolve_relays(cli, fm, None)
        assert result1 == result2

    # --- All valid results are valid URLs ---

    @given(cli=st.lists(valid_wss_url, min_size=1))
    def test_all_result_elements_are_valid_ws(self, cli):
        """All result elements are valid WebSocket URLs."""
        result = resolve_relays(cli, [], None)
        for relay in result:
            assert validate_relay_url(relay)

    # --- Deprecated default_relays parameter is ignored ---

    def test_default_relays_parameter_ignored(self):
        """The deprecated default_relays parameter is ignored."""
        cli = ["wss://cli.com"]
        result_with_defaults = resolve_relays(cli, [], ["wss://default.com"])
        result_without_defaults = resolve_relays(cli, [], None)
        assert result_with_defaults == result_without_defaults

    # --- Result is non-empty when CLI provided ---

    @given(cli=st.lists(valid_wss_url, min_size=1))
    def test_result_non_empty(self, cli):
        """Result is always non-empty when CLI relays provided."""
        result = resolve_relays(cli, [], None)
        assert len(result) > 0

    # --- Result contains no duplicates ---

    @given(cli=st.lists(valid_wss_url, min_size=1))
    def test_result_contains_no_duplicates(self, cli):
        """Result contains no duplicates."""
        result = resolve_relays(cli, [], None)
        assert len(result) == len(set(result))


class TestIsLocalhostRelay:
    """Tests for is_localhost_relay function."""

    def test_localhost_hostname(self):
        """Example-based: localhost hostname is detected."""
        assert is_localhost_relay("ws://localhost:8080") is True
        assert is_localhost_relay("wss://localhost:8080") is True
        assert is_localhost_relay("ws://localhost") is True

    def test_127_0_0_1(self):
        """Example-based: 127.0.0.1 is detected as localhost."""
        assert is_localhost_relay("ws://127.0.0.1:8080") is True
        assert is_localhost_relay("wss://127.0.0.1:8080") is True
        assert is_localhost_relay("ws://127.0.0.1") is True

    def test_ipv6_localhost(self):
        """Example-based: IPv6 localhost [::1] is detected."""
        assert is_localhost_relay("ws://[::1]:8080") is True
        assert is_localhost_relay("wss://[::1]:8080") is True

    def test_non_localhost(self):
        """Example-based: Non-localhost hostnames return False."""
        assert is_localhost_relay("ws://relay.example.com:8080") is False
        assert is_localhost_relay("wss://relay.example.com") is False
        assert is_localhost_relay("ws://192.168.1.1:8080") is False
        assert is_localhost_relay("wss://10.0.0.1:8080") is False

    def test_case_insensitive_localhost(self):
        """Example-based: localhost check is case-insensitive."""
        assert is_localhost_relay("ws://LOCALHOST:8080") is True
        assert is_localhost_relay("ws://Localhost:8080") is True
        assert is_localhost_relay("ws://LocalHost:8080") is True

    def test_invalid_url_returns_false(self):
        """Example-based: Invalid URLs return False gracefully."""
        assert is_localhost_relay("not-a-url") is False
        assert is_localhost_relay("") is False


class TestWarnInsecureRelays:
    """Tests for warn_insecure_relays function."""

    def test_no_warning_for_wss_relays(self, capsys):
        """Example-based: wss:// relays do not trigger warning."""
        warn_insecure_relays(["wss://relay.example.com", "wss://another.relay.com"])
        captured = capsys.readouterr()
        assert captured.err == ""

    def test_no_warning_for_ws_localhost(self, capsys):
        """Example-based: ws:// on localhost does not trigger warning."""
        warn_insecure_relays(["ws://localhost:8080", "ws://127.0.0.1:8080"])
        captured = capsys.readouterr()
        assert captured.err == ""

    def test_warning_for_ws_non_localhost(self, capsys):
        """Example-based: ws:// on non-localhost triggers warning."""
        warn_insecure_relays(["ws://relay.example.com:8080"])
        captured = capsys.readouterr()
        assert "WARNING" in captured.err
        assert "ws://relay.example.com:8080" in captured.err
        assert "wss://" in captured.err  # Suggests using wss://

    def test_mixed_relays_only_warns_insecure(self, capsys):
        """Example-based: Only non-localhost ws:// relays trigger warning."""
        warn_insecure_relays(["wss://secure.example.com", "ws://localhost:8080", "ws://insecure.example.com"])
        captured = capsys.readouterr()
        assert "WARNING" in captured.err
        assert "ws://insecure.example.com" in captured.err
        assert "ws://localhost:8080" not in captured.err
        assert "wss://secure.example.com" not in captured.err

    def test_empty_list_no_warning(self, capsys):
        """Example-based: Empty relay list produces no warning."""
        warn_insecure_relays([])
        captured = capsys.readouterr()
        assert captured.err == ""

    def test_multiple_insecure_relays(self, capsys):
        """Example-based: Multiple insecure relays are all listed."""
        warn_insecure_relays(["ws://relay1.example.com", "ws://relay2.example.com"])
        captured = capsys.readouterr()
        assert "ws://relay1.example.com" in captured.err
        assert "ws://relay2.example.com" in captured.err
