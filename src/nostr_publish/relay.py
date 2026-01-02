"""Relay resolution with allowlist model.

Implements CLI relay allowlist per spec section 7.
"""

import sys
from typing import Optional
from urllib.parse import urlparse

from .errors import InvalidRelayURLError, NoRelaysError, RelayNotInAllowlistError
from .utils import deduplicate_preserving_order


def resolve_relays(
    cli_relays: list[str], frontmatter_relays: list[str], default_relays: Optional[list[str]] = None
) -> list[str]:
    """Resolve final relay list using CLI as allowlist per spec section 7.

    CONTRACT:
      Inputs:
        - cli_relays: list of relay URLs from CLI --relay arguments (allowlist + defaults)
        - frontmatter_relays: list of relay URLs from frontmatter relays field
        - default_relays: DEPRECATED, ignored (kept for API compatibility)

      Outputs:
        - relays: list of unique, valid relay URLs

      Invariants:
        - CLI relays serve as allowlist AND default set
        - At least one CLI relay required, else raise NoRelaysError
        - If frontmatter specifies relays:
          - "*" in frontmatter means use CLI defaults
          - Each non-"*" relay must be in CLI allowlist, else raise RelayNotInAllowlistError
          - Only frontmatter relays are used (subset of allowlist)
        - If frontmatter omits relays field (empty list), use CLI defaults
        - All relay URLs must use WebSocket scheme (wss:// or ws://)
        - Duplicates removed, preserving order

      Properties:
        - Deterministic: same inputs yield same output
        - Allowlist-enforced: frontmatter cannot add relays not in CLI
        - Uniqueness: no duplicate URLs in output

      Algorithm:
        1. Validate CLI relays exist (at least one required)
        2. Validate all CLI relay URLs are valid WebSocket URLs
        3. If frontmatter is empty or contains "*":
           a. Use CLI relays as defaults
        4. If frontmatter specifies relays (non-empty, no "*"):
           a. Validate each frontmatter relay is in CLI allowlist
           b. Validate each frontmatter relay URL is valid WebSocket URL
           c. Use frontmatter relays only
        5. Deduplicate preserving order
        6. Return result
    """
    # Step 1: CLI relays are mandatory
    if not cli_relays:
        raise NoRelaysError("At least one --relay is required")

    # Step 2: Validate CLI relay URLs
    for relay in cli_relays:
        if not validate_relay_url(relay):
            raise InvalidRelayURLError(f"Invalid relay URL: {relay}")

    # Build allowlist set for O(1) lookup
    allowlist = set(cli_relays)

    # Step 3 & 4: Determine which relays to use
    if not frontmatter_relays or "*" in frontmatter_relays:
        # Use CLI relays as defaults
        result_relays = cli_relays
    else:
        # Frontmatter specifies explicit relays - validate against allowlist
        for relay in frontmatter_relays:
            if not validate_relay_url(relay):
                raise InvalidRelayURLError(f"Invalid relay URL: {relay}")
            if relay not in allowlist:
                raise RelayNotInAllowlistError(f"Relay not in allowlist: {relay}")
        result_relays = frontmatter_relays

    # Step 5: Deduplicate preserving order
    result = deduplicate_preserving_order(result_relays)

    return result


def validate_relay_url(url: str) -> bool:
    """Validate that URL is a valid WebSocket relay URL.

    CONTRACT:
      Inputs:
        - url: string, potential relay URL

      Outputs:
        - valid: boolean, True if URL is valid ws:// or wss:// URL

      Invariants:
        - Returns True only if URL starts with "wss://" or "ws://"
        - Case-sensitive check

      Properties:
        - Deterministic: same URL yields same result
        - Prefix-based: only checks protocol scheme

      Algorithm:
        1. Check if url starts with "wss://" or "ws://" (case-sensitive)
        2. Return True if starts with valid scheme, False otherwise
    """
    return url.startswith("wss://") or url.startswith("ws://")


def is_localhost_relay(url: str) -> bool:
    """Check if a relay URL points to localhost.

    CONTRACT:
      Inputs:
        - url: string, relay URL

      Outputs:
        - is_local: boolean, True if relay is on localhost

      Algorithm:
        1. Parse the URL to extract hostname
        2. Check if hostname is localhost, 127.0.0.1, or [::1]
    """
    try:
        parsed = urlparse(url)
        hostname = parsed.hostname or ""
        return hostname.lower() in ("localhost", "127.0.0.1", "::1")
    except Exception:
        return False


def warn_insecure_relays(relays: list[str]) -> None:
    """Emit warning to stderr for ws:// relays that are not localhost.

    Production deployments should use wss:// for encrypted transport.
    ws:// is permitted for local development and testing.

    CONTRACT:
      Inputs:
        - relays: list of relay URLs

      Outputs:
        - None (writes warning to stderr if applicable)

      Algorithm:
        1. For each relay URL:
           a. If starts with ws:// (not wss://)
           b. And hostname is not localhost/127.0.0.1/::1
           c. Emit warning to stderr
    """
    insecure_relays = [relay for relay in relays if relay.startswith("ws://") and not is_localhost_relay(relay)]

    if insecure_relays:
        sys.stderr.write("WARNING: Using unencrypted ws:// for non-localhost relay(s):\n")
        for relay in insecure_relays:
            sys.stderr.write(f"  {relay}\n")
        sys.stderr.write("Consider using wss:// for production deployments.\n")
