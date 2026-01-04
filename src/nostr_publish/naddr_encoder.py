"""NIP-19 naddr encoding via nak CLI.

Encodes Nostr address references (pubkey, kind, d-tag) into bech32 naddr format.
"""

import subprocess

from .errors import NakInvocationError


def encode_naddr(pubkey: str, slug: str) -> str:
    """Encode NIP-19 naddr for kind 30023 long-form article.

    CONTRACT:
      Inputs:
        - pubkey: hex-encoded public key (64 hex characters)
          Example: "3bf0c63fcb93463407af97a5e5ee64fa883d107ef9e558472c4eb9aaaefa459d"
        - slug: article slug from frontmatter (d-tag identifier)
          Example: "my-article-slug"

      Outputs:
        - naddr: NIP-19 bech32 encoded address string with "naddr1" prefix
          Example: "naddr1qqxnzd3cxsmnjv3hx56rjwf3qgs8eseg5zxak2hal8umuaa7laxgxjyll9uhyxp86c522shn9gj8crssrqsqqqa28a2lhkx"

      Invariants:
        - Output always starts with "naddr1" prefix
        - Output is valid bech32 encoding
        - Encoding includes pubkey, kind 30023, and slug as d-tag
        - No relay hints included (per specification preference)

      Properties:
        - Deterministic: same pubkey and slug always produce same naddr
        - Reversible: naddr can be decoded back to original components
        - Format: valid NIP-19 naddr per specification

      Algorithm:
        1. Validate inputs:
           a. pubkey must be 64 hex characters (lowercase or uppercase a-f, 0-9)
           b. slug must be non-empty string
        2. Invoke external encoder (nak CLI tool):
           a. Pass pubkey, kind 30023, and slug as identifier
           b. Request naddr encoding without relay hints
           c. Capture encoded output
        3. Normalize output:
           a. Remove leading/trailing whitespace
        4. Validate output format:
           a. Must start with "naddr1" prefix
           b. Must contain additional characters after prefix
           c. Must be valid bech32 encoding
        5. Return validated naddr string

      Error Handling:
        - Invalid pubkey format raises NakInvocationError
        - Invalid slug (empty) raises NakInvocationError
        - nak command failure raises NakInvocationError with stderr
        - Invalid output format raises NakInvocationError
        - Missing nak binary raises NakInvocationError

      Raises:
        - NakInvocationError: nak command failed, invalid inputs, or invalid output
    """
    if not pubkey or not isinstance(pubkey, str):
        raise NakInvocationError("pubkey must be non-empty string")

    if not slug or not isinstance(slug, str):
        raise NakInvocationError("slug must be non-empty string")

    if len(pubkey) != 64:
        raise NakInvocationError(f"pubkey must be 64 hex characters, got {len(pubkey)}")

    try:
        int(pubkey, 16)
    except ValueError:
        raise NakInvocationError("pubkey must be valid hexadecimal") from None

    cmd = ["nak", "encode", "naddr", "--kind", "30023", "--identifier", slug, "--pubkey", pubkey]

    try:
        process = subprocess.Popen(
            cmd, stdin=subprocess.PIPE, stdout=subprocess.PIPE, stderr=subprocess.PIPE, text=True
        )
    except FileNotFoundError:
        raise NakInvocationError("nak binary not found in system PATH") from None
    except OSError as e:
        raise NakInvocationError(f"Failed to start nak subprocess: {e}") from None

    try:
        stdout, stderr = process.communicate(timeout=30)
    except subprocess.TimeoutExpired:
        process.kill()
        process.wait()
        raise NakInvocationError("nak subprocess timed out") from None
    except Exception as e:
        process.kill()
        process.wait()
        raise NakInvocationError(f"Failed to communicate with nak subprocess: {e}") from None

    if process.returncode != 0:
        raise NakInvocationError(stderr.strip() if stderr else f"nak exited with code {process.returncode}")

    naddr = stdout.strip()

    if not naddr:
        raise NakInvocationError("nak produced empty output")

    if not naddr.startswith("naddr1"):
        raise NakInvocationError(f"naddr must start with 'naddr1', got: {naddr[:10] if len(naddr) > 10 else naddr}")

    if len(naddr) < 7:
        raise NakInvocationError(f"naddr must be at least 7 characters, got: {len(naddr)}")

    validate_naddr(naddr)

    return naddr


def validate_naddr(naddr: str) -> None:
    """Validate naddr format without full decoding.

    CONTRACT:
      Inputs:
        - naddr: string to validate as NIP-19 naddr

      Outputs:
        - None (validates in-place, raises on invalid)

      Invariants:
        - naddr must start with "naddr1" prefix
        - naddr must be at least 7 characters (prefix + minimal bech32 data)
        - naddr must not contain whitespace

      Properties:
        - Fail-fast: raises on first validation error
        - Deterministic: same input always produces same validation result

      Algorithm:
        1. Check for whitespace (leading, trailing, internal)
        2. Check minimum length (at least "naddr1X")
        3. Check prefix is exactly "naddr1"
        4. Check characters after prefix are valid bech32 (lowercase alphanumeric excluding 1, b, i, o)

      Raises:
        - NakInvocationError: naddr format is invalid
    """
    if not naddr or not isinstance(naddr, str):
        raise NakInvocationError("naddr must be non-empty string")

    if any(c.isspace() for c in naddr):
        raise NakInvocationError("naddr must not contain whitespace")

    if len(naddr) < 7:
        raise NakInvocationError(f"naddr must be at least 7 characters, got {len(naddr)}")

    if not naddr.startswith("naddr1"):
        raise NakInvocationError(f"naddr must start with 'naddr1', got: {naddr[:10]}")

    bech32_chars = set("023456789acdefghjklmnpqrstuvwxyz")
    data_part = naddr[6:]

    for char in data_part:
        if char not in bech32_chars:
            raise NakInvocationError(f"naddr contains invalid bech32 character: '{char}'")
