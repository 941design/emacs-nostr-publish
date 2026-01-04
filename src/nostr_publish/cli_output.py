"""CLI output formatting for structured JSON results.

Formats publish results as JSON for Emacs integration and machine parsing.
Breaking change from previous text-based output format.
"""

import json
from typing import Optional

from .models import PublishResult


def format_publish_result(result: PublishResult, cover_metadata: Optional[dict] = None) -> str:
    """Format publish result as single JSON object.

    CONTRACT:
      Inputs:
        - result: PublishResult containing event_id, pubkey, and optional naddr
          Example: PublishResult(event_id="abc123...", pubkey="def456...", naddr="naddr1...")
        - cover_metadata: optional dictionary with keys (hash, url, dim, mime)
          Example: {"hash": "sha256...", "url": "https://...", "dim": "1200x630", "mime": "image/jpeg"}

      Outputs:
        - json_string: single-line JSON string with all result data
          Example: '{"event_id": "abc123...", "pubkey": "def456...", "naddr": "naddr1...", "image": {...}}'

      Invariants:
        - Output is valid JSON (parseable by json.loads)
        - event_id field always present
        - pubkey field always present
        - naddr field present if result.naddr is not None
        - image field present if cover_metadata is not None
        - No trailing newline (caller adds if needed)

      Properties:
        - Deterministic: same inputs produce same JSON output
        - Complete: all non-None fields from result and cover_metadata included
        - Parseable: output is valid JSON that can be parsed by JSON libraries
        - Single-line: no embedded newlines in output string

      Algorithm:
        1. Create output dictionary:
           a. Add event_id from result
           b. Add pubkey from result
        2. Conditionally add naddr:
           a. If result.naddr is not None and non-empty string, add to dictionary
        3. Conditionally add image:
           a. If cover_metadata is not None, add entire nested dictionary to output
        4. Serialize to JSON:
           a. Convert dictionary to JSON string (compact format, no indentation)
           b. Preserve UTF-8 characters (do not escape to ASCII)
           c. Sort dictionary keys alphabetically for deterministic output
        5. Return JSON string (single line, no trailing newline)

      Error Handling:
        - None inputs treated as missing/omitted from output
        - Empty string naddr treated as None (omitted)
        - JSON serialization errors not caught (let propagate)

      Example Outputs:
        Minimal (no naddr, no image):
        {"event_id": "abc", "pubkey": "def"}

        With naddr:
        {"event_id": "abc", "naddr": "naddr1xyz", "pubkey": "def"}

        With image:
        {"image": {"dim": "1200x630", "hash": "sha256...", "mime": "image/jpeg", "url": "https://..."}, "event_id": "abc", "pubkey": "def"}

        Complete (all fields):
        {"image": {"dim": "1200x630", "hash": "sha256...", "mime": "image/jpeg", "url": "https://..."}, "event_id": "abc", "naddr": "naddr1xyz", "pubkey": "def"}

      Raises:
        - Does not raise (assumes valid inputs)
    """
    output = {"event_id": result.event_id, "pubkey": result.pubkey}

    if result.naddr is not None and result.naddr:
        output["naddr"] = result.naddr

    if cover_metadata is not None:
        output["image"] = cover_metadata

    return json.dumps(output, ensure_ascii=False, sort_keys=True, separators=(", ", ": "))
