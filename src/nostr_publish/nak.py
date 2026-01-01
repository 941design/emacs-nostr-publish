"""Nak subprocess invocation for NIP-46 signing and publishing.

Handles external process communication with nak CLI tool.
"""

import json
import subprocess
from typing import List

from .errors import NakInvocationError, PublishTimeoutError, SigningError
from .models import PublishResult, UnsignedEvent


def invoke_nak(event: UnsignedEvent, bunker_uri: str, relays: List[str], timeout: int = 30) -> PublishResult:
    """Invoke nak subprocess to sign and publish event via NIP-46.

    CONTRACT:
      Inputs:
        - event: UnsignedEvent instance to be signed and published
        - bunker_uri: string, NIP-46 bunker connection URI
        - relays: list of relay URLs to publish to
        - timeout: positive integer, seconds to wait for nak completion

      Outputs:
        - result: PublishResult with event_id and pubkey

      Invariants:
        - Event JSON passed to nak via stdin
        - Nak invoked with bunker URI and relay list
        - Exit code 0 indicates success, non-zero indicates failure
        - Success output contains event ID and pubkey
        - Timeout expiry raises PublishTimeoutError
        - Process errors raise NakInvocationError
        - Signing rejection raises SigningError

      Properties:
        - Deterministic input: same event always generates same JSON input to nak
        - Timeout-bounded: always returns or raises within timeout period
        - Process-isolated: nak runs as separate subprocess

      Algorithm:
        1. Serialize event to JSON:
           a. Convert UnsignedEvent.to_dict() to JSON string
        2. Construct nak command:
           a. Base command: "nak event --sec <bunker_uri>"
           b. Add relay URLs as positional arguments at end
           c. Stdin mode: accept event JSON on stdin
        3. Spawn nak subprocess:
           a. Set stdin to PIPE (for event JSON)
           b. Set stdout to PIPE (for result)
           c. Set stderr to PIPE (for errors)
           d. Set timeout to timeout parameter
        4. Write event JSON to nak stdin and close
        5. Wait for nak completion with timeout:
           a. If timeout expires: kill process, raise PublishTimeoutError
           b. If process exits non-zero:
              - Parse stderr for error message
              - If signing-related: raise SigningError
              - Otherwise: raise NakInvocationError
        6. Parse nak stdout:
           a. Extract event ID (required)
           b. Extract pubkey (required)
        7. Return PublishResult with extracted values

      Raises:
        - NakInvocationError: Failed to start nak or communication error
        - SigningError: Signer rejected signing request
        - PublishTimeoutError: Nak did not complete within timeout period

      Note:
        nak outputs relay success/failure as human-readable text, not structured
        JSON. Individual relay status is not tracked in the result.
    """
    event_json = json.dumps(event.to_dict())

    cmd = ["nak", "event", "--sec", bunker_uri]
    cmd.extend(relays)

    try:
        process = subprocess.Popen(
            cmd, stdin=subprocess.PIPE, stdout=subprocess.PIPE, stderr=subprocess.PIPE, text=True
        )
    except FileNotFoundError:
        raise NakInvocationError("nak binary not found in system PATH") from None
    except OSError:
        raise NakInvocationError("Failed to start nak subprocess") from None

    try:
        stdout, stderr = process.communicate(input=event_json, timeout=timeout)
    except subprocess.TimeoutExpired:
        process.kill()
        process.wait()
        raise PublishTimeoutError(f"Nak subprocess timed out after {timeout} seconds") from None
    except Exception:
        process.kill()
        process.wait()
        raise NakInvocationError("Failed to communicate with nak subprocess") from None

    if process.returncode != 0:
        stderr_lower = stderr.lower()
        if any(keyword in stderr_lower for keyword in ["rejected", "deny", "signing", "user rejected", "signer"]):
            raise SigningError(stderr.strip() if stderr else "Signing rejected")
        else:
            raise NakInvocationError(stderr.strip() if stderr else f"Nak exited with code {process.returncode}")

    return parse_nak_output(stdout)


def parse_nak_output(stdout: str) -> PublishResult:
    """Parse nak stdout to extract publish result.

    CONTRACT:
      Inputs:
        - stdout: string, nak process stdout content

      Outputs:
        - result: PublishResult instance with event_id and pubkey

      Invariants:
        - stdout contains the signed event as JSON (may include other text)
        - Event JSON contains id field (required)
        - Event JSON contains pubkey field (required)
        - Missing required fields raise NakInvocationError

      Properties:
        - Deterministic: same stdout yields same result
        - Robust: extracts JSON from mixed stdout containing status messages

      Algorithm:
        1. Extract JSON object from stdout (nak outputs status lines + JSON)
        2. Parse JSON to extract event_id and pubkey
        3. Return PublishResult with extracted values

      Raises:
        - NakInvocationError: Cannot parse stdout or missing required fields

    Note:
        nak outputs relay success/failure as human-readable text lines
        (e.g., "publishing to relay.example.com... success."), not as structured
        JSON fields. Relay status tracking is not supported.
    """
    # nak outputs status messages plus the event JSON on its own line
    # Find the JSON object in the output
    json_line = None
    for line in stdout.strip().split("\n"):
        line = line.strip()
        if line.startswith("{") and line.endswith("}"):
            json_line = line
            break

    if not json_line:
        raise NakInvocationError("No JSON event found in nak output")

    try:
        data = json.loads(json_line)
    except json.JSONDecodeError as e:
        raise NakInvocationError(f"Failed to parse nak output as JSON: {e}") from e

    if not isinstance(data, dict):
        raise NakInvocationError("Nak output is not a JSON object")

    event_id = data.get("id")
    if not event_id or not isinstance(event_id, str) or not event_id.strip():
        raise NakInvocationError("Nak output missing required field: id")

    pubkey = data.get("pubkey")
    if not pubkey or not isinstance(pubkey, str) or not pubkey.strip():
        raise NakInvocationError("Nak output missing required field: pubkey")

    return PublishResult(event_id=event_id, pubkey=pubkey)
