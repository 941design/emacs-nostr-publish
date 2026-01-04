"""Nak subprocess invocation for NIP-46 signing and publishing.

Handles external process communication with nak CLI tool.
"""

import json
import os
import subprocess
from urllib.parse import urljoin

from .errors import BlossomUploadError, NakInvocationError, PublishTimeoutError, SigningError
from .models import PublishResult, UnsignedEvent


def invoke_nak(event: UnsignedEvent, bunker_uri: str, relays: list[str], timeout: int = 30) -> PublishResult:
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


def upload_to_blossom(file_path: str, blossom_url: str, bunker_uri: str, timeout: int = 30) -> dict[str, str]:
    """Upload file to Blossom server via nak.

    CONTRACT:
      Inputs:
        - file_path: string, absolute path to file to upload
        - blossom_url: string, Blossom server HTTP base URL (e.g., "http://localhost:3000")
        - bunker_uri: string, NIP-46 bunker URI for authentication (e.g., "bunker://pubkey?relay=...")
        - timeout: positive integer, seconds to wait for upload completion (default 30)

      Outputs:
        - result: dictionary with keys:
          * "hash": string, opaque blob identifier from Blossom server
          * "url": string, public HTTP(S) URL for accessing the uploaded blob

      Invariants:
        - File must exist and be readable
        - Blossom URL must be valid HTTP(S) URL
        - bunker_uri must be provided for Blossom authentication
        - Exit code 0 indicates success, non-zero indicates failure
        - Success output contains hash and url fields
        - Timeout expiry raises BlossomUploadError
        - Process errors raise BlossomUploadError

      Properties:
        - Timeout-bounded: always returns or raises within timeout period
        - Process-isolated: nak runs as separate subprocess
        - URL normalization: if server returns relative URL, joins with blossom_url base
        - Authenticated: uses bunker_uri for NIP-46 signing of upload request

      Algorithm:
        1. Verify file exists and is readable:
           a. Check file_path exists
           b. If not, raise BlossomUploadError
        2. Construct nak Blossom upload command:
           a. Command: ["nak", "blossom", "--server", blossom_url, "--sec", bunker_uri, "upload", file_path, "--json"]
           b. The --sec flag uses bunker_uri for NIP-46 authentication
           c. nak outputs JSON with "sha256" (hash) and "url" fields
        3. Spawn nak subprocess:
           a. Set stdout to PIPE (for JSON result)
           b. Set stderr to PIPE (for errors)
           c. Set timeout to timeout parameter
        4. Wait for nak completion with timeout:
           a. If timeout expires: kill process, raise BlossomUploadError
           b. If process exits non-zero:
              - Parse stderr for error message
              - Raise BlossomUploadError with error details
        5. Parse nak stdout as JSON:
           a. Extract "sha256" field as hash (required)
           b. Extract "url" field (required)
           c. If missing fields, raise BlossomUploadError
        6. Normalize URL:
           a. If url is relative (does not start with http:// or https://):
              - Join with blossom_url base to form absolute URL
           b. If url is already absolute, use as-is
        7. Return dictionary with hash and url

      Raises:
        - BlossomUploadError: Any upload failure (file not found, nak error, invalid output, timeout)

      Example Output:
        {
          "hash": "abc123def456...",
          "url": "http://blossom.example/abc123def456.jpg"
        }
    """
    if not os.path.exists(file_path):
        raise BlossomUploadError(f"File not found: {file_path}")

    # nak blossom command: --server and --sec come before the subcommand
    # nak blossom upload outputs JSON by default (no --json flag needed)
    cmd = ["nak", "blossom", "--server", blossom_url, "--sec", bunker_uri, "upload", file_path]

    try:
        process = subprocess.Popen(cmd, stdout=subprocess.PIPE, stderr=subprocess.PIPE, text=True)
    except FileNotFoundError:
        raise BlossomUploadError("nak binary not found in system PATH") from None
    except OSError as e:
        raise BlossomUploadError(f"Failed to start nak subprocess: {e}") from None

    try:
        stdout, stderr = process.communicate(timeout=timeout)
    except subprocess.TimeoutExpired:
        process.kill()
        process.wait()
        raise BlossomUploadError(f"Blossom upload timed out after {timeout} seconds") from None
    except Exception as e:
        process.kill()
        process.wait()
        raise BlossomUploadError(f"Failed to communicate with nak subprocess: {e}") from None

    if process.returncode != 0:
        # Sanitize error message - avoid exposing file paths or internal details
        stderr_lower = stderr.lower() if stderr else ""
        if "timeout" in stderr_lower:
            safe_msg = "Blossom upload timed out"
        elif "connection" in stderr_lower or "connect" in stderr_lower:
            safe_msg = "Blossom server connection failed"
        elif "auth" in stderr_lower:
            safe_msg = "Blossom authentication failed"
        elif "not found" in stderr_lower:
            safe_msg = "Blossom upload failed: resource not found"
        else:
            safe_msg = f"Blossom upload failed (exit code {process.returncode})"
        raise BlossomUploadError(safe_msg)

    try:
        data = json.loads(stdout)
    except json.JSONDecodeError as e:
        raise BlossomUploadError(f"Failed to parse nak output as JSON: {e}") from e

    if not isinstance(data, dict):
        raise BlossomUploadError("Nak output is not a JSON object")

    # nak blossom outputs "sha256" field for the hash
    hash_value = data.get("sha256")
    if not hash_value or not isinstance(hash_value, str) or not hash_value.strip():
        raise BlossomUploadError("Nak output missing required field: sha256")

    url = data.get("url")
    if not url or not isinstance(url, str) or not url.strip():
        raise BlossomUploadError("Nak output missing required field: url")

    # Security validation: Prevent YAML injection via newlines
    if "\n" in hash_value or "\r" in hash_value:
        raise BlossomUploadError("Invalid hash: contains newline characters")
    if "\n" in url or "\r" in url:
        raise BlossomUploadError("Invalid URL: contains newline characters")

    # Normalize URL: if relative, join with blossom_url base
    if not url.startswith(("http://", "https://")):
        url = urljoin(blossom_url, url)

    # Validate URL format after normalization
    if not url.startswith(("http://", "https://")):
        raise BlossomUploadError("Invalid URL format: must be http:// or https://")

    return {"hash": hash_value, "url": url}
