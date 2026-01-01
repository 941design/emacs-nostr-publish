"""CLI entrypoint for nostr-publish.

Command-line interface orchestrating full publish workflow.
"""

import argparse
import json
import sys
from pathlib import Path
from typing import List, Optional

from . import __version__
from .errors import NostrPublishError
from .event import construct_event
from .frontmatter import dict_to_frontmatter, parse_frontmatter
from .nak import invoke_nak
from .relay import resolve_relays, warn_insecure_relays
from .validator import validate_frontmatter, validate_frontmatter_dict


def main(argv: Optional[List[str]] = None) -> int:
    """Main CLI entrypoint.

    CONTRACT:
      Inputs:
        - argv: list of command-line argument strings (or None to use sys.argv)

      Outputs:
        - exit_code: integer, 0 for success, non-zero for failure

      Invariants:
        - Parses CLI arguments per spec section 10
        - Reads Markdown file from FILE.md argument
        - Orchestrates full publish workflow
        - Prints result or error message to stdout/stderr
        - Returns 0 on success, non-zero on any error

      Properties:
        - Deterministic: same inputs and file content yield same behavior
        - Fail-fast: aborts on first error encountered
        - User-friendly: error messages are human-readable

      Algorithm:
        1. Parse CLI arguments:
           a. Required: FILE.md (positional)
           b. Required: --relay wss://... (at least one, serves as allowlist + defaults)
           c. Required for publishing: --bunker URI
           d. Optional: --dry-run (flag)
           e. Optional: --timeout SECONDS (default: 30)
        2. Read Markdown file:
           a. Check file exists, else error
           b. Read file content as UTF-8
        3. Parse frontmatter:
           a. Call parse_frontmatter(content)
           b. If no frontmatter or parsing fails, abort with error
        4. Validate frontmatter:
           a. Call validate_frontmatter_dict(frontmatter_dict)
           b. Convert to Frontmatter: dict_to_frontmatter(frontmatter_dict)
           c. Call validate_frontmatter(fm)
        5. Construct event:
           a. Call construct_event(frontmatter, body)
        6. Resolve relays (spec section 7):
           a. CLI relays serve as allowlist and defaults
           b. If frontmatter specifies relays, validate against allowlist
           c. If frontmatter omits relays or uses "*", use CLI defaults
        7. If dry-run:
           a. Print event JSON and relay list
           b. Return 0
        8. Invoke nak:
           a. Require bunker_uri from CLI --bunker
           b. Call invoke_nak(event, bunker_uri, relays, timeout)
        9. Print success result:
           a. Event ID
           b. Pubkey
           c. Relay success/failure counts
        10. Return 0

      Error Handling:
        - Catch all NostrPublishError exceptions
        - Print error message to stderr with format: "ERROR: {error_type}: {message}"
        - Return non-zero exit code

      Raises:
        - Does not raise (catches all exceptions and converts to exit codes)
    """
    try:
        # Parse CLI arguments
        args = parse_arguments(argv if argv is not None else sys.argv[1:])

        # Read Markdown file
        content = read_markdown_file(args["file"])

        # Parse frontmatter
        frontmatter_dict, body = parse_frontmatter(content)
        if frontmatter_dict is None:
            sys.stderr.write("ERROR: FrontmatterParseError: No frontmatter found in file\n")
            return 1

        # Validate frontmatter
        validate_frontmatter_dict(frontmatter_dict)
        frontmatter = dict_to_frontmatter(frontmatter_dict)
        frontmatter = validate_frontmatter(frontmatter)

        # Construct event
        event = construct_event(frontmatter, body)

        # Resolve relays (CLI serves as allowlist + defaults per spec section 7)
        relays = resolve_relays(args["relays"], frontmatter.relays)

        # Warn about insecure ws:// relays for non-localhost
        warn_insecure_relays(relays)

        # Handle dry-run mode
        if args["dry_run"]:
            print(json.dumps(event.to_dict()))
            print(f"Relays: {json.dumps(relays)}")
            return 0

        # Invoke nak to sign and publish (--bunker required)
        bunker_uri = args["bunker_uri"]
        if not bunker_uri:
            sys.stderr.write("ERROR: NakInvocationError: --bunker URI required for publishing\n")
            return 1

        result = invoke_nak(event, bunker_uri, relays, args["timeout"])

        # Print success result
        print(f"Published: {result.event_id}")
        print(f"Pubkey: {result.pubkey}")

        return 0

    except NostrPublishError as e:
        error_type = type(e).__name__
        sys.stderr.write(f"ERROR: {error_type}: {str(e)}\n")
        return 1
    except FileNotFoundError as e:
        sys.stderr.write(f"ERROR: FileNotFoundError: {str(e)}\n")
        return 1
    except PermissionError as e:
        sys.stderr.write(f"ERROR: PermissionError: {str(e)}\n")
        return 1
    except UnicodeDecodeError as e:
        sys.stderr.write(f"ERROR: UnicodeDecodeError: {str(e)}\n")
        return 1
    except SystemExit:
        raise
    except Exception as e:
        sys.stderr.write(f"ERROR: {type(e).__name__}: {str(e)}\n")
        return 1


def parse_arguments(argv: List[str]) -> dict:
    """Parse CLI arguments into structured dictionary.

    CONTRACT:
      Inputs:
        - argv: list of command-line argument strings (excluding program name)

      Outputs:
        - args: dictionary with keys:
          * file: Path to Markdown file
          * bunker_uri: string or None
          * relays: list of relay URLs
          * dry_run: boolean
          * timeout: positive integer

      Invariants:
        - file argument is required (first positional)
        - At least one --relay is required (serves as allowlist + defaults per spec section 7)
        - --relay can appear multiple times
        - --timeout value must be positive integer
        - Invalid arguments cause error with usage message

      Properties:
        - Deterministic: same argv yields same args dictionary
        - Complete: all CLI options from spec section 10 supported

      Algorithm:
        1. Initialize args dictionary with defaults:
           - bunker_uri: None
           - relays: []
           - dry_run: False
           - timeout: 30
        2. Parse positional arguments:
           a. First positional is file path (required)
           b. Convert to Path object
        3. Parse optional arguments:
           a. --bunker: next argument is bunker URI
           b. --relay: next argument is relay URL (append to list)
           c. --dry-run: set flag to True
           d. --timeout: next argument is integer seconds
        4. Validate:
           a. File argument present, else error
           b. At least one --relay provided, else error
           c. Timeout is positive integer, else error
        5. Return args dictionary
    """
    parser = argparse.ArgumentParser(
        prog="nostr-publish", description="Publish Markdown articles to Nostr via NIP-46 remote signing"
    )

    parser.add_argument("--version", action="version", version=f"%(prog)s {__version__}")
    parser.add_argument("file", help="Markdown file to publish")
    parser.add_argument(
        "--bunker",
        dest="bunker_uri",
        default=None,
        help="NIP-46 bunker connection URI (required for publishing, not needed with --dry-run)",
    )
    parser.add_argument(
        "--relay",
        dest="relays",
        action="append",
        default=[],
        help="Relay URL (required, including for --dry-run). Serves as allowlist for frontmatter relays and default publish targets.",
    )
    parser.add_argument(
        "--dry-run",
        dest="dry_run",
        action="store_true",
        help="Validate and construct event without publishing (does not require --bunker)",
    )
    parser.add_argument(
        "--timeout",
        dest="timeout",
        type=int,
        default=30,
        help="Timeout in seconds for signer and publish operations (default: 30)",
    )

    parsed = parser.parse_args(argv)

    # Validate at least one relay is provided
    if not parsed.relays:
        if parsed.dry_run:
            parser.error("At least one --relay is required (needed for validation even in --dry-run mode)")
        else:
            parser.error("At least one --relay is required")

    # Validate timeout is positive
    if parsed.timeout <= 0:
        parser.error("--timeout must be a positive integer")

    return {
        "file": Path(parsed.file),
        "bunker_uri": parsed.bunker_uri,
        "relays": parsed.relays,
        "dry_run": parsed.dry_run,
        "timeout": parsed.timeout,
    }


def read_markdown_file(file_path: Path) -> str:
    """Read Markdown file content as UTF-8.

    CONTRACT:
      Inputs:
        - file_path: Path object pointing to Markdown file

      Outputs:
        - content: string, UTF-8 decoded file content

      Invariants:
        - File must exist, else raise FileNotFoundError
        - File must be readable, else raise PermissionError
        - Content decoded as UTF-8, else raise UnicodeDecodeError

      Properties:
        - Deterministic: same file yields same content
        - Complete: entire file content returned

      Algorithm:
        1. Check file exists:
           a. If not, raise FileNotFoundError with file path
        2. Open file in read mode with UTF-8 encoding
        3. Read entire content
        4. Return content string
    """
    if not file_path.exists():
        raise FileNotFoundError(f"File not found: {file_path}")

    return file_path.read_text(encoding="utf-8")


if __name__ == "__main__":
    sys.exit(main())
