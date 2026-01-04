"""CLI orchestration for cover image upload workflow.

Extends main CLI workflow with local file upload to Blossom.
"""

from pathlib import Path

from .image_metadata import validate_image_metadata
from .image_processing import get_processed_cover_path, process_cover_image
from .models import Frontmatter
from .nak import upload_to_blossom


def orchestrate_cover_upload(
    frontmatter: Frontmatter,
    markdown_file_path: Path,
    blossom_url: str,
    bunker_uri: str,
    blossom_timeout: int,
    cover_size: str,
) -> dict[str, str] | None:
    """Orchestrate cover image processing and Blossom upload workflow.

    CONTRACT:
      Inputs:
        - frontmatter: Frontmatter instance with potentially image.file field
        - markdown_file_path: Path to Markdown file (for resolving relative paths)
        - blossom_url: string, Blossom server base URL (e.g., "http://localhost:3000")
        - bunker_uri: string, NIP-46 bunker URI for Blossom authentication
        - blossom_timeout: positive integer, timeout in seconds for Blossom upload
        - cover_size: string, target dimensions in "WIDTHxHEIGHT" format (e.g., "1200x630")

      Outputs:
        - result: dictionary with cover metadata or None
          * If cover.file is present: returns {"hash": "...", "url": "...", "dim": "...", "mime": "..."}
          * If cover.file is absent: returns None
          * Output is suitable for JSON serialization to stdout for Emacs

      Invariants:
        - If frontmatter.image is None or image.file is None, returns None immediately
        - If image.file is present, upload MUST complete before returning
        - Returned metadata reflects actual processed image (dimensions, MIME type)
        - All processing errors propagate as exceptions (fail-fast)

      Properties:
        - Conditional execution: only processes if cover.file is present
        - Atomic: upload completes or raises, no partial state
        - Deterministic output path: processed file always in .nostr-publish/cache/covers/<slug>/
        - Authenticated: uses bunker_uri for NIP-46 signing of Blossom upload

      Algorithm:
        1. Check if cover upload needed:
           a. If frontmatter.image is None: return None
           b. If frontmatter.image.file is None: return None
           c. Otherwise, proceed with upload workflow
        2. Validate image metadata with file path resolution:
           a. Call validate_image_metadata(frontmatter.image, markdown_file_dir)
           b. This resolves relative paths and checks file existence
        3. Parse cover_size into target dimensions:
           a. Extract width and height from "WIDTHxHEIGHT" format
           b. If invalid format, raise error
        4. Determine processed cover output path:
           a. Call get_processed_cover_path(markdown_file_path, frontmatter.slug)
        5. Process image (EXIF strip + resize):
           a. Call process_cover_image(input_path, output_path, target_width, target_height)
           b. Returns (processed_path, actual_width, actual_height)
        6. Compute hash of processed image:
           a. Read processed image file as bytes
           b. Compute SHA256 hash of bytes
           c. Convert to lowercase hex string (64 characters)
        7. Check if upload can be skipped (idempotent publish):
           a. If frontmatter.image.hash exists and equals computed hash:
              - Skip upload (image unchanged)
              - Reuse existing frontmatter.image.url
              - Construct result with existing url + computed hash
              - Return result (skip step 8)
           b. Else (hash missing or different):
              - Proceed to upload (step 8)
        8. Upload to Blossom:
           a. Call upload_to_blossom(processed_path, blossom_url, bunker_uri, blossom_timeout)
           b. Returns {"hash": "...", "url": "..."}
        9. Construct result metadata:
           a. Combine hash, url from upload result OR skip logic
           b. Add dim: "actual_width x actual_height"
           c. Add mime: "image/jpeg" (processed format)
        10. Return result dictionary

      Raises:
        - InvalidFieldValueError: Invalid cover_size format
        - ImageProcessingError: Image processing failure (EXIF, resize)
        - BlossomUploadError: Upload failure
        - Any other error propagates from called functions

      Example:
        Input: frontmatter with cover.file = "./my-image.png", cover_size = "1200x630"
        Workflow:
          1. Validate: my-image.png exists
          2. Process: strip EXIF, resize to 1200x630, save as .nostr-publish/cache/covers/<slug>/cover.jpg
          3. Upload: upload cover.jpg to Blossom, get hash and URL
          4. Return: {"hash": "abc123...", "url": "http://...", "dim": "1200x630", "mime": "image/jpeg"}
    """
    from .errors import InvalidFieldValueError

    if frontmatter.image is None or frontmatter.image.file is None:
        return None

    markdown_file_dir = str(Path(markdown_file_path).parent)

    # Validate and resolve file path to absolute
    validated_image = validate_image_metadata(frontmatter.image, markdown_file_dir)

    cover_size_parts = cover_size.split("x")
    if len(cover_size_parts) != 2:
        raise InvalidFieldValueError(f'cover_size must be in WIDTHxHEIGHT format, got "{cover_size}"')

    try:
        target_width = int(cover_size_parts[0])
        target_height = int(cover_size_parts[1])
    except ValueError as e:
        raise InvalidFieldValueError(f'cover_size must be in WIDTHxHEIGHT format, got "{cover_size}"') from e

    processed_cover_path = get_processed_cover_path(str(markdown_file_path), frontmatter.slug)

    processed_path, actual_width, actual_height = process_cover_image(
        validated_image.file, processed_cover_path, target_width, target_height
    )

    # Check if upload can be skipped (idempotent publish)
    # Only compute hash if there's an existing hash to compare against
    existing_hash = frontmatter.image.hash
    if existing_hash:
        # Compute hash of processed image to compare
        import hashlib

        with open(processed_path, "rb") as f:
            processed_bytes = f.read()
        computed_hash = hashlib.sha256(processed_bytes).hexdigest()

        if existing_hash == computed_hash:
            # Hash matches - skip upload, reuse existing URL
            existing_url = frontmatter.image.url
            return {
                "hash": computed_hash,
                "url": existing_url,
                "dim": f"{actual_width}x{actual_height}",
                "mime": "image/jpeg",
            }

    # Hash missing or different - upload to Blossom
    upload_result = upload_to_blossom(processed_path, blossom_url, bunker_uri, blossom_timeout)

    return {
        "hash": upload_result["hash"],
        "url": upload_result["url"],
        "dim": f"{actual_width}x{actual_height}",
        "mime": "image/jpeg",
    }


def add_cover_arguments(parser):
    """Add cover image CLI arguments to argument parser.

    CONTRACT:
      Inputs:
        - parser: argparse.ArgumentParser instance

      Outputs:
        - None (modifies parser in-place by adding arguments)

      Invariants:
        - Adds --blossom argument (optional)
        - Adds --blossom-timeout argument (optional, default 30)
        - Adds --cover-size argument (optional, default "1200x630")
        - Adds --allow-dry-run-without-upload argument (optional flag)

      Algorithm:
        1. Add --blossom argument:
           a. Type: string
           b. Default: None
           c. Help text: "Blossom server HTTP base URL (required if cover.file is used)"
        2. Add --blossom-timeout argument:
           a. Type: integer
           b. Default: 30
           c. Help text: "Timeout in seconds for Blossom upload operations"
        3. Add --cover-size argument:
           a. Type: string
           b. Default: "1200x630"
           c. Help text: "Target cover image dimensions in WIDTHxHEIGHT format"
        4. Add --allow-dry-run-without-upload argument:
           a. Type: boolean flag
           b. Default: False
           c. Help text: "Allow dry-run with cover.file without requiring cover.url"
    """
    parser.add_argument(
        "--blossom",
        dest="blossom_url",
        default=None,
        help="Blossom server HTTP base URL (required if cover.file is used in frontmatter)",
    )
    parser.add_argument(
        "--blossom-timeout",
        dest="blossom_timeout",
        type=int,
        default=30,
        help="Timeout in seconds for Blossom upload operations (default: 30)",
    )
    parser.add_argument(
        "--cover-size",
        dest="cover_size",
        default="1200x630",
        help="Target cover image dimensions in WIDTHxHEIGHT format (default: 1200x630)",
    )
    parser.add_argument(
        "--allow-dry-run-without-upload",
        dest="allow_dry_run_without_upload",
        action="store_true",
        help="Allow dry-run with cover.file without requiring cover.url (event will have placeholder)",
    )


def validate_cover_arguments(args: dict, has_cover_file: bool, has_cover_url: bool = False):
    """Validate cover-related CLI arguments.

    CONTRACT:
      Inputs:
        - args: dictionary from parse_arguments containing CLI args
        - has_cover_file: boolean, True if frontmatter has cover.file field
        - has_cover_url: boolean, True if frontmatter has cover.url field

      Outputs:
        - None (raises exception if validation fails)

      Invariants:
        - If has_cover_file is True and not dry-run, blossom_url must be provided
        - If has_cover_file is True and not dry-run, bunker_uri must be provided
        - If dry-run with cover.file and no cover.url, require --allow-dry-run-without-upload
        - blossom_timeout must be positive integer if provided
        - cover_size must match "WIDTHxHEIGHT" pattern if provided

      Algorithm:
        1. If dry-run mode with cover.file:
           a. If no cover.url and not allow_dry_run_without_upload, error
           b. Skip blossom/bunker validation (not uploading)
        2. If not dry-run and has_cover_file:
           a. Check args["blossom_url"] is not None
           b. Check args["bunker_uri"] is not None
        3. Validate blossom_timeout:
           a. Check blossom_timeout > 0
        4. Validate cover_size format:
           a. Check matches pattern "NxN" (e.g., "1200x630")

      Raises:
        - InvalidFieldValueError: Validation failure
    """
    import re

    from .errors import InvalidFieldValueError

    is_dry_run = args.get("dry_run", False)

    if has_cover_file:
        if is_dry_run:
            # In dry-run mode, we skip upload. If no cover.url exists,
            # we need --allow-dry-run-without-upload flag
            if not has_cover_url and not args.get("allow_dry_run_without_upload", False):
                raise InvalidFieldValueError(
                    "Dry-run with cover.file requires cover.url in frontmatter, "
                    "or use --allow-dry-run-without-upload flag"
                )
            # Don't validate blossom/bunker - not uploading in dry-run
        else:
            # Not dry-run - require blossom and bunker for upload
            if not args.get("blossom_url"):
                raise InvalidFieldValueError("--blossom URL required when cover.file is present in frontmatter")
            if not args.get("bunker_uri"):
                raise InvalidFieldValueError("--bunker URI required when cover.file is present in frontmatter")

    blossom_timeout = args.get("blossom_timeout", 30)
    if blossom_timeout <= 0:
        raise InvalidFieldValueError("--blossom-timeout must be a positive integer")

    cover_size = args.get("cover_size", "1200x630")
    if not re.match(r"^\d+x\d+$", cover_size):
        raise InvalidFieldValueError('--cover-size must be in WIDTHxHEIGHT format (e.g., "1200x630")')
