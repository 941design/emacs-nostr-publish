"""Image metadata processing for NIP-92 imeta tags.

Handles parsing, validation, and MIME type inference for cover images.
"""

import re
from typing import Optional, Union

from .errors import InvalidFieldTypeError, InvalidFieldValueError
from .models import ImageMetadata


def parse_image_field(image_value: Union[str, dict, None]) -> Optional[ImageMetadata]:
    """Parse image field from frontmatter YAML into ImageMetadata.

    CONTRACT:
      Inputs:
        - image_value: value from frontmatter, one of:
          * None (field not present)
          * string (URL)
          * dictionary with keys: url OR file (mutually exclusive), mime, alt, dim, hash (optional)

      Outputs:
        - ImageMetadata instance or None
          * None if image_value is None
          * ImageMetadata with url/file and optional fields if present

      Invariants:
        - If image_value is string, result has url=image_value, other fields None
        - If image_value is dict, result has url OR file from dict, optional fields from dict
        - url and file are mutually exclusive (at most one present)
        - Optional fields (mime, alt, dim, hash) are string or None

      Properties:
        - Deterministic: same input always yields same output
        - Type-safe: result is always ImageMetadata or None
        - Normalized: both string and dict inputs produce ImageMetadata structure

      Algorithm:
        1. If image_value is None:
           a. Return None
        2. If image_value is string:
           a. Create ImageMetadata with url=image_value, other fields None
           b. Return ImageMetadata
        3. If image_value is dict:
           a. Extract optional "url" and "file" from dict
           b. Extract optional fields: "mime", "alt", "dim", "hash"
           c. Create ImageMetadata with extracted values
           d. Return ImageMetadata
        4. If image_value is any other type:
           a. Raise InvalidFieldTypeError

      Raises:
        - InvalidFieldTypeError: image_value is not None, string, or dict
    """
    if image_value is None:
        return None

    if isinstance(image_value, str):
        return ImageMetadata(url=image_value)

    if isinstance(image_value, dict):
        url = image_value.get("url")
        file = image_value.get("file")
        mime = image_value.get("mime")
        alt = image_value.get("alt")
        dim = image_value.get("dim")
        hash_value = image_value.get("hash")
        return ImageMetadata(url=url, file=file, mime=mime, alt=alt, dim=dim, hash=hash_value)

    raise InvalidFieldTypeError(f"image field must be None, string, or dict, got {type(image_value).__name__}")


def infer_mime_type(url: str) -> Optional[str]:
    """Infer MIME type from URL file extension.

    CONTRACT:
      Inputs:
        - url: string, URL with potential file extension

      Outputs:
        - mime_type: string MIME type or None
          * Returns inferred MIME type for known extensions
          * Returns None for unknown or missing extensions

      Invariants:
        - Mapping is case-insensitive (e.g., ".JPG" and ".jpg" both map to "image/jpeg")
        - Only standard image MIME types returned
        - URL without extension returns None

      Properties:
        - Deterministic: same URL always yields same MIME type
        - Case-insensitive: extension case does not affect result
        - Partial: does not attempt to handle all possible MIME types

      Algorithm:
        1. Extract file extension from URL:
           a. Find last "." in URL
           b. Extract substring after "."
           c. Convert to lowercase
        2. Map extension to MIME type using standard mappings:
           * .jpg, .jpeg → "image/jpeg"
           * .png → "image/png"
           * .gif → "image/gif"
           * .webp → "image/webp"
           * .svg → "image/svg+xml"
           * (other extensions as needed)
        3. If extension not in mapping:
           a. Return None
        4. Return mapped MIME type

      Note: Inference is conservative - only maps well-known image extensions.
    """
    mime_type_map = {
        "jpg": "image/jpeg",
        "jpeg": "image/jpeg",
        "png": "image/png",
        "gif": "image/gif",
        "webp": "image/webp",
        "svg": "image/svg+xml",
    }

    last_dot_index = url.rfind(".")
    if last_dot_index == -1:
        return None

    extension = url[last_dot_index + 1 :].lower()

    if not extension:
        return None

    return mime_type_map.get(extension)


def resolve_image_file_path(file_path: str, markdown_file_dir: str) -> str:
    """Resolve image file path to absolute path.

    CONTRACT:
      Inputs:
        - file_path: string, absolute or relative path from frontmatter
        - markdown_file_dir: string, absolute path to directory containing Markdown file

      Outputs:
        - absolute_path: string, absolute path to image file

      Invariants:
        - If file_path is absolute, return as-is
        - If file_path is relative, resolve relative to markdown_file_dir
        - Result is always an absolute path

      Properties:
        - Deterministic: same inputs yield same output
        - Idempotent: resolving an already-absolute path returns it unchanged

      Algorithm:
        1. Check if file_path is absolute:
           a. Use pathlib.Path.is_absolute()
        2. If absolute:
           a. Return file_path as-is
        3. If relative:
           a. Join markdown_file_dir with file_path
           b. Normalize the resulting path
           c. Return absolute path

      Note: Does NOT check file existence - that's done during validation.
    """
    from pathlib import Path

    path = Path(file_path)
    if path.is_absolute():
        return str(path)
    else:
        return str((Path(markdown_file_dir) / path).resolve())


def validate_image_metadata(image: ImageMetadata, markdown_file_dir: Optional[str] = None) -> ImageMetadata:
    """Validate ImageMetadata field values and constraints.

    CONTRACT:
      Inputs:
        - image: ImageMetadata instance (may have invalid values)
        - markdown_file_dir: optional string, absolute path to Markdown file directory
          * If provided and image.file is present: resolves relative paths and checks existence
          * If None and image.file is present: skips path resolution (deferred validation)

      Outputs:
        - validated_image: ImageMetadata instance with validated, normalized values
          * url: trimmed, non-empty string, valid URL format (if present)
          * file: trimmed non-empty string; absolute path if markdown_file_dir provided (if present)
          * url and file are mutually exclusive
          * mime: if None and URL has known extension, inferred from URL
          * mime: if present, trimmed non-empty string
          * alt: if present, trimmed non-empty string
          * dim: if present, trimmed non-empty string in "WIDTHxHEIGHT" format
          * hash: if present, trimmed non-empty string

      Invariants:
        - Exactly one SOURCE must be present: either url OR file
        - When file is present (local source mode):
          * url and hash are optional derived metadata (ignored for source determination)
          * url and hash may coexist with file
        - When only url is present (remote source mode):
          * file must NOT be present
        - url: non-empty string after trim, must start with "http://" or "https://"
        - file: non-empty string after trim; if markdown_file_dir provided, must exist and be readable
        - mime: if present, non-empty string after trim, format: "type/subtype"
        - alt: if present, non-empty string after trim
        - dim: if present, non-empty string matching pattern "NxN" (e.g., "1920x1080")
        - hash: if present, non-empty string after trim

      Properties:
        - Source determination: file presence determines mode, not url presence
        - Normalization: trimming whitespace is idempotent
        - Deterministic: same input yields same output
        - Fail-fast: raises on first validation error

      Algorithm:
        1. Determine source mode:
           a. If file is present:
              - Local source mode
              - url and hash are optional derived metadata
              - Allow file + url + hash coexistence
           b. Else if url is present:
              - Remote source mode
              - file must NOT be present (defensive check, should be impossible)
           c. Else:
              - Error: "image must have either url or file field"
        2. If url is present:
           a. Check type is string
           b. Trim whitespace
           c. Check non-empty
           d. Check starts with "http://" or "https://"
        3. If file is present:
           a. Check type is string
           b. Trim whitespace
           c. Check non-empty
           d. If markdown_file_dir is provided:
              - Resolve to absolute path using markdown_file_dir
              - Check file exists and is readable
        4. Infer MIME type if missing:
           a. If mime is None and url is present:
              - Call infer_mime_type(url)
              - If result is not None, use inferred value
        5. Validate and normalize mime (if present):
           a. If not None:
              - Check type is string
              - Trim whitespace
              - Check non-empty
              - Check format matches "type/subtype" pattern
        6. Validate and normalize alt (if present):
           a. If not None:
              - Check type is string
              - Trim whitespace
              - Check non-empty
        7. Validate and normalize dim (if present):
           a. If not None:
              - Check type is string
              - Trim whitespace
              - Check non-empty
              - Check format matches "WIDTHxHEIGHT" pattern (e.g., "1920x1080")
        8. Validate and normalize hash (if present):
           a. If not None:
              - Check type is string
              - Trim whitespace
              - Check non-empty
        9. Return new ImageMetadata with validated values

      Raises:
        - InvalidFieldTypeError: Field has wrong type
        - InvalidFieldValueError: Field value violates constraints
    """
    from pathlib import Path

    # Determine source mode (local vs remote)
    has_url = image.url is not None
    has_file = image.file is not None

    # Local source mode: file is present, url and hash are optional derived metadata
    # Remote source mode: only url is present
    # Error: neither file nor url present
    if has_file:
        # Local source mode - allow file + url + hash coexistence
        pass
    elif has_url:
        # Remote source mode - url is the source
        pass
    else:
        # Neither source present
        raise InvalidFieldValueError("image must have either url or file field")

    validated_url = None
    validated_file = None

    # Validate url if present
    if has_url:
        if not isinstance(image.url, str):
            raise InvalidFieldTypeError("image.url must be a string")

        validated_url = image.url.strip()
        if not validated_url:
            raise InvalidFieldValueError("image.url cannot be empty")

        # Security: Reject YAML structural characters and newlines (YAML injection prevention)
        if any(char in validated_url for char in ["\n", "\r", "[", "]", "{", "}"]):
            raise InvalidFieldValueError("image.url contains invalid characters (newlines or YAML structural chars)")

        if not (validated_url.startswith("http://") or validated_url.startswith("https://")):
            raise InvalidFieldValueError("image.url must start with http:// or https://")

    # Validate file if present
    if has_file:
        if not isinstance(image.file, str):
            raise InvalidFieldTypeError("image.file must be a string")

        validated_file = image.file.strip()
        if not validated_file:
            raise InvalidFieldValueError("image.file cannot be empty")

        # Resolve to absolute path and check existence only if markdown_file_dir is provided
        # When markdown_file_dir is None, skip path resolution (deferred to orchestrate_cover_upload)
        if markdown_file_dir is not None:
            validated_file = resolve_image_file_path(validated_file, markdown_file_dir)

            # Check file exists
            if not Path(validated_file).exists():
                raise InvalidFieldValueError(f"image.file does not exist: {validated_file}")

            if not Path(validated_file).is_file():
                raise InvalidFieldValueError(f"image.file is not a file: {validated_file}")

    # Validate and infer MIME type
    validated_mime = image.mime
    if validated_mime is not None:
        if not isinstance(validated_mime, str):
            raise InvalidFieldTypeError("image.mime must be a string or None")
        validated_mime = validated_mime.strip()
        if not validated_mime:
            raise InvalidFieldValueError("image.mime cannot be empty after trimming")
        # Security: Reject YAML structural characters and newlines
        if any(char in validated_mime for char in ["\n", "\r", "[", "]", "{", "}"]):
            raise InvalidFieldValueError("image.mime contains invalid characters (newlines or YAML structural chars)")
        if "/" not in validated_mime:
            raise InvalidFieldValueError('image.mime must be in "type/subtype" format')
    else:
        if validated_url:
            validated_mime = infer_mime_type(validated_url)

    # Validate alt
    validated_alt = image.alt
    if validated_alt is not None:
        if not isinstance(validated_alt, str):
            raise InvalidFieldTypeError("image.alt must be a string or None")
        validated_alt = validated_alt.strip()
        if not validated_alt:
            raise InvalidFieldValueError("image.alt cannot be empty after trimming")
        # Security: Reject YAML structural characters and newlines
        if any(char in validated_alt for char in ["\n", "\r", "[", "]", "{", "}"]):
            raise InvalidFieldValueError("image.alt contains invalid characters (newlines or YAML structural chars)")

    # Validate dim
    validated_dim = image.dim
    if validated_dim is not None:
        if not isinstance(validated_dim, str):
            raise InvalidFieldTypeError("image.dim must be a string or None")
        validated_dim = validated_dim.strip()
        if not validated_dim:
            raise InvalidFieldValueError("image.dim cannot be empty after trimming")
        # Security: Reject YAML structural characters and newlines
        if any(char in validated_dim for char in ["\n", "\r", "[", "]", "{", "}"]):
            raise InvalidFieldValueError("image.dim contains invalid characters (newlines or YAML structural chars)")
        if not re.match(r"^\d+x\d+$", validated_dim):
            raise InvalidFieldValueError('image.dim must match "WIDTHxHEIGHT" pattern (e.g., "1920x1080")')

    # Validate hash
    validated_hash = image.hash
    if validated_hash is not None:
        if not isinstance(validated_hash, str):
            raise InvalidFieldTypeError("image.hash must be a string or None")
        validated_hash = validated_hash.strip()
        if not validated_hash:
            raise InvalidFieldValueError("image.hash cannot be empty after trimming")
        # Security: Reject YAML structural characters and newlines
        if any(char in validated_hash for char in ["\n", "\r", "[", "]", "{", "}"]):
            raise InvalidFieldValueError("image.hash contains invalid characters (newlines or YAML structural chars)")

    return ImageMetadata(
        url=validated_url,
        file=validated_file,
        mime=validated_mime,
        alt=validated_alt,
        dim=validated_dim,
        hash=validated_hash,
    )
