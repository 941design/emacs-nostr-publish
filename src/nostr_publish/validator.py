"""Frontmatter validation per spec section 5.

Strict validation of all frontmatter fields with fail-fast error handling.
"""

import re

from .errors import InvalidFieldTypeError, InvalidFieldValueError, MissingFieldError, UnknownFieldError
from .image_metadata import validate_image_metadata
from .models import Frontmatter
from .utils import deduplicate_preserving_order

# Slug must contain only lowercase letters, numbers, and hyphens
SLUG_PATTERN = re.compile(r"^[a-z0-9-]+$")

ALLOWED_FIELDS = {"title", "slug", "summary", "published_at", "tags", "relays", "image", "naddr"}
REQUIRED_FIELDS = {"title", "slug"}


def validate_frontmatter_dict(frontmatter_dict: dict) -> None:
    """Validate raw frontmatter dictionary structure and field presence.

    CONTRACT:
      Inputs:
        - frontmatter_dict: dictionary with string keys, arbitrary values

      Outputs:
        - None (validates in-place, raises on error)

      Invariants:
        - All keys in dictionary must be in ALLOWED_FIELDS
        - All REQUIRED_FIELDS must be present
        - Unknown keys result in UnknownFieldError
        - Missing required keys result in MissingFieldError

      Properties:
        - Fail-fast: raises on first validation error encountered
        - Deterministic error messages: same input yields same error message
        - Order-independent: dictionary key order does not affect validation

      Algorithm:
        1. Check for unknown fields:
           a. For each key in frontmatter_dict:
              - If key not in ALLOWED_FIELDS, raise UnknownFieldError with field name
        2. Check for missing required fields:
           a. For each field in REQUIRED_FIELDS:
              - If field not in frontmatter_dict, raise MissingFieldError with field name

      Raises:
        - UnknownFieldError: Dictionary contains keys not in ALLOWED_FIELDS
        - MissingFieldError: Dictionary missing keys from REQUIRED_FIELDS
    """
    unknown_fields = set(frontmatter_dict.keys()) - ALLOWED_FIELDS
    if unknown_fields:
        first_unknown = sorted(unknown_fields)[0]
        raise UnknownFieldError(f"Unknown field: {first_unknown}")

    missing_fields = REQUIRED_FIELDS - set(frontmatter_dict.keys())
    if missing_fields:
        first_missing = sorted(missing_fields)[0]
        raise MissingFieldError(f"Missing required field: {first_missing}")


def validate_frontmatter(fm: Frontmatter) -> Frontmatter:
    """Validate Frontmatter instance field types and value constraints.

    CONTRACT:
      Inputs:
        - fm: Frontmatter instance (may have invalid field values)

      Outputs:
        - validated_fm: Frontmatter instance with validated, normalized fields
          * Strings trimmed of leading/trailing whitespace
          * Tags deduplicated (case-sensitive, preserving first occurrence order)
          * All constraints satisfied

      Invariants:
        - title: non-empty string after trim
        - slug: non-empty string after trim, lowercase letters/numbers/hyphens only
        - summary: if present, non-empty string after trim
        - published_at: if present, non-negative integer
        - tags: list of non-empty strings after trim, no duplicates
        - relays: list of valid WebSocket URLs (wss:// or ws://), or "*" wildcard
        - image: if present, validated ImageMetadata with valid URL and format constraints

      Properties:
        - Normalization: trimming whitespace is idempotent
        - Deduplication: applying twice yields same result
        - Deterministic: same input yields same output

      Algorithm:
        1. Validate and normalize title:
           a. Check type is string, else raise InvalidFieldTypeError
           b. Trim whitespace
           c. Check non-empty, else raise InvalidFieldValueError
        2. Validate and normalize slug:
           a. Check type is string, else raise InvalidFieldTypeError
           b. Trim whitespace
           c. Check non-empty, else raise InvalidFieldValueError
           d. Check format (lowercase letters, numbers, hyphens only), else raise InvalidFieldValueError
        3. Validate and normalize summary (if present):
           a. If not None:
              - Check type is string, else raise InvalidFieldTypeError
              - Trim whitespace
              - Check non-empty, else raise InvalidFieldValueError
        4. Validate published_at (if present):
           a. If not None:
              - Check type is integer, else raise InvalidFieldTypeError
              - Check value >= 0, else raise InvalidFieldValueError
        5. Validate and normalize tags (if present):
           a. Check type is list, else raise InvalidFieldTypeError
           b. For each tag in list:
              - Check type is string, else raise InvalidFieldTypeError
              - Trim whitespace
              - Check non-empty, else raise InvalidFieldValueError
           c. Deduplicate tags (case-sensitive, preserve first occurrence order)
        6. Validate relays (if present):
           a. Check type is list, else raise InvalidFieldTypeError
           b. For each relay in list:
              - Check type is string, else raise InvalidFieldTypeError
              - If relay is "*", allow it (wildcard for all CLI relays per spec 8.2)
              - Otherwise check starts with "wss://" or "ws://", else raise InvalidFieldValueError
        7. Validate image (if present):
           a. If not None:
              - Call validate_image_metadata(image)
              - Return validated ImageMetadata
        8. Return new Frontmatter instance with normalized values

      Raises:
        - InvalidFieldTypeError: Field has wrong type
        - InvalidFieldValueError: Field value violates constraints (empty, negative, invalid URL)
    """
    if not isinstance(fm.title, str):
        raise InvalidFieldTypeError("Field 'title' must be a string")
    title = fm.title.strip()
    if not title:
        raise InvalidFieldValueError("Field 'title' must not be empty")

    if not isinstance(fm.slug, str):
        raise InvalidFieldTypeError("Field 'slug' must be a string")
    slug = fm.slug.strip()
    if not slug:
        raise InvalidFieldValueError("Field 'slug' must not be empty")
    if not SLUG_PATTERN.match(slug):
        raise InvalidFieldValueError(f"Field 'slug' must contain only lowercase letters, numbers, and hyphens: {slug}")

    summary = None
    if fm.summary is not None:
        if not isinstance(fm.summary, str):
            raise InvalidFieldTypeError("Field 'summary' must be a string")
        summary = fm.summary.strip()
        if not summary:
            raise InvalidFieldValueError("Field 'summary' must not be empty")

    published_at = None
    if fm.published_at is not None:
        if not isinstance(fm.published_at, int) or isinstance(fm.published_at, bool):
            raise InvalidFieldTypeError("Field 'published_at' must be an integer")
        if fm.published_at < 0:
            raise InvalidFieldValueError("Field 'published_at' must be non-negative")
        published_at = fm.published_at

    tags = []
    if fm.tags:
        if not isinstance(fm.tags, list):
            raise InvalidFieldTypeError("Field 'tags' must be a list")
        trimmed_tags = []
        for tag in fm.tags:
            if not isinstance(tag, str):
                raise InvalidFieldTypeError("Each tag must be a string")
            trimmed = tag.strip()
            if not trimmed:
                raise InvalidFieldValueError("Tags must not be empty")
            trimmed_tags.append(trimmed)
        tags = deduplicate_preserving_order(trimmed_tags)

    relays = []
    if fm.relays:
        if not isinstance(fm.relays, list):
            raise InvalidFieldTypeError("Field 'relays' must be a list")
        for relay in fm.relays:
            if not isinstance(relay, str):
                raise InvalidFieldTypeError("Each relay must be a string")
            # Allow "*" as wildcard meaning "use all CLI relays" (spec section 8.2)
            if relay != "*" and not (relay.startswith("wss://") or relay.startswith("ws://")):
                raise InvalidFieldValueError(f"Relay must be a WebSocket URL (wss:// or ws://): {relay}")
        relays = fm.relays

    image = None
    if fm.image is not None:
        image = validate_image_metadata(fm.image)

    return Frontmatter(
        title=title, slug=slug, summary=summary, published_at=published_at, tags=tags, relays=relays, image=image
    )
