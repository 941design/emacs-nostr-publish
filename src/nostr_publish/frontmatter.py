"""Frontmatter parsing from Markdown files.

Parses YAML frontmatter delimited by --- markers and extracts body content.
"""

from typing import Optional

import yaml

from .errors import FrontmatterParseError
from .image_metadata import parse_image_field
from .models import Frontmatter


def parse_frontmatter(markdown_content: str) -> tuple[Optional[dict], str]:
    """Extract YAML frontmatter and body from Markdown content.

    CONTRACT:
      Inputs:
        - markdown_content: string, UTF-8 Markdown text, may or may not contain frontmatter

      Outputs:
        - frontmatter_dict: dictionary or None
          * None if no frontmatter present
          * dictionary of parsed YAML if frontmatter present
        - body: string, Markdown content with frontmatter removed

      Invariants:
        - If frontmatter present, it must be first bytes in file (no leading whitespace except single newline)
        - Frontmatter delimited by "---" on own line at start and end
        - Body begins immediately after closing delimiter (one newline allowed)
        - If frontmatter present but invalid YAML, raise FrontmatterParseError
        - If frontmatter delimiters present but empty (just "---\\n---"), raise FrontmatterParseError
        - Returned body never contains frontmatter delimiters or YAML content

      Properties:
        - Idempotent: parsing output of parse_frontmatter (if no frontmatter) returns same result
        - Deterministic: same input always yields same output
        - Reversible structure: frontmatter_dict + body capture all information from input

      Algorithm:
        1. Check if content starts with "---\\n" (frontmatter marker)
        2. If no frontmatter:
           a. Return (None, original_content)
        3. If frontmatter marker present:
           a. Find closing "---\\n" delimiter
           b. Extract YAML content between delimiters
           c. Parse YAML content to dictionary
              - If YAML parsing fails, raise FrontmatterParseError
              - If YAML is empty/null, raise FrontmatterParseError
           d. Extract body (everything after closing delimiter, trim one leading newline if present)
           e. Return (parsed_yaml_dict, body)

      Raises:
        - FrontmatterParseError: Invalid YAML syntax, empty frontmatter, or malformed delimiters
    """
    if not markdown_content.startswith("---\n"):
        return (None, markdown_content)

    rest_of_content = markdown_content[4:]

    try:
        closing_index = rest_of_content.index("---\n")
    except ValueError:
        raise FrontmatterParseError("Frontmatter start delimiter found but no closing delimiter") from None

    yaml_content = rest_of_content[:closing_index]

    if not yaml_content.strip():
        raise FrontmatterParseError("Frontmatter is empty")

    try:
        frontmatter_dict = yaml.safe_load(yaml_content)
    except yaml.YAMLError:
        raise FrontmatterParseError("Invalid YAML syntax in frontmatter") from None

    if frontmatter_dict is None:
        raise FrontmatterParseError("Frontmatter parsed as null/empty")

    if not isinstance(frontmatter_dict, dict):
        raise FrontmatterParseError("Frontmatter must be a dictionary/object")

    body_start_index = closing_index + 4
    body = rest_of_content[body_start_index:]

    if body.startswith("\n"):
        body = body[1:]

    return (frontmatter_dict, body)


def dict_to_frontmatter(frontmatter_dict: dict) -> Frontmatter:
    """Convert raw frontmatter dictionary to typed Frontmatter model.

    CONTRACT:
      Inputs:
        - frontmatter_dict: dictionary with string keys, values of various types

      Outputs:
        - frontmatter: Frontmatter instance with all fields populated from dictionary

      Invariants:
        - All keys in dictionary must correspond to Frontmatter fields
        - Unknown keys cause UnknownFieldError (handled by validator)
        - This function performs basic type coercion and structure extraction
        - Does NOT validate constraints (validator's responsibility)

      Properties:
        - Deterministic: same dictionary always yields same Frontmatter instance
        - Structure-preserving: dictionary keys map directly to Frontmatter attributes

      Algorithm:
        1. Extract known fields from dictionary:
           - title: string or None
           - slug: string or None
           - summary: string or None
           - published_at: integer or None
           - tags: list of strings or None
           - relays: list of strings or None
           - image: string, dict, or None (formerly "cover")
        2. Parse image field to ImageMetadata using parse_image_field()
        3. Create Frontmatter instance with extracted values
        4. Return Frontmatter instance

      Note: Validation of field values is deferred to FrontmatterValidator.
    """
    title = frontmatter_dict.get("title")
    slug = frontmatter_dict.get("slug")
    summary = frontmatter_dict.get("summary")
    published_at = frontmatter_dict.get("published_at")
    tags = frontmatter_dict.get("tags")
    relays = frontmatter_dict.get("relays")

    if tags is None:
        tags = []
    else:
        tags = list(tags)

    if relays is None:
        relays = []
    else:
        relays = list(relays)

    # Extract image field (formerly "cover")
    image_value = frontmatter_dict.get("image")
    image = parse_image_field(image_value)

    return Frontmatter(
        title=title, slug=slug, summary=summary, published_at=published_at, tags=tags, relays=relays, image=image
    )
