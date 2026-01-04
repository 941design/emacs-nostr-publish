"""NIP-23 event construction.

Deterministic unsigned event generation from validated frontmatter and body.
"""

from .models import Frontmatter, ImageMetadata, UnsignedEvent


def construct_event(frontmatter: Frontmatter, body: str) -> UnsignedEvent:
    """Construct unsigned NIP-23 event from frontmatter and Markdown body.

    CONTRACT:
      Inputs:
        - frontmatter: Frontmatter instance (validated)
        - body: string, Markdown content (frontmatter already removed)

      Outputs:
        - event: UnsignedEvent instance with kind 30023, deterministic tags

      Invariants:
        - kind always equals 30023 (NIP-23 long-form content)
        - content equals body exactly (no modifications)
        - tags follow strict ordering per spec section 6.2
        - tags is always a list of lists (each tag is [tag_name, tag_value, ...])

      Properties:
        - Deterministic: same frontmatter + body always yields identical event
        - Complete: all frontmatter fields represented in tags per spec
        - Ordered: tag ordering is normative and reproducible

      Algorithm:
        1. Initialize empty tags list
        2. Add required tags in order:
           a. Add ["d", frontmatter.slug]
           b. Add ["title", frontmatter.title]
        3. Add optional tags in order:
           a. If frontmatter.summary is not None:
              - Add ["summary", frontmatter.summary]
           b. If frontmatter.published_at is not None:
              - Convert published_at to string
              - Add ["published_at", string_value]
        4. Add content tags:
           a. Sort frontmatter.tags lexicographically (case-sensitive)
           b. For each tag in sorted order:
              - Add ["t", tag]
        5. Create UnsignedEvent:
           - kind: 30023
           - content: body
           - tags: constructed tags list
        6. Return UnsignedEvent

      Note: No additional tags beyond those specified are permitted.
    """
    tags = build_tags(frontmatter)
    return UnsignedEvent(kind=30023, content=body, tags=tags)


def build_tags(frontmatter: Frontmatter) -> list[list[str]]:
    """Build deterministic tag list from frontmatter.

    CONTRACT:
      Inputs:
        - frontmatter: Frontmatter instance (validated)

      Outputs:
        - tags: list of tag arrays, each tag is [tag_name, tag_value, ...]

      Invariants:
        - Tag ordering follows spec section 6.2 exactly (updated for image field)
        - First tag is always ["d", slug]
        - Second tag is always ["title", title]
        - Optional metadata tags appear in defined order
        - imeta tag (if present) appears after published_at, before content tags
        - Content tags (["t", tag]) are sorted lexicographically and appear last

      Properties:
        - Deterministic: same frontmatter yields same tag list
        - Ordered: tag sequence is normative and reproducible
        - Complete: all frontmatter metadata represented

      Algorithm:
        1. Initialize empty tags list
        2. Add required tags in order:
           a. Add ["d", frontmatter.slug]
           b. Add ["title", frontmatter.title]
        3. Add optional metadata tags in order:
           a. If frontmatter.summary is not None:
              - Add ["summary", frontmatter.summary]
           b. If frontmatter.published_at is not None:
              - Convert published_at to string
              - Add ["published_at", string_value]
           c. If frontmatter.image is not None:
              - Call build_imeta_tag(frontmatter.image)
              - Add resulting imeta tag to tags list
        4. Add content tags:
           a. Sort frontmatter.tags lexicographically (case-sensitive)
           b. For each tag in sorted order:
              - Add ["t", tag]
        5. Return tags list
    """
    tags = []

    # Add required tags
    tags.append(["d", frontmatter.slug])
    tags.append(["title", frontmatter.title])

    # Add optional metadata tags in order
    if frontmatter.summary is not None:
        tags.append(["summary", frontmatter.summary])

    if frontmatter.published_at is not None:
        tags.append(["published_at", str(frontmatter.published_at)])

    if frontmatter.image is not None:
        tags.append(build_imeta_tag(frontmatter.image))

    # Add content tags (sorted lexicographically)
    for tag in sorted(frontmatter.tags):
        tags.append(["t", tag])

    return tags


def build_imeta_tag(image: ImageMetadata) -> list[str]:
    """Build NIP-92 imeta tag from ImageMetadata.

    CONTRACT:
      Inputs:
        - image: ImageMetadata instance (validated)

      Outputs:
        - imeta_tag: list of strings representing NIP-92 imeta tag
          Format: ["imeta", "url <url>", "m <mime>", "alt <alt>", "dim <dim>", "x <hash>"]
          Only includes fields that are not None

      Invariants:
        - First element is always "imeta"
        - Second element is always "url <url_value>"
        - Subsequent elements follow NIP-92 key-value format: "key value"
        - Order of optional fields: url (required), m (mime), alt, dim, x (hash)
        - Only non-None fields are included

      Properties:
        - Deterministic: same ImageMetadata yields same tag
        - NIP-92 compliant: follows space-delimited key/value format
        - Complete: all present metadata fields represented

      Algorithm:
        1. Initialize tag list with "imeta"
        2. Add url field: append "url <image.url>"
        3. If image.mime is not None:
           a. Append "m <image.mime>"
        4. If image.alt is not None:
           a. Append "alt <image.alt>"
        5. If image.dim is not None:
           a. Append "dim <image.dim>"
        6. If image.hash is not None:
           a. Append "x <image.hash>"
        7. Return tag list

      Note: NIP-92 format uses space-delimited key-value pairs within tag elements.
    """
    tag = ["imeta"]
    tag.append(f"url {image.url}")

    if image.mime is not None:
        tag.append(f"m {image.mime}")

    if image.alt is not None:
        tag.append(f"alt {image.alt}")

    if image.dim is not None:
        tag.append(f"dim {image.dim}")

    # NIP-92: x field contains the file hash (SHA256)
    if image.hash is not None:
        tag.append(f"x {image.hash}")

    return tag
