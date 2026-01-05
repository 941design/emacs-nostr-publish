"""NIP-23 event construction.

Deterministic unsigned event generation from validated frontmatter and body.
"""

from .models import Frontmatter, ImageMetadata, UnsignedEvent


def construct_event(frontmatter: Frontmatter, body: str, extra_tags: list[list[str]] | None = None) -> UnsignedEvent:
    """Construct unsigned NIP-23 event from frontmatter and Markdown body.

    CONTRACT:
      Inputs:
        - frontmatter: Frontmatter instance (validated)
        - body: string, Markdown content (frontmatter already removed)
        - extra_tags: optional list of arbitrary tag arrays to inject, each tag is [key, value]

      Outputs:
        - event: UnsignedEvent instance with kind 30023, deterministic tags

      Invariants:
        - kind always equals 30023 (NIP-23 long-form content)
        - content equals body exactly (no modifications)
        - tags follow strict ordering per spec section 7.2
        - tags is always a list of lists (each tag is [tag_name, tag_value, ...])

      Properties:
        - Deterministic: same frontmatter + body + extra_tags always yields identical event
        - Complete: all frontmatter fields represented in tags per spec
        - Ordered: tag ordering is normative and reproducible

      Algorithm:
        1. Call build_tags(frontmatter, extra_tags) to construct tag list
        2. Create UnsignedEvent:
           - kind: 30023
           - content: body
           - tags: constructed tags list
        3. Return UnsignedEvent
    """
    tags = build_tags(frontmatter, extra_tags)
    return UnsignedEvent(kind=30023, content=body, tags=tags)


def build_tags(frontmatter: Frontmatter, extra_tags: list[list[str]] | None = None) -> list[list[str]]:
    """Build deterministic tag list from frontmatter with optional injected tags.

    CONTRACT:
      Inputs:
        - frontmatter: Frontmatter instance (validated)
        - extra_tags: optional list of arbitrary tag arrays to inject, each tag is [key, value]
          Example: [["x-emacs-nostr-publish", "preview"], ["client", "emacs"]]

      Outputs:
        - tags: list of tag arrays, each tag is [tag_name, tag_value, ...]

      Invariants:
        - Tag ordering follows spec section 7.2 exactly
        - First tag is always ["d", slug]
        - Second tag is always ["title", title]
        - Optional metadata tags appear in defined order
        - image tag (NIP-23 simple format) appears after published_at
        - imeta tag (NIP-92 rich metadata) appears after image tag
        - Extra tags (if present) appear after imeta, before content tags
        - Content tags (["t", tag]) are sorted lexicographically and appear last

      Properties:
        - Deterministic: same frontmatter + extra_tags yields same tag list
        - Ordered: tag sequence is normative and reproducible
        - Complete: all frontmatter metadata represented
        - Extensible: arbitrary tags can be injected without modifying core logic

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
              - Add ["image", frontmatter.image.url] (NIP-23 simple format)
              - Call build_imeta_tag(frontmatter.image)
              - Add resulting imeta tag to tags list (NIP-92 rich metadata)
        4. Add extra tags if provided:
           a. If extra_tags is not None and not empty:
              - For each tag in extra_tags:
                * Add tag to tags list (preserving order from extra_tags)
        5. Add content tags:
           a. Sort frontmatter.tags lexicographically (case-sensitive)
           b. For each tag in sorted order:
              - Add ["t", tag]
        6. Return tags list
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
        # NIP-23 simple format for compatibility with clients like njump
        tags.append(["image", frontmatter.image.url])
        # NIP-92 rich metadata format for clients that support it
        tags.append(build_imeta_tag(frontmatter.image))

    # Add extra tags if provided (after imeta, before content tags)
    if extra_tags:
        tags.extend(extra_tags)

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
