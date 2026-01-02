"""NIP-23 event construction.

Deterministic unsigned event generation from validated frontmatter and body.
"""

from .models import Frontmatter, UnsignedEvent


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
        - Tag ordering follows spec section 6.2 exactly
        - First tag is always ["d", slug]
        - Second tag is always ["title", title]
        - Optional tags appear in defined order
        - Content tags (["t", tag]) are sorted lexicographically

      Properties:
        - Deterministic: same frontmatter yields same tag list
        - Ordered: tag sequence is normative and reproducible
        - Complete: all frontmatter metadata represented

      Algorithm:
        Same as construct_event algorithm steps 1-4 (tag construction portion).
    """
    tags: list[list[str]] = []

    tags.append(["d", frontmatter.slug])
    tags.append(["title", frontmatter.title])

    if frontmatter.summary is not None:
        tags.append(["summary", frontmatter.summary])

    if frontmatter.published_at is not None:
        tags.append(["published_at", str(frontmatter.published_at)])

    for tag in sorted(frontmatter.tags):
        tags.append(["t", tag])

    return tags
