"""Data models for nostr-publish.

Fully implemented data classes representing frontmatter, events, and related structures.
"""

from dataclasses import dataclass, field


@dataclass
class ImageMetadata:
    """Validated image metadata for NIP-92 imeta tag.

    Represents cover/preview image metadata extracted from frontmatter.
    All fields validated per NIP-92 specification.

    Extended for NIP-92 Blossom support:
    - file: local file path (mutually exclusive with url when specified)
    - hash: Blossom blob identifier after upload
    - url and file are mutually exclusive (at least one required)
    """

    url: str | None = None
    mime: str | None = None
    alt: str | None = None
    dim: str | None = None
    file: str | None = None
    hash: str | None = None


@dataclass
class Frontmatter:
    """Parsed and validated frontmatter from Markdown file.

    All fields represent validated data per spec section 5.
    """

    title: str
    slug: str
    summary: str | None = None
    published_at: int | None = None
    tags: list[str] = field(default_factory=list)
    relays: list[str] = field(default_factory=list)
    image: ImageMetadata | None = None
    naddr: str | None = None

    def __post_init__(self):
        """Ensure mutable defaults are instance-specific."""
        if self.tags is None:
            self.tags = []
        if self.relays is None:
            self.relays = []


@dataclass
class UnsignedEvent:
    """Unsigned NIP-23 event ready for signing via nak.

    Represents the event structure passed to nak for NIP-46 signing.
    Fields id, sig, pubkey, created_at are omitted (signer provides).
    """

    kind: int
    content: str
    tags: list[list[str]]

    def to_dict(self) -> dict:
        """Convert to dictionary for JSON serialization."""
        return {"kind": self.kind, "content": self.content, "tags": self.tags}


@dataclass
class PublishResult:
    """Result of successful publish operation via nak."""

    event_id: str
    pubkey: str
    naddr: str | None = None
