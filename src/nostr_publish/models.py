"""Data models for nostr-publish.

Fully implemented data classes representing frontmatter, events, and related structures.
"""

from dataclasses import dataclass, field
from typing import List, Optional


@dataclass
class Frontmatter:
    """Parsed and validated frontmatter from Markdown file.

    All fields represent validated data per spec section 5.
    """

    title: str
    slug: str
    summary: Optional[str] = None
    published_at: Optional[int] = None
    tags: List[str] = field(default_factory=list)
    relays: List[str] = field(default_factory=list)

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
    tags: List[List[str]]

    def to_dict(self) -> dict:
        """Convert to dictionary for JSON serialization."""
        return {"kind": self.kind, "content": self.content, "tags": self.tags}


@dataclass
class PublishResult:
    """Result of successful publish operation via nak."""

    event_id: str
    pubkey: str
