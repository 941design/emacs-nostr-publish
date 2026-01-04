"""Error types for nostr-publish.

All errors follow fail-fast semantics per spec section 13.
"""


class NostrPublishError(Exception):
    """Base exception for all nostr-publish errors."""

    pass


class FrontmatterParseError(NostrPublishError):
    """Frontmatter is not valid YAML or has structural issues."""

    pass


class FrontmatterValidationError(NostrPublishError):
    """Frontmatter has invalid field values or types."""

    pass


class UnknownFieldError(FrontmatterValidationError):
    """Frontmatter contains unknown/unrecognized fields."""

    pass


class MissingFieldError(FrontmatterValidationError):
    """Required frontmatter field is missing."""

    pass


class InvalidFieldTypeError(FrontmatterValidationError):
    """Frontmatter field has incorrect type."""

    pass


class InvalidFieldValueError(FrontmatterValidationError):
    """Frontmatter field value violates constraints."""

    pass


class NoRelaysError(NostrPublishError):
    """No relays resolved from any source (CLI, frontmatter, defaults)."""

    pass


class NakInvocationError(NostrPublishError):
    """Failed to invoke or communicate with nak subprocess."""

    pass


class SigningError(NostrPublishError):
    """Signer rejected signing or signing failed."""

    pass


class PublishTimeoutError(NostrPublishError):
    """Operation timed out (signer interaction or publish)."""

    pass


class InvalidRelayURLError(NostrPublishError):
    """Relay URL is not a valid wss:// URL."""

    pass


class RelayNotInAllowlistError(NostrPublishError):
    """Frontmatter relay is not in CLI allowlist."""

    pass


class ImageProcessingError(NostrPublishError):
    """Failed to process image (EXIF stripping, resizing, etc.)."""

    pass


class BlossomUploadError(NostrPublishError):
    """Failed to upload to Blossom server."""

    pass
