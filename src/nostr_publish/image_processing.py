"""Image processing for cover images.

Handles EXIF stripping, resizing, and normalization for Blossom upload.
"""

from pathlib import Path

# Minimum dimension constraints for cover images
MIN_DIMENSION = 200  # Minimum width or height in pixels
MIN_ASPECT_RATIO = 1 / 4  # Minimum aspect ratio (width/height) - prevents very tall images
MAX_ASPECT_RATIO = 4 / 1  # Maximum aspect ratio (width/height) - prevents very wide images


def validate_image_dimensions(width: int, height: int, min_dimension: int = MIN_DIMENSION) -> tuple[bool, str | None]:
    """Validate image dimensions for suitability as a cover image.

    CONTRACT:
      Inputs:
        - width: positive integer, image width in pixels
        - height: positive integer, image height in pixels
        - min_dimension: positive integer, minimum allowed dimension (default 200)

      Outputs:
        - tuple: (is_valid, error_message)
          * is_valid: boolean, True if dimensions are acceptable
          * error_message: string or None, human-readable error if invalid

      Invariants:
        - Both width and height must be at least min_dimension
        - Aspect ratio must be between MIN_ASPECT_RATIO and MAX_ASPECT_RATIO

      Properties:
        - Deterministic: same inputs always yield same result
        - Pure function: no side effects

      Algorithm:
        1. Check minimum dimension:
           a. If width < min_dimension, return (False, "too narrow")
           b. If height < min_dimension, return (False, "too short")
        2. Check aspect ratio:
           a. Calculate aspect_ratio = width / height
           b. If aspect_ratio < MIN_ASPECT_RATIO, return (False, "too tall")
           c. If aspect_ratio > MAX_ASPECT_RATIO, return (False, "too wide")
        3. Return (True, None)
    """
    if width < min_dimension:
        return (False, f"image too narrow: {width}px (minimum {min_dimension}px)")

    if height < min_dimension:
        return (False, f"image too short: {height}px (minimum {min_dimension}px)")

    aspect_ratio = width / height

    if aspect_ratio < MIN_ASPECT_RATIO:
        return (False, f"image aspect ratio too extreme (too tall): {width}x{height}")

    if aspect_ratio > MAX_ASPECT_RATIO:
        return (False, f"image aspect ratio too extreme (too wide): {width}x{height}")

    return (True, None)


def process_cover_image(
    input_path: str, output_path: str, target_width: int = 1200, target_height: int = 630
) -> tuple[str, int, int]:
    """Process cover image: strip EXIF, resize, and normalize.

    CONTRACT:
      Inputs:
        - input_path: string, absolute path to input image file
        - output_path: string, absolute path where processed image will be saved
        - target_width: positive integer, target width in pixels (default 1200)
        - target_height: positive integer, target height in pixels (default 630)

      Outputs:
        - tuple: (processed_path, actual_width, actual_height)
          * processed_path: string, absolute path to the processed image file (equals output_path)
          * actual_width: positive integer, actual width of processed image
          * actual_height: positive integer, actual height of processed image

      Invariants:
        - Input file must exist and be readable
        - Input file must be PNG, JPEG, or WebP
        - Output file is JPEG format
        - Output file has all EXIF metadata stripped
        - Output dimensions match target dimensions if input is larger
        - If input is smaller than target, do NOT upscale (preserve original dimensions)
        - Aspect ratio preserved via center-crop if needed to match target exactly

      Properties:
        - Deterministic: same input file produces identical output bytes (best-effort, Pillow-dependent)
        - Idempotent: processing already-processed image produces same result
        - Fail-fast: raises on unsupported format or processing error

      Algorithm:
        1. Load image from input_path:
           a. Open image using Pillow (PIL)
           b. Verify format is PNG, JPEG, or WebP
           c. If unsupported format, raise ImageProcessingError
        2. Strip EXIF metadata:
           a. Remove all EXIF data from image object
           b. If stripping fails, raise ImageProcessingError
        3. Determine output dimensions:
           a. Get input dimensions (width, height)
           b. Compare to target dimensions
           c. If input is smaller than target in both dimensions:
              - Use input dimensions (no upscale)
           d. If input is larger or aspect ratio differs:
              - Calculate scale factor to cover target dimensions
              - Apply center-crop to match target exactly
        4. Resize image:
           a. Use high-quality resampling (e.g., Lanczos)
           b. Apply center-crop if aspect ratios differ
           c. Result dimensions must match calculated output dimensions
        5. Save processed image:
           a. Create output directory if needed
           b. Save as JPEG format
           c. Use quality=95 for high quality
           d. Ensure no EXIF data in output
        6. Return tuple: (output_path, actual_width, actual_height)

      Error Handling:
        - Unsupported image format → ImageProcessingError with format name
        - File I/O error → ImageProcessingError with file path
        - EXIF stripping failure → ImageProcessingError
        - Resize failure → ImageProcessingError

      Raises:
        - ImageProcessingError: Any processing failure (format, I/O, manipulation)

      Example:
        Input: 3000x2000 image, target 1200x630
        Algorithm:
          - Scale factor: max(1200/3000, 630/2000) = max(0.4, 0.315) = 0.4
          - Scaled: 1200x800
          - Center-crop to 1200x630: remove 85px from top and bottom
        Output: 1200x630 JPEG

        Input: 800x400 image, target 1200x630
        Algorithm:
          - Input smaller than target
          - No upscale policy applies
        Output: 800x400 JPEG (original dimensions preserved)
    """
    from PIL import Image

    from nostr_publish.errors import ImageProcessingError

    try:
        with Image.open(input_path) as img:
            format_name = img.format or "unknown"
            if format_name not in ("PNG", "JPEG", "WEBP"):
                raise ImageProcessingError(f"unsupported image format: {format_name}")

            img_rgb = img.convert("RGB")

            data = img_rgb.getexif()
            if data:
                img_rgb.getexif().clear()

            input_width, input_height = img_rgb.size

            # Validate dimensions before processing
            is_valid, error_msg = validate_image_dimensions(input_width, input_height)
            if not is_valid:
                raise ImageProcessingError(error_msg)

            if input_width < target_width and input_height < target_height:
                actual_width, actual_height = input_width, input_height
                output_image = img_rgb
            else:
                scale_factor = max(target_width / input_width, target_height / input_height)
                scaled_width = int(input_width * scale_factor)
                scaled_height = int(input_height * scale_factor)

                output_image = img_rgb.resize((scaled_width, scaled_height), Image.Resampling.LANCZOS)

                left = (scaled_width - target_width) // 2
                top = (scaled_height - target_height) // 2
                right = left + target_width
                bottom = top + target_height

                output_image = output_image.crop((left, top, right, bottom))
                actual_width, actual_height = target_width, target_height

        output_dir = Path(output_path).parent
        output_dir.mkdir(parents=True, exist_ok=True)

        # Auto-cleanup: remove old cached cover if exists (keeps cache minimal)
        output_file = Path(output_path)
        if output_file.exists():
            output_file.unlink()

        output_image.save(output_path, format="JPEG", quality=95, exif=b"")

        return (output_path, actual_width, actual_height)

    except ImageProcessingError:
        raise
    except FileNotFoundError as e:
        raise ImageProcessingError(f"input file not found: {input_path}") from e
    except Exception as e:
        raise ImageProcessingError(f"failed to process image: {str(e)}") from e


def get_processed_cover_path(markdown_file_path: str, slug: str) -> str:
    """Generate deterministic path for processed cover image.

    CONTRACT:
      Inputs:
        - markdown_file_path: string, absolute path to Markdown file
        - slug: string, article slug from frontmatter

      Outputs:
        - processed_path: string, absolute path to processed cover image
          * Format: <markdown_dir>/.nostr-publish/cache/covers/<slug>/cover.jpg

      Invariants:
        - Output path is always absolute
        - Output path is deterministic for same inputs
        - Output directory may not exist yet (created during processing)

      Properties:
        - Deterministic: same inputs always yield same path
        - Idempotent: calling multiple times with same inputs returns same path

      Algorithm:
        1. Get directory of markdown_file_path:
           a. Use pathlib.Path(markdown_file_path).parent
        2. Construct cache directory path:
           a. Join with ".nostr-publish/cache/covers/<slug>"
        3. Construct file path:
           a. Join cache directory with "cover.jpg"
        4. Return absolute path

      Note: Does NOT create directory or check existence - creation happens during processing.
    """
    md_dir = Path(markdown_file_path).parent
    cache_dir = md_dir / ".nostr-publish" / "cache" / "covers" / slug
    return str(cache_dir / "cover.jpg")
