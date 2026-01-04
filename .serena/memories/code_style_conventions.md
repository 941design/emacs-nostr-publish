# Code Style and Conventions

## Python Version
- Target: Python 3.9+ (use features available in 3.9)
- Configured in pyproject.toml: `target-version = "py39"`

## Formatting (ruff)
- Line length: 120 characters
- Skip magic trailing comma enabled
- Source directories: `src/`, `tests/`

## Linting Rules (ruff)
- E, W: pycodestyle errors and warnings
- F: Pyflakes
- I: isort (import sorting)
- B: flake8-bugbear
- C4: flake8-comprehensions
- UP: pyupgrade
- E501 ignored (handled by formatter)

## Naming Conventions
- Classes: PascalCase (e.g., `Frontmatter`, `UnsignedEvent`, `PublishResult`)
- Functions/Methods: snake_case (e.g., `parse_frontmatter`, `to_dict`)
- Variables: snake_case
- Constants: UPPER_SNAKE_CASE
- Private: prefix with underscore

## Type Hints
- **Required**: All function signatures must have type hints
- Use `X | None` for nullable types (Python 3.10+) or `Optional[T]` for 3.9 compat
- Use built-in generics: `list[T]`, `tuple[T, ...]`, `dict[K, V]` (Python 3.9+)
- Example: `def parse_frontmatter(markdown_content: str) -> tuple[dict | None, str]:`

## Docstrings
- Required for all public functions, classes, and modules
- Format: Triple quotes with brief description
- Contract-style docstrings for complex functions:
  - Inputs, Outputs, Invariants, Properties, Algorithm, Raises
- Example:
  ```python
  def parse_frontmatter(markdown_content: str) -> Tuple[Optional[dict], str]:
      """Extract YAML frontmatter and body from Markdown content.
      
      CONTRACT:
        Inputs:
          - markdown_content: string, UTF-8 Markdown text
        Outputs:
          - frontmatter_dict: dictionary or None
          - body: string
        Raises:
          - FrontmatterParseError: Invalid YAML syntax
      """
  ```

## Data Classes
- Use `@dataclass` for data models
- Define type hints for all fields
- Use `field(default_factory=list)` for mutable defaults
- Include `__post_init__` only when necessary

## Error Handling
- Use custom exception hierarchy (base: `NostrPublishError`)
- Specific exceptions for specific error types
- Fail-fast semantics: raise early and clearly
- Short, informative exception docstrings

## Imports
- Sorted by isort (configured via ruff)
- Known first-party: `nostr_publish`
- Split on trailing comma disabled

## Testing
- Test classes: `TestXxx` pattern
- Test functions: `test_xxx` pattern
- Use Hypothesis for property-based tests
- Use pytest markers for test categories
- Docstrings describe what the test verifies

## Design Patterns
- Dataclasses for immutable value objects
- Pure functions where possible
- Composition over inheritance
- No key management in production code
