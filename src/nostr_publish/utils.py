"""Shared utility functions for nostr-publish.

Common utilities used across multiple modules.
"""


def deduplicate_preserving_order(items: list[str]) -> list[str]:
    """Remove duplicates from list while preserving first occurrence order.

    CONTRACT:
      Inputs:
        - items: list of strings (may contain duplicates)

      Outputs:
        - unique_items: list of strings with duplicates removed, preserving order

      Invariants:
        - Output contains no duplicate strings (exact string comparison)
        - Relative order of first occurrences preserved
        - All elements in output exist in input

      Properties:
        - Idempotent: deduplicating twice yields same result
        - Order-preserving: first occurrence index maintained
        - Subset: output is subset of input

      Algorithm:
        1. Create empty set for tracking seen items
        2. Create empty list for result
        3. For each item in input (in order):
           a. If item not in seen set:
              - Add item to result list
              - Add item to seen set
        4. Return result list
    """
    seen = set()
    result = []
    for item in items:
        if item not in seen:
            result.append(item)
            seen.add(item)
    return result
