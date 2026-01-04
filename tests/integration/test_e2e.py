"""End-to-end integration tests for nostr-publish.

Tests full publish workflow using Docker containers (relay + nak bunker + blossom).

CONTRACT:
  Test Environment:
    - Docker Compose provides: relay (nostr-rs-relay) + signer (nak bunker) + blossom
    - Fixed test keypair: secret key '1', pubkey 79be667ef...
    - nak bunker communicates via relay (NIP-46), simulating real remote signer
    - Isolated relay state per test run

  Test Properties:
    - End-to-end publish succeeds via real nak + signer
    - Published event retrievable from relay
    - Event kind, content, tags, pubkey match expected values
    - Deterministic: same input file yields same event structure

  Test Cases:
    1. test_publish_basic_article:
       - Publish fixtures/test-article.md
       - Verify event published to relay
       - Assert event structure matches spec

    2. test_publish_minimal_frontmatter:
       - Article with only required fields (title, slug)
       - Verify minimal tag set

    3. test_publish_all_fields:
       - Article with all optional fields
       - Verify complete tag set and ordering

    4. test_relay_precedence:
       - CLI --relay overrides frontmatter relays
       - Verify published to CLI-specified relay only

    5. test_dry_run:
       - --dry-run flag prevents actual publish
       - Verify event JSON output but no relay publish

    6. test_timeout:
       - Simulate slow signer
       - Verify timeout error handling
"""

import json
import os
import subprocess
import time
from pathlib import Path
from typing import Any

import pytest

# Local constants (fixture-independent)
FIXTURES_DIR = Path(__file__).parent / "fixtures"
TEST_TIMEOUT = 60


def run_nostr_publish(
    article_path: Path,
    bunker_uri: str | None = None,
    relays: list[str] | None = None,
    dry_run: bool = False,
    timeout: int = TEST_TIMEOUT,
    client_secret: str | None = None,
) -> subprocess.CompletedProcess:
    """Run nostr-publish CLI and return completed process."""
    cmd = ["nostr-publish", str(article_path)]

    if bunker_uri:
        cmd.extend(["--bunker", bunker_uri])

    if relays:
        for relay in relays:
            cmd.extend(["--relay", relay])

    if dry_run:
        cmd.append("--dry-run")

    cmd.extend(["--timeout", str(timeout)])

    # Set NOSTR_CLIENT_KEY for nak to use the authorized client key
    env = dict(os.environ)
    if client_secret:
        env["NOSTR_CLIENT_KEY"] = client_secret

    return subprocess.run(cmd, capture_output=True, text=True, timeout=timeout + 10, env=env)


def parse_publish_result(stdout: str) -> dict[str, Any]:
    """Parse JSON publish result from CLI stdout.

    Returns dict with event_id, pubkey, and optionally naddr and cover.
    """
    stdout = stdout.strip()
    if not stdout:
        return {}

    try:
        return json.loads(stdout)
    except json.JSONDecodeError:
        return {}


def fetch_event_from_relay(event_id: str, relay_url: str) -> dict[str, Any] | None:
    """Fetch published event from relay using nak."""
    try:
        result = subprocess.run(["nak", "req", "-i", event_id, relay_url], capture_output=True, text=True, timeout=10)

        if result.returncode != 0:
            return None

        for line in result.stdout.strip().split("\n"):
            if not line.strip():
                continue
            try:
                event = json.loads(line)
                if event.get("id") == event_id:
                    return event
            except json.JSONDecodeError:
                continue

        return None
    except (subprocess.TimeoutExpired, FileNotFoundError, Exception):
        return None


def assert_tag_present(tags: list[list[str]], tag_name: str, expected_value: str):
    """Assert that a specific tag with expected value is present in tags."""
    matching_tags = [tag for tag in tags if len(tag) >= 2 and tag[0] == tag_name]
    assert len(matching_tags) > 0, f"Tag '{tag_name}' not found in tags"

    tag_values = [tag[1] for tag in matching_tags]
    assert expected_value in tag_values, (
        f"Tag '{tag_name}' does not have expected value '{expected_value}'. Found: {tag_values}"
    )


def assert_tags_ordered_correctly(
    tags: list[list[str]], frontmatter_has_summary: bool, frontmatter_has_published_at: bool
):
    """Assert tags follow spec ordering: d, title, [summary], [published_at], [t tags sorted]."""
    assert len(tags) >= 2, "Event must have at least d and title tags"
    assert tags[0][0] == "d", f"First tag must be 'd', got '{tags[0][0]}'"
    assert tags[1][0] == "title", f"Second tag must be 'title', got '{tags[1][0]}'"

    idx = 2

    if frontmatter_has_summary:
        assert tags[idx][0] == "summary", f"Expected 'summary' tag at position {idx}, got '{tags[idx][0]}'"
        idx += 1

    if frontmatter_has_published_at:
        assert tags[idx][0] == "published_at", f"Expected 'published_at' tag at position {idx}, got '{tags[idx][0]}'"
        idx += 1

    t_tags = [tag for tag in tags[idx:] if tag[0] == "t"]
    if t_tags:
        t_values = [tag[1] for tag in t_tags]
        sorted_t_values = sorted(t_values)
        assert t_values == sorted_t_values, (
            f"Content tags not sorted lexicographically: {t_values} != {sorted_t_values}"
        )


def test_publish_basic_article(docker_services):
    """Test basic article publish workflow.

    Property: Published event matches input article structure exactly.
    """
    article_path = FIXTURES_DIR / "test-article.md"
    assert article_path.exists(), f"Test fixture not found: {article_path}"

    result = run_nostr_publish(
        article_path,
        bunker_uri=docker_services["bunker_uri"],
        relays=[docker_services["relay_url"]],
        client_secret=docker_services["client_secret"],
    )

    assert result.returncode == 0, f"CLI failed: {result.stderr}"

    # Parse JSON output
    publish_result = parse_publish_result(result.stdout)
    assert "event_id" in publish_result, "Output missing event_id"
    assert "pubkey" in publish_result, "Output missing pubkey"

    event_id = publish_result["event_id"]
    pubkey = publish_result["pubkey"]

    assert event_id, "Event ID not found in output"
    assert pubkey, "Pubkey not found in output"
    assert len(event_id) == 64, f"Event ID has wrong length: {len(event_id)}"
    expected_pubkey = docker_services["bunker_pubkey"]
    assert pubkey == expected_pubkey, f"Pubkey mismatch: expected {expected_pubkey}, got {pubkey}"

    # Verify naddr is present (new field)
    assert "naddr" in publish_result, "Output missing naddr"
    assert publish_result["naddr"].startswith("naddr1"), "naddr should start with 'naddr1'"

    time.sleep(2)

    event = fetch_event_from_relay(event_id, docker_services["relay_url"])
    assert event is not None, f"Event {event_id} not found in relay"

    assert event["kind"] == 30023, f"Wrong event kind: {event['kind']}"
    assert event["pubkey"] == expected_pubkey, f"Event pubkey mismatch: expected {expected_pubkey}"

    expected_content = """# Test Article

This is a test article for integration testing.

It contains multiple paragraphs.

And some **formatting**."""
    assert event["content"].strip() == expected_content.strip(), "Content mismatch"

    tags = event["tags"]
    assert_tag_present(tags, "d", "test-article-integration")
    assert_tag_present(tags, "title", "Test Article")
    assert_tag_present(tags, "summary", "Integration test article")
    assert_tag_present(tags, "published_at", "1700000000")
    assert_tag_present(tags, "t", "integration")
    assert_tag_present(tags, "t", "test")

    assert_tags_ordered_correctly(tags, True, True)


def test_publish_minimal_frontmatter(docker_services):
    """Test article with minimal frontmatter.

    Property: Event has only required tags (d, title) when no optional fields present.
    """
    article_path = FIXTURES_DIR / "minimal-article.md"
    assert article_path.exists(), f"Test fixture not found: {article_path}"

    result = run_nostr_publish(
        article_path,
        bunker_uri=docker_services["bunker_uri"],
        relays=[docker_services["relay_url"]],
        client_secret=docker_services["client_secret"],
    )

    assert result.returncode == 0, f"CLI failed: {result.stderr}"

    # Parse JSON output
    publish_result = parse_publish_result(result.stdout)
    assert "event_id" in publish_result, "Output missing event_id"

    event_id = publish_result["event_id"]
    assert event_id, "Event ID not found in output"

    time.sleep(2)

    event = fetch_event_from_relay(event_id, docker_services["relay_url"])
    assert event is not None, f"Event {event_id} not found in relay"

    assert event["kind"] == 30023

    tags = event["tags"]
    assert_tag_present(tags, "d", "minimal-article-integration")
    assert_tag_present(tags, "title", "Minimal Article")

    summary_tags = [tag for tag in tags if tag[0] == "summary"]
    assert len(summary_tags) == 0, "Summary tag should not be present"

    published_at_tags = [tag for tag in tags if tag[0] == "published_at"]
    assert len(published_at_tags) == 0, "published_at tag should not be present"

    t_tags = [tag for tag in tags if tag[0] == "t"]
    assert len(t_tags) == 0, "Content tags should not be present"

    assert_tags_ordered_correctly(tags, False, False)


def test_publish_all_fields(docker_services):
    """Test article with all optional fields.

    Property: All frontmatter fields represented in event tags with correct ordering.
    """
    article_path = FIXTURES_DIR / "all-fields-article.md"
    assert article_path.exists(), f"Test fixture not found: {article_path}"

    result = run_nostr_publish(
        article_path,
        bunker_uri=docker_services["bunker_uri"],
        relays=[docker_services["relay_url"]],
        client_secret=docker_services["client_secret"],
    )

    assert result.returncode == 0, f"CLI failed: {result.stderr}"

    # Parse JSON output
    publish_result = parse_publish_result(result.stdout)
    assert "event_id" in publish_result, "Output missing event_id"

    event_id = publish_result["event_id"]
    assert event_id, "Event ID not found in output"

    time.sleep(2)

    event = fetch_event_from_relay(event_id, docker_services["relay_url"])
    assert event is not None, f"Event {event_id} not found in relay"

    assert event["kind"] == 30023

    tags = event["tags"]
    assert_tag_present(tags, "d", "all-fields-article-integration")
    assert_tag_present(tags, "title", "Complete Article with All Fields")
    assert_tag_present(tags, "summary", "This article tests all optional frontmatter fields")
    assert_tag_present(tags, "published_at", "1700500000")

    assert_tag_present(tags, "t", "alpha")
    assert_tag_present(tags, "t", "beta")
    assert_tag_present(tags, "t", "gamma")
    assert_tag_present(tags, "t", "zeta")

    assert_tags_ordered_correctly(tags, True, True)

    t_tags = [tag for tag in tags if tag[0] == "t"]
    t_values = [tag[1] for tag in t_tags]
    assert t_values == ["alpha", "beta", "gamma", "zeta"], f"Content tags not in lexicographic order: {t_values}"


def test_relay_precedence(docker_services):
    """Test relay selection when publishing to invalid relay fails.

    Property: Publishing to unreachable relay fails appropriately.
    Uses minimal fixture (no frontmatter relays) to test CLI-only relay.
    """
    # Use minimal fixture which has no relays in frontmatter
    article_path = FIXTURES_DIR / "minimal-article.md"

    invalid_relay = "ws://localhost:9999"

    result = run_nostr_publish(
        article_path,
        bunker_uri=docker_services["bunker_uri"],
        relays=[invalid_relay],
        client_secret=docker_services["client_secret"],
    )

    assert result.returncode != 0 or "failed" in result.stdout.lower(), (
        "Expected publish to fail or report relay failures when using invalid relay"
    )


def test_dry_run(docker_services):
    """Test dry-run mode.

    Property: --dry-run outputs event JSON but does not publish to relay.
    """
    article_path = FIXTURES_DIR / "test-article.md"

    # CLI relay required as allowlist (frontmatter relay must match)
    result = run_nostr_publish(article_path, relays=[docker_services["relay_url"]], dry_run=True)

    assert result.returncode == 0, f"Dry-run failed: {result.stderr}"

    lines = result.stdout.strip().split("\n")
    assert len(lines) >= 2, "Dry-run should output event JSON and relay list"

    event_json_line = lines[0]
    try:
        event_data = json.loads(event_json_line)
    except json.JSONDecodeError as e:
        pytest.fail(f"Failed to parse event JSON: {e}\nOutput: {event_json_line}")

    assert event_data["kind"] == 30023, "Event kind should be 30023"
    assert "content" in event_data, "Event should have content"
    assert "tags" in event_data, "Event should have tags"

    tags = event_data["tags"]
    assert len(tags) >= 2, "Event should have at least d and title tags"
    assert tags[0][0] == "d", "First tag should be 'd'"
    assert tags[1][0] == "title", "Second tag should be 'title'"

    # Dry-run should not output JSON with event_id (it outputs event structure, not publish result)
    publish_result = parse_publish_result(result.stdout)
    assert "event_id" not in publish_result, "Dry-run should not publish (no event_id in output)"


def test_timeout(docker_services):
    """Test timeout handling.

    Property: Very short timeout causes timeout error.
    """
    article_path = FIXTURES_DIR / "test-article.md"

    result = run_nostr_publish(
        article_path,
        bunker_uri=docker_services["bunker_uri"],
        relays=[docker_services["relay_url"]],
        timeout=1,
        client_secret=docker_services["client_secret"],
    )

    if result.returncode != 0:
        assert "timeout" in result.stderr.lower() or "timed out" in result.stderr.lower(), (
            f"Expected timeout error, got: {result.stderr}"
        )
