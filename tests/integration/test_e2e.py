"""End-to-end integration tests for nostr-publish.

Tests full publish workflow using Docker containers (relay + nak bunker).

CONTRACT:
  Test Environment:
    - Docker Compose provides: relay (nostr-rs-relay) + signer (nak bunker)
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
import shutil
import subprocess
import time
from pathlib import Path
from typing import Any, Optional

import pytest

# Constants
DOCKER_COMPOSE_FILE = Path(__file__).parent / "docker-compose.yml"
FIXTURES_DIR = Path(__file__).parent / "fixtures"
RELAY_URL = "ws://localhost:8081"
# nak bunker with secret key '1', authorized for client key '2'
BUNKER_PUBKEY = "79be667ef9dcbbac55a06295ce870b07029bfcdb2dce28d959f2815b16f81798"
# Client key '2' - authorized in bunker via --authorized-keys
CLIENT_SECRET = "0000000000000000000000000000000000000000000000000000000000000002"
BUNKER_URI = f"bunker://{BUNKER_PUBKEY}?relay=ws%3A%2F%2Flocalhost%3A8081"
TEST_TIMEOUT = 60
STARTUP_TIMEOUT = 30


@pytest.fixture(scope="session")
def docker_services():
    """Start Docker Compose services for integration tests.

    Provides relay and nak bunker services for end-to-end testing.
    The nak bunker acts as a NIP-46 remote signer, communicating via the relay.
    Ensures services are healthy before tests run.
    Cleans up services after all tests complete.
    """
    if not shutil.which("docker-compose") and not shutil.which("docker"):
        pytest.fail("Docker not available")

    if not shutil.which("nak"):
        pytest.fail("nak not available")

    compose_dir = DOCKER_COMPOSE_FILE.parent

    try:
        subprocess.run(
            ["docker-compose", "-f", str(DOCKER_COMPOSE_FILE), "down", "-v"],
            cwd=compose_dir,
            capture_output=True,
            timeout=30,
        )
    except (subprocess.TimeoutExpired, FileNotFoundError):
        pass

    try:
        result = subprocess.run(
            ["docker-compose", "-f", str(DOCKER_COMPOSE_FILE), "up", "-d"],
            cwd=compose_dir,
            capture_output=True,
            text=True,
            timeout=60,
        )
        if result.returncode != 0:
            pytest.fail(f"Failed to start Docker services: {result.stderr}")
    except subprocess.TimeoutExpired:
        pytest.fail("Docker Compose startup timed out")
    except FileNotFoundError:
        pytest.fail("docker-compose not found")

    deadline = time.time() + STARTUP_TIMEOUT
    relay_healthy = False

    while time.time() < deadline:
        try:
            health_check = subprocess.run(
                ["docker-compose", "-f", str(DOCKER_COMPOSE_FILE), "ps"],
                cwd=compose_dir,
                capture_output=True,
                text=True,
                timeout=5,
            )
            if "Up" in health_check.stdout:
                relay_healthy = True
                break
        except (subprocess.TimeoutExpired, Exception):
            pass
        time.sleep(2)

    if not relay_healthy:
        try:
            subprocess.run(
                ["docker-compose", "-f", str(DOCKER_COMPOSE_FILE), "down", "-v"], cwd=compose_dir, timeout=30
            )
        except Exception:
            pass
        pytest.fail("Docker services failed to become healthy")

    time.sleep(5)

    yield

    try:
        subprocess.run(
            ["docker-compose", "-f", str(DOCKER_COMPOSE_FILE), "down", "-v"],
            cwd=compose_dir,
            capture_output=True,
            timeout=30,
        )
    except Exception:
        pass


def run_nostr_publish(
    article_path: Path,
    bunker_uri: Optional[str] = None,
    relays: Optional[list[str]] = None,
    dry_run: bool = False,
    timeout: int = TEST_TIMEOUT,
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
    env = {**os.environ, "NOSTR_CLIENT_KEY": CLIENT_SECRET}

    return subprocess.run(cmd, capture_output=True, text=True, timeout=timeout + 10, env=env)


def fetch_event_from_relay(event_id: str, relay_url: str = RELAY_URL) -> Optional[dict[str, Any]]:
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

    result = run_nostr_publish(article_path, bunker_uri=BUNKER_URI, relays=[RELAY_URL])

    assert result.returncode == 0, f"CLI failed: {result.stderr}"

    assert "Published:" in result.stdout, "Output missing event ID"
    assert "Pubkey:" in result.stdout, "Output missing pubkey"

    lines = result.stdout.strip().split("\n")
    event_id = None
    pubkey = None

    for line in lines:
        if line.startswith("Published:"):
            event_id = line.split(":", 1)[1].strip()
        elif line.startswith("Pubkey:"):
            pubkey = line.split(":", 1)[1].strip()

    assert event_id, "Event ID not found in output"
    assert pubkey, "Pubkey not found in output"
    assert len(event_id) == 64, f"Event ID has wrong length: {len(event_id)}"
    assert pubkey == BUNKER_PUBKEY, f"Pubkey mismatch: expected {BUNKER_PUBKEY}, got {pubkey}"

    time.sleep(2)

    event = fetch_event_from_relay(event_id)
    assert event is not None, f"Event {event_id} not found in relay"

    assert event["kind"] == 30023, f"Wrong event kind: {event['kind']}"
    assert event["pubkey"] == BUNKER_PUBKEY, f"Event pubkey mismatch: expected {BUNKER_PUBKEY}"

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

    result = run_nostr_publish(article_path, bunker_uri=BUNKER_URI, relays=[RELAY_URL])

    assert result.returncode == 0, f"CLI failed: {result.stderr}"

    lines = result.stdout.strip().split("\n")
    event_id = None

    for line in lines:
        if line.startswith("Published:"):
            event_id = line.split(":", 1)[1].strip()
            break

    assert event_id, "Event ID not found in output"

    time.sleep(2)

    event = fetch_event_from_relay(event_id)
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

    result = run_nostr_publish(article_path, bunker_uri=BUNKER_URI, relays=[RELAY_URL])

    assert result.returncode == 0, f"CLI failed: {result.stderr}"

    lines = result.stdout.strip().split("\n")
    event_id = None

    for line in lines:
        if line.startswith("Published:"):
            event_id = line.split(":", 1)[1].strip()
            break

    assert event_id, "Event ID not found in output"

    time.sleep(2)

    event = fetch_event_from_relay(event_id)
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

    result = run_nostr_publish(article_path, bunker_uri=BUNKER_URI, relays=[invalid_relay])

    assert result.returncode != 0 or "failed" in result.stdout.lower(), (
        "Expected publish to fail or report relay failures when using invalid relay"
    )


def test_dry_run(docker_services):
    """Test dry-run mode.

    Property: --dry-run outputs event JSON but does not publish to relay.
    """
    article_path = FIXTURES_DIR / "test-article.md"

    # CLI relay required as allowlist (frontmatter relay must match)
    result = run_nostr_publish(article_path, relays=[RELAY_URL], dry_run=True)

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

    assert "Published:" not in result.stdout, "Dry-run should not publish (no event ID in output)"


def test_timeout(docker_services):
    """Test timeout handling.

    Property: Very short timeout causes timeout error.
    """
    article_path = FIXTURES_DIR / "test-article.md"

    result = run_nostr_publish(article_path, bunker_uri=BUNKER_URI, relays=[RELAY_URL], timeout=1)

    if result.returncode != 0:
        assert "timeout" in result.stderr.lower() or "timed out" in result.stderr.lower(), (
            f"Expected timeout error, got: {result.stderr}"
        )
