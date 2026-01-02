"""End-to-end Emacs integration tests for nostr-publish.

Tests the complete Emacs → CLI → nak → bunker → relay flow using Docker infrastructure.

CONTRACT:
  Test Environment:
    - Docker Compose provides: relay (nostr-rs-relay) + signer (nak bunker)
    - Emacs runs in batch mode with nostr-publish.el loaded
    - Same bunker/relay configuration as Python CLI tests
    - Requires Emacs 27.1+ installed on host

  Test Properties:
    - End-to-end publish from Emacs succeeds
    - Event published to relay
    - Success/failure messages displayed correctly

  Test Cases:
    1. test_emacs_basic_publish: Full article via Emacs
    2. test_emacs_minimal_frontmatter: Minimal article via Emacs
    3. test_emacs_no_bunker_fails: Verify error without bunker URI
"""

import os
import shutil
import subprocess
from pathlib import Path

import pytest

# Import shared fixtures and constants from main e2e tests
from test_e2e import (
    BUNKER_PUBKEY,
    CLIENT_SECRET,
    RELAY_URL,
    docker_services,  # noqa: F401 - pytest fixture
)

# Paths
PROJECT_ROOT = Path(__file__).parent.parent.parent
NOSTR_PUBLISH_EL = PROJECT_ROOT / "nostr-publish.el"
TEST_EMACS_E2E_EL = Path(__file__).parent / "test_emacs_e2e.el"

# Bunker URI for tests (same format as Python tests)
BUNKER_URI = f"bunker://{BUNKER_PUBKEY}?relay=ws%3A%2F%2Flocalhost%3A8081"


@pytest.fixture(scope="module")
def emacs_available():
    """Check if Emacs is available and meets minimum version requirement."""
    if not shutil.which("emacs"):
        pytest.fail("Emacs not available - install Emacs 27.1+ to run integration tests")

    # Check version
    try:
        result = subprocess.run(["emacs", "--version"], capture_output=True, text=True, timeout=10)
        version_line = result.stdout.split("\n")[0]
        # Extract version number (e.g., "GNU Emacs 29.1")
        version_str = version_line.split()[-1]
        major_version = int(version_str.split(".")[0])
        if major_version < 27:
            pytest.fail(f"Emacs version {version_str} < 27.1 (minimum required)")
    except (subprocess.TimeoutExpired, ValueError, IndexError):
        pytest.fail("Could not determine Emacs version")

    return True


def run_emacs_tests(env_vars: dict) -> subprocess.CompletedProcess:
    """Run Emacs integration tests in batch mode.

    Args:
        env_vars: Environment variables to pass to Emacs subprocess

    Returns:
        CompletedProcess with stdout, stderr, and returncode
    """
    cmd = [
        "emacs",
        "--batch",
        "--quick",  # Don't load user init files
        "-l",
        str(NOSTR_PUBLISH_EL),
        "-l",
        str(TEST_EMACS_E2E_EL),
        "-f",
        "test-emacs-e2e-run",
    ]

    # Merge environment
    full_env = {**os.environ, **env_vars}

    return subprocess.run(cmd, capture_output=True, text=True, timeout=120, env=full_env)


def test_emacs_integration_full_suite(docker_services, emacs_available):  # noqa: F811
    """Run the full Emacs integration test suite.

    This test runs all Emacs E2E tests in a single batch invocation.
    Individual test results are reported in Emacs output.
    """
    env_vars = {"NOSTR_CLIENT_KEY": CLIENT_SECRET, "TEST_BUNKER_URI": BUNKER_URI, "TEST_RELAY_URL": RELAY_URL}

    result = run_emacs_tests(env_vars)

    # Print output for debugging
    print("\n=== Emacs stdout ===")
    print(result.stdout)
    if result.stderr:
        print("\n=== Emacs stderr ===")
        print(result.stderr)

    # Check for success
    assert result.returncode == 0, (
        f"Emacs integration tests failed with exit code {result.returncode}\n"
        f"stdout:\n{result.stdout}\n"
        f"stderr:\n{result.stderr}"
    )

    # Verify expected test output patterns
    assert "[PASS]" in result.stdout, "No passing tests found in output"
    assert "tests passed" in result.stdout, "Summary not found in output"


def test_emacs_files_exist():
    """Verify required Emacs files exist."""
    assert NOSTR_PUBLISH_EL.exists(), f"nostr-publish.el not found at {NOSTR_PUBLISH_EL}"
    assert TEST_EMACS_E2E_EL.exists(), f"test_emacs_e2e.el not found at {TEST_EMACS_E2E_EL}"


def test_emacs_syntax_valid(emacs_available):  # noqa: F811
    """Verify Emacs Lisp files have valid syntax."""
    # Test nostr-publish.el standalone
    result = subprocess.run(
        ["emacs", "--batch", "--quick", "-l", str(NOSTR_PUBLISH_EL), "--eval", '(message "Loaded successfully")'],
        capture_output=True,
        text=True,
        timeout=30,
    )
    assert result.returncode == 0, f"Failed to load nostr-publish.el:\n{result.stderr}"

    # Test test_emacs_e2e.el (requires nostr-publish.el to be loaded first)
    result = subprocess.run(
        [
            "emacs",
            "--batch",
            "--quick",
            "-l",
            str(NOSTR_PUBLISH_EL),
            "-l",
            str(TEST_EMACS_E2E_EL),
            "--eval",
            '(message "Loaded successfully")',
        ],
        capture_output=True,
        text=True,
        timeout=30,
    )
    assert result.returncode == 0, f"Failed to load test_emacs_e2e.el:\n{result.stderr}"
