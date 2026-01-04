"""Shared fixtures for integration tests.

Provides Docker Compose services (relay, bunker, blossom) for E2E testing.

IMPORTANT: Port Configuration
-----------------------------
Integration tests read port configuration from environment variables:
- NOSTR_PUBLISH_RELAY_PORT (default: 8080)
- NOSTR_PUBLISH_BLOSSOM_PORT (default: 3000)

These are loaded from .env file (if present) via python-dotenv.
This allows tests to work regardless of invocation method (make vs pytest directly).

CAVEAT: Hardcoding relay URLs in test fixtures is dangerous. If a different
relay is running on that port (e.g., a production relay on localhost:8080),
tests could accidentally publish to it. Test fixtures should use:
- No relays field (defaults to CLI relays)
- relays: ["*"] (explicit wildcard, uses CLI relays)
"""

import json
import os
import shutil
import subprocess
import time
from pathlib import Path
from urllib.parse import quote

import pytest
from dotenv import load_dotenv

# Load .env file if present (enables running pytest directly without make)
load_dotenv()

# Port configuration from environment (with defaults matching .env.example)
RELAY_PORT = os.environ.get("NOSTR_PUBLISH_RELAY_PORT", "8080")
BLOSSOM_PORT = os.environ.get("NOSTR_PUBLISH_BLOSSOM_PORT", "3000")

# Derived URLs
DOCKER_COMPOSE_FILE = Path(__file__).parent / "docker-compose.yml"
RELAY_URL = f"ws://localhost:{RELAY_PORT}"
BLOSSOM_URL = f"http://localhost:{BLOSSOM_PORT}"

# nak bunker with secret key '1', authorized for client key '2'
BUNKER_PUBKEY = "79be667ef9dcbbac55a06295ce870b07029bfcdb2dce28d959f2815b16f81798"
# Client key '2' - authorized in bunker via --authorized-keys
CLIENT_SECRET = "0000000000000000000000000000000000000000000000000000000000000002"
# URL-encode the relay URL for bunker URI
BUNKER_URI = f"bunker://{BUNKER_PUBKEY}?relay={quote(RELAY_URL, safe='')}"
TEST_TIMEOUT = 60
STARTUP_TIMEOUT = 60


def _services_running(required_services: set[str]) -> bool:
    """Check if specified services are running."""
    compose_dir = DOCKER_COMPOSE_FILE.parent
    try:
        result = subprocess.run(
            ["docker", "compose", "-f", str(DOCKER_COMPOSE_FILE), "ps", "--format", "json"],
            cwd=compose_dir,
            capture_output=True,
            text=True,
            timeout=5,
        )
        if result.returncode == 0 and result.stdout.strip():
            services = [json.loads(line) for line in result.stdout.strip().split("\n") if line]
            running = {s.get("Service") for s in services if s.get("State") == "running"}
            return required_services.issubset(running)
    except Exception:
        pass
    return False


def _bunker_healthy() -> bool:
    """Check if bunker is ready by looking for 'listening at' in logs."""
    compose_dir = DOCKER_COMPOSE_FILE.parent
    try:
        result = subprocess.run(
            ["docker", "compose", "-f", str(DOCKER_COMPOSE_FILE), "logs", "bunker"],
            cwd=compose_dir,
            capture_output=True,
            text=True,
            timeout=5,
        )
        return "listening at" in result.stdout or "listening at" in result.stderr
    except Exception:
        return False


def _blossom_healthy() -> bool:
    """Check if Blossom server is responding."""
    try:
        result = subprocess.run(
            ["curl", "-s", "-o", "/dev/null", "-w", "%{http_code}", BLOSSOM_URL],
            capture_output=True,
            text=True,
            timeout=5,
        )
        # Blossom returns 200 for root or 404 for unknown paths - both indicate it's up
        return result.returncode == 0 and result.stdout.strip() in ["200", "404"]
    except Exception:
        return False


def _start_docker_services(required_services: set[str]) -> bool:
    """Start Docker Compose services if not running.

    Returns True if we started the services (and should clean up later).
    """
    compose_dir = DOCKER_COMPOSE_FILE.parent

    if _services_running(required_services):
        return False  # Already running, we didn't start them

    # Clean up any stale containers
    try:
        subprocess.run(
            ["docker", "compose", "-f", str(DOCKER_COMPOSE_FILE), "down", "-v"],
            cwd=compose_dir,
            capture_output=True,
            timeout=30,
        )
    except (subprocess.TimeoutExpired, FileNotFoundError):
        pass

    # Start services
    try:
        result = subprocess.run(
            ["docker", "compose", "-f", str(DOCKER_COMPOSE_FILE), "up", "-d"],
            cwd=compose_dir,
            capture_output=True,
            text=True,
            timeout=120,
        )
        if result.returncode != 0:
            pytest.fail(f"Failed to start Docker services: {result.stderr}")
    except subprocess.TimeoutExpired:
        pytest.fail("Docker Compose startup timed out")
    except FileNotFoundError:
        pytest.fail("docker not found")

    return True  # We started the services


def _wait_for_service(health_check, service_name: str, timeout: int = STARTUP_TIMEOUT):
    """Wait for a service to become healthy."""
    compose_dir = DOCKER_COMPOSE_FILE.parent
    deadline = time.time() + timeout

    while time.time() < deadline:
        if health_check():
            return
        time.sleep(2)

    # Timeout - get logs for debugging
    try:
        logs = subprocess.run(
            ["docker", "compose", "-f", str(DOCKER_COMPOSE_FILE), "logs", service_name],
            cwd=compose_dir,
            capture_output=True,
            text=True,
            timeout=5,
        )
        pytest.fail(f"{service_name} failed to become healthy. Logs:\n{logs.stdout}\n{logs.stderr}")
    except Exception:
        pytest.fail(f"{service_name} failed to become healthy (could not get logs)")


def _cleanup_docker_services():
    """Stop and remove Docker Compose services."""
    compose_dir = DOCKER_COMPOSE_FILE.parent
    try:
        subprocess.run(
            ["docker", "compose", "-f", str(DOCKER_COMPOSE_FILE), "down", "-v"],
            cwd=compose_dir,
            capture_output=True,
            timeout=30,
        )
    except Exception:
        pass


@pytest.fixture(scope="session")
def docker_services():
    """Start Docker Compose services for integration tests.

    Provides relay, nak bunker, and Blossom server for end-to-end testing.
    The nak bunker acts as a NIP-46 remote signer, communicating via the relay.
    Detects if services are already running (e.g., started by CI) and skips restart.
    Ensures all services are healthy before tests run.
    Only cleans up services if this fixture started them.
    """
    if not shutil.which("docker"):
        pytest.fail("Docker not available")

    if not shutil.which("nak"):
        pytest.fail("nak not available")

    required_services = {"relay", "bunker", "blossom"}

    # Start services if needed
    we_started_services = _start_docker_services(required_services)

    # Wait for bunker to be healthy
    _wait_for_service(_bunker_healthy, "bunker")

    # Wait for blossom to be healthy
    _wait_for_service(_blossom_healthy, "blossom")

    # Give services a moment to fully initialize
    time.sleep(2)

    yield {
        "relay_url": RELAY_URL,
        "blossom_url": BLOSSOM_URL,
        "bunker_uri": BUNKER_URI,
        "bunker_pubkey": BUNKER_PUBKEY,
        "client_secret": CLIENT_SECRET,
    }

    # Only tear down if we started the services
    if we_started_services:
        _cleanup_docker_services()
