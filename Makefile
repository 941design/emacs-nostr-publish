# Load environment variables from .env if it exists
-include .env
export

# Default values (used if .env not present)
NOSTR_PUBLISH_RELAY_PORT ?= 8080
NOSTR_PUBLISH_BLOSSOM_PORT ?= 3000

.PHONY: help sync sync-dev install build publish publish-test test test-unit test-e2e test-emacs test-emacs-compile test-emacs-unit \
        lint format format-md clean clean-emacs install-hooks version-patch version-minor version-major stack-up stack-down

help: ## Show this help message
	@grep -hE '^[a-zA-Z0-9_-]+:.*?## .*$$' $(MAKEFILE_LIST) | sort | awk 'BEGIN {FS = ":.*?## "}; {printf "\033[36m%-15s\033[0m %s\n", $$1, $$2}'

sync: ## Sync venv with production dependencies
	uv sync --no-dev

sync-dev: ## Sync venv with dev dependencies
	uv sync --extra dev

install: ## Install CLI tool globally via uv (editable)
	uv tool install --editable . --force

build: ## Build distribution packages
	uv build

publish-test: build ## Publish to TestPyPI
	uv publish --publish-url https://test.pypi.org/legacy/

publish: build ## Publish to PyPI (requires UV_PUBLISH_TOKEN)
	uv publish

test: test-unit test-e2e test-emacs ## Run all tests

test-unit: sync-dev ## Run unit tests only
	uv run pytest tests/unit/ -v

test-e2e: sync-dev ## Run integration tests only
	uv run pytest tests/integration/ -v

test-emacs: test-emacs-compile test-emacs-unit ## Run all Emacs tests (compile + unit)

test-emacs-compile: ## Byte-compile Emacs Lisp files
	emacs -Q --batch -f batch-byte-compile nostr-publish.el
	emacs -Q --batch -L . -f batch-byte-compile nostr-publish-tests.el
	@echo "Emacs byte-compilation complete"

test-emacs-unit: ## Run Emacs ERT unit tests
	emacs -Q --batch \
		-L . \
		-l nostr-publish.el \
		-l nostr-publish-tests.el \
		-f ert-run-tests-batch-and-exit

stack-up: ## Start local test stack (relay + signer + blossom + njump)
	@NOSTR_PUBLISH_BLOSSOM_PORT=$(NOSTR_PUBLISH_BLOSSOM_PORT) envsubst < tests/integration/blossom-config.yml.template > tests/integration/blossom-config.yml
	docker compose -f tests/integration/docker-compose.yml up -d

stack-down: ## Stop local test stack and remove volumes
	docker compose -f tests/integration/docker-compose.yml down -v

lint: ## Run linter and formatter check
	uv run ruff check src/ tests/
	uv run ruff format --check src/ tests/

format: ## Auto-fix linting and formatting issues
	uv run ruff check --fix src/ tests/
	uv run ruff format src/ tests/

format-md: ## Format markdown tables in all .md files
	npx markdown-table-formatter "**/*.md"

install-hooks: ## Install git hooks
	cp scripts/pre-commit .git/hooks/pre-commit
	cp scripts/pre-push .git/hooks/pre-push
	chmod +x .git/hooks/pre-commit .git/hooks/pre-push
	@echo "Git hooks installed"

clean: clean-emacs ## Clean build artifacts
	rm -rf build/
	rm -rf dist/
	rm -rf *.egg-info/
	rm -rf src/*.egg-info/
	rm -rf .pytest_cache/
	rm -rf .hypothesis/
	find . -type d -name __pycache__ -exec rm -rf {} + 2>/dev/null || true
	find . -type f -name "*.pyc" -delete 2>/dev/null || true

clean-emacs: ## Clean Emacs byte-compiled files
	rm -f *.elc 2>/dev/null || true
	@echo "Emacs byte-compiled files cleaned"

dry-run: ## Dry-run publish example (requires test fixture)
	uv run nostr-publish tests/integration/fixtures/test-article.md --relay ws://localhost:$(NOSTR_PUBLISH_RELAY_PORT) --dry-run

# Version management
CURRENT_VERSION := $(shell sed -n 's/^version = "\([^"]*\)"/\1/p' pyproject.toml)
VERSION_PARTS := $(subst ., ,$(CURRENT_VERSION))
MAJOR := $(word 1,$(VERSION_PARTS))
MINOR := $(word 2,$(VERSION_PARTS))
PATCH := $(word 3,$(VERSION_PARTS))

define bump-version
	@echo "Current version: $(CURRENT_VERSION)"
	@echo "New version: $(1)"
	@sed -i.bak 's/^version = "$(CURRENT_VERSION)"/version = "$(1)"/' pyproject.toml && rm pyproject.toml.bak
	@sed -i.bak 's/^;; Version: $(CURRENT_VERSION)/;; Version: $(1)/' nostr-publish.el && rm nostr-publish.el.bak
	@sed -i.bak 's/^__version__ = "$(CURRENT_VERSION)"/__version__ = "$(1)"/' src/nostr_publish/__init__.py && rm src/nostr_publish/__init__.py.bak
	@git add pyproject.toml nostr-publish.el src/nostr_publish/__init__.py
	@git commit -m "chore: bump version to $(1)"
	@echo "Version bumped to $(1) and committed"
endef

version-patch: ## Bump patch version (0.1.0 -> 0.1.1)
	$(call bump-version,$(MAJOR).$(MINOR).$(shell echo $$(($(PATCH)+1))))

version-minor: ## Bump minor version (0.1.0 -> 0.2.0)
	$(call bump-version,$(MAJOR).$(shell echo $$(($(MINOR)+1))).0)

version-major: ## Bump major version (0.1.0 -> 1.0.0)
	$(call bump-version,$(shell echo $$(($(MAJOR)+1))).0.0)
