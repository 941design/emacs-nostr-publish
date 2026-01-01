# Publishing Guide

This project consists of two independently distributed packages:

| Package                      | Registry | Install Command                         |
|------------------------------|----------|-----------------------------------------|
| `nostr-publish` (Python CLI) | PyPI     | `pipx install nostr-publish`            |
| `nostr-publish` (Emacs)      | MELPA    | `M-x package-install RET nostr-publish` |

The GitHub repository is named `emacs-nostr-publish` to reflect the Emacs-first focus, while the PyPI package uses the shorter `nostr-publish` name for CLI convenience.

## Prerequisites

### PyPI

1. Create account at https://pypi.org
2. Create API token at https://pypi.org/manage/account/token/
3. Set environment variable: `export UV_PUBLISH_TOKEN=pypi-...`

For testing, create a separate account/token at https://test.pypi.org.

### MELPA

1. Fork https://github.com/melpa/melpa
2. The recipe is prepared in `recipes/nostr-publish`
3. Submit PR to MELPA with the recipe file

## Release Workflow

### 1. Bump Version

```bash
# Patch release (0.1.0 -> 0.1.1)
make version-patch

# Minor release (0.1.0 -> 0.2.0)
make version-minor

# Major release (0.1.0 -> 1.0.0)
make version-major
```

This updates both `pyproject.toml` and `nostr-publish.el` and creates a commit.

### 2. Create Git Tag

```bash
git tag -a v0.1.0 -m "Release v0.1.0"
git push origin master --tags
```

### 3. Publish to PyPI

```bash
# Test first (optional but recommended)
make publish-test
pip install -i https://test.pypi.org/simple/ nostr-publish

# Publish to production PyPI
make publish
```

### 4. MELPA

MELPA automatically picks up new commits from the repository. No manual action needed after the initial recipe is merged.

## First-Time Setup

### Initial PyPI Publish

```bash
# Verify package name is available
# Visit https://pypi.org/project/nostr-publish/ - should show 404

# Build and publish
export UV_PUBLISH_TOKEN=pypi-...
make publish
```

### Initial MELPA Submission

1. Copy `recipes/nostr-publish` to your MELPA fork under `recipes/`
2. Test locally per MELPA guidelines
3. Submit PR to https://github.com/melpa/melpa

## Verification

After publishing:

```bash
# Verify PyPI
pipx install nostr-publish
nostr-publish --help

# Verify MELPA (after PR merge + sync)
# In Emacs:
M-x package-refresh-contents
M-x package-install RET nostr-publish
```

## Troubleshooting

### "Package name already exists"

The PyPI name `nostr-publish` is reserved once first published. If you see this error, either:
- You're trying to publish to wrong account
- Someone else claimed the name (check pypi.org/project/nostr-publish/)

### "Invalid token"

Ensure `UV_PUBLISH_TOKEN` is set and starts with `pypi-`.

### MELPA Build Failures

Check https://melpa.org/packages/nostr-publish-badge.svg status. Common issues:
- Missing `provide` statement
- Invalid `Package-Requires` header
- Byte-compilation errors
