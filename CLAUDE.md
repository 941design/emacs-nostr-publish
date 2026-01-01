# General Guidelines

- MUST use `uv` for package management and virtual environment.
- ALWAYS cleanup temporary files after implementation. That accounts for markdown files, as well as temporary backups of code.
- Do NOT create markdown documents for results unless absolutely necessary (e.g. for resuming a task) or when asked to.
- ALL implementation guides you create MUST be optimized for/addressed at AI coding agents.
- When asked for an opinion, ALWAYS provide a critical, balanced assessment looking at both pros and cons.
- NEVER rewrite git history.
- NEVER tailor production code towards tests. Production code MUST NOT contain adaptations, or tweaks that are necessary only to satisfy the test environment.
- DO NOT include recommendations for third party software or services, unless explicitly required by this project. This tool is agnostic to concreate implementations of the standards used.

## Documentation Guidelines

**specs/spec.md** — Software specification for agents
- High-level requirements, architecture, behavior, and acceptance criteria
- No concrete implementation advice unless there's a compelling functional/non-functional reason
- Avoid specific file paths, code snippets, or configuration JSON
- Focus on *what* and *why*, not *how*

**README.md** — For human readers
- Usage, installation, development workflow, and maintenance
- No implementation details; link to `docs/` for in-depth topics
- Keep command examples practical and copy-pasteable

**docs/** — Detailed guides
- Step-by-step procedures
- Implementation-level documentation when depth is needed
- Technical architecture details
