# How coding agents consume linter rules (2026)

Research for wayfinder issue #6: how do coding agents (Claude Code, opencode, GitHub Copilot) consume linter rules, and what are the established mechanisms for giving agents custom lintr rules?

## 1. Rule carriers: instruction files (AGENTS.md / CLAUDE.md / copilot-instructions.md)

The dominant mechanism in 2026: linter rules are not "enforced" on agents, they are **written down in instruction files** that agents load into context, plus **executed as commands** the agent runs. The file formats:

- **opencode** — `AGENTS.md` is the rules carrier. Project rules at repo root, global rules at `~/.config/opencode/AGENTS.md`, both are loaded and *combined*. `CLAUDE.md` is used as a fallback if no `AGENTS.md` exists (V1 docs). The `opencode.json` `instructions` field can add extra files (e.g. `docs/development-standards.md`, globs like `packages/*/AGENTS.md`). `/init` generates AGENTS.md; the docs explicitly say it should capture "build, lint, and test commands". Since Feb 2026, `.opencode/AGENTS.md` is also loaded in addition to the local rule file (PR #12096). Note: V2 (new instructions engine) currently only discovers `AGENTS.md` — the CLAUDE.md fallback is V1 behavior (opencode V2 docs). Source: [opencode.ai/docs/rules](https://opencode.ai/docs/rules/), [opencode.ai/v2/docs/instructions](https://opencode.ai/v2/docs/instructions), [PR #12096](https://github.com/anomalyco/opencode/pull/12096).

- **Claude Code** — `CLAUDE.md` at project root (or `.claude/CLAUDE.md`), loaded at session start; nested CLAUDE.md files in subdirectories load on demand. `@path` imports organize content. `.claude/rules/*.md` rules with YAML `paths:` frontmatter scope rules to file globs (load only when matching files are read). Target < 200 lines; emphasis like `IMPORTANT`/`YOU MUST` improves adherence. **Important caveat (primary source):** CLAUDE.md is delivered as a *user message* after the system prompt, not as part of it — "Claude reads it and tries to follow it, but there's no guarantee of strict compliance." It is context, not configuration. Source: [code.claude.com/docs/en/claude-md](https://code.claude.com/docs/en/claude-md), [memory docs](https://code.claude.com/docs/en/memory).

- **GitHub Copilot** — three mechanisms: `.github/copilot-instructions.md` (repo-wide), `.github/instructions/*.instructions.md` (path-scoped via `applyTo:` glob frontmatter), and `AGENTS.md`/`CLAUDE.md`/`GEMINI.md` as agent instructions (nearest AGENTS.md wins). The docs' onboarding prompt for the Copilot cloud agent explicitly instructs documenting *lint* commands and validation pipelines. Source: [docs.github.com — repository custom instructions](https://docs.github.com/en/copilot/customizing-copilot/adding-repository-custom-instructions-for-github-copilot).

- **Cross-tool standard** — `AGENTS.md` is an open, cross-tool standard (agentsmd/agents.md), adopted by opencode, Copilot, Codex, Cursor, Gemini CLI, and read by Claude Code. It is the lowest-common-denominator carrier for "agents must run lintr" instructions. Source: [github.com/agentsmd/agents.md](https://github.com/agentsmd/agents.md), cited in GitHub Copilot docs.

**Pattern for "rules agents obey":** the consistent 2026 guidance across opencode/Claude Code/Copilot docs is the same: (a) put *lint commands and acceptance criteria* in instruction files ("run `lintr::lint_package()` before committing", "all lint must pass"), (b) treat instruction files as advisory — for *deterministic* enforcement use hooks/CI (sections 2–3), (c) keep files short, specific, and conflict-free.

## 2. MCP lint servers — agents calling linters as tools

2025–2026 established a real pattern: **linters ship MCP servers** so agents call linting as a tool instead of running CLI commands. Confirmed examples:

- **ESLint** — official MCP server inside the ESLint CLI (`npx @eslint/mcp`). Registered as an MCP server in VS Code/Cursor/Copilot, exposing lint tools that agents call ("Check this file for linting errors", "Fix all ESLint issues"). Source: [eslint.org/docs/latest/use/mcp](https://eslint.org/docs/latest/use/mcp).
- **Biome** — multiple MCP servers: [yriveiro/biome-mcp](https://github.com/yriveiro/biome-mcp) (structured, token-efficient diagnostics: `biome_lint`, `biome_check`), [RyuzakiShinji/biome-mcp-server](https://github.com/RyuzakiShinji/biome-mcp-server).
- **Ruff/ty/Vulture/Biome combo** — [mcp-server-analyzer](https://pypi.org/project/mcp-server-analyzer/) (ruff-check, ty-check, vulture-scan, biome-check).
- **Multi-linter** — [@paretools/lint](https://www.npmjs.com/package/@paretools/lint) (ESLint/Prettier/Biome structured diagnostics), vale MCP server ([theletterf/vale-mcp-server](https://github.com/theletterf/vale-mcp-server)).

**For R / lintr:** no lintr MCP server exists (GitHub search "lintr MCP server": 0 repositories, 2026-08). Existing R MCP servers ([saidsurucu/rlang-mcp-python](https://github.com/saidsurucu/rlang-mcp-python), its Go inspiration [gdbelvin/rlang-mcp-server](https://github.com/gdbelvin/rlang-mcp-server)) execute R code in Docker — they do not expose linting tools. MCP clients (opencode, Claude Code, Copilot) all support arbitrary local/remote MCP servers, so a lintr MCP server is a plausible but **unbuilt** integration point. Sources: [opencode.ai/docs/mcp-servers](https://opencode.ai/docs/mcp-servers/), [code.claude.com/docs/en/mcp](https://code.claude.com/docs/en/mcp).

## 3. Pre-commit / CI gates — enforcement agents cannot bypass

Instruction files are advisory; **pre-commit and CI are the hard gates** that agents' output must pass. Established R-specific mechanisms:

- **precommit package (lorenzwalthert/precommit)** — pre-commit hooks for R; ships a `lintr` hook (`id: lintr`) that runs `lintr::lint()` on staged `.R`/`.Rmd` files, respects the `.lintr` config, supports `--warn_only` (non-blocking) and `--load_package` (pkgload::load_all for package-aware linting). `precommit::use_precommit()` sets it up; runs locally and in CI. Sources: [lorenzwalthert.github.io/precommit](https://lorenzwalthert.github.io/precommit/articles/available-hooks.html), [github.com/lorenzwalthert/precommit](https://github.com/lorenzwalthert/precommit).
- **r-lib GitHub Actions** — official lintr CI workflows: `usethis::use_github_action("lint")` (lint_package), `"lint-project"` (lint_dir), `"lint-changed-files"`. They set `LINTR_ERROR_ON_LINT=true` so a lint failure fails the build and produces PR annotations. Sources: [r-lib/actions examples](https://github.com/r-lib/actions/blob/v2/examples/README.md), [lintr CI vignette](https://lintr.r-lib.org/articles/continuous-integration.html).
- **Claude Code hooks** — the deterministic equivalent in-agent: `PostToolUse` hooks (e.g. run a lint script after every `Edit|Write`) and `Stop` hooks (`npm run lint && npm test`, blocking) force the agent to self-correct before finishing. Source: [code.claude.com/docs/en/hooks](https://code.claude.com/docs/en/hooks).

## 4. Editor LSP integration

For humans (and as a possible agent feedback path):

- **languageserver** (R, the reference R LSP server, CRAN) integrates lintr natively: `r.lsp.diagnostics: true` (default) publishes lintr diagnostics via LSP `publishDiagnostics`. With lintr v2.0.0+ it reads the `.lintr` project/home config. Works in VS Code via vscode-R (lintr diagnostics by default), RStudio Markers pane, Emacs lsp-mode, JupyterLab, Nova. Sources: [lintr editors vignette](https://cran.r-project.org/web/packages/lintr/vignettes/editors.html), [REditorSupport/languageserver](https://github.com/REditorSupport/languageserver), [vscode-R wiki: Code linting](https://github.com/REditorSupport/vscode-R/wiki/Code-linting).
- **Rust-based R servers** — lsp-r (REditorSupport/lsp-r) does **not** integrate lintr (no lint/lintr references in repo or README); newer entrants `ry` (felix-andreas/ry: "check" = lint + type-check) and `arity` (jolars/arity: linter + formatter + LSP) are alternatives but don't use lintr.
- **opencode LSP** — V1: opencode can use LSP servers and "use diagnostics as feedback for the agent"; built-in server list covers common languages but **no R server**; custom servers can be configured (so `languageserver` could be wired up manually). V2: LSP config schema exists but "does not currently use LSP" at runtime — config only. Sources: [opencode.ai/docs/lsp](https://opencode.ai/docs/lsp/), [opencode.ai/v2/docs/lsp](https://opencode.ai/v2/docs/lsp).
- **Claude Code** — does not run LSP servers; diagnostics flow via hooks, MCP tools, or CLI output in the conversation.

## 5. Established patterns for giving agents custom lintr rules

- **Instruction-file pattern (widespread):** GitHub code search finds ~2,752 public `AGENTS.md` files mentioning `lintr` (2026-08) — documenting lintr usage in agent instruction files is an established, common pattern (e.g. "run lintr before committing", CI gate references).
- **Custom rules stay in `.lintr`:** lintr's canonical customization point is the project `.lintr` config file (and `linters_with_defaults()`), which is respected by all integration points above (languageserver diagnostics, precommit hook, CI). Agents discover and read `.lintr` like any other config file; the instruction file just tells them to run lint.
- **Recommended stack per the docs:** instructions (AGENTS.md/CLAUDE.md: "run `lintr::lint_package()` with `LINTR_ERROR_ON_LINT=true`") + deterministic gate (precommit hook and/or r-lib lint GitHub Action) + optionally a Claude Code `PostToolUse` hook. This mirrors the general 2026 guidance that formatting/style rules don't belong in instruction files — they belong in the linter, wired into the agent loop. Source: [lintr continuous integration vignette](https://lintr.r-lib.org/articles/continuous-integration.html), Claude Code best practices via [code.claude.com/docs/en/claude-md](https://code.claude.com/docs/en/claude-md).

## 6. How opencode / Claude Code actually surface lintr diagnostics today

- **opencode:** no R LSP server ships built-in; V2 doesn't run LSP yet at all. The realistic path today: the agent runs `lintr` itself via Bash (`lintr::lint_dir()`, `lintr::lint_package()`), and the diagnostics land in the conversation as command output — which the docs explicitly recommend over LSP ("better to have the agent run lint, typecheck, or other diagnostic CLI tools directly... Document those commands in instruction files such as AGENTS.md"). A custom `lsp` entry pointing at `languageserver` is possible in V1 but requires manual setup and is discouraged by the docs. Source: [opencode.ai/docs/lsp](https://opencode.ai/docs/lsp/).
- **Claude Code:** no built-in R linting; diagnostics surface through (a) hook output injected into context (PostToolUse/Stop hooks), (b) an MCP server if one exists (none for lintr yet), (c) the agent running `lintr::lint_package()` via Bash after edits. The hooks reference shows exactly this pattern with a lint script on `Edit|Write` matcher. Source: [code.claude.com/docs/en/hooks](https://code.claude.com/docs/en/hooks).
- **GitHub Copilot:** surfaces lint via ESLint MCP server in the IDE (section 2); for R, only via instructions + CI review.

**Bottom line for the wayfinding effort:** the established 2026 stack is *instructions (AGENTS.md/CLAUDE.md/copilot-instructions.md) + deterministic gates (precommit lintr hook, r-lib lint Action) + agent-run lintr CLI output in context*. MCP lint servers are a growing pattern (ESLint/Biome/Ruff all have them) but no lintr MCP server exists — a concrete gap/opportunity. LSP (languageserver) serves editors and RStudio, not agent loops.

## Sources

- https://opencode.ai/docs/rules/ — opencode AGENTS.md rules, precedence, /init
- https://opencode.ai/v2/docs/instructions — opencode V2 instruction loading (AGENTS.md only)
- https://github.com/anomalyco/opencode/pull/12096 — `.opencode/AGENTS.md` support
- https://code.claude.com/docs/en/claude-md — CLAUDE.md, .claude/rules/, imports, 200-line guidance, user-message caveat
- https://code.claude.com/docs/en/hooks — PostToolUse lint hook example, Stop hook
- https://docs.github.com/en/copilot/customizing-copilot/adding-repository-custom-instructions-for-github-copilot — copilot-instructions.md, .github/instructions/, AGENTS.md/CLAUDE.md/GEMINI.md
- https://github.com/agentsmd/agents.md — AGENTS.md open standard
- https://eslint.org/docs/latest/use/mcp — official ESLint MCP server
- https://github.com/yriveiro/biome-mcp — Biome MCP server with lint tools
- https://pypi.org/project/mcp-server-analyzer/ — Ruff/ty/Vulture/Biome MCP server
- https://www.npmjs.com/package/@paretools/lint — multi-linter MCP server
- https://github.com/saidsurucu/rlang-mcp-python — R execution MCP server (no linting)
- https://opencode.ai/docs/mcp-servers/ — opencode MCP config
- https://opencode.ai/docs/lsp/ and https://opencode.ai/v2/docs/lsp — opencode LSP (V1 diagnostics feedback; V2 config-only)
- https://lorenzwalthert.github.io/precommit/articles/available-hooks.html — precommit `lintr` hook
- https://github.com/lorenzwalthert/precommit — pre-commit hooks for R
- https://github.com/r-lib/actions/blob/v2/examples/README.md — lint / lint-project / lint-changed-files workflows, LINTR_ERROR_ON_LINT
- https://lintr.r-lib.org/articles/continuous-integration.html — lintr CI vignette
- https://cran.r-project.org/web/packages/lintr/vignettes/editors.html — lintr editor integrations
- https://github.com/REditorSupport/languageserver — R LSP server, `r.lsp.diagnostics` via lintr
- https://github.com/REditorSupport/vscode-R/wiki/Code-linting — vscode-R lintr diagnostics
- https://github.com/REditorSupport/lsp-r — Rust R LSP server (no lintr integration)
- https://github.com/felix-andreas/ry and https://github.com/jolars/arity — Rust R toolchains with their own linting
- GitHub code search (2026-08): `lintr filename:AGENTS.md` → 2,752 files