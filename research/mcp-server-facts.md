# R MCP server frameworks and a lintr-based MCP server: facts for a Go/No-Go (issue #10)

Research for [issue #10](https://github.com/fabiandistler/lintrhelper/issues/10) (wayfinder
ticket). State: August 2026. All claims cite primary sources: CRAN package pages, the package
repos/docs sites (posit-dev/mcptools, devOpifex/mcpr, tosidata/mcplite, tidyverse/ellmer),
client docs (opencode.ai, developers.openai.com/codex, code.claude.com), and the repo's own
prior research `research/lintr-state.md`.

## Verdict (short version)

A lintr-based MCP server is **fully feasible with existing, maintained R tooling**. The natural
base is Posit's **mcptools** (CRAN, stdio transport by default, tools defined as
`ellmer::tool()` objects with generated JSON Schema); **mcplite** (CRAN, Aug 2026) is a lean
stdio-only alternative. All three clients (opencode, Codex, Claude Code) register a stdio
server with a one-line `Rscript` command. The gap identified in issue #6 — no lintr-specific
MCP server exists — still holds as of Aug 2026. Recommended decision: **Go**, small effort.

## 1. R MCP server frameworks (2026)

### 1.1 mcptools (posit-dev) — the flagship, recommended base

- **What**: "Model Context Protocol Servers and Clients" for R. Server side: `mcp_server()`
  serves arbitrary R functions as MCP tools; client side: `mcp_tools()` registers third-party
  servers with ellmer chats. Source: CRAN page `cran.r-project.org/package=mcptools`.
- **Status**: on CRAN since 2026-03-17 (v0.2.1, R >= 4.1.0); lifecycle badge **experimental**;
  maintained by Posit (Simon Couch, Winston Chang, Charlie Gao); active development (docs site
  tracks 1.x). Dependencies: ellmer, httpuv, httr2, jsonlite, nanonext, processx, promises.
- **Transport**: `mcp_server(type = "stdio")` is the default; `type = "http"` (streamable HTTP)
  also supported, including deployment to Posit Connect (`_server.yml` with `engine: mcptools`
  + `tools: tools.R`, deployed as `contentCategory = "mcp"`). Source: mcptools reference
  `server.html`, vignette `R as an MCP server`.
- **Tool definitions with JSON Schema**: yes. `mcp_server(tools = ...)` takes either a list of
  `ellmer::tool()` objects or a path to an `.R` file that yields such a list. `ellmer::tool()`
  builds an S7 `ToolDef` with name, description, and per-argument schemas from `type_*()`
  helpers (`type_string()`, `type_integer()`, `type_number()`, `type_boolean()`, `type_enum()`,
  `type_array()`, `type_object()`, `type_ignore()`); `convert = TRUE` maps JSON inputs back to
  R types. Custom tool results are possible via `ContentToolResult`. Sources: ellmer reference
  `tool.html`; mcptools vignette `server.Rmd`.
- **Optional live-session integration**: `mcp_session()` in `.Rprofile` exposes interactive R
  sessions (RStudio/Positron) to the server; not required — without it, tools run in the
  server process itself. Not needed for a lint server.
- **Client wiring**: `Rscript -e "mcptools::mcp_server(tools = ...)"`; e.g. for Claude Code
  `claude mcp add -s "user" r-mcptools -- Rscript -e "mcptools::mcp_server()"`.
- **Companion**: Posit's **btw** package provides a ready-made tool set on top of mcptools
  (`btw_mcp_server()`); a useful model for how lintrhelper could ship a preconfigured server
  wrapper. Source: mcptools README.
- **Known constraint**: server must not write to stdout outside the JSON-RPC protocol (clients
  read stdout); console output should go to stderr. Relevant to this repo's own rule
  (message()/warning(), not cat()).

### 1.2 mcpr (devOpifex) — capable but single-maintainer, GitHub-only

- **What**: "Model Context Protocol for R" — server and client. `new_server()` / `new_mcp()`,
  `new_tool(name, description, input_schema, handler)`, `serve_io()` (stdio), `serve_http()`
  (HTTP via ambiorix); explicit JSON Schema via `schema()` / `properties()` /
  `property_string()`, `property_number()`, `property_enum()`, etc. Source: mcpr.opifex.org.
- **Status**: GitHub-only (no CRAN release), version 0.0.2.9000 on r-universe (built
  2026-07-04), GPL (>= 2), maintained by John Coene (Opifex). Lifecycle: experimental. Also
  ships an MCP roxygen2 roclet (`@mcp` tags) that generates a server file from package
  functions, and client functions (`new_client_io()`, `new_client_http()`) plus an
  `ellmer_to_mcpr_tool()` bridge.
- **Assessment**: functionally complete (tools, resources, prompts, client), but
  single-maintainer, not on CRAN, and masks `base::write` and `methods::initialize`. A
  separate older fork (chi2labs/mcpr) uses a different API (`mcp_http()`), which signals
  ecosystem churn.

### 1.3 mcplite (tosidata) — lean stdio-only alternative, new on CRAN

- **What**: "Lightweight Stdio MCP Server for R". `tool(fun, description, arguments, ...)`
  with `type_*()` schema helpers (including `type_from_schema()` for raw JSON Schema and
  `output_schema` for structured results via `tool_result()`); `mcp_server(tools)`. Source:
  CRAN page `cran.r-project.org/package=mcplite` (v0.1.0, published 2026-08-03, MIT, Juha
  Itkonen).
- **Scope**: stdio only; no HTTP, no prompts/resources/sampling; supports MCP protocol
  versions 2024-11-05, 2025-06-18, 2025-11-25; compatible `ellmer::tool()` objects accepted.
  Recommended launch pattern: a `server.R` ending in `mcplite::mcp_server(tools)`, started
  with `Rscript --vanilla /absolute/path/to/server.R`.
- **Assessment**: brand-new (repo created 2026-07-23) but protocol-aware and dependency-light;
  a credible second choice if a minimal footprint matters more than Posit backing.

### 1.4 Adjacent, not applicable

- **rmcp (finite-sample)**: a **Python** MCP server (PyPI) that shells out to R for statistical
  analysis (54 tools, 210 stars) — not an R package, not relevant for embedding in lintrhelper.
- **Linter MCP servers in other ecosystems** (established pattern from issue #6): official
  ESLint server, Ruff/ty (python-mcp), Vale; none for lintr.

### 1.5 Framework comparison (Aug 2026)

| | mcptools | mcpr | mcplite |
|---|---|---|---|
| CRAN | 0.2.1 (2026-03) | — (r-universe only) | 0.1.0 (2026-08-03) |
| Maintainer | Posit (Couch/Chang/Gao) | John Coene | Juha Itkonen |
| License | MIT | GPL >= 2 | MIT |
| stdio transport | yes (default) | yes (`serve_io()`) | yes (only) |
| HTTP transport | yes | yes | no |
| Tool JSON Schema | via ellmer `type_*()` | explicit `property_*()` | via `type_*()` |
| Structured results | via ContentToolResult | `response_text()` | `tool_result()` + output_schema |
| Extras | mcp_session(), Posit Connect, btw | roxygen2 roclet, MCP client | MCP client interop via ellmer |
| Risk | low (Posit-backed) | medium (solo, churn) | low-medium (new, focused) |

## 2. How a lintr-based MCP server concretely works

### 2.1 Process model and transport

- stdio: the client spawns `Rscript` with a small expression/script and speaks JSON-RPC 2.0
  over stdin/stdout. Cold start of an R process is ~1-3 s plus package load, which matters for
  client timeouts (see 3.4). All console output must go to stderr (`message()`), never stdout.
- Recommended skeleton with mcptools:
  `Rscript -e "lintrhelper::mcp_server()"` where `mcp_server()` calls
  `mcptools::mcp_server(tools = list(lint_file_tool, lint_project_tool, ...))`, or an `.R`-file
  argument pointing at `inst/mcp/tools.R` (the pattern mcptools already supports for Connect).

### 2.2 Tool definitions

Each tool is an `ellmer::tool()` with a verbose description (agents pick tools from
descriptions) and `type_*()` argument schemas. Candidate surface for v0.2:

- `lint_file(path, linter_file = NULL)` — lint one file with the project's resolved config;
  returns lints as JSON.
- `lint_project(dir = ".", ...)` — lint the whole project/package (wraps `lintr::lint_package()`
  or `lint_dir()` semantics), e.g. with a `changed_only = TRUE` git-aware mode like the
  python-mcp `changed_only` pattern.
- `list_rules(tags = NULL)` — `lintr::available_linters()` metadata (linter names, tags,
  descriptions); tag filter like `linters_with_tags()`.
- `explain_rule(name)` — rule documentation/description for an agent before fixing code.
- Optionally `get_config(dir)` — echo the resolved `.lintr`/`.lintr.R` settings.

Schemas are generated JSON Schema (2020-12) — supported by every client in section 3.

### 2.3 Config resolution (.lintr / .lintr.R)

- lintr's config discovery is a 5-step chain (per repo's own research `research/lintr-state.md`,
  citing lintr docs): `options("lintr.linter_file")` / `R_LINTR_LINTER_FILE` env var ->
  project-local `.lintr` (DCF) or `.lintr.R` (experimental R config, takes precedence) or
  `.github/linters/` child dir -> nearest parent directories upward -> `$HOME` -> user config
  dir. Function arguments (`linter_file =`) override everything.
- **Key design point**: the chain must be resolved **relative to the project**, not to the
  server process's cwd. Two mechanisms:
  1. per-call `dir`/`path` arguments — the agent (or a client-injected env var, see 3.4)
     supplies the project root;
  2. client-injected environment: Claude Code sets `CLAUDE_PROJECT_DIR` in every stdio server
     process; opencode passes `cwd` (workspace root) and the server inherits it as the working
     directory; Codex supports a per-server `cwd` option. Reading it in R:
     `Sys.getenv("CLAUDE_PROJECT_DIR")`, and `lintr::lint(path, linter_file = ...)` then
     resolves the config from the project root. Agents can also pass the project dir per call,
     which is more robust across clients.

### 2.4 Return format

- Serialize lint results to JSON for agents: each lint has `filename`, `line_number`,
  `column_number`, `type` (style/warning/error), `message`, `linter` — a compact structured
  diagnostic record. Keep output token-efficient (group by file, one line per lint), following
  the pattern of python-mcp ("Tool output is parsed structurally and reduced to compact
  diagnostics before it reaches the model").

## 3. Client configuration formats

### 3.1 opencode — `opencode.json` (project root, committed)

V1 (current stable, opencode.ai/docs/mcp-servers):

```jsonc
{
  "$schema": "https://opencode.ai/config.json",
  "mcp": {
    "lintr": {
      "type": "local",
      "command": ["Rscript", "-e", "lintrhelper::mcp_server()"],
      "cwd": ".",
      "environment": {},
      "enabled": true,
      "timeout": 10000
    }
  }
}
```

Caveats (V1): `command` must be an **array** (binary + args in one list); env vars go in
`environment`, **not** `env`; `cwd` resolves relative to the workspace; `timeout` (ms) governs
tool fetching and defaults to 5000 — raise it for a cold R start. V2 (rolling out,
opencode.ai/v2/docs/mcp-servers) moves servers under `mcp.servers` and renames `enabled` to
`disabled`, plus a `codemode` field. opencode reads `opencode.json` from the current working
directory, so agents must run at the project root.

### 3.2 Codex — TOML, not `codex.json`

Important correction to the ticket's premise: Codex stores MCP config in **`config.toml`**
(TOML tables), not `codex.json`. Sources: developers.openai.com/codex/mcp (primary),
corroborated by the codex issue tracker (JSON attempts fail silently; a proposal for a local
`cwd/mcp.json` — PR #12718 — was **closed/not merged**; per-project config stays
`.codex/config.toml`).

- Global: `~/.codex/config.toml`; project-scoped: `.codex/config.toml` (loaded only for
  **trusted** projects).
- Stdio server entry:

```toml
[mcp_servers.lintr]
command = "Rscript"
args = ["-e", "lintrhelper::mcp_server()"]
cwd = "."              # optional; working directory of the server
startup_timeout_sec = 20   # default 10 — raise for cold R start
tool_timeout_sec = 120     # default 60
default_tools_approval_mode = "auto"  # lint tools are read-only; "auto" avoids prompts
```

- CLI equivalent: `codex mcp add lintr -- Rscript -e "lintrhelper::mcp_server()"`.
- Further knobs: `env` (vars to set), `env_vars` (allowlist to forward), `enabled`,
  `enabled_tools`/`disabled_tools`, per-tool `approval_mode`. Config is shared with the
  ChatGPT desktop app and the IDE extension.

### 3.3 Claude Code — `.mcp.json` (project root, committed)

```json
{
  "mcpServers": {
    "lintr": {
      "type": "stdio",
      "command": "Rscript",
      "args": ["-e", "lintrhelper::mcp_server()"],
      "env": {}
    }
  }
}
```

- Project scope (team-shared, version-controlled); on first use Claude Code prompts for
  approval (`claude mcp reset-project-choices` re-asks). Alternatives: `--scope local`/`user`
  in `~/.claude.json`. CLI: `claude mcp add --scope project lintr -- Rscript -e "..."`.
- `type` defaults to `stdio` when `command` is present; `${VAR}` / `${VAR:-default}` expansion
  works in `command`, `args`, `env`.
- **Key feature for config resolution**: Claude Code injects `CLAUDE_PROJECT_DIR` (project
  root) into every stdio server process — the server can read it to anchor lintr's config
  search chain. Referencing it in `command`/`args` requires `${CLAUDE_PROJECT_DIR:-.}`.
- Startup timeout default 30 s (`MCP_TIMEOUT` env var overrides) — comfortable for R.

### 3.4 Cross-client caveats

- **R startup latency vs. client timeouts**: opencode default tool-fetch timeout is 5 s;
  Codex startup timeout is 10 s; Claude Code 30 s. A cold `Rscript` + package load can
  approach the opencode/Codex defaults; raise them (or keep the server warm) as shown above.
- **Project root anchoring** differs per client: Claude Code `CLAUDE_PROJECT_DIR`; opencode
  `cwd`/workspace (server inherits cwd); Codex `cwd` option or per-call arguments. Robust
  design: accept the project dir as a tool argument with an env-var fallback.
- **Read-only tools** should be declared as such where clients support it (Codex approval
  modes; MCP tool annotations via `tool_annotations()` in ellmer/mcptools) so agents can call
  them without prompts.

## 4. Implications for lintrhelper

- **Verdict: Go.** Building the MCP server is a small, well-trodden addition: a tools file
  (~100-150 lines of R wrapping lintr) plus three client config snippets documented in the
  README. No new framework needs to be written; the differentiated wedge from issue #7 is
  achievable.
- **Recommended stack**: mcptools (Posit-maintained, CRAN, ellmer interop, HTTP upgrade path)
  as runtime dependency; mcplite as documented alternative for lean installs. mcpr is not
  recommended (GitHub-only, solo maintainer, GPL).
- **Differentiation still open**: no lintr MCP server exists on GitHub as of Aug 2026 (issue
  #6 finding reconfirmed); no other R MCP server exposes linting — they execute R code or
  offer package-dev helpers (r-packagedev-mcp) without lintr.
- **Open design questions for the Go ticket (#9)**: tool naming/surface (2.2), whether to ship
  a `mcp_server()` helper in lintrhelper itself or as `inst/mcp/tools.R` consumed by
  mcptools, and which config file(s) to commit (`.mcp.json` is the natural team-shared one;
  opencode.json V1 vs V2 format depends on the team's opencode version).