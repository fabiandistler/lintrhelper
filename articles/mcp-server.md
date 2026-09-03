# Use lintrhelper from a coding agent

## What this gets you

`lintrhelper` ships an MCP (Model Context Protocol) server, so a coding
agent can ask for lints itself instead of a human pasting them into the
chat. The agent lints under whichever `.lintr` configuration your
project already has, so it sees the same rules you do.

## Before you start: the two Suggests

Writing linters needs nothing but this package. The MCP server
additionally needs `mcptools` and `ellmer`, which are **Suggests**
precisely so that people who only author linters are not made to install
the MCP stack:

``` r

pak::pak(c("mcptools", "ellmer"))
```

[`start_mcp_server()`](https://fabiandistler.github.io/lintrhelper/reference/start_mcp_server.md)
gates on both with
[`rlang::check_installed()`](https://rlang.r-lib.org/reference/is_installed.html).
In an interactive session that offers to install them. Under the
non-interactive `Rscript` launch every client below uses it cannot
prompt, so it errors instead — the client then reports the server as
failing to start, with the real reason on stderr. Installing the two
packages up front avoids that entirely.

## 1. Check the interpreter your agent will launch

Every configuration below starts the server with `Rscript`. That has to
be an `Rscript` on your `PATH` whose R library contains `lintrhelper` —
a copy installed into some other library (an IDE’s, an `renv` project’s)
is invisible to it. Confirm this before editing any config:

``` sh
Rscript --no-init-file --no-site-file -e 'packageVersion("lintrhelper")'
```

A version number means you are ready. An error means the package is not
in that library — install it there.

## 2. Register the server with your client

Put the configuration at your own project root. Working copies for all
three clients are committed at the root of [the lintrhelper
repository](https://github.com/fabiandistler/lintrhelper), so you can
copy one verbatim, or open that repo in your client to try it.

**Claude Code** — `.mcp.json`, project scope. Its 30 s default startup
timeout is ample, so no timeout key is needed:

``` json
{
  "mcpServers": {
    "lintrhelper": {
      "type": "stdio",
      "command": "Rscript",
      "args": [
        "--no-init-file",
        "--no-site-file",
        "-e",
        "lintrhelper::start_mcp_server()"
      ],
      "env": {}
    }
  }
}
```

The other two clients express the same launch differently. Rather than
transcribe them here, this is what changes:

|  | Claude Code | opencode | Codex |
|----|----|----|----|
| File | `.mcp.json` | `opencode.json` | `.codex/config.toml` |
| Format | JSON | JSON | **TOML** |
| Server type key | `"type": "stdio"` | `"type": "local"` | — |
| Command shape | `command` string + `args` array | single `command` **array** | `command` string + `args` array |
| Environment key | `env` | `environment` | — |
| Timeout | 30 s default, leave it | `"timeout": 60000` | `startup_timeout_sec = 20` |

Two traps in that table are worth spelling out. Codex reads **TOML, not
JSON**: a `codex.json` is ignored silently, which presents as “nothing
happens” rather than as an error. And a project-scoped Codex config only
takes effect in a **trusted project**, so trust the project in Codex
first.

One caveat: the Claude Code and opencode registrations have been driven
against a running server, the Codex one has not. Its shape follows
Codex’s documented config format and the file is checked in, but if it
misbehaves you are the first to find out — please open an issue rather
than assume you configured it wrong.

Two things worth knowing about all three:

- **The server name is yours to pick.** `lintrhelper` in the first two
  configs and `lintr` in the Codex one are client-side labels. Each
  client builds its tool names around the label you gave it, in whatever
  scheme that client uses — Claude Code shows the tools below as
  `mcp__lintrhelper__lint_file` and so on. The labels need not match
  across clients, and renaming one breaks nothing.
- **Keep `--no-init-file --no-site-file`.** Only JSON-RPC may reach
  stdout. Anything an `.Rprofile` or `Rprofile.site` prints arrives
  ahead of the handshake and breaks it — and `mcptools` itself tells
  users to put
  [`mcptools::mcp_session()`](https://posit-dev.github.io/mcptools/reference/server.html)
  into their `.Rprofile`, which makes this collision a live one rather
  than a hypothetical.

### Where the timeout numbers come from

Measured against the lintrhelper repository, the server answers
`initialize` in 1.3–4.0 s, and a first `lint_file` call has come back
around 8.4 s after launch — the R cold start plus loading lintr, paid
once. The two startup timeouts are sized against the first figure, so
Codex’s 20 s and Claude Code’s 30 s default both have room. opencode’s
`timeout` has to cover the tool call as well, and that is what puts its
5000 ms default out of reach and 60000 ms comfortably inside.

## 3. Ask for lints

Restart the client so it picks up the config, then ask the agent
something like “lint R/mcp_server.R” or “lint this project and fix what
it reports”. The agent calls the tools below on its own.

## The tools

| Tool | Arguments | What it does |
|----|----|----|
| `lint_file` | `path` (required), `project_dir` | Lints one R file under whichever `.lintr` config lintr finds for it. |
| `lint_project` | `dir`, `changed_only` | Lints a whole project in one call. A directory holding a `DESCRIPTION` goes through [`lintr::lint_package()`](https://lintr.r-lib.org/reference/lint.html), everything else through [`lintr::lint_dir()`](https://lintr.r-lib.org/reference/lint.html). With `changed_only = TRUE` the project is still linted whole and only the diagnostics for the files git reports as changed come back — working tree, index, and untracked files, but not what is already committed. |
| `list_rules` | `tags` | Reports the linters [`lintr::available_linters()`](https://lintr.r-lib.org/reference/available_linters.html) knows about, as name, providing package, and tags. Tags filter as a union; `tags = "default"` is the set that runs without a project `.lintr`. |
| `explain_rule` | `name` | Returns one linter’s title, description, usage, and arguments, read from its installed help page — so an agent rewriting code to satisfy a lint follows the rule rather than its guess at the rule. |

None of the four ever writes to your files, and all four carry MCP’s
read-only annotation, which is what lets a client call them without
stopping for approval.

The two linting tools return compact diagnostics — `filename`, `line`,
`column`, `type`, `message`, `linter` — grouped by file. A clean file or
project comes back empty rather than as an error. Mistakes an agent can
correct by itself, such as an unknown path or a directory where a file
was expected, come back as tool errors carrying the message, not as
JSON-RPC internal errors.

The project anchor follows the precedence tool argument, then
`CLAUDE_PROJECT_DIR`, then the server process’ working directory.
Relative paths resolve against it and lintr runs with it as the working
directory; discovering the config itself stays lintr’s own upward search
from the linted file.

## Not in this release

Deliberately out of scope, so nobody goes looking:

- **Rule distribution** (the team rules package) — shipping a shared
  rule set as its own versioned R package. It arrives once a concrete
  team rule exists to distribute.
- **Auto-fix** — the tools report lints, they never rewrite your code.
- **Agent config export** — generating `AGENTS.md` / `CLAUDE.md` entries
  from a rule set.
- **CI gate** — no workflow that fails a build on lints.
