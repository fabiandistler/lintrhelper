# lintrhelper

> Helper Functions for Creating Custom Lintr Rules - **No XPath
> Knowledge Required!**

`lintrhelper` makes it incredibly easy to create custom linters for the
[lintr](https://lintr.r-lib.org/) package. You don’t need to know XPath,
XML, or parse trees - just use simple, intuitive functions to describe
what you want to lint!

It also ships an **MCP server**, so coding agents — Claude Code,
opencode, Codex — can ask for lints themselves and work from the same
rules you do. See [Use it from your coding
agent](#use-it-from-your-coding-agent-mcp-server).

📖 **Documentation site:**
<https://fabiandistler.github.io/lintrhelper/>

## Installation

Install the development version from GitHub with `pak` (recommended) or
`devtools`:

``` r

# install.packages("pak")
pak::pak("fabiandistler/lintrhelper")

# or
# install.packages("devtools")
devtools::install_github("fabiandistler/lintrhelper")
```

### On a machine without GitHub access

Install from an internal git mirror with the same `pak` call, pointed at
the mirror instead of the public repo:

``` r

pak::pak("git::https://git.internal.example/r-packages/lintrhelper.git")
```

`git.internal.example` is a placeholder — substitute your team’s actual
mirror host. `pak` speaks git directly, so nothing beyond credentials
for that host is required.

### Authoring only, or the MCP server too?

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

## Use it from your coding agent (MCP server)

`lintrhelper` ships an MCP (Model Context Protocol) server, so a coding
agent can ask for lints itself instead of a human pasting them into the
chat. The agent lints under whichever `.lintr` configuration your
project already has, so it sees the same rules you do.

### 1. Check the interpreter your agent will launch

Every snippet below starts the server with `Rscript`. That has to be an
`Rscript` on your `PATH` whose R library contains `lintrhelper` — a copy
installed into some other library (an IDE’s, an `renv` project’s) is
invisible to it. Confirm this before editing any config:

``` sh
Rscript --no-init-file --no-site-file -e 'packageVersion("lintrhelper")'
```

A version number means you are ready. An error means the package is not
in that library — install it there.

### 2. Register the server with your client

All three snippets are committed at this repo’s root, so you can copy
one verbatim or just open this repo in your client to try it. Put the
snippet at your own project root.

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

**opencode** — `opencode.json`. Note `type: "local"`, `command` as an
**array** rather than a string, and `environment` rather than `env`. The
5000 ms default timeout is too tight once the first lint is added to the
R cold start, hence 60000:

``` json
{
  "$schema": "https://opencode.ai/config.json",
  "mcp": {
    "lintrhelper": {
      "type": "local",
      "command": ["Rscript", "--no-init-file", "--no-site-file", "-e", "lintrhelper::start_mcp_server()"],
      "environment": {},
      "timeout": 60000
    }
  }
}
```

**Codex** — `.codex/config.toml`. Codex reads **TOML, not JSON**: a
`codex.json` is ignored silently, which presents as “nothing happens”
rather than as an error. `startup_timeout_sec` is raised from its
default of 10. A project-scoped config only takes effect in a **trusted
project**, so trust the project in Codex first:

``` toml
[mcp_servers.lintr]
command = "Rscript"
args = ["--no-init-file", "--no-site-file", "-e", "lintrhelper::start_mcp_server()"]
startup_timeout_sec = 20
```

One caveat on this last one: the Claude Code and opencode registrations
have been driven against a running server, the Codex one has not. Its
shape follows Codex’s documented config format and the file is checked
in here, but if it misbehaves you are the first to find out — please
open an issue rather than assume you configured it wrong.

Two things worth knowing about all three:

- **The server name is yours to pick.** `lintrhelper` in the first two
  snippets and `lintr` in the Codex one are client-side labels. Each
  client builds its tool names around the label you gave it, in whatever
  scheme that client uses — Claude Code shows the tools above as
  `mcp__lintrhelper__lint_file` and so on. The labels need not match
  across clients, and renaming one breaks nothing.
- **Keep `--no-init-file --no-site-file`.** Only JSON-RPC may reach
  stdout. Anything an `.Rprofile` or `Rprofile.site` prints arrives
  ahead of the handshake and breaks it — and `mcptools` itself tells
  users to put
  [`mcptools::mcp_session()`](https://posit-dev.github.io/mcptools/reference/server.html)
  into their `.Rprofile`, which makes this collision a live one rather
  than a hypothetical.

Where the timeout numbers come from: measured against this repo, the
server answers `initialize` in 1.3–4.0 s, and a first `lint_file` call
has come back around 8.4 s after launch — the R cold start plus loading
lintr, paid once. The two startup timeouts are sized against the first
figure, so Codex’s 20 s and Claude Code’s 30 s default both have room.
opencode’s `timeout` has to cover the tool call as well, and that is
what puts its 5000 ms default out of reach and 60000 ms comfortably
inside.

### 3. Ask for lints

Restart the client so it picks up the config, then ask the agent
something like “lint R/mcp_server.R” or “lint this project and fix what
it reports”. The agent calls the tools below on its own.

### The tools

| Tool | Arguments | What it does |
|----|----|----|
| `lint_file` | `path` (required), `project_dir` | Lints one R file under whichever `.lintr` config lintr finds for it. |
| `lint_project` | `dir`, `changed_only` | Lints a whole project in one call. A directory holding a `DESCRIPTION` goes through [`lintr::lint_package()`](https://lintr.r-lib.org/reference/lint.html), everything else through [`lintr::lint_dir()`](https://lintr.r-lib.org/reference/lint.html). With `changed_only = TRUE` only the files git reports as changed are linted — working tree, index, and untracked files, but not what is already committed. |
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

### Not in this release

Deliberately out of scope, so nobody goes looking:

- **Rule distribution** (the team rules package) — shipping a shared
  rule set as its own versioned R package. It arrives once a concrete
  team rule exists to distribute.
- **Auto-fix** — the tools report lints, they never rewrite your code.
- **Agent config export** — generating `AGENTS.md` / `CLAUDE.md` entries
  from a rule set.
- **CI gate** — no workflow that fails a build on lints.

## Hello World

Three lines to ban `T`/`F` in any R script:

``` r

library(lintrhelper)
no_t_f <- forbid_symbols(c("T", "F"), "Use TRUE/FALSE instead of {symbol}.")
lintr::lint("script.R", linters = list(no_t_f()))
```

That’s it. The rest of this README walks through the building blocks.

## Why lintrhelper?

Creating custom linters for lintr typically involves: - Understanding
R’s XML parse tree structure - Writing complex XPath expressions -
Handling source expressions and XML nodes - Setting up proper testing

`lintrhelper` **eliminates all of this complexity**! Just describe what
you want in plain terms.

## Quick Start - No XPath Required!

### Forbid Specific Symbols/Variables

``` r

library(lintrhelper)

# Ban T and F - just list them!
no_t_f <- forbid_symbols(
  c("T", "F"),
  "Use TRUE/FALSE instead of {symbol}."
)

# Test it
test_linter(no_t_f, "x <- T", should_lint = TRUE)
test_linter(no_t_f, "x <- TRUE", should_lint = FALSE)

# Use with lintr
lintr::lint("my_script.R", linters = no_t_f())
```

### Forbid Specific Functions

``` r

# Ban dangerous functions - just name them!
no_attach <- forbid_functions(
  "attach",
  "Don't use {function}(). Use with() instead."
)

# Or suggest alternatives automatically
no_sapply <- forbid_functions(
  "sapply",
  alternatives = "vapply"  # Auto-generates helpful message!
)

# Ban multiple functions at once
no_deprecated <- forbid_functions(
  c("sapply", "mapply", "tapply"),
  "Function {function}() is discouraged."
)
```

### Enforce Naming Conventions

``` r

# Require snake_case - just give a regex pattern!
snake_case <- require_naming_pattern(
  "^[a-z][a-z0-9_]*$",
  "Variable '{symbol}' should use snake_case."
)

test_linter(snake_case, "myVar <- 1", should_lint = TRUE)
test_linter(snake_case, "my_var <- 1", should_lint = FALSE)
```

### Enforce Assignment Style

``` r

# Prefer <- (the most common style)
use_arrow <- enforce_assignment_operator("<-")

test_linter(use_arrow, "x = 5", should_lint = TRUE)
test_linter(use_arrow, "x <- 5", should_lint = FALSE)
```

## Main Functions

### 🚀 High-Level Functions (No XPath!)

These functions let you create linters without any XPath knowledge:

- **[`forbid_symbols()`](https://fabiandistler.github.io/lintrhelper/reference/forbid_symbols.md)** -
  Ban specific variable names
- **[`forbid_functions()`](https://fabiandistler.github.io/lintrhelper/reference/forbid_functions.md)** -
  Ban specific function calls (with optional alternatives)
- **[`require_naming_pattern()`](https://fabiandistler.github.io/lintrhelper/reference/require_naming_pattern.md)** -
  Enforce naming conventions with regex
- **[`require_function_naming_pattern()`](https://fabiandistler.github.io/lintrhelper/reference/require_function_naming_pattern.md)** -
  Enforce function naming conventions
- **[`enforce_assignment_operator()`](https://fabiandistler.github.io/lintrhelper/reference/enforce_assignment_operator.md)** -
  Prefer `<-`, `=`, or `->`
- **[`require_function_arguments()`](https://fabiandistler.github.io/lintrhelper/reference/require_function_arguments.md)** -
  Ensure functions are called with specific arguments
- **[`limit_line_length()`](https://fabiandistler.github.io/lintrhelper/reference/limit_line_length.md)** -
  Enforce maximum line length

### 🧪 Testing Utilities

- **[`test_linter()`](https://fabiandistler.github.io/lintrhelper/reference/test_linter.md)** -
  Simplified testing for your linters
- **[`quick_test()`](https://fabiandistler.github.io/lintrhelper/reference/quick_test.md)** -
  One-liner for rapid testing

### 📚 Advanced (If You Want XPath)

For advanced users who want more control:

- **[`create_simple_linter()`](https://fabiandistler.github.io/lintrhelper/reference/create_simple_linter.md)** -
  Build XPath-based linters
- **[`create_function_call_linter()`](https://fabiandistler.github.io/lintrhelper/reference/create_function_call_linter.md)** -
  XPath-based function call linters
- **[`create_assignment_linter()`](https://fabiandistler.github.io/lintrhelper/reference/create_assignment_linter.md)** -
  XPath-based assignment linters

### 💡 Rules lintr Already Ships

lintr covers many common rules out of the box — reach for those before
writing your own:

- [`lintr::T_and_F_symbol_linter()`](https://lintr.r-lib.org/reference/T_and_F_symbol_linter.html) -
  `T`/`F` instead of `TRUE`/`FALSE`
- `lintr::attach_linter()` - use of
  [`attach()`](https://rdrr.io/r/base/attach.html)
- [`lintr::assignment_linter()`](https://lintr.r-lib.org/reference/assignment_linter.html) -
  assignment operator style
- [`lintr::seq_linter()`](https://lintr.r-lib.org/reference/seq_linter.html) -
  `1:length(x)` instead of `seq_along(x)`
- `lintr::sapply_linter()` -
  [`sapply()`](https://rdrr.io/r/base/lapply.html) instead of
  [`vapply()`](https://rdrr.io/r/base/lapply.html)

Use
[`lintr::available_linters()`](https://lintr.r-lib.org/reference/available_linters.html)
to see the full set. lintrhelper is for the rules lintr does not have.

## Getting Help

### Worked Examples

The [“Creating Linters Without
XPath”](https://fabiandistler.github.io/lintrhelper/articles/no-xpath-guide.html)
vignette walks through the seven common scenarios (forbidden symbols and
functions, naming conventions, assignment style, required arguments,
line length) and shows how to assemble them into a team style guide.

### XPath Reference

The [“Understanding
XPath”](https://fabiandistler.github.io/lintrhelper/articles/getting-started.html#understanding-xpath)
section of the “Getting Started” vignette lists the patterns you need
for the XPath builders, and the [“Advanced Custom
Linters”](https://fabiandistler.github.io/lintrhelper/articles/getting-started.html#advanced-custom-linters)
section shows a linter with custom logic beyond XPath matching.

## More Examples

### Ban Single-Letter Variables

``` r

# No XPath needed - just list the letters!
no_single_letters <- forbid_symbols(
  letters,  # a, b, c, ..., z
  "Avoid single-letter variable name '{symbol}'."
)

test_linter(no_single_letters, "x <- 5", should_lint = TRUE)
test_linter(no_single_letters, "count <- 5", should_lint = FALSE)
```

### Enforce Function Naming Convention

``` r

# Functions should start with verbs
verb_functions <- require_function_naming_pattern(
  "^(get|set|calculate|compute|check|is|has|create|update|delete|find|load|save)",
  "Function '{function}' should start with a verb."
)

test_linter(verb_functions, "result <- process()", should_lint = TRUE)
test_linter(verb_functions, "result <- calculate_total()", should_lint = FALSE)
```

### Forbid “temp” in Names

``` r

# Use invert=TRUE to forbid patterns that DO match
no_temp_names <- require_naming_pattern(
  "temp",
  "Variable '{symbol}' should not contain 'temp'.",
  invert = TRUE
)

test_linter(no_temp_names, "temp_var <- 1", should_lint = TRUE)
test_linter(no_temp_names, "result <- 1", should_lint = FALSE)
```

### Require Explicit Arguments

``` r

# Always specify stringsAsFactors
explicit_saf <- require_function_arguments(
  "data.frame",
  "stringsAsFactors",
  "Always specify stringsAsFactors explicitly in data.frame()."
)

# Lints this:
test_linter(explicit_saf, "df <- data.frame(x = 1:3)", should_lint = TRUE)

# Passes this:
test_linter(
  explicit_saf,
  "df <- data.frame(x = 1:3, stringsAsFactors = FALSE)",
  should_lint = FALSE
)
```

### Team Style Guide Example

``` r

# Combine multiple rules for your team
my_team_linters <- lintr::linters_with_defaults(
  no_t_f = forbid_symbols(c("T", "F"), "Use TRUE/FALSE")(),
  snake_case = require_naming_pattern("^[a-z][a-z0-9_]*$", "Use snake_case")(),
  use_arrow = enforce_assignment_operator("<-")(),
  no_attach = forbid_functions("attach", alternatives = "with")(),
  line_length = limit_line_length(80)()
)

# Apply to your project
lintr::lint_package(linters = my_team_linters)
```

## Testing Your Linters

The
[`test_linter()`](https://fabiandistler.github.io/lintrhelper/reference/test_linter.md)
function provides several ways to verify your linters:

``` r

my_linter <- create_simple_linter(
  xpath = "//SYMBOL[text() = 'T']",
  message = "Don't use T",
  linter_name = "no_t"
)

# Basic: should it lint or not?
test_linter(my_linter, "x <- T", should_lint = TRUE)
test_linter(my_linter, "x <- TRUE", should_lint = FALSE)

# Check exact number of lints
test_linter(my_linter, "a <- T; b <- T", n_lints = 2)

# Check message content
test_linter(
  my_linter,
  "x <- T",
  should_lint = TRUE,
  message_pattern = "Don't use"
)
```

## Quick XPath Testing

When developing XPath expressions, use
[`quick_test()`](https://fabiandistler.github.io/lintrhelper/reference/quick_test.md):

``` r

# Test if your XPath matches what you expect
quick_test("//SYMBOL[text() = 'T']", "x <- T")
# Found 1 lint(s):
# <text>:1:6: warning: [temp_test_linter] Found match
# x <- T
#      ^

quick_test("//SYMBOL_FUNCTION_CALL[text() = 'mean']", "x <- mean(y)")
# Found 1 lint(s): ...
```

## Learn More

- [lintrhelper documentation
  site](https://fabiandistler.github.io/lintrhelper/) — full reference
  and articles
- [Creating linters (official lintr
  guide)](https://lintr.r-lib.org/articles/creating_linters.html)
- [XPath tutorial](https://www.w3schools.com/xml/xpath_intro.asp)
- [Understanding R’s parse
  tree](https://github.com/r-lib/lintr#how-it-works)

## Contributing

Contributions are welcome! Please feel free to submit a Pull Request.

## License

MIT License
