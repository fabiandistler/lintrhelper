# lintrhelper 0.1.0.9001 (development)

## New features

* `start_mcp_server()` starts a Model Context Protocol server over stdio so coding agents can ask for lintr diagnostics themselves (#11). It exposes a single read-only tool, `lint_file(path, project_dir = NULL)`, which lints one file under whichever `.lintr` configuration lintr finds for it and returns compact diagnostics (`filename`, `line`, `column`, `type`, `message`, `linter`) grouped by file.
* The server also exposes a read-only `lint_project(dir = NULL)` tool, which lints a whole project in one call instead of the agent walking files one at a time (#12). It returns the same per-lint shape and file grouping as `lint_file`, and a clean project comes back as an empty result rather than an error. A directory holding a `DESCRIPTION` is linted with `lintr::lint_package()`, everything else with `lintr::lint_dir()`; the `DESCRIPTION` check looks at the anchor itself and never upwards, since `lint_package()` would otherwise walk up and lint an enclosing package instead of the directory asked for.
* `lint_project()` takes `changed_only` (#15): with `changed_only = TRUE` the tool lints only what git reports as changed — the edits in the working tree and the index, plus untracked files — so an agent fixing a branch does not pay for the whole repository on every call. Changes that are already committed stay out; the question asked is what the agent has touched and not finished. The narrowed set is a subset of the full lint: a file lintr does not read, a file the change deleted, a path a directory lint never walks — a `.github/` script, an `renv/` library — and, in a package, anything outside the directories `lintr::lint_package()` walks all stay out of it, exactly as they stay out of a whole-project lint. A directory git cannot report on — an unversioned one above all — is not an error: it is linted whole and the reply carries a `message` beside the lints saying why. `changed_only = FALSE` remains the default and lints everything, as before.
* A third read-only tool, `list_rules(tags = NULL)`, reports the linters `lintr::available_linters()` knows about (#13), so the agent and the human argue about the same ruleset instead of the agent guessing at rule names. Each rule comes back as name, providing package, and tags — metadata, not a documentation dump. Tags filter as a union: a linter is returned when it carries any one of them, and since lintr tags its own defaults `"default"`, `list_rules("default")` is the set that runs without a project `.lintr`. A tag no linter carries is not an error: the result is empty and carries a message naming the tag alongside the ones that do exist, which is what an agent needs to correct itself in one step rather than retrying blind.
* A fourth read-only tool, `explain_rule(name)`, returns one linter's documentation (#14), so an agent rewriting code to satisfy a lint follows the rule rather than its guess at what the rule wants. The reply carries the linter's title, description, usage, and arguments, read from the installed help page and trimmed to what an agent acts on — the examples, the tag section, and the see-also links are what a human browses and stay out. A name no linter carries is not an error, the same way an unknown tag is not one in `list_rules()`: the result reports `found = FALSE` and names the closest linters, so `"assignment"` or a typo is corrected in one step. Deprecated linters are explainable even though `list_rules()` leaves them out of its listing — a deprecated name only turns up in a `.lintr` or an old lint after the fact, which is exactly when the help page needs to say so.

* The project anchor follows the precedence tool argument, then `CLAUDE_PROJECT_DIR`, then the server process' working directory. Relative paths resolve against it and lintr runs with it as the working directory; config discovery stays lintr's own upward search from the linted file.
* Failures the agent can correct itself — an unknown path, a directory instead of a file, a missing project directory — come back as tool errors carrying the message, rather than as JSON-RPC internal errors that clients surface as a hard failure.
* `.mcp.json` at the project root registers the server with Claude Code at project scope. It launches `Rscript --no-init-file --no-site-file`, since anything an `.Rprofile` prints would arrive ahead of the JSON-RPC handshake and break it. It requires lintrhelper to be installed in the R library the `Rscript` on `PATH` sees.
* `mcptools` and `ellmer` are Suggests behind an `rlang::check_installed()` gate, so authoring linters does not pull in the MCP stack.

## Documentation

* The README now takes a colleague from nothing to a working lintr MCP server (#18): installation via `pak` from public GitHub with an internal git-mirror fallback for machines without GitHub access, a check that the `Rscript` on `PATH` sees the package before any config is edited, and the three client registration snippets as the repo itself ships them — Claude Code `.mcp.json` at project scope on its 30 s default, opencode `opencode.json` with `type: local`, an array `command`, `environment`, and a 60000 ms timeout (#16), and Codex `.codex/config.toml` with `[mcp_servers.lintr]`, `startup_timeout_sec = 20`, and the trusted-project requirement (#17). The differing server names across the snippets are documented as the client-side labels they are, since each client builds its tool names around the one it was given — Claude Code surfaces them as `mcp__lintrhelper__lint_file` and so on. The four shipped tools are tabulated with their arguments, the `mcptools`/`ellmer` Suggests gate is explained together with the way `rlang::check_installed()` fails under a non-interactive `Rscript` launch, and the v0.2 non-goals — team rules package, auto-fix, config/`AGENTS.md` export, CI gate — are stated outright so nobody goes looking for them.
* `opencode.json` and `.codex/config.toml` are committed at the project root beside the existing `.mcp.json`, so all three registrations are reachable from the repo itself. Both are added to `.Rbuildignore`, keeping the "non-standard files at top level" NOTE away.

## Bug fixes

* `forbid_symbols()`, `create_function_call_linter()`, `require_naming_pattern()`, and `require_function_naming_pattern()` now correctly return multiple lints when several locations match. Previously the per-node messages were assembled with `unlist(recursive = FALSE)`, which broke the `lints` object structure and silently dropped all results.
* `create_assignment_linter()` (and therefore `enforce_assignment_operator()`) now matches `=` assignments. The previous XPath assumed the assignment was always wrapped in `<expr>`, but `=` lives under `<expr_or_assign_or_help>`.
* `require_naming_pattern()` and `require_function_naming_pattern()` honour the `invert` argument again — both branches of the conditional were returning the same value.
* `require_function_arguments()` now inspects the full call expression (the grandparent of `SYMBOL_FUNCTION_CALL`) when looking for named arguments, so calls that do supply the required argument no longer trigger.
* `limit_line_length()` is now scoped to the file level (it used to run once per expression and rely on a non-existent `$lines` field).

## Tooling

* `R CMD check` is clean again: `LICENSE` is now the DCF stub that `MIT + file LICENSE` expects (the full text moved to `LICENSE.md`), and the `{symbol}` / `{function}` placeholders in `@param message` are escaped so `checkRd` no longer reports lost braces.
* `.Rbuildignore` now covers `AGENTS.md` and `CONTEXT.md`, clearing the remaining "non-standard files at top level" NOTE. Both are agent-facing repo documentation and have no business in the built package.
* Added GitHub Actions workflow for `R CMD check` across macOS, Windows, and Ubuntu (R devel/release/oldrel-1).
* Added pkgdown workflow that deploys the documentation site to <https://fabiandistler.github.io/lintrhelper/>.
* Added `_pkgdown.yml` configuration grouping the reference index by user-facing categories.
* Generated `man/` pages from existing roxygen comments (`R CMD check` previously failed because no Rd files were committed).
* `DESCRIPTION`: real author, declared `utils` as an import (for `capture.output`), dropped the unused `rlang` import, dropped `LazyData` (no `data/` directory). `rlang` returned as an import with `start_mcp_server()`, which needs `rlang::check_installed()` for its Suggests gate.

# lintrhelper 0.1.0

## Initial Release

### High-Level Functions (No XPath Required!)

The main feature of lintrhelper is that **you don't need to know XPath** to create linters:

* Added `forbid_symbols()` - Ban specific variable names
* Added `forbid_functions()` - Ban function calls with auto-generated alternative suggestions
* Added `require_naming_pattern()` - Enforce naming conventions with regex (no XPath!)
* Added `require_function_naming_pattern()` - Enforce function naming with regex
* Added `enforce_assignment_operator()` - Prefer `<-`, `=`, or `->`
* Added `require_function_arguments()` - Ensure functions are called with specific arguments
* Added `limit_line_length()` - Enforce maximum line length

### Testing Utilities

* Added `test_linter()` for simplified linter testing
* Added `quick_test()` for rapid development

### Advanced (XPath-Based) Functions

For users who want more control:

* Added `create_simple_linter()` for XPath-based linter creation
* Added `create_function_call_linter()` for XPath-based function call linters
* Added `create_assignment_linter()` for XPath-based assignment operator linters
* Added `linter_template()` for code templates
* Added `xpath_patterns()` for XPath reference

### Ready-to-Use Examples

* Included example linters: `no_t_f_linter()`, `no_attach_linter()`, `prefer_arrow_assignment_linter()`, `no_sapply_linter()`, `no_one_length_linter()`

### Documentation

* Added comprehensive README emphasizing no-XPath approach
* Added "Getting Started" vignette
* Added "Creating Linters Without XPath" vignette with extensive examples
* Added unit tests for all functions
