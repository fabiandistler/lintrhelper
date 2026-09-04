
<!-- README.md is generated from README.Rmd. Please edit that file -->

# lintrhelper

<!-- badges: start -->

[![R-CMD-check](https://github.com/fabiandistler/lintrhelper/actions/workflows/R-CMD-check.yaml/badge.svg)](https://github.com/fabiandistler/lintrhelper/actions/workflows/R-CMD-check.yaml)
[![pkgdown](https://github.com/fabiandistler/lintrhelper/actions/workflows/pkgdown.yaml/badge.svg)](https://github.com/fabiandistler/lintrhelper/actions/workflows/pkgdown.yaml)
[![Lifecycle:
experimental](https://img.shields.io/badge/lifecycle-experimental-orange.svg)](https://lifecycle.r-lib.org/articles/stages.html#experimental)
<!-- badges: end -->

Custom [lintr](https://lintr.r-lib.org/) rules, described rather than
hand-written.
`forbid_symbols(c("T", "F"), "Use TRUE/FALSE instead of {symbol}.")` is
a working linter — no XPath expression, no XML parse tree, no
source-expression plumbing.

It is for the rules lintr does not already cover. lintr ships a lot of
configurable linters and several overlap with what is here, so read
[Where lintrhelper fits](#where-lintrhelper-fits) before writing your
own.

Documentation: <https://fabiandistler.github.io/lintrhelper/>

## Quick start

``` r
# install.packages("pak")
pak::pak("fabiandistler/lintrhelper")
```

``` r
library(lintrhelper)

no_t_f <- forbid_symbols(c("T", "F"), "Use TRUE/FALSE instead of {symbol}.")
lintr::lint(text = "x <- T", linters = no_t_f())
#> <text>:1:6: style: [no_t_f] Use TRUE/FALSE instead of T.
#> x <- T
#>      ^
```

`no_t_f` is a linter factory — call it to get a linter. Point it at real
code with `lintr::lint("script.R", linters = no_t_f())`, or at a whole
package with `lintr::lint_package(linters = no_t_f())`.

## What you can build

Ban functions, and have the message written for you:

``` r
no_sapply <- forbid_functions("sapply", alternatives = "vapply")
lintr::lint(text = "res <- sapply(xs, length)", linters = no_sapply())
#> <text>:1:8: warning: [no_sapply] Use vapply() instead of sapply().
#> res <- sapply(xs, length)
#>        ^~~~~~
```

Enforce a naming convention with a regex:

``` r
snake_case <- require_naming_pattern(
  "^[a-z][a-z0-9_]*$",
  "Variable {symbol} should be snake_case."
)
lintr::lint(text = "myVar <- 1", linters = snake_case())
#> <text>:1:1: style: [snake_case] Variable myVar should be snake_case.
#> myVar <- 1
#> ^~~~~
```

Require an argument to be passed explicitly:

``` r
explicit_saf <- require_function_arguments(
  "data.frame",
  "stringsAsFactors",
  "Always set stringsAsFactors in data.frame()."
)
lintr::lint(text = "df <- data.frame(x = 1:3)", linters = explicit_saf())
#> <text>:1:7: warning: [explicit_saf] Always set stringsAsFactors in data.frame().
#> df <- data.frame(x = 1:3)
#>       ^~~~~~~~~~
```

Combine rules into a team style guide on top of lintr’s defaults:

``` r
team_linters <- lintr::linters_with_defaults(
  explicit_saf = explicit_saf(),
  no_attach = forbid_functions("attach", alternatives = "with")()
)

lintr::lint(text = "attach(df)", linters = team_linters)
#> <text>:1:1: warning: [no_attach] Use with() instead of attach().
#> attach(df)
#> ^~~~~~
```

Apply that set to your package with
`lintr::lint_package(linters = team_linters)`, or save it to a `.lintr`
file so every run picks it up.

Three more builders — function-name patterns, assignment style, and line
length — are covered in the [“Creating Linters Without
XPath”](https://fabiandistler.github.io/lintrhelper/articles/no-xpath-guide.html)
vignette.

## Testing your linters

``` r
test_linter(no_t_f, "x <- T", should_lint = TRUE)
test_linter(no_t_f, "x <- TRUE", should_lint = FALSE)
```

`test_linter()` is silent on success and throws on failure, so it drops
straight into a `testthat` file. It also takes `n_lints` for an exact
count and `message_pattern` to check the wording.

While developing an XPath expression, `quick_test()` reports what it
matched:

``` r
quick_test("//SYMBOL_FUNCTION_CALL[text() = 'mean']", "x <- mean(y)")
#> Found 1 lint(s):
#> <text>:1:6: warning: [temp_linter] Found match
#> x <- mean(y)
#>      ^~~~
```

## Where lintrhelper fits

Several of lintr’s own linters are configurable and cover the same
ground:

| lintrhelper                                                     | lintr equivalent                                              |
|-----------------------------------------------------------------|---------------------------------------------------------------|
| `forbid_symbols()`, `forbid_functions()`                        | `undesirable_function_linter(fun =, symbol_is_undesirable =)` |
| `require_naming_pattern()`, `require_function_naming_pattern()` | `object_name_linter(regexes =)`                               |
| `enforce_assignment_operator()`                                 | `assignment_linter()`                                         |
| `limit_line_length()`                                           | `line_length_linter(length =)`                                |
| `require_function_arguments()`                                  | — none                                                        |

Where they overlap the difference is ergonomics: a regex and a message
string here, against assembling the equivalent lintr call. Where they do
not — `require_function_arguments()` — this package is the only option.

The quick start above is a case in point:
`lintr::T_and_F_symbol_linter()` already bans `T` and `F`. It is used
here because it is the smallest rule that demonstrates anything — in
real code, use lintr’s.

`lintr::available_linters()` lists everything lintr ships. Reach for
those first; lintrhelper is for the rules that are left.

## Use it from your coding agent

lintrhelper ships an MCP server, so a coding agent — Claude Code,
opencode, Codex — can ask for lints itself instead of you pasting them
into the chat. It lints under whichever `.lintr` configuration your
project already has, so the agent sees the same rules you do.

Four read-only tools: `lint_file`, `lint_project`, `list_rules`, and
`explain_rule`. Setup, per-client configuration, and the tool reference
are in the [“Use lintrhelper from a coding
agent”](https://fabiandistler.github.io/lintrhelper/articles/mcp-server.html)
vignette.

## Reference and guides

- [Function
  reference](https://fabiandistler.github.io/lintrhelper/reference/index.html)
  — every exported function
- [“Creating Linters Without
  XPath”](https://fabiandistler.github.io/lintrhelper/articles/no-xpath-guide.html)
  — the seven builders, worked through
- [“Getting
  Started”](https://fabiandistler.github.io/lintrhelper/articles/getting-started.html)
  — including [“Understanding
  XPath”](https://fabiandistler.github.io/lintrhelper/articles/getting-started.html#understanding-xpath),
  for when you do want to write XPath by hand. `create_simple_linter()`,
  `create_function_call_linter()`, and `create_assignment_linter()` take
  an XPath expression directly.
- [“Use lintrhelper from a coding
  agent”](https://fabiandistler.github.io/lintrhelper/articles/mcp-server.html)
  — the MCP server
- [Creating
  linters](https://lintr.r-lib.org/articles/creating_linters.html) —
  lintr’s own guide, for when you outgrow this package

## Contributing

Contributions are welcome! Please feel free to submit a Pull Request.

## License

MIT License
