# Test a Linter with Example Code

Simplified interface for testing custom linters. This function wraps
lintr's testing functionality to make it easier to quickly test your
custom linters during development.

## Usage

``` r
test_linter(
  linter,
  code,
  should_lint = TRUE,
  n_lints = NULL,
  message_pattern = NULL
)
```

## Arguments

- linter:

  A linter function (or linter factory that returns a linter).

- code:

  Character string or vector of code to lint.

- should_lint:

  Logical. If TRUE (default), expects the linter to find issues. If
  FALSE, expects the linter to find no issues.

- n_lints:

  Integer. Expected number of lints. If NULL, just checks if linting
  occurred (or didn't occur based on should_lint).

- message_pattern:

  Optional regex pattern to match against lint messages.

## Value

Invisibly returns the lints found (as a list).

## Examples

``` r
if (FALSE) { # \dontrun{
my_linter <- create_simple_linter(
  xpath = "//SYMBOL[text() = 'T']",
  message = "Don't use T",
  linter_name = "no_t"
)

# Test that it catches T
test_linter(my_linter, "x <- T", should_lint = TRUE)

# Test that it doesn't flag TRUE
test_linter(my_linter, "x <- TRUE", should_lint = FALSE)

# Test exact number of lints
test_linter(my_linter, "x <- T; y <- T", n_lints = 2)
} # }
```
