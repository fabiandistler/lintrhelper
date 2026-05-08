# Require Naming Pattern

Create a linter that enforces naming conventions for symbols
(variables). No XPath knowledge required - just specify the pattern!

## Usage

``` r
require_naming_pattern(
  pattern,
  message = "Name '{symbol}' does not follow naming convention.",
  type = c("style", "warning", "error"),
  invert = FALSE
)
```

## Arguments

- pattern:

  Regular expression pattern that names must match.

- message:

  The lint message. Use symbol as placeholder.

- type:

  The lint type. Defaults to "style".

- invert:

  If TRUE, flags names that DO match the pattern (for forbidding
  patterns).

## Value

A linter function.

## Examples

``` r
if (FALSE) { # \dontrun{
# Require snake_case
snake_case_linter <- require_naming_pattern(
  "^[a-z][a-z0-9_]*$",
  "Variable '{symbol}' should use snake_case."
)

# Forbid names starting with uppercase (invert = TRUE)
no_uppercase_start <- require_naming_pattern(
  "^[A-Z]",
  "Variable '{symbol}' should not start with uppercase.",
  invert = TRUE
)

test_linter(snake_case_linter, "myVar <- 1", should_lint = TRUE)
test_linter(snake_case_linter, "my_var <- 1", should_lint = FALSE)
} # }
```
