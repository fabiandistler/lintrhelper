# Require Function Naming Pattern

Like require_naming_pattern but specifically for function names.

## Usage

``` r
require_function_naming_pattern(
  pattern,
  message = "Function '{function}' does not follow naming convention.",
  type = c("style", "warning", "error"),
  invert = FALSE
)
```

## Arguments

- pattern:

  Regular expression that function names must match.

- message:

  The lint message. Use function as placeholder.

- type:

  The lint type. Defaults to "style".

- invert:

  If TRUE, flags names that DO match (for forbidding patterns).

## Value

A linter function.

## Examples

``` r
if (FALSE) { # \dontrun{
# Require functions to start with verb
verb_functions <- require_function_naming_pattern(
  "^(get|set|calculate|check|is|has|create|update|delete)",
  "Function '{function}' should start with a verb."
)

# Forbid functions with "temp" in name
no_temp_functions <- require_function_naming_pattern(
  "temp",
  "Function '{function}' should not contain 'temp'.",
  invert = TRUE
)
} # }
```
