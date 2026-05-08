# Require Specific Function Arguments

Check if certain functions are called with specific arguments. Useful
for enforcing explicit parameter usage.

## Usage

``` r
require_function_arguments(
  function_name,
  required_args,
  message = sprintf("Function %s() should include argument(s): %s", function_name,
    paste(required_args, collapse = ", ")),
  type = c("warning", "style", "error")
)
```

## Arguments

- function_name:

  The function to check.

- required_args:

  Character vector of required argument names.

- message:

  The lint message.

- type:

  The lint type. Defaults to "warning".

## Value

A linter function.

## Examples

``` r
if (FALSE) { # \dontrun{
# Require explicit stringsAsFactors in data.frame()
explicit_saf <- require_function_arguments(
  "data.frame",
  "stringsAsFactors",
  "Always specify stringsAsFactors explicitly in data.frame()."
)

# This would lint:
# data.frame(x = 1:3)

# This would pass:
# data.frame(x = 1:3, stringsAsFactors = FALSE)
} # }
```
