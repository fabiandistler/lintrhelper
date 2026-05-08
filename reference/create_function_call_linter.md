# Create a Function Call Linter

Specialized helper for creating linters that target specific function
calls. This is a common pattern in linting, so this function makes it
even easier.

## Usage

``` r
create_function_call_linter(
  function_names,
  message,
  linter_name,
  type = c("warning", "error", "style")
)
```

## Arguments

- function_names:

  Character vector of function names to lint against.

- message:

  A character string with the lint message. Use function as a
  placeholder for the actual function name found.

- linter_name:

  A character string naming the linter.

- type:

  The type of lint. Defaults to "warning".

## Value

A linter function.

## Examples

``` r
if (FALSE) { # \dontrun{
# Warn against using paste0 in favor of paste
no_paste0_linter <- create_function_call_linter(
  function_names = "paste0",
  message = "Use paste(..., sep = '') instead of {function}.",
  linter_name = "no_paste0_linter"
)
} # }
```
