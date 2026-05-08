# Forbid Specific Functions

Create a linter that flags specific function calls. This is a simplified
version that doesn't require XPath knowledge.

## Usage

``` r
forbid_functions(
  functions,
  message = NULL,
  alternatives = NULL,
  type = c("warning", "error", "style")
)
```

## Arguments

- functions:

  Character vector of function names to forbid.

- message:

  The lint message. Use function as placeholder.

- alternatives:

  Optional character vector of alternative functions to suggest.

- type:

  The lint type. Defaults to "warning".

## Value

A linter function.

## Examples

``` r
if (FALSE) { # \dontrun{
# Ban attach()
no_attach <- forbid_functions(
  "attach",
  "Don't use {function}(). Use with() instead."
)

# With alternatives
no_sapply <- forbid_functions(
  "sapply",
  alternatives = "vapply"
)

# Multiple functions
no_deprecated <- forbid_functions(
  c("sapply", "mapply", "tapply"),
  "Function {function}() is discouraged."
)
} # }
```
