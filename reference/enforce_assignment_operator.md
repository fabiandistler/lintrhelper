# Enforce Assignment Operator

Simple way to enforce a specific assignment operator without XPath.

## Usage

``` r
enforce_assignment_operator(
  prefer = c("<-", "=", "->"),
  message = NULL,
  type = c("style", "warning", "error")
)
```

## Arguments

- prefer:

  Which operator to prefer: "\<-", "=", or "-\>".

- message:

  Optional custom message.

- type:

  The lint type. Defaults to "style".

## Value

A linter function.

## Examples

``` r
if (FALSE) { # \dontrun{
# Prefer <- (most common)
use_arrow <- enforce_assignment_operator("<-")

# Prefer =
use_equals <- enforce_assignment_operator("=")

test_linter(use_arrow, "x = 5", should_lint = TRUE)
test_linter(use_arrow, "x <- 5", should_lint = FALSE)
} # }
```
