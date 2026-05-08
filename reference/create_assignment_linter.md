# Create an Assignment Linter

Helper for creating linters that check assignment patterns.

## Usage

``` r
create_assignment_linter(
  forbidden_operators,
  message,
  linter_name,
  type = c("style", "warning", "error")
)
```

## Arguments

- forbidden_operators:

  Character vector of forbidden assignment operators (e.g., c("=",
  "\<\<-")).

- message:

  The lint message to display.

- linter_name:

  A character string naming the linter.

- type:

  The type of lint. Defaults to "style".

## Value

A linter function.

## Examples

``` r
if (FALSE) { # \dontrun{
# Enforce <- over =
no_equals_assignment_linter <- create_assignment_linter(
  forbidden_operators = "=",
  message = "Use <- for assignment, not =.",
  linter_name = "no_equals_assignment"
)
} # }
```
