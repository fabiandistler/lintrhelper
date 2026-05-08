# Limit Line Length

Flag lines that exceed a certain character length.

## Usage

``` r
limit_line_length(
  max_length = 80,
  message = NULL,
  type = c("style", "warning", "error")
)
```

## Arguments

- max_length:

  Maximum allowed line length. Default is 80.

- message:

  The lint message.

- type:

  The lint type. Defaults to "style".

## Value

A linter function.

## Examples

``` r
if (FALSE) { # \dontrun{
# Standard 80 characters
line_length_80 <- limit_line_length(80)

# More strict
line_length_72 <- limit_line_length(72)
} # }
```
