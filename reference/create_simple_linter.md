# Create a Simple XPath-Based Linter

This function simplifies the creation of custom linters using XPath
expressions. It wraps the standard lintr pattern of finding XML nodes
and converting them to lints.

## Usage

``` r
create_simple_linter(
  xpath,
  message,
  linter_name,
  type = c("warning", "error", "style")
)
```

## Arguments

- xpath:

  A character string containing the XPath expression to match
  problematic code.

- message:

  A character string with the lint message to display. Can use function
  placeholder which will be replaced with the function name if
  applicable.

- linter_name:

  A character string naming the linter (used for the lint type).

- type:

  The type of lint to create. Defaults to "warning". Can be "error",
  "warning", or "style".

## Value

A linter function that can be used with lintr.

## Examples

``` r
if (FALSE) { # \dontrun{
# Create a linter that warns against using T/F instead of TRUE/FALSE
tf_linter <- create_simple_linter(
  xpath = "//SYMBOL[text() = 'T' or text() = 'F']",
  message = "Use TRUE/FALSE instead of T/F.",
  linter_name = "tf_linter"
)

# Use it with lintr
lintr::lint("x <- T", linters = tf_linter())
} # }
```
