# Quick Lint Test

Even simpler one-liner for quick testing during development.

## Usage

``` r
quick_test(xpath, code, message = "Found match")
```

## Arguments

- xpath:

  XPath expression to test.

- code:

  Code to test against.

- message:

  Message for the lint (optional, for display).

## Value

The lints found.

## Examples

``` r
if (FALSE) { # \dontrun{
# Quick test if an XPath matches
quick_test("//SYMBOL[text() = 'T']", "x <- T")
} # }
```
