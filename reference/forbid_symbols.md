# Forbid Specific Symbols (Variable Names)

Create a linter that flags specific symbol names. Useful for banning
certain variable names or enforcing naming conventions.

## Usage

``` r
forbid_symbols(
  symbols,
  message = "Symbol '{symbol}' should not be used.",
  type = c("style", "warning", "error")
)
```

## Arguments

- symbols:

  Character vector of symbol names to forbid.

- message:

  The lint message. Use symbol as placeholder for the symbol name.

- type:

  The lint type. Defaults to "style".

## Value

A linter function.

## Examples

``` r
if (FALSE) { # \dontrun{
# Ban T and F
no_t_f <- forbid_symbols(
  c("T", "F"),
  "Use TRUE/FALSE instead of {symbol}."
)

# Ban single-letter variable names
no_single_letters <- forbid_symbols(
  letters,
  "Avoid single-letter variable name '{symbol}'."
)

test_linter(no_t_f, "x <- T", should_lint = TRUE)
} # }
```
