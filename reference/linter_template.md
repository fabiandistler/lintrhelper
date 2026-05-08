# Get Linter Template Code

Returns template code for creating different types of linters. This is
helpful for getting started with custom linters.

## Usage

``` r
linter_template(
  type = c("simple", "function_call", "assignment", "advanced", "all"),
  cat_output = TRUE
)
```

## Arguments

- type:

  The type of template to generate. Options include:

  - "simple" - Basic XPath-based linter

  - "function_call" - Function call linter

  - "assignment" - Assignment pattern linter

  - "advanced" - More advanced linter with custom logic

  - "all" - Shows all templates

- cat_output:

  Logical. If TRUE (default), prints the template to console. If FALSE,
  returns the template as a character string.

## Value

If cat_output is FALSE, returns the template code as a character string.
Otherwise, returns NULL invisibly after printing.

## Examples

``` r
# View a simple linter template
linter_template("simple")
#> 
#> # Simple XPath-based linter example
#> library(lintrhelper)
#> 
#> my_simple_linter <- create_simple_linter(
#>   xpath = "//SYMBOL[text() = 'T' or text() = 'F']",
#>   message = "Use TRUE/FALSE instead of T/F.",
#>   linter_name = "no_t_f_linter",
#>   type = "warning"
#> )
#> 
#> # Test it
#> test_linter(my_simple_linter, "x <- T", should_lint = TRUE)
#> test_linter(my_simple_linter, "x <- TRUE", should_lint = FALSE)

# Get function call linter template as string
code <- linter_template("function_call", cat_output = FALSE)
```
