# Getting Started with lintrhelper

## Introduction

`lintrhelper` makes it easy to create custom linters for the `lintr`
package. **The best part? You don’t need to know XPath or understand XML
parse trees!** This vignette will show you how to create powerful
linters using simple, intuitive functions.

## Installation

``` r

# Install from GitHub
devtools::install_github("fabiandistler/lintrhelper")
```

``` r

library(lintrhelper)
```

## Why Create Custom Linters?

While `lintr` comes with many built-in linters, you might want to create
custom linters for:

- **Team-specific style guides**: Enforce conventions unique to your
  team or organization
- **Domain-specific rules**: Check for patterns specific to your field
  (e.g., statistical best practices)
- **Package-specific warnings**: Flag deprecated functions or suggest
  package-specific alternatives
- **Project conventions**: Enforce naming conventions, file structure
  rules, etc.

## Your First Linter - No XPath!

Let’s create a linter that warns against using `T` and `F` instead of
`TRUE` and `FALSE`:

``` r

# Just list the symbols you want to forbid!
no_t_f <- forbid_symbols(
  c("T", "F"),
  "Use TRUE/FALSE instead of {symbol}."
)

# Test it
test_linter(no_t_f, "x <- T", should_lint = TRUE)
test_linter(no_t_f, "x <- TRUE", should_lint = FALSE)
```

That’s it! No XPath, no XML, just plain R.

### Breaking it Down

- **symbols**: A character vector of variable names to forbid
- **message**: The message to show users. `{symbol}` gets replaced with
  the actual symbol found
- **type**: (optional) Severity level - “style”, “warning”, or “error”

## Understanding XPath

XPath is a query language for XML. R code is parsed into an XML tree,
and linters use XPath to find problematic patterns.

### Common XPath Patterns

**Symbols**

- `//SYMBOL` - All symbols (variable names)
- `//SYMBOL[text() = 'my_var']` - A specific symbol
- `//SYMBOL[contains(text(), 'temp')]` - Symbols matching a pattern

**Functions**

- `//SYMBOL_FUNCTION_CALL` - All function calls
- `//SYMBOL_FUNCTION_CALL[text() = 'mean']` - A specific function call
- `//FUNCTION` - Function definitions

**Operators**

- `//LEFT_ASSIGN | //RIGHT_ASSIGN | //EQ_ASSIGN` - All assignments
- `//LEFT_ASSIGN` - Left assign (`<-`)
- `//EQ_ASSIGN` - Equals assign (`=`)
- `//OP-PLUS | //OP-MINUS | //OP-TIMES | //OP-DIVIDE` - Arithmetic
  operators

**Literals**

- `//NUM_CONST` - Numeric constants
- `//STR_CONST` - String constants
- `//NULL_CONST` - `NULL` values

**Comments**

- `//COMMENT` - All comments

**Navigating the tree**

- `//SYMBOL_FUNCTION_CALL/following-sibling::expr` - Arguments of a call
- `//LEFT_ASSIGN/preceding-sibling::expr` - Assignment target
- `//LEFT_ASSIGN/following-sibling::expr` - Assignment value
- `//IF/following-sibling::expr[1]` - `if` conditions

### Testing XPath Expressions

Use
[`quick_test()`](https://fabiandistler.github.io/lintrhelper/reference/quick_test.md)
to rapidly test XPath expressions:

``` r

quick_test("//SYMBOL[text() = 'T']", "x <- T")
# Shows if the XPath matches and where
```

## Function Call Linters

A very common use case is flagging specific function calls:

``` r

no_sapply <- create_function_call_linter(
  function_names = "sapply",
  message = "Use vapply() instead of {function} for type-safe code.",
  linter_name = "no_sapply_linter"
)

# Test it
test_linter(no_sapply, "result <- sapply(1:10, sqrt)", should_lint = TRUE)
```

You can flag multiple functions:

``` r

deprecated_funcs <- create_function_call_linter(
  function_names = c("sapply", "mapply"),
  message = "Function {function} is deprecated in our style guide.",
  linter_name = "deprecated_functions"
)
```

Note the `{function}` placeholder - it will be replaced with the actual
function name found.

## Assignment Linters

Another common pattern is enforcing assignment operator style:

``` r

prefer_arrow <- create_assignment_linter(
  forbidden_operators = "=",
  message = "Use <- for assignment, not =.",
  linter_name = "prefer_arrow_assignment",
  type = "style"
)

test_linter(prefer_arrow, "x = 5", should_lint = TRUE)
test_linter(prefer_arrow, "x <- 5", should_lint = FALSE)
```

## Testing Your Linters

The
[`test_linter()`](https://fabiandistler.github.io/lintrhelper/reference/test_linter.md)
function provides several testing modes:

``` r

# Basic: does it lint?
test_linter(my_linter, "x <- T", should_lint = TRUE)

# Expect no lints
test_linter(my_linter, "x <- TRUE", should_lint = FALSE)

# Check exact number of lints
test_linter(my_linter, "a <- T; b <- T", n_lints = 2)

# Verify message content
test_linter(
  my_linter,
  "x <- T",
  message_pattern = "TRUE/FALSE"
)
```

## Check lintr’s Built-in Linters First

Before writing a linter, see whether lintr already has one. The rules
above are useful as teaching examples, but in real projects use lintr’s
own:

``` r

lintr::lint("my_script.R", linters = lintr::T_and_F_symbol_linter())
lintr::lint("my_script.R", linters = lintr::attach_linter())
lintr::lint("my_script.R", linters = lintr::assignment_linter())
lintr::lint("my_script.R", linters = lintr::seq_linter())
lintr::lint("my_script.R", linters = lintr::sapply_linter())

# The full list
lintr::available_linters()
```

lintrhelper is for the rules lintr does not already cover.

## Advanced Custom Linters

For more complex logic, you can build linters using the full lintr API:

``` r

library(lintr)
library(xml2)

advanced_linter <- function() {
  lintr::Linter(function(source_expression) {
    if (!lintr::is_lint_level(source_expression, "file")) {
      return(list())
    }

    xml <- source_expression$full_xml_parsed_content

    # Find all function calls
    nodes <- xml2::xml_find_all(xml, "//SYMBOL_FUNCTION_CALL")

    # Custom filtering logic
    bad_nodes <- Filter(function(node) {
      func_name <- xml2::xml_text(node)
      # Flag functions starting with "old_"
      grepl("^old_", func_name)
    }, nodes)

    # Generate custom lints
    lints <- lapply(bad_nodes, function(node) {
      func_name <- xml2::xml_text(node)
      new_name <- sub("^old_", "new_", func_name)

      lintr::xml_nodes_to_lints(
        node,
        source_expression = source_expression,
        lint_message = sprintf(
          "Function %s is deprecated. Use %s instead.",
          func_name, new_name
        ),
        type = "warning"
      )
    })

    unlist(lints, recursive = FALSE)
  })
}
```

## Using Your Linters with lintr

Once you’ve created your linters, use them with lintr:

``` r

# Single file
lintr::lint("my_script.R", linters = my_linter())

# Multiple linters
lintr::lint(
  "my_script.R",
  linters = lintr::linters_with_defaults(
    my_linter = my_linter(),
    another_linter = another_linter()
  )
)

# Entire package
lintr::lint_package(
  linters = lintr::linters_with_defaults(
    my_linter = my_linter()
  )
)
```

## Best Practices

1.  **Start simple**: Begin with
    [`create_simple_linter()`](https://fabiandistler.github.io/lintrhelper/reference/create_simple_linter.md)
    and XPath
2.  **Test thoroughly**: Use
    [`test_linter()`](https://fabiandistler.github.io/lintrhelper/reference/test_linter.md)
    with various code examples
3.  **Use quick_test()**: Iterate quickly when developing XPath
    expressions
4.  **Clear messages**: Write helpful, actionable lint messages
5.  **Appropriate severity**: Use “style” for preferences, “warning” for
    potential issues, “error” for serious problems
6.  **Document your linters**: Explain why a pattern is problematic

## Next Steps

- Read the [lintr creating linters
  vignette](https://lintr.r-lib.org/articles/creating_linters.html)
- Learn more about
  [XPath](https://www.w3schools.com/xml/xpath_intro.asp)
- Explore the source code of built-in lintr linters
- Share your useful linters with the community!

## Getting Help

If you encounter issues or have questions:

- Check
  [`?create_simple_linter`](https://fabiandistler.github.io/lintrhelper/reference/create_simple_linter.md)
  and other function documentation
- Revisit [Common XPath Patterns](#common-xpath-patterns) and the
  [Creating Linters Without
  XPath](https://fabiandistler.github.io/lintrhelper/articles/no-xpath-guide.md)
  vignette for reference
- Visit the [GitHub
  repository](https://github.com/fabiandistler/lintrhelper)
