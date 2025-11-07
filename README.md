# lintrhelper

> Helper Functions for Creating Custom Lintr Rules

`lintrhelper` makes it easy to create custom linters for the [lintr](https://lintr.r-lib.org/) package. It provides simplified interfaces, testing utilities, and templates to help you quickly build and test your own linting rules.

## Installation

You can install the development version from GitHub:

```r
# install.packages("devtools")
devtools::install_github("fabiandistler/lintrhelper")
```

## Why lintrhelper?

Creating custom linters for lintr typically involves:
- Understanding R's XML parse tree structure
- Writing complex XPath expressions
- Handling source expressions and XML nodes
- Setting up proper testing

`lintrhelper` simplifies all of this with intuitive helper functions.

## Quick Start

### Create a Simple Linter

```r
library(lintrhelper)

# Create a linter that warns against using T/F instead of TRUE/FALSE
my_linter <- create_simple_linter(
  xpath = "//SYMBOL[text() = 'T' or text() = 'F']",
  message = "Use TRUE/FALSE instead of T/F.",
  linter_name = "no_t_f_linter",
  type = "warning"
)

# Test it
test_linter(my_linter, "x <- T", should_lint = TRUE)
test_linter(my_linter, "x <- TRUE", should_lint = FALSE)

# Use it with lintr
lintr::lint("my_script.R", linters = my_linter())
```

### Create a Function Call Linter

```r
# Warn against using deprecated functions
no_sapply <- create_function_call_linter(
  function_names = "sapply",
  message = "Use vapply() instead of {function} for type-safe code.",
  linter_name = "no_sapply_linter"
)

test_linter(no_sapply, "result <- sapply(1:10, sqrt)", should_lint = TRUE)
```

### Create an Assignment Linter

```r
# Enforce <- over =
prefer_arrow <- create_assignment_linter(
  forbidden_operators = "=",
  message = "Use <- for assignment, not =.",
  linter_name = "prefer_arrow_assignment"
)

test_linter(prefer_arrow, "x = 5", should_lint = TRUE)
test_linter(prefer_arrow, "x <- 5", should_lint = FALSE)
```

## Main Functions

### Linter Builders

- **`create_simple_linter()`** - Build XPath-based linters quickly
- **`create_function_call_linter()`** - Specialized for linting function calls
- **`create_assignment_linter()`** - Specialized for linting assignment operators

### Testing Utilities

- **`test_linter()`** - Simplified testing for your linters
- **`quick_test()`** - One-liner for rapid XPath testing

### Templates & Examples

- **`linter_template()`** - Get code templates for different linter types
- **`xpath_patterns()`** - Reference guide for common XPath patterns
- **Example linters included**: `no_t_f_linter()`, `no_attach_linter()`, `prefer_arrow_assignment_linter()`, etc.

## Getting Help

### View Templates

```r
# See all available templates
linter_template("all")

# Get specific template
linter_template("function_call")
linter_template("advanced")
```

### XPath Reference

```r
# View common XPath patterns
xpath_patterns()

# See specific pattern category
xpath_patterns("functions")
xpath_patterns("operators")
```

## Examples

### Example 1: No paste0

```r
no_paste0 <- create_function_call_linter(
  function_names = "paste0",
  message = "Consider using paste(..., sep = '') instead of {function}.",
  linter_name = "no_paste0_linter",
  type = "style"
)

# Test
code <- "result <- paste0(first_name, ' ', last_name)"
test_linter(no_paste0, code, should_lint = TRUE, message_pattern = "paste")
```

### Example 2: Multiple Deprecated Functions

```r
deprecated_linter <- create_function_call_linter(
  function_names = c("sapply", "mapply", "tapply"),
  message = "Function {function} is deprecated in our style guide.",
  linter_name = "deprecated_functions"
)

test_linter(
  deprecated_linter,
  "result <- sapply(x, mean)",
  n_lints = 1
)
```

### Example 3: Advanced Custom Linter

For more complex linting logic, you can still use the full lintr API:

```r
library(lintr)
library(xml2)

complex_linter <- function() {
  lintr::Linter(function(source_expression) {
    if (!lintr::is_lint_level(source_expression, "file")) {
      return(list())
    }

    xml <- source_expression$full_xml_parsed_content

    # Your custom logic here
    nodes <- xml2::xml_find_all(xml, "//SYMBOL_FUNCTION_CALL")

    bad_nodes <- Filter(function(node) {
      func_name <- xml2::xml_text(node)
      grepl("^old_", func_name)  # Custom pattern
    }, nodes)

    lintr::xml_nodes_to_lints(
      bad_nodes,
      source_expression = source_expression,
      lint_message = "This function is deprecated.",
      type = "warning"
    )
  })
}
```

## Testing Your Linters

The `test_linter()` function provides several ways to verify your linters:

```r
my_linter <- create_simple_linter(
  xpath = "//SYMBOL[text() = 'T']",
  message = "Don't use T",
  linter_name = "no_t"
)

# Basic: should it lint or not?
test_linter(my_linter, "x <- T", should_lint = TRUE)
test_linter(my_linter, "x <- TRUE", should_lint = FALSE)

# Check exact number of lints
test_linter(my_linter, "a <- T; b <- T", n_lints = 2)

# Check message content
test_linter(
  my_linter,
  "x <- T",
  should_lint = TRUE,
  message_pattern = "Don't use"
)
```

## Quick XPath Testing

When developing XPath expressions, use `quick_test()`:

```r
# Test if your XPath matches what you expect
quick_test("//SYMBOL[text() = 'T']", "x <- T")
# Found 1 lint(s):
# <text>:1:6: warning: [temp_test_linter] Found match
# x <- T
#      ^

quick_test("//SYMBOL_FUNCTION_CALL[text() = 'mean']", "x <- mean(y)")
# Found 1 lint(s): ...
```

## Learn More

- [Creating linters (official lintr guide)](https://lintr.r-lib.org/articles/creating_linters.html)
- [XPath tutorial](https://www.w3schools.com/xml/xpath_intro.asp)
- [Understanding R's parse tree](https://github.com/r-lib/lintr#how-it-works)

## Contributing

Contributions are welcome! Please feel free to submit a Pull Request.

## License

MIT License
