# lintrhelper

> Helper Functions for Creating Custom Lintr Rules - **No XPath
> Knowledge Required!**

`lintrhelper` makes it incredibly easy to create custom linters for the
[lintr](https://lintr.r-lib.org/) package. You don’t need to know XPath,
XML, or parse trees - just use simple, intuitive functions to describe
what you want to lint!

📖 **Documentation site:**
<https://fabiandistler.github.io/lintrhelper/>

## Installation

Install the development version from GitHub with either `pak`
(recommended) or `devtools`:

``` r

# install.packages("pak")
pak::pak("fabiandistler/lintrhelper")

# or
# install.packages("devtools")
devtools::install_github("fabiandistler/lintrhelper")
```

## Hello World

Three lines to ban `T`/`F` in any R script:

``` r

library(lintrhelper)
no_t_f <- forbid_symbols(c("T", "F"), "Use TRUE/FALSE instead of {symbol}.")
lintr::lint("script.R", linters = list(no_t_f()))
```

That’s it. The rest of this README walks through the building blocks.

## Why lintrhelper?

Creating custom linters for lintr typically involves: - Understanding
R’s XML parse tree structure - Writing complex XPath expressions -
Handling source expressions and XML nodes - Setting up proper testing

`lintrhelper` **eliminates all of this complexity**! Just describe what
you want in plain terms.

## Quick Start - No XPath Required!

### Forbid Specific Symbols/Variables

``` r

library(lintrhelper)

# Ban T and F - just list them!
no_t_f <- forbid_symbols(
  c("T", "F"),
  "Use TRUE/FALSE instead of {symbol}."
)

# Test it
test_linter(no_t_f, "x <- T", should_lint = TRUE)
test_linter(no_t_f, "x <- TRUE", should_lint = FALSE)

# Use with lintr
lintr::lint("my_script.R", linters = no_t_f())
```

### Forbid Specific Functions

``` r

# Ban dangerous functions - just name them!
no_attach <- forbid_functions(
  "attach",
  "Don't use {function}(). Use with() instead."
)

# Or suggest alternatives automatically
no_sapply <- forbid_functions(
  "sapply",
  alternatives = "vapply"  # Auto-generates helpful message!
)

# Ban multiple functions at once
no_deprecated <- forbid_functions(
  c("sapply", "mapply", "tapply"),
  "Function {function}() is discouraged."
)
```

### Enforce Naming Conventions

``` r

# Require snake_case - just give a regex pattern!
snake_case <- require_naming_pattern(
  "^[a-z][a-z0-9_]*$",
  "Variable '{symbol}' should use snake_case."
)

test_linter(snake_case, "myVar <- 1", should_lint = TRUE)
test_linter(snake_case, "my_var <- 1", should_lint = FALSE)
```

### Enforce Assignment Style

``` r

# Prefer <- (the most common style)
use_arrow <- enforce_assignment_operator("<-")

test_linter(use_arrow, "x = 5", should_lint = TRUE)
test_linter(use_arrow, "x <- 5", should_lint = FALSE)
```

## Main Functions

### 🚀 High-Level Functions (No XPath!)

These functions let you create linters without any XPath knowledge:

- **[`forbid_symbols()`](https://fabiandistler.github.io/lintrhelper/reference/forbid_symbols.md)** -
  Ban specific variable names
- **[`forbid_functions()`](https://fabiandistler.github.io/lintrhelper/reference/forbid_functions.md)** -
  Ban specific function calls (with optional alternatives)
- **[`require_naming_pattern()`](https://fabiandistler.github.io/lintrhelper/reference/require_naming_pattern.md)** -
  Enforce naming conventions with regex
- **[`require_function_naming_pattern()`](https://fabiandistler.github.io/lintrhelper/reference/require_function_naming_pattern.md)** -
  Enforce function naming conventions
- **[`enforce_assignment_operator()`](https://fabiandistler.github.io/lintrhelper/reference/enforce_assignment_operator.md)** -
  Prefer `<-`, `=`, or `->`
- **[`require_function_arguments()`](https://fabiandistler.github.io/lintrhelper/reference/require_function_arguments.md)** -
  Ensure functions are called with specific arguments
- **[`limit_line_length()`](https://fabiandistler.github.io/lintrhelper/reference/limit_line_length.md)** -
  Enforce maximum line length

### 🧪 Testing Utilities

- **[`test_linter()`](https://fabiandistler.github.io/lintrhelper/reference/test_linter.md)** -
  Simplified testing for your linters
- **[`quick_test()`](https://fabiandistler.github.io/lintrhelper/reference/quick_test.md)** -
  One-liner for rapid testing

### 📚 Advanced (If You Want XPath)

For advanced users who want more control:

- **[`create_simple_linter()`](https://fabiandistler.github.io/lintrhelper/reference/create_simple_linter.md)** -
  Build XPath-based linters
- **[`create_function_call_linter()`](https://fabiandistler.github.io/lintrhelper/reference/create_function_call_linter.md)** -
  XPath-based function call linters
- **[`create_assignment_linter()`](https://fabiandistler.github.io/lintrhelper/reference/create_assignment_linter.md)** -
  XPath-based assignment linters
- **[`linter_template()`](https://fabiandistler.github.io/lintrhelper/reference/linter_template.md)** -
  Code templates
- **[`xpath_patterns()`](https://fabiandistler.github.io/lintrhelper/reference/xpath_patterns.md)** -
  XPath reference guide

### 💡 Ready-to-Use Examples

- [`no_t_f_linter()`](https://fabiandistler.github.io/lintrhelper/reference/example_linters.md),
  [`no_attach_linter()`](https://fabiandistler.github.io/lintrhelper/reference/example_linters.md),
  [`prefer_arrow_assignment_linter()`](https://fabiandistler.github.io/lintrhelper/reference/example_linters.md),
  [`no_sapply_linter()`](https://fabiandistler.github.io/lintrhelper/reference/example_linters.md),
  [`no_one_length_linter()`](https://fabiandistler.github.io/lintrhelper/reference/example_linters.md)

## Getting Help

### View Templates

``` r

# See all available templates
linter_template("all")

# Get specific template
linter_template("function_call")
linter_template("advanced")
```

### XPath Reference

``` r

# View common XPath patterns
xpath_patterns()

# See specific pattern category
xpath_patterns("functions")
xpath_patterns("operators")
```

## More Examples

### Ban Single-Letter Variables

``` r

# No XPath needed - just list the letters!
no_single_letters <- forbid_symbols(
  letters,  # a, b, c, ..., z
  "Avoid single-letter variable name '{symbol}'."
)

test_linter(no_single_letters, "x <- 5", should_lint = TRUE)
test_linter(no_single_letters, "count <- 5", should_lint = FALSE)
```

### Enforce Function Naming Convention

``` r

# Functions should start with verbs
verb_functions <- require_function_naming_pattern(
  "^(get|set|calculate|compute|check|is|has|create|update|delete|find|load|save)",
  "Function '{function}' should start with a verb."
)

test_linter(verb_functions, "result <- process()", should_lint = TRUE)
test_linter(verb_functions, "result <- calculate_total()", should_lint = FALSE)
```

### Forbid “temp” in Names

``` r

# Use invert=TRUE to forbid patterns that DO match
no_temp_names <- require_naming_pattern(
  "temp",
  "Variable '{symbol}' should not contain 'temp'.",
  invert = TRUE
)

test_linter(no_temp_names, "temp_var <- 1", should_lint = TRUE)
test_linter(no_temp_names, "result <- 1", should_lint = FALSE)
```

### Require Explicit Arguments

``` r

# Always specify stringsAsFactors
explicit_saf <- require_function_arguments(
  "data.frame",
  "stringsAsFactors",
  "Always specify stringsAsFactors explicitly in data.frame()."
)

# Lints this:
test_linter(explicit_saf, "df <- data.frame(x = 1:3)", should_lint = TRUE)

# Passes this:
test_linter(
  explicit_saf,
  "df <- data.frame(x = 1:3, stringsAsFactors = FALSE)",
  should_lint = FALSE
)
```

### Team Style Guide Example

``` r

# Combine multiple rules for your team
my_team_linters <- lintr::linters_with_defaults(
  no_t_f = forbid_symbols(c("T", "F"), "Use TRUE/FALSE")(),
  snake_case = require_naming_pattern("^[a-z][a-z0-9_]*$", "Use snake_case")(),
  use_arrow = enforce_assignment_operator("<-")(),
  no_attach = forbid_functions("attach", alternatives = "with")(),
  line_length = limit_line_length(80)()
)

# Apply to your project
lintr::lint_package(linters = my_team_linters)
```

## Testing Your Linters

The
[`test_linter()`](https://fabiandistler.github.io/lintrhelper/reference/test_linter.md)
function provides several ways to verify your linters:

``` r

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

When developing XPath expressions, use
[`quick_test()`](https://fabiandistler.github.io/lintrhelper/reference/quick_test.md):

``` r

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

- [lintrhelper documentation
  site](https://fabiandistler.github.io/lintrhelper/) — full reference
  and articles
- [Creating linters (official lintr
  guide)](https://lintr.r-lib.org/articles/creating_linters.html)
- [XPath tutorial](https://www.w3schools.com/xml/xpath_intro.asp)
- [Understanding R’s parse
  tree](https://github.com/r-lib/lintr#how-it-works)

## Contributing

Contributions are welcome! Please feel free to submit a Pull Request.

## License

MIT License
