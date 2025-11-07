#' Get Linter Template Code
#'
#' Returns template code for creating different types of linters.
#' This is helpful for getting started with custom linters.
#'
#' @param type The type of template to generate. Options include:
#'   \itemize{
#'     \item "simple" - Basic XPath-based linter
#'     \item "function_call" - Function call linter
#'     \item "assignment" - Assignment pattern linter
#'     \item "advanced" - More advanced linter with custom logic
#'     \item "all" - Shows all templates
#'   }
#' @param cat_output Logical. If TRUE (default), prints the template to console.
#'   If FALSE, returns the template as a character string.
#'
#' @return If cat_output is FALSE, returns the template code as a character string.
#'   Otherwise, returns NULL invisibly after printing.
#'
#' @examples
#' # View a simple linter template
#' linter_template("simple")
#'
#' # Get function call linter template as string
#' code <- linter_template("function_call", cat_output = FALSE)
#'
#' @export
linter_template <- function(type = c("simple", "function_call", "assignment", "advanced", "all"),
                            cat_output = TRUE) {
  type <- match.arg(type)

  templates <- list(
    simple = '
# Simple XPath-based linter example
library(lintrhelper)

my_simple_linter <- create_simple_linter(
  xpath = "//SYMBOL[text() = \'T\' or text() = \'F\']",
  message = "Use TRUE/FALSE instead of T/F.",
  linter_name = "no_t_f_linter",
  type = "warning"
)

# Test it
test_linter(my_simple_linter, "x <- T", should_lint = TRUE)
test_linter(my_simple_linter, "x <- TRUE", should_lint = FALSE)
',

    function_call = '
# Function call linter example
library(lintrhelper)

no_paste0_linter <- create_function_call_linter(
  function_names = "paste0",
  message = "Consider using paste(..., sep = \'\') instead of {function}.",
  linter_name = "no_paste0_linter",
  type = "style"
)

# Test it
test_linter(
  no_paste0_linter,
  "result <- paste0(a, b)",
  should_lint = TRUE,
  message_pattern = "paste"
)

# Can also flag multiple functions
deprecated_funcs_linter <- create_function_call_linter(
  function_names = c("sapply", "mapply"),
  message = "Use purrr::{function} alternatives instead.",
  linter_name = "deprecated_funcs"
)
',

    assignment = '
# Assignment linter example
library(lintrhelper)

no_equals_assign_linter <- create_assignment_linter(
  forbidden_operators = "=",
  message = "Use <- for assignment, not =.",
  linter_name = "no_equals_assignment",
  type = "style"
)

# Test it
test_linter(no_equals_assign_linter, "x = 5", should_lint = TRUE)
test_linter(no_equals_assign_linter, "x <- 5", should_lint = FALSE)
',

    advanced = '
# Advanced linter with custom logic
library(lintr)
library(xml2)

# This shows how to build a linter with more complex logic
# that goes beyond simple XPath matching

complex_linter <- function() {
  lintr::Linter(function(source_expression) {
    if (!lintr::is_lint_level(source_expression, "file")) {
      return(list())
    }

    xml <- source_expression$full_xml_parsed_content

    # Find candidate nodes
    nodes <- xml2::xml_find_all(xml, "//SYMBOL_FUNCTION_CALL")

    # Custom filtering logic
    bad_nodes <- Filter(function(node) {
      func_name <- xml2::xml_text(node)

      # Custom condition: flag functions starting with "old_"
      grepl("^old_", func_name)
    }, nodes)

    # Generate lints with custom messages per node
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

# Test the advanced linter
lintr::lint(text = "result <- old_calculate(x)", linters = complex_linter())
'
  )

  # Determine what to show
  if (type == "all") {
    output <- paste(
      "=== ALL LINTER TEMPLATES ===\n",
      paste(sprintf("\n## %s ##\n%s", toupper(names(templates)), templates),
            collapse = "\n"),
      sep = ""
    )
  } else {
    output <- templates[[type]]
  }

  if (cat_output) {
    cat(output)
    invisible(NULL)
  } else {
    output
  }
}


#' List Common XPath Patterns for R Code
#'
#' Provides a reference of useful XPath patterns for linting R code.
#'
#' @param pattern Optional. If specified, shows only the requested pattern.
#'   Options: "symbols", "functions", "operators", "literals", "comments".
#'
#' @return Prints XPath patterns to console.
#'
#' @examples
#' # See all patterns
#' xpath_patterns()
#'
#' # See just function-related patterns
#' xpath_patterns("functions")
#'
#' @export
xpath_patterns <- function(pattern = NULL) {
  patterns <- list(
    symbols = c(
      "All symbols: //SYMBOL",
      "Specific symbol: //SYMBOL[text() = 'my_var']",
      "Symbols matching pattern: //SYMBOL[contains(text(), 'temp')]"
    ),

    functions = c(
      "Function calls: //SYMBOL_FUNCTION_CALL",
      "Specific function: //SYMBOL_FUNCTION_CALL[text() = 'mean']",
      "Function definitions: //FUNCTION"
    ),

    operators = c(
      "All assignments: //LEFT_ASSIGN | //RIGHT_ASSIGN | //EQ_ASSIGN",
      "Left assign (<-): //LEFT_ASSIGN",
      "Equals assign (=): //EQ_ASSIGN",
      "Arithmetic ops: //OP-PLUS | //OP-MINUS | //OP-TIMES | //OP-DIVIDE"
    ),

    literals = c(
      "Numeric constants: //NUM_CONST",
      "String constants: //STR_CONST",
      "NULL values: //NULL_CONST"
    ),

    comments = c(
      "All comments: //COMMENT"
    ),

    common_patterns = c(
      "Arguments in function call: //SYMBOL_FUNCTION_CALL/following-sibling::expr",
      "Assignment target: //LEFT_ASSIGN/preceding-sibling::expr",
      "Assignment value: //LEFT_ASSIGN/following-sibling::expr",
      "If conditions: //IF/following-sibling::expr[1]"
    )
  )

  if (!is.null(pattern)) {
    if (!pattern %in% names(patterns)) {
      stop(sprintf(
        "Unknown pattern '%s'. Available: %s",
        pattern,
        paste(names(patterns), collapse = ", ")
      ))
    }
    cat(sprintf("=== %s ===\n", toupper(pattern)))
    cat(paste(patterns[[pattern]], collapse = "\n"), "\n")
  } else {
    for (name in names(patterns)) {
      cat(sprintf("\n=== %s ===\n", toupper(name)))
      cat(paste(patterns[[name]], collapse = "\n"), "\n")
    }
  }

  invisible(NULL)
}
