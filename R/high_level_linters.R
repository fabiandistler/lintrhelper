#' Forbid Specific Symbols (Variable Names)
#'
#' Create a linter that flags specific symbol names. Useful for banning
#' certain variable names or enforcing naming conventions.
#'
#' @param symbols Character vector of symbol names to forbid.
#' @param message The lint message. Use {symbol} as placeholder for the symbol name.
#' @param type The lint type. Defaults to "style".
#'
#' @return A linter function.
#'
#' @examples
#' \dontrun{
#' # Ban T and F
#' no_t_f <- forbid_symbols(
#'   c("T", "F"),
#'   "Use TRUE/FALSE instead of {symbol}."
#' )
#'
#' # Ban single-letter variable names
#' no_single_letters <- forbid_symbols(
#'   letters,
#'   "Avoid single-letter variable name '{symbol}'."
#' )
#'
#' test_linter(no_t_f, "x <- T", should_lint = TRUE)
#' }
#'
#' @export
forbid_symbols <- function(symbols,
                           message = "Symbol '{symbol}' should not be used.",
                           type = c("style", "warning", "error")) {
  type <- match.arg(type)

  function() {
    # Build XPath internally
    if (length(symbols) == 1) {
      xpath_condition <- sprintf("text() = '%s'", symbols)
    } else {
      conditions <- sprintf("text() = '%s'", symbols)
      xpath_condition <- paste(conditions, collapse = " or ")
    }

    xpath <- sprintf("//SYMBOL[%s]", xpath_condition)

    lintr::Linter(function(source_expression) {
      if (!lintr::is_lint_level(source_expression, "file")) {
        return(list())
      }

      xml <- source_expression$full_xml_parsed_content
      bad_nodes <- xml2::xml_find_all(xml, xpath)

      messages <- vapply(bad_nodes, function(node) {
        gsub("\\{symbol\\}", xml2::xml_text(node), message, fixed = FALSE)
      }, character(1))

      lintr::xml_nodes_to_lints(
        bad_nodes,
        source_expression = source_expression,
        lint_message = messages,
        type = type
      )
    })
  }
}


#' Forbid Specific Functions
#'
#' Create a linter that flags specific function calls. This is a simplified
#' version that doesn't require XPath knowledge.
#'
#' @param functions Character vector of function names to forbid.
#' @param message The lint message. Use {function} as placeholder.
#' @param alternatives Optional character vector of alternative functions to suggest.
#' @param type The lint type. Defaults to "warning".
#'
#' @return A linter function.
#'
#' @examples
#' \dontrun{
#' # Ban attach()
#' no_attach <- forbid_functions(
#'   "attach",
#'   "Don't use {function}(). Use with() instead."
#' )
#'
#' # With alternatives
#' no_sapply <- forbid_functions(
#'   "sapply",
#'   alternatives = "vapply"
#' )
#'
#' # Multiple functions
#' no_deprecated <- forbid_functions(
#'   c("sapply", "mapply", "tapply"),
#'   "Function {function}() is discouraged."
#' )
#' }
#'
#' @export
forbid_functions <- function(functions,
                              message = NULL,
                              alternatives = NULL,
                              type = c("warning", "error", "style")) {
  type <- match.arg(type)

  # Auto-generate message if not provided
  if (is.null(message)) {
    if (!is.null(alternatives)) {
      if (length(alternatives) == 1) {
        message <- sprintf("Use %s() instead of {function}().", alternatives)
      } else {
        alts <- paste(paste0(alternatives, "()"), collapse = " or ")
        message <- sprintf("Use %s instead of {function}().", alts)
      }
    } else {
      message <- "Function {function}() should not be used."
    }
  }

  create_function_call_linter(
    function_names = functions,
    message = message,
    linter_name = "forbid_functions",
    type = type
  )
}


#' Require Naming Pattern
#'
#' Create a linter that enforces naming conventions for symbols (variables).
#' No XPath knowledge required - just specify the pattern!
#'
#' @param pattern Regular expression pattern that names must match.
#' @param message The lint message. Use {symbol} as placeholder.
#' @param type The lint type. Defaults to "style".
#' @param invert If TRUE, flags names that DO match the pattern (for forbidding patterns).
#'
#' @return A linter function.
#'
#' @examples
#' \dontrun{
#' # Require snake_case
#' snake_case_linter <- require_naming_pattern(
#'   "^[a-z][a-z0-9_]*$",
#'   "Variable '{symbol}' should use snake_case."
#' )
#'
#' # Forbid names starting with uppercase (invert = TRUE)
#' no_uppercase_start <- require_naming_pattern(
#'   "^[A-Z]",
#'   "Variable '{symbol}' should not start with uppercase.",
#'   invert = TRUE
#' )
#'
#' test_linter(snake_case_linter, "myVar <- 1", should_lint = TRUE)
#' test_linter(snake_case_linter, "my_var <- 1", should_lint = FALSE)
#' }
#'
#' @export
require_naming_pattern <- function(pattern,
                                    message = "Name '{symbol}' does not follow naming convention.",
                                    type = c("style", "warning", "error"),
                                    invert = FALSE) {
  type <- match.arg(type)

  function() {
    lintr::Linter(function(source_expression) {
      if (!lintr::is_lint_level(source_expression, "file")) {
        return(list())
      }

      xml <- source_expression$full_xml_parsed_content

      all_symbols <- xml2::xml_find_all(xml, "//SYMBOL")
      symbol_names <- vapply(all_symbols, xml2::xml_text, character(1))

      reserved <- symbol_names %in% c("NA", "NULL", "TRUE", "FALSE", "Inf", "NaN")
      matches <- grepl(pattern, symbol_names)
      keep <- if (invert) matches & !reserved else !matches & !reserved

      bad_nodes <- all_symbols[keep]
      messages <- vapply(symbol_names[keep], function(name) {
        gsub("{symbol}", name, message, fixed = TRUE)
      }, character(1), USE.NAMES = FALSE)

      lintr::xml_nodes_to_lints(
        bad_nodes,
        source_expression = source_expression,
        lint_message = messages,
        type = type
      )
    })
  }
}


#' Require Function Naming Pattern
#'
#' Like require_naming_pattern but specifically for function names.
#'
#' @param pattern Regular expression that function names must match.
#' @param message The lint message. Use {function} as placeholder.
#' @param type The lint type. Defaults to "style".
#' @param invert If TRUE, flags names that DO match (for forbidding patterns).
#'
#' @return A linter function.
#'
#' @examples
#' \dontrun{
#' # Require functions to start with verb
#' verb_functions <- require_function_naming_pattern(
#'   "^(get|set|calculate|check|is|has|create|update|delete)",
#'   "Function '{function}' should start with a verb."
#' )
#'
#' # Forbid functions with "temp" in name
#' no_temp_functions <- require_function_naming_pattern(
#'   "temp",
#'   "Function '{function}' should not contain 'temp'.",
#'   invert = TRUE
#' )
#' }
#'
#' @export
require_function_naming_pattern <- function(pattern,
                                             message = "Function '{function}' does not follow naming convention.",
                                             type = c("style", "warning", "error"),
                                             invert = FALSE) {
  type <- match.arg(type)

  function() {
    lintr::Linter(function(source_expression) {
      if (!lintr::is_lint_level(source_expression, "file")) {
        return(list())
      }

      xml <- source_expression$full_xml_parsed_content
      all_functions <- xml2::xml_find_all(xml, "//SYMBOL_FUNCTION_CALL")
      func_names <- vapply(all_functions, xml2::xml_text, character(1))

      matches <- grepl(pattern, func_names)
      keep <- if (invert) matches else !matches

      bad_nodes <- all_functions[keep]
      messages <- vapply(func_names[keep], function(name) {
        gsub("{function}", name, message, fixed = TRUE)
      }, character(1), USE.NAMES = FALSE)

      lintr::xml_nodes_to_lints(
        bad_nodes,
        source_expression = source_expression,
        lint_message = messages,
        type = type
      )
    })
  }
}


#' Enforce Assignment Operator
#'
#' Simple way to enforce a specific assignment operator without XPath.
#'
#' @param prefer Which operator to prefer: "<-", "=", or "->".
#' @param message Optional custom message.
#' @param type The lint type. Defaults to "style".
#'
#' @return A linter function.
#'
#' @examples
#' \dontrun{
#' # Prefer <- (most common)
#' use_arrow <- enforce_assignment_operator("<-")
#'
#' # Prefer =
#' use_equals <- enforce_assignment_operator("=")
#'
#' test_linter(use_arrow, "x = 5", should_lint = TRUE)
#' test_linter(use_arrow, "x <- 5", should_lint = FALSE)
#' }
#'
#' @export
enforce_assignment_operator <- function(prefer = c("<-", "=", "->"),
                                         message = NULL,
                                         type = c("style", "warning", "error")) {
  prefer <- match.arg(prefer)
  type <- match.arg(type)

  # Determine forbidden operators
  all_ops <- c("<-", "=", "->")
  forbidden <- all_ops[all_ops != prefer]

  # Auto-generate message
  if (is.null(message)) {
    message <- sprintf("Use %s for assignment, not {operator}.", prefer)
  }

  # Use existing function
  create_assignment_linter(
    forbidden_operators = forbidden,
    message = message,
    linter_name = "enforce_assignment_operator",
    type = type
  )
}


#' Require Specific Function Arguments
#'
#' Check if certain functions are called with specific arguments.
#' Useful for enforcing explicit parameter usage.
#'
#' @param function_name The function to check.
#' @param required_args Character vector of required argument names.
#' @param message The lint message.
#' @param type The lint type. Defaults to "warning".
#'
#' @return A linter function.
#'
#' @examples
#' \dontrun{
#' # Require explicit stringsAsFactors in data.frame()
#' explicit_saf <- require_function_arguments(
#'   "data.frame",
#'   "stringsAsFactors",
#'   "Always specify stringsAsFactors explicitly in data.frame()."
#' )
#'
#' # This would lint:
#' # data.frame(x = 1:3)
#'
#' # This would pass:
#' # data.frame(x = 1:3, stringsAsFactors = FALSE)
#' }
#'
#' @export
require_function_arguments <- function(function_name,
                                        required_args,
                                        message = sprintf(
                                          "Function %s() should include argument(s): %s",
                                          function_name,
                                          paste(required_args, collapse = ", ")
                                        ),
                                        type = c("warning", "style", "error")) {
  type <- match.arg(type)

  function() {
    lintr::Linter(function(source_expression) {
      if (!lintr::is_lint_level(source_expression, "file")) {
        return(list())
      }

      xml <- source_expression$full_xml_parsed_content

      # Find calls to the specific function
      func_calls <- xml2::xml_find_all(
        xml,
        sprintf("//SYMBOL_FUNCTION_CALL[text() = '%s']", function_name)
      )

      bad_calls <- Filter(function(node) {
        # The call expression is the grandparent: SYMBOL_FUNCTION_CALL is wrapped
        # in an <expr>, which is itself a child of the outer call <expr>.
        call_expr <- xml2::xml_parent(xml2::xml_parent(node))

        # Find named arguments in this call
        named_args <- xml2::xml_find_all(call_expr, ".//SYMBOL_SUB")
        arg_names <- vapply(named_args, xml2::xml_text, character(1))

        # Check if all required args are present
        !all(required_args %in% arg_names)
      }, func_calls)

      lintr::xml_nodes_to_lints(
        bad_calls,
        source_expression = source_expression,
        lint_message = message,
        type = type
      )
    })
  }
}


#' Limit Line Length
#'
#' Flag lines that exceed a certain character length.
#'
#' @param max_length Maximum allowed line length. Default is 80.
#' @param message The lint message.
#' @param type The lint type. Defaults to "style".
#'
#' @return A linter function.
#'
#' @examples
#' \dontrun{
#' # Standard 80 characters
#' line_length_80 <- limit_line_length(80)
#'
#' # More strict
#' line_length_72 <- limit_line_length(72)
#' }
#'
#' @export
limit_line_length <- function(max_length = 80,
                               message = NULL,
                               type = c("style", "warning", "error")) {
  type <- match.arg(type)

  if (is.null(message)) {
    message <- sprintf("Line exceeds %d characters.", max_length)
  }

  function() {
    lintr::Linter(function(source_expression) {
      # Only run once per file, using full file lines
      if (!lintr::is_lint_level(source_expression, "file")) {
        return(list())
      }

      lines <- source_expression$file_lines

      # Find lines that are too long
      long_lines <- which(nchar(lines) > max_length)

      if (length(long_lines) == 0) {
        return(list())
      }

      # Create lints for each long line
      lints <- lapply(long_lines, function(line_num) {
        lintr::Lint(
          filename = source_expression$filename,
          line_number = line_num,
          column_number = max_length + 1,
          type = type,
          message = sprintf("%s (currently %d)", message, nchar(lines[line_num])),
          line = lines[line_num],
          linter = "limit_line_length"
        )
      })

      structure(lints, class = "lints")
    })
  }
}
