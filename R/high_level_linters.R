#' Wrap a lintr Linter in This Package's Message and Type
#'
#' Most of the helpers here ask a question lintr already answers, so they
#' delegate the finding to lintr's own linter and keep only what they add:
#' the message, with its `{placeholder}` filled in, and the lint type. What
#' code is flagged, and where, is then lintr's to get right and lintr's to
#' keep right across releases.
#'
#' The wrapper runs at the same lint level as the linter it wraps, so the
#' framework hands it the source expressions that linter expects.
#'
#' @param built_in The [lintr::Linter()] to delegate the finding to.
#' @param message A function of one lint returning its message.
#' @param type The lint type to report.
#'
#' @return A linter factory function.
#'
#' @keywords internal
#' @noRd
delegate_linter <- function(built_in, message, type) {
  function() {
    lintr::Linter(
      linter_level = attr(built_in, "linter_level"),
      function(source_expression) {
        lapply(built_in(source_expression), function(lint) {
          lint$message <- message(lint)
          lint$type <- type
          lint
        })
      }
    )
  }
}


#' The Source Text a Lint Points At
#'
#' The flagged function name, symbol, or operator, read back off the line
#' the lint carries. It is what fills a `{function}`, `{symbol}`, or
#' `{operator}` placeholder.
#'
#' Surrounding backticks and quotes come off: a name written
#' `` `quoted name` ``, or assigned through `assign("name", ...)`, is
#' still that name, and a message reads better naming it than naming its
#' quoting.
#'
#' @param lint A lint produced by a lintr linter.
#'
#' @return The flagged text, or `""` when the lint marks no range.
#'
#' @keywords internal
#' @noRd
lint_source <- function(lint) {
  if (length(lint$ranges) == 0L) {
    return("")
  }

  range <- lint$ranges[[1L]]
  text <- substring(lint$line, range[[1L]], range[[2L]])

  gsub("^[\"'`]|[\"'`]$", "", text)
}


#' Fill a Placeholder With the Flagged Source Text
#'
#' @param message The message template.
#' @param placeholder The placeholder name, without braces.
#'
#' @return A function of one lint returning its message.
#'
#' @keywords internal
#' @noRd
fill_placeholder <- function(message, placeholder) {
  target <- paste0("{", placeholder, "}")

  function(lint) {
    gsub(target, lint_source(lint), message, fixed = TRUE)
  }
}


#' Forbid Specific Functions
#'
#' Create a linter that flags specific function calls. This is a simplified
#' version that doesn't require XPath knowledge.
#'
#' @details
#' A thin wrapper over [lintr::undesirable_function_linter()], which is
#' what finds the calls. Only calls are flagged, not the bare symbol, so
#' passing a forbidden function by name is not a lint.
#'
#' @param functions Character vector of function names to forbid.
#' @param message The lint message. Use \{function\} as placeholder.
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

  undesirable <- as.list(rep(NA_character_, length(functions)))
  names(undesirable) <- functions

  delegate_linter(
    lintr::undesirable_function_linter(
      fun = undesirable,
      symbol_is_undesirable = FALSE
    ),
    fill_placeholder(message, "function"),
    type
  )
}


#' Require Naming Pattern
#'
#' Create a linter that enforces naming conventions for symbols (variables).
#' No XPath knowledge required - just specify the pattern!
#'
#' @details
#' A thin wrapper over [lintr::object_name_linter()], which is what finds
#' the names. It checks names where they are *defined* — an assignment, a
#' function argument — and reports each definition once, rather than every
#' place the name is used.
#'
#' `invert = TRUE` is expressed as a negated pattern, so a name is flagged
#' when `pattern` matches it anywhere. Anchors keep their meaning:
#' `"^[A-Z]"` inverts to "starts with an uppercase letter".
#'
#' @param pattern Regular expression pattern that names must match.
#' @param message The lint message. Use \{symbol\} as placeholder.
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

  regex <- if (invert) {
    sprintf("^(?![\\s\\S]*(?:%s))", pattern)
  } else {
    pattern
  }

  delegate_linter(
    lintr::object_name_linter(regexes = c(convention = regex)),
    fill_placeholder(message, "symbol"),
    type
  )
}


#' Require Function Naming Pattern
#'
#' Like require_naming_pattern but specifically for function names.
#'
#' @param pattern Regular expression that function names must match.
#' @param message The lint message. Use \{function\} as placeholder.
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
#' @details
#' A thin wrapper over [lintr::assignment_linter()], which is what finds
#' the assignments. Preferring an arrow allows its super-assignment form
#' too — `<<-` goes with `<-`, `->>` with `->` — since the choice being
#' enforced is which arrow to write, not which scope to assign in. `%<>%`
#' is left alone; it is magrittr's pipe-assign, not one of the three.
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

  if (is.null(message)) {
    message <- sprintf("Use %s for assignment, not {operator}.", prefer)
  }

  super_assignment <- c("<-" = "<<-", "->" = "->>")
  allowed <- c(prefer, super_assignment[names(super_assignment) == prefer])

  delegate_linter(
    lintr::assignment_linter(operator = unname(c(allowed, "%<>%"))),
    fill_placeholder(message, "operator"),
    type
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
#' @details
#' A thin wrapper over [lintr::line_length_linter()], which is what
#' measures the lines.
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

  delegate_linter(
    lintr::line_length_linter(length = max_length),
    function(lint) sprintf("%s (currently %d)", message, nchar(lint$line)),
    type
  )
}
