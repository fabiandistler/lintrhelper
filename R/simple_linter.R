create_linter_factory <- function(xpath, message, type, placeholder = NULL) {
  function() {
    lintr::Linter(function(source_expression) {
      # Return early if this is not a full expression
      if (!lintr::is_lint_level(source_expression, "file")) {
        return(list())
      }

      # Find all matching nodes
      bad_nodes <- xml2::xml_find_all(
        source_expression$full_xml_parsed_content,
        xpath
      )

      if (!is.null(placeholder)) {
        messages <- vapply(bad_nodes, function(node) {
          gsub(
            paste0("\\{", placeholder, "\\}"),
            xml2::xml_text(node),
            message
          )
        }, character(1))
      } else {
        messages <- message
      }

      # Convert to lints
      lintr::xml_nodes_to_lints(
        bad_nodes,
        source_expression = source_expression,
        lint_message = messages,
        type = type
      )
    })
  }
}


#' Forbid Specific Symbols (Variable Names)
#'
#' Create a linter that flags specific symbol names. Useful for banning
#' certain variable names or enforcing naming conventions.
#'
#' @param symbols Character vector of symbol names to forbid.
#' @param message The lint message. Use \{symbol\} as placeholder for the symbol name.
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

  # Build XPath internally
  xpath_condition <- paste(
    sprintf("text() = '%s'", symbols),
    collapse = " or "
  )
  xpath <- sprintf("//SYMBOL[%s]", xpath_condition)

  create_linter_factory(xpath, message, type, placeholder = "symbol")
}


#' Create a Simple XPath-Based Linter
#'
#' This function simplifies the creation of custom linters using XPath expressions.
#' It wraps the standard lintr pattern of finding XML nodes and converting them to lints.
#'
#' @param xpath A character string containing the XPath expression to match problematic code.
#' @param message A character string with the lint message to display. Can use \{function\}
#'   placeholder which will be replaced with the function name if applicable.
#' @param linter_name A character string naming the linter (used for the lint type).
#' @param type The type of lint to create. Defaults to "warning". Can be "error", "warning", or "style".
#'
#' @return A linter function that can be used with lintr.
#'
#' @examples
#' \dontrun{
#' # Create a linter that warns against using T/F instead of TRUE/FALSE
#' tf_linter <- create_simple_linter(
#'   xpath = "//SYMBOL[text() = 'T' or text() = 'F']",
#'   message = "Use TRUE/FALSE instead of T/F.",
#'   linter_name = "tf_linter"
#' )
#'
#' # Use it with lintr
#' lintr::lint("x <- T", linters = tf_linter())
#' }
#'
#' @export
create_simple_linter <- function(xpath,
                                  message,
                                  linter_name,
                                  type = c("warning", "error", "style")) {
  type <- match.arg(type)

  # Return a linter factory function
  create_linter_factory(xpath, message, type)
}


#' Create a Function Call Linter
#'
#' Specialized helper for creating linters that target specific function calls.
#' This is a common pattern in linting, so this function makes it even easier.
#'
#' @param function_names Character vector of function names to lint against.
#' @param message A character string with the lint message. Use \{function\} as a
#'   placeholder for the actual function name found.
#' @param linter_name A character string naming the linter.
#' @param type The type of lint. Defaults to "warning".
#'
#' @return A linter function.
#'
#' @examples
#' \dontrun{
#' # Warn against using paste0 in favor of paste
#' no_paste0_linter <- create_function_call_linter(
#'   function_names = "paste0",
#'   message = "Use paste(..., sep = '') instead of {function}.",
#'   linter_name = "no_paste0_linter"
#' )
#' }
#'
#' @export
create_function_call_linter <- function(function_names,
                                         message,
                                         linter_name,
                                         type = c("warning", "error", "style")) {
  type <- match.arg(type)

  # Build XPath for multiple function names
  xpath_condition <- paste(
    sprintf("text() = '%s'", function_names),
    collapse = " or "
  )
  xpath <- sprintf("//SYMBOL_FUNCTION_CALL[%s]", xpath_condition)

  # Replace {function} placeholder with each node's function name.
  create_linter_factory(xpath, message, type, placeholder = "function")
}


#' Create an Assignment Linter
#'
#' Helper for creating linters that check assignment patterns.
#'
#' @param forbidden_operators Character vector of forbidden assignment operators
#'   (e.g., c("=", "<<-")).
#' @param message The lint message to display.
#' @param linter_name A character string naming the linter.
#' @param type The type of lint. Defaults to "style".
#'
#' @return A linter function.
#'
#' @examples
#' \dontrun{
#' # Enforce <- over =
#' no_equals_assignment_linter <- create_assignment_linter(
#'   forbidden_operators = "=",
#'   message = "Use <- for assignment, not =.",
#'   linter_name = "no_equals_assignment"
#' )
#' }
#'
#' @export
create_assignment_linter <- function(forbidden_operators,
                                      message,
                                      linter_name,
                                      type = c("style", "warning", "error")) {
  type <- match.arg(type)

  # Build XPath for assignment operators
  # In R's XML, assignments are typically LEFT_ASSIGN, RIGHT_ASSIGN, or EQ_ASSIGN
  operator_map <- list(
    "<-" = "LEFT_ASSIGN",
    "<<-" = "LEFT_ASSIGN", # Super assignment also uses LEFT_ASSIGN
    "->" = "RIGHT_ASSIGN",
    "->>" = "RIGHT_ASSIGN",
    "=" = "EQ_ASSIGN"
  )

  xml_operators <- unique(unlist(operator_map[forbidden_operators]))

  # Match operator nodes directly (their parent depends on operator: <expr> for <-,
  # <expr_or_assign_or_help> for =, etc.).
  xpath <- paste(sprintf("//%s", xml_operators), collapse = " | ")

  create_linter_factory(xpath, message, type)
}
