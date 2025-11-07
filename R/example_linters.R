#' Example Linters
#'
#' A collection of example linters that demonstrate common patterns.
#' These can be used as-is or as templates for your own linters.
#'
#' @name example_linters
#' @rdname example_linters
NULL


#' @describeIn example_linters Warns against using T/F instead of TRUE/FALSE
#' @export
no_t_f_linter <- function() {
  create_simple_linter(
    xpath = "//SYMBOL[text() = 'T' or text() = 'F']",
    message = "Use TRUE or FALSE instead of T or F.",
    linter_name = "no_t_f_linter",
    type = "warning"
  )()
}


#' @describeIn example_linters Warns against using attach()
#' @export
no_attach_linter <- function() {
  create_function_call_linter(
    function_names = "attach",
    message = "Avoid using attach(). Use with() or explicit references instead.",
    linter_name = "no_attach_linter",
    type = "warning"
  )()
}


#' @describeIn example_linters Suggests using <- instead of = for assignment
#' @export
prefer_arrow_assignment_linter <- function() {
  create_assignment_linter(
    forbidden_operators = "=",
    message = "Use <- for assignment instead of =.",
    linter_name = "prefer_arrow_assignment",
    type = "style"
  )()
}


#' @describeIn example_linters Warns against 1:length(x) pattern
#' @export
no_one_length_linter <- function() {
  # This is a more advanced example showing custom XPath
  lintr::Linter(function(source_expression) {
    if (!lintr::is_lint_level(source_expression, "file")) {
      return(list())
    }

    xml <- source_expression$full_xml_parsed_content

    # Look for pattern: 1:length(...)
    # In XML this is: OP-COLON with NUM_CONST[1] on left and length call on right
    xpath <- "
      //OP-COLON[
        preceding-sibling::NUM_CONST[text() = '1'] and
        following-sibling::expr[SYMBOL_FUNCTION_CALL[text() = 'length']]
      ]
    "

    bad_nodes <- xml2::xml_find_all(xml, xpath)

    lintr::xml_nodes_to_lints(
      bad_nodes,
      source_expression = source_expression,
      lint_message = "Use seq_along() instead of 1:length().",
      type = "warning"
    )
  })
}


#' @describeIn example_linters Style guide: warns against sapply
#' @export
no_sapply_linter <- function() {
  create_function_call_linter(
    function_names = "sapply",
    message = "Use vapply() instead of sapply() for type-safe code.",
    linter_name = "no_sapply_linter",
    type = "style"
  )()
}
