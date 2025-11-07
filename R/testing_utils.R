#' Test a Linter with Example Code
#'
#' Simplified interface for testing custom linters. This function wraps
#' lintr's testing functionality to make it easier to quickly test your
#' custom linters during development.
#'
#' @param linter A linter function (or linter factory that returns a linter).
#' @param code Character string or vector of code to lint.
#' @param should_lint Logical. If TRUE (default), expects the linter to find issues.
#'   If FALSE, expects the linter to find no issues.
#' @param n_lints Integer. Expected number of lints. If NULL, just checks if
#'   linting occurred (or didn't occur based on should_lint).
#' @param message_pattern Optional regex pattern to match against lint messages.
#'
#' @return Invisibly returns the lints found (as a list).
#'
#' @examples
#' \dontrun{
#' my_linter <- create_simple_linter(
#'   xpath = "//SYMBOL[text() = 'T']",
#'   message = "Don't use T",
#'   linter_name = "no_t"
#' )
#'
#' # Test that it catches T
#' test_linter(my_linter, "x <- T", should_lint = TRUE)
#'
#' # Test that it doesn't flag TRUE
#' test_linter(my_linter, "x <- TRUE", should_lint = FALSE)
#'
#' # Test exact number of lints
#' test_linter(my_linter, "x <- T; y <- T", n_lints = 2)
#' }
#'
#' @export
test_linter <- function(linter,
                        code,
                        should_lint = TRUE,
                        n_lints = NULL,
                        message_pattern = NULL) {
  # If code is a vector, collapse it
  if (length(code) > 1) {
    code <- paste(code, collapse = "\n")
  }

  # Get the linter function (handle both linter and linter factory)
  if (is.function(linter) && !inherits(linter, "linter")) {
    linter_func <- linter()
  } else {
    linter_func <- linter
  }

  # Run the linter
  lints <- lintr::lint(text = code, linters = linter_func)

  # Check results
  n_found <- length(lints)

  if (!is.null(n_lints)) {
    if (n_found != n_lints) {
      stop(sprintf(
        "Expected %d lint(s) but found %d.\nLints: %s",
        n_lints,
        n_found,
        paste(capture.output(print(lints)), collapse = "\n")
      ))
    }
  } else if (should_lint && n_found == 0) {
    stop("Expected linter to find issues but it found none.")
  } else if (!should_lint && n_found > 0) {
    stop(sprintf(
      "Expected linter to find no issues but it found %d.\nLints: %s",
      n_found,
      paste(capture.output(print(lints)), collapse = "\n")
    ))
  }

  # Check message pattern if provided
  if (!is.null(message_pattern) && n_found > 0) {
    messages <- vapply(lints, function(l) l$message, character(1))
    matches <- grepl(message_pattern, messages)

    if (!all(matches)) {
      stop(sprintf(
        "Not all lint messages matched pattern '%s'.\nMessages: %s",
        message_pattern,
        paste(messages, collapse = "; ")
      ))
    }
  }

  # Return lints invisibly for further inspection if needed
  invisible(lints)
}


#' Quick Lint Test
#'
#' Even simpler one-liner for quick testing during development.
#'
#' @param xpath XPath expression to test.
#' @param code Code to test against.
#' @param message Message for the lint (optional, for display).
#'
#' @return The lints found.
#'
#' @examples
#' \dontrun{
#' # Quick test if an XPath matches
#' quick_test("//SYMBOL[text() = 'T']", "x <- T")
#' }
#'
#' @export
quick_test <- function(xpath, code, message = "Found match") {
  temp_linter <- create_simple_linter(
    xpath = xpath,
    message = message,
    linter_name = "temp_test_linter"
  )

  lints <- lintr::lint(text = code, linters = temp_linter())

  if (length(lints) > 0) {
    cat("Found", length(lints), "lint(s):\n")
    print(lints)
  } else {
    cat("No lints found.\n")
  }

  invisible(lints)
}
