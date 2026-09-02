# Characterization tests for R/testing_utils.R.
#
# These lock in the behaviour observed on 2026-09-02, not the behaviour the
# documentation promises. Names prefixed with SUSPECT_ record behaviour that
# looks wrong; see FINDINGS.md. They are documentation, not approval.

no_t_factory <- function() {
  create_simple_linter(
    xpath = "//SYMBOL[text() = 'T']",
    message = "Don't use T",
    linter_name = "no_t"
  )
}


# --- test_linter(): the happy paths -----------------------------------------

test_that("test_linter accepts a linter factory and returns the lints invisibly", {
  skip_if_not_installed("lintr")

  result <- withVisible(test_linter(no_t_factory(), "x <- T"))

  expect_false(result$visible)
  expect_s3_class(result$value, "lints")
  expect_length(result$value, 1L)
  expect_identical(result$value[[1L]]$message, "Don't use T")
})


test_that("test_linter accepts an already instantiated linter object", {
  skip_if_not_installed("lintr")

  linter <- no_t_factory()()
  expect_s3_class(linter, "linter")

  expect_length(test_linter(linter, "x <- T"), 1L)
})


test_that("test_linter collapses a code vector into newline separated lines", {
  skip_if_not_installed("lintr")

  lints <- test_linter(no_t_factory(), c("x <- T", "y <- T"), n_lints = 2L)

  expect_identical(vapply(lints, function(l) l$line_number, integer(1L)), 1:2)
})


# --- test_linter(): the failure paths ---------------------------------------

test_that("test_linter fails when should_lint is TRUE and nothing is found", {
  skip_if_not_installed("lintr")

  expect_error(
    test_linter(no_t_factory(), "x <- TRUE"),
    "Expected linter to find issues but it found none.",
    fixed = TRUE
  )
})


test_that("test_linter fails when should_lint is FALSE and something is found", {
  skip_if_not_installed("lintr")

  expect_error(
    test_linter(no_t_factory(), "x <- T", should_lint = FALSE),
    "Expected linter to find no issues but it found 1."
  )
})


test_that("test_linter reports the expected and the observed count on a mismatch", {
  skip_if_not_installed("lintr")

  expect_error(
    test_linter(no_t_factory(), "x <- T", n_lints = 2L),
    "Expected 2 lint\\(s\\) but found 1\\."
  )
})


test_that("test_linter checks message_pattern against every lint message", {
  skip_if_not_installed("lintr")

  expect_length(
    test_linter(no_t_factory(), "x <- T", message_pattern = "Don't use"),
    1L
  )
  expect_error(
    test_linter(no_t_factory(), "x <- T", message_pattern = "ZZZ"),
    "Not all lint messages matched pattern 'ZZZ'",
    fixed = TRUE
  )
})


# --- test_linter(): edge cases ----------------------------------------------

test_that("test_linter treats empty and NA code as code without lints", {
  skip_if_not_installed("lintr")

  expect_length(test_linter(no_t_factory(), "", should_lint = FALSE), 0L)
  expect_length(
    test_linter(no_t_factory(), character(0), should_lint = FALSE),
    0L
  )
  expect_length(
    test_linter(no_t_factory(), NA_character_, should_lint = FALSE),
    0L
  )
})


test_that("test_linter carries non-ASCII lint messages through unchanged", {
  skip_if_not_installed("lintr")

  message <- "Größe: keine Umlaute im Code"
  umlaut_linter <- create_simple_linter(
    xpath = "//SYMBOL[text() = 'groesse']",
    message = message,
    linter_name = "umlaut"
  )

  lints <- test_linter(
    umlaut_linter,
    "groesse <- 1",
    message_pattern = "Größe"
  )

  expect_identical(lints[[1L]]$message, message)
})


test_that("test_linter propagates a non-logical should_lint as an R condition error", {
  skip_if_not_installed("lintr")

  expect_error(
    test_linter(no_t_factory(), "x <- T", should_lint = NA),
    "missing value where TRUE/FALSE needed",
    fixed = TRUE
  )
})


test_that("test_linter rejects a linter that is neither a function nor NULL", {
  skip_if_not_installed("lintr")

  # The message is lintr's, so match only the word it is certain to carry.
  expect_error(test_linter("//SYMBOL", "x <- T"), "linter")
})


# --- test_linter(): SUSPECT behaviour ---------------------------------------

test_that("SUSPECT_test_linter runs lintr's default linters when linter is NULL", {
  skip_if_not_installed("lintr")

  # `is.function(NULL)` is FALSE, so NULL is passed straight to
  # `lintr::lint(linters = NULL)`, which falls back to the default linter
  # set. A test written against a linter that failed to build therefore
  # passes while asserting nothing about that linter.
  lints <- test_linter(NULL, "x <- T")

  expect_length(lints, 1L)
  expect_identical(lints[[1L]]$linter, "T_and_F_symbol_linter")
})


test_that("SUSPECT_test_linter lets n_lints silently override should_lint", {
  skip_if_not_installed("lintr")

  # should_lint = TRUE and n_lints = 0 contradict each other; the count wins
  # and the contradiction is never reported.
  expect_length(
    test_linter(no_t_factory(), "x <- TRUE", should_lint = TRUE, n_lints = 0L),
    0L
  )
  expect_length(
    test_linter(
      no_t_factory(),
      "x <- T; y <- T",
      should_lint = FALSE,
      n_lints = 2L
    ),
    2L
  )
})


test_that("SUSPECT_test_linter skips message_pattern when no lint was found", {
  skip_if_not_installed("lintr")

  # The pattern is only checked `if (n_found > 0)`, so an assertion about
  # lint messages passes vacuously when there are no messages at all.
  expect_length(
    test_linter(
      no_t_factory(),
      "x <- TRUE",
      should_lint = FALSE,
      message_pattern = "never matched"
    ),
    0L
  )
})


test_that("SUSPECT_test_linter compares n_lints without coercing it", {
  skip_if_not_installed("lintr")

  # `n_found != n_lints` coerces the integer count to character, so the
  # string "1" passes as a count. When such a value does not match, the
  # report itself dies: sprintf("%d") cannot format a character, so the
  # caller never sees the intended "Expected ... but found ..." message.
  expect_length(test_linter(no_t_factory(), "x <- T", n_lints = "1"), 1L)
  expect_error(
    test_linter(no_t_factory(), "x <- T", n_lints = "01"),
    "invalid format '%d'; use format %s for character objects",
    fixed = TRUE
  )
})


test_that("SUSPECT_test_linter surfaces a lintr internal error for NULL code", {
  skip_if_not_installed("lintr")

  # NULL is not caught by the `length(code) > 1` collapse, so it reaches
  # `lintr::lint()` as a missing `text` and the user sees lintr's internals.
  expect_error(
    test_linter(no_t_factory(), NULL),
    "argument \"filename\" is missing, with no default",
    fixed = TRUE
  )
})


# --- quick_test() -----------------------------------------------------------

test_that("quick_test reports the hits on stderr and returns them invisibly", {
  skip_if_not_installed("lintr")

  expect_message(
    capture.output(lints <- quick_test("//SYMBOL[text() = 'T']", "x <- T")),
    "Found 1 lint(s):",
    fixed = TRUE
  )
  expect_s3_class(lints, "lints")
  expect_length(lints, 1L)
  expect_identical(lints[[1L]]$message, "Found match")

  suppressMessages(
    capture.output(
      visibility <- withVisible(quick_test("//SYMBOL[text() = 'T']", "x <- T"))
    )
  )
  expect_false(visibility$visible)
})


test_that("quick_test reports an empty result rather than failing", {
  skip_if_not_installed("lintr")

  expect_message(
    lints <- quick_test("//SYMBOL[text() = 'T']", "x <- TRUE"),
    "No lints found.",
    fixed = TRUE
  )
  expect_length(lints, 0L)

  expect_message(quick_test("//SYMBOL", ""), "No lints found.", fixed = TRUE)
  expect_message(
    quick_test("//SYMBOL", character(0)),
    "No lints found.",
    fixed = TRUE
  )
})


test_that("quick_test uses the supplied message and lints a code vector per line", {
  skip_if_not_installed("lintr")

  expect_message(
    capture.output(
      lints <- quick_test("//SYMBOL", c("a <- 1", "b <- 2"), message = "hit")
    ),
    "Found 2 lint(s):",
    fixed = TRUE
  )
  expect_identical(vapply(lints, function(l) l$line_number, integer(1L)), 1:2)
  expect_identical(unique(vapply(lints, function(l) l$message, "")), "hit")
})


test_that("SUSPECT_quick_test reports a broken XPath as 'No lints found.'", {
  skip_if_not_installed("lintr")

  # An unparseable XPath only warns inside xml2; quick_test then reports the
  # empty result, so a typo in the expression is indistinguishable from a
  # linter that correctly matches nothing.
  expect_warning(
    expect_message(
      lints <- quick_test("//[[", "x <- T"),
      "No lints found.",
      fixed = TRUE
    ),
    "Invalid expression"
  )
  expect_length(lints, 0L)
})


test_that("SUSPECT_quick_test does not use the linter_name it passes on", {
  skip_if_not_installed("lintr")

  # quick_test asks create_simple_linter() for the name "temp_test_linter",
  # but create_simple_linter() drops linter_name entirely, so lintr names the
  # linter after the call expression instead.
  expect_message(
    capture.output(lints <- quick_test("//SYMBOL[text() = 'T']", "x <- T")),
    "Found 1 lint(s):",
    fixed = TRUE
  )
  expect_false(identical(lints[[1L]]$linter, "temp_test_linter"))
  expect_identical(lints[[1L]]$linter, "temp_linter")
})
