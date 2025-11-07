test_that("create_simple_linter works", {
  skip_if_not_installed("lintr")

  # Create a simple test linter
  test_linter <- create_simple_linter(
    xpath = "//SYMBOL[text() = 'T']",
    message = "Don't use T",
    linter_name = "test_no_t"
  )

  # Should lint code with T
  lints <- lintr::lint(text = "x <- T", linters = test_linter())
  expect_length(lints, 1)
  expect_match(lints[[1]]$message, "Don't use T")

  # Should not lint code with TRUE
  lints <- lintr::lint(text = "x <- TRUE", linters = test_linter())
  expect_length(lints, 0)
})


test_that("create_function_call_linter works", {
  skip_if_not_installed("lintr")

  test_linter <- create_function_call_linter(
    function_names = "paste0",
    message = "Don't use {function}",
    linter_name = "test_no_paste0"
  )

  # Should lint paste0
  lints <- lintr::lint(text = "x <- paste0('a', 'b')", linters = test_linter())
  expect_length(lints, 1)
  expect_match(lints[[1]]$message, "paste0")

  # Should not lint paste
  lints <- lintr::lint(text = "x <- paste('a', 'b')", linters = test_linter())
  expect_length(lints, 0)
})


test_that("create_function_call_linter works with multiple functions", {
  skip_if_not_installed("lintr")

  test_linter <- create_function_call_linter(
    function_names = c("foo", "bar"),
    message = "Don't use {function}",
    linter_name = "test_no_foo_bar"
  )

  # Should lint both
  lints <- lintr::lint(text = "foo(); bar()", linters = test_linter())
  expect_length(lints, 2)
})


test_that("create_assignment_linter works", {
  skip_if_not_installed("lintr")

  test_linter <- create_assignment_linter(
    forbidden_operators = "=",
    message = "Use <- not =",
    linter_name = "test_no_equals"
  )

  # Should lint =
  lints <- lintr::lint(text = "x = 5", linters = test_linter())
  expect_length(lints, 1)

  # Should not lint <-
  lints <- lintr::lint(text = "x <- 5", linters = test_linter())
  expect_length(lints, 0)
})
