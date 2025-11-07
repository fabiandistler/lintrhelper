test_that("forbid_symbols works", {
  skip_if_not_installed("lintr")

  no_t_f <- forbid_symbols(c("T", "F"), "Use TRUE/FALSE instead of {symbol}.")

  # Should catch T and F
  lints <- lintr::lint(text = "x <- T", linters = no_t_f())
  expect_length(lints, 1)
  expect_match(lints[[1]]$message, "TRUE/FALSE")

  lints <- lintr::lint(text = "y <- F", linters = no_t_f())
  expect_length(lints, 1)

  # Should not catch TRUE/FALSE
  lints <- lintr::lint(text = "x <- TRUE", linters = no_t_f())
  expect_length(lints, 0)
})


test_that("forbid_functions works", {
  skip_if_not_installed("lintr")

  no_attach <- forbid_functions("attach", "Don't use {function}().")

  lints <- lintr::lint(text = "attach(mtcars)", linters = no_attach())
  expect_length(lints, 1)
  expect_match(lints[[1]]$message, "attach")

  lints <- lintr::lint(text = "detach(mtcars)", linters = no_attach())
  expect_length(lints, 0)
})


test_that("forbid_functions with alternatives works", {
  skip_if_not_installed("lintr")

  linter <- forbid_functions("sapply", alternatives = "vapply")

  lints <- lintr::lint(text = "sapply(1:10, sqrt)", linters = linter())
  expect_length(lints, 1)
  expect_match(lints[[1]]$message, "vapply")
})


test_that("forbid_functions with multiple alternatives", {
  skip_if_not_installed("lintr")

  linter <- forbid_functions("apply", alternatives = c("vapply", "lapply"))

  lints <- lintr::lint(text = "apply(mat, 1, sum)", linters = linter())
  expect_length(lints, 1)
  expect_match(lints[[1]]$message, "vapply")
  expect_match(lints[[1]]$message, "lapply")
})


test_that("require_naming_pattern works for snake_case", {
  skip_if_not_installed("lintr")

  snake_case <- require_naming_pattern(
    "^[a-z][a-z0-9_]*$",
    "Variable '{symbol}' should use snake_case."
  )

  # Should flag camelCase
  lints <- lintr::lint(text = "myVar <- 1", linters = snake_case())
  expect_length(lints, 1)

  # Should pass snake_case
  lints <- lintr::lint(text = "my_var <- 1", linters = snake_case())
  expect_length(lints, 0)
})


test_that("require_naming_pattern with invert works", {
  skip_if_not_installed("lintr")

  no_uppercase <- require_naming_pattern(
    "^[A-Z]",
    "Variable '{symbol}' should not start with uppercase.",
    invert = TRUE
  )

  # Should flag uppercase start
  lints <- lintr::lint(text = "MyVar <- 1", linters = no_uppercase())
  expect_length(lints, 1)

  # Should pass lowercase start
  lints <- lintr::lint(text = "myVar <- 1", linters = no_uppercase())
  expect_length(lints, 0)
})


test_that("require_function_naming_pattern works", {
  skip_if_not_installed("lintr")

  verb_start <- require_function_naming_pattern(
    "^(get|set|calculate|check|is)",
    "Function '{function}' should start with a verb."
  )

  # Should flag non-verb functions
  lints <- lintr::lint(text = "result <- myfunction()", linters = verb_start())
  expect_length(lints, 1)

  # Should pass verb functions
  lints <- lintr::lint(text = "result <- calculate_sum()", linters = verb_start())
  expect_length(lints, 0)
})


test_that("enforce_assignment_operator prefers <-", {
  skip_if_not_installed("lintr")

  use_arrow <- enforce_assignment_operator("<-")

  # Should flag =
  lints <- lintr::lint(text = "x = 5", linters = use_arrow())
  expect_length(lints, 1)

  # Should pass <-
  lints <- lintr::lint(text = "x <- 5", linters = use_arrow())
  expect_length(lints, 0)
})


test_that("enforce_assignment_operator prefers =", {
  skip_if_not_installed("lintr")

  use_equals <- enforce_assignment_operator("=")

  # Should flag <-
  lints <- lintr::lint(text = "x <- 5", linters = use_equals())
  expect_length(lints, 1)

  # Should pass =
  lints <- lintr::lint(text = "x = 5", linters = use_equals())
  expect_length(lints, 0)
})


test_that("require_function_arguments works", {
  skip_if_not_installed("lintr")

  explicit_saf <- require_function_arguments(
    "data.frame",
    "stringsAsFactors"
  )

  # Should flag without argument
  lints <- lintr::lint(
    text = "df <- data.frame(x = 1:3)",
    linters = explicit_saf()
  )
  expect_length(lints, 1)

  # Should pass with argument
  lints <- lintr::lint(
    text = "df <- data.frame(x = 1:3, stringsAsFactors = FALSE)",
    linters = explicit_saf()
  )
  expect_length(lints, 0)
})


test_that("limit_line_length works", {
  skip_if_not_installed("lintr")

  length_10 <- limit_line_length(10)

  # Should flag long lines
  lints <- lintr::lint(text = "x <- 1234567890", linters = length_10())
  expect_length(lints, 1)

  # Should pass short lines
  lints <- lintr::lint(text = "x <- 1", linters = length_10())
  expect_length(lints, 0)
})
