test_that("no_t_f_linter works", {
  skip_if_not_installed("lintr")

  # Should catch T and F
  lints <- lintr::lint(text = "x <- T", linters = no_t_f_linter())
  expect_length(lints, 1)

  lints <- lintr::lint(text = "x <- F", linters = no_t_f_linter())
  expect_length(lints, 1)

  # Should not catch TRUE/FALSE
  lints <- lintr::lint(text = "x <- TRUE; y <- FALSE", linters = no_t_f_linter())
  expect_length(lints, 0)
})


test_that("no_attach_linter works", {
  skip_if_not_installed("lintr")

  lints <- lintr::lint(text = "attach(mtcars)", linters = no_attach_linter())
  expect_length(lints, 1)

  lints <- lintr::lint(text = "with(mtcars, mean(mpg))", linters = no_attach_linter())
  expect_length(lints, 0)
})


test_that("prefer_arrow_assignment_linter works", {
  skip_if_not_installed("lintr")

  lints <- lintr::lint(text = "x = 5", linters = prefer_arrow_assignment_linter())
  expect_length(lints, 1)

  lints <- lintr::lint(text = "x <- 5", linters = prefer_arrow_assignment_linter())
  expect_length(lints, 0)
})


test_that("no_sapply_linter works", {
  skip_if_not_installed("lintr")

  lints <- lintr::lint(text = "sapply(1:10, sqrt)", linters = no_sapply_linter())
  expect_length(lints, 1)

  lints <- lintr::lint(text = "vapply(1:10, sqrt, numeric(1))", linters = no_sapply_linter())
  expect_length(lints, 0)
})
