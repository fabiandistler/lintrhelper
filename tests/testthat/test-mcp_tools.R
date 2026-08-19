DIAGNOSTIC_FIELDS <- c(
  "filename", "line", "column", "type", "message", "linter"
)

lint_linters <- function(result) {
  unlist(lapply(result, function(group) {
    vapply(group$lints, function(lint) lint$linter, character(1))
  }))
}

make_project <- function(dir, lintr_config = NULL, code = "x = 1") {
  dir.create(file.path(dir, "R"), recursive = TRUE, showWarnings = FALSE)
  writeLines(code, file.path(dir, "R", "foo.R"))
  if (!is.null(lintr_config)) {
    writeLines(lintr_config, file.path(dir, ".lintr"))
  }
  dir
}

add_file <- function(dir, path, code = "y = 2") {
  target <- file.path(dir, path)
  dir.create(dirname(target), recursive = TRUE, showWarnings = FALSE)
  writeLines(code, target)
  dir
}

make_package <- function(dir, ...) {
  make_project(dir, ...)
  writeLines(
    c("Package: probe", "Version: 0.0.1", "Title: Probe"),
    file.path(dir, "DESCRIPTION")
  )
  dir
}

lint_filenames <- function(result) {
  vapply(result, function(group) group$filename, character(1))
}

test_that("resolve_project_dir prefers the tool argument", {
  arg <- withr::local_tempdir()
  env <- withr::local_tempdir()
  withr::local_envvar(CLAUDE_PROJECT_DIR = env)

  expect_equal(
    resolve_project_dir(arg),
    normalizePath(arg, winslash = "/", mustWork = FALSE)
  )
})

test_that("resolve_project_dir falls back to CLAUDE_PROJECT_DIR", {
  env <- withr::local_tempdir()
  cwd <- withr::local_tempdir()
  withr::local_envvar(CLAUDE_PROJECT_DIR = env)
  withr::local_dir(cwd)

  expect_equal(
    resolve_project_dir(NULL),
    normalizePath(env, winslash = "/", mustWork = FALSE)
  )
})

test_that("resolve_project_dir falls back to the process working directory", {
  cwd <- withr::local_tempdir()
  withr::local_envvar(CLAUDE_PROJECT_DIR = NA)
  withr::local_dir(cwd)

  expect_equal(
    resolve_project_dir(NULL),
    normalizePath(cwd, winslash = "/", mustWork = FALSE)
  )
})

test_that("resolve_project_dir rejects a directory that does not exist", {
  expect_error(
    resolve_project_dir(file.path(tempdir(), "definitely-not-here")),
    "does not exist"
  )
})

test_that("lint_file returns diagnostics grouped by file", {
  skip_if_not_installed("lintr")

  proj <- make_project(withr::local_tempdir())
  withr::local_envvar(CLAUDE_PROJECT_DIR = NA)

  result <- lint_file("R/foo.R", project_dir = proj)

  expect_length(result, 1)
  expect_named(result[[1]], c("filename", "lints"))
  expect_equal(result[[1]]$filename, "R/foo.R")
  expect_true(length(result[[1]]$lints) >= 1)

  for (lint in result[[1]]$lints) {
    expect_setequal(names(lint), DIAGNOSTIC_FIELDS)
    expect_equal(lint$filename, "R/foo.R")
    expect_type(lint$line, "integer")
    expect_type(lint$column, "integer")
    expect_type(lint$type, "character")
    expect_type(lint$message, "character")
    expect_type(lint$linter, "character")
  }

  linters <- vapply(result[[1]]$lints, function(x) x$linter, character(1))
  expect_true("assignment_linter" %in% linters)
})

test_that("lint_file returns an empty list for a file with no lints", {
  skip_if_not_installed("lintr")

  proj <- make_project(withr::local_tempdir(), code = "x <- 1")
  withr::local_envvar(CLAUDE_PROJECT_DIR = NA)

  result <- lint_file("R/foo.R", project_dir = proj)

  expect_type(result, "list")
  expect_length(result, 0)
})

test_that("lint_file honours the project's .lintr config via the anchor", {
  skip_if_not_installed("lintr")

  configured <- make_project(
    withr::local_tempdir(),
    lintr_config = "linters: linters_with_defaults(assignment_linter = NULL)"
  )
  plain <- make_project(withr::local_tempdir())

  elsewhere <- withr::local_tempdir()
  withr::local_dir(elsewhere)
  withr::local_envvar(CLAUDE_PROJECT_DIR = NA)

  with_config <- lint_file("R/foo.R", project_dir = configured)
  without_config <- lint_file("R/foo.R", project_dir = plain)

  expect_false("assignment_linter" %in% lint_linters(with_config))
  expect_true("assignment_linter" %in% lint_linters(without_config))
})

test_that("lint_file resolves relative paths against CLAUDE_PROJECT_DIR", {
  skip_if_not_installed("lintr")

  proj <- make_project(withr::local_tempdir())
  elsewhere <- withr::local_tempdir()
  withr::local_dir(elsewhere)
  withr::local_envvar(CLAUDE_PROJECT_DIR = proj)

  result <- lint_file("R/foo.R")

  expect_length(result, 1)
  expect_equal(result[[1]]$filename, "R/foo.R")
})

test_that("lint_file accepts an absolute path", {
  skip_if_not_installed("lintr")

  proj <- make_project(withr::local_tempdir())
  withr::local_envvar(CLAUDE_PROJECT_DIR = NA)

  result <- lint_file(file.path(proj, "R", "foo.R"), project_dir = proj)

  expect_length(result, 1)
  expect_equal(result[[1]]$filename, "R/foo.R")
})

test_that("lint_file rejects a directory with an actionable message", {
  proj <- make_project(withr::local_tempdir())
  withr::local_envvar(CLAUDE_PROJECT_DIR = NA)

  expect_error(lint_file("R", project_dir = proj), "directory")
})

test_that("the MCP wrapper reports failures as tool errors, not crashes", {
  skip_if_not_installed("ellmer")

  proj <- make_project(withr::local_tempdir())
  withr::local_envvar(CLAUDE_PROJECT_DIR = NA)

  missing <- mcp_lint_file("R/nope.R", project_dir = proj)
  expect_true(inherits(missing, "ellmer::ContentToolResult"))
  expect_false(is.null(missing@error))
  expect_match(
    asNamespace("ellmer")[["tool_string"]](missing),
    "File not found"
  )

  a_directory <- mcp_lint_file("R", project_dir = proj)
  expect_false(is.null(a_directory@error))

  bad_anchor <- mcp_lint_file("R/foo.R", project_dir = "/no/such/dir")
  expect_false(is.null(bad_anchor@error))
})

test_that("lint_file errors on a missing file", {
  proj <- withr::local_tempdir()
  withr::local_envvar(CLAUDE_PROJECT_DIR = NA)

  expect_error(lint_file("R/nope.R", project_dir = proj), "not found")
})

test_that("lint_file writes nothing to stdout", {
  skip_if_not_installed("lintr")

  proj <- make_project(withr::local_tempdir())
  withr::local_envvar(CLAUDE_PROJECT_DIR = NA)

  stdout <- capture.output(result <- lint_file("R/foo.R", project_dir = proj))

  expect_length(stdout, 0)
  expect_length(result, 1)
})

test_that("the MCP wrapper serialises diagnostics as compact JSON", {
  skip_if_not_installed("ellmer")
  skip_if_not_installed("jsonlite")

  proj <- make_project(withr::local_tempdir())
  withr::local_envvar(CLAUDE_PROJECT_DIR = NA)

  result <- mcp_lint_file("R/foo.R", project_dir = proj)
  expect_true(inherits(result, "ellmer::ContentToolResult"))

  json <- asNamespace("ellmer")[["tool_string"]](result)
  parsed <- jsonlite::fromJSON(json, simplifyVector = FALSE)

  expect_length(parsed, 1)
  expect_equal(parsed[[1]]$filename, "R/foo.R")
  expect_setequal(names(parsed[[1]]$lints[[1]]), DIAGNOSTIC_FIELDS)
  expect_type(parsed[[1]]$lints[[1]]$line, "integer")
})

test_that("a clean file serialises to an empty JSON array", {
  skip_if_not_installed("ellmer")

  proj <- make_project(withr::local_tempdir(), code = "x <- 1")
  withr::local_envvar(CLAUDE_PROJECT_DIR = NA)

  json <- asNamespace("ellmer")[["tool_string"]](
    mcp_lint_file("R/foo.R", project_dir = proj)
  )

  expect_equal(as.character(json), "[]")
})

test_that("lint_project groups lints from every file in the project", {
  skip_if_not_installed("lintr")

  proj <- add_file(make_project(withr::local_tempdir()), "scratch/bar.R")
  withr::local_envvar(CLAUDE_PROJECT_DIR = NA)

  result <- lint_project(proj)

  expect_setequal(lint_filenames(result), c("R/foo.R", "scratch/bar.R"))

  for (group in result) {
    expect_named(group, c("filename", "lints"))
    expect_true(length(group$lints) >= 1)
    for (lint in group$lints) {
      expect_setequal(names(lint), DIAGNOSTIC_FIELDS)
      expect_equal(lint$filename, group$filename)
      expect_type(lint$line, "integer")
      expect_type(lint$column, "integer")
      expect_type(lint$type, "character")
      expect_type(lint$message, "character")
      expect_type(lint$linter, "character")
    }
  }

  expect_true("assignment_linter" %in% lint_linters(result))
})

test_that("lint_project returns an empty list for a clean project", {
  skip_if_not_installed("lintr")

  proj <- make_project(withr::local_tempdir(), code = "x <- 1")
  withr::local_envvar(CLAUDE_PROJECT_DIR = NA)

  result <- lint_project(proj)

  expect_type(result, "list")
  expect_length(result, 0)
})

test_that("lint_project returns an empty list for a project without R files", {
  skip_if_not_installed("lintr")

  empty <- withr::local_tempdir()
  withr::local_envvar(CLAUDE_PROJECT_DIR = NA)

  expect_length(lint_project(empty), 0)
})

test_that("lint_project lints a package the way lintr lints packages", {
  skip_if_not_installed("lintr")

  pkg <- add_file(make_package(withr::local_tempdir()), "scratch/bar.R")
  withr::local_envvar(CLAUDE_PROJECT_DIR = NA)

  expect_equal(lint_filenames(lint_project(pkg)), "R/foo.R")
})

test_that("lint_project ignores a DESCRIPTION above the anchor", {
  skip_if_not_installed("lintr")

  outer <- make_package(withr::local_tempdir())
  inner <- add_file(outer, "sub/R/inner.R")
  withr::local_envvar(CLAUDE_PROJECT_DIR = NA)

  expect_equal(
    lint_filenames(lint_project(file.path(inner, "sub"))),
    "R/inner.R"
  )
})

test_that("lint_project honours the project's .lintr config", {
  skip_if_not_installed("lintr")

  configured <- make_project(
    withr::local_tempdir(),
    lintr_config = "linters: linters_with_defaults(assignment_linter = NULL)"
  )
  plain <- make_project(withr::local_tempdir())

  elsewhere <- withr::local_tempdir()
  withr::local_dir(elsewhere)
  withr::local_envvar(CLAUDE_PROJECT_DIR = NA)

  expect_false("assignment_linter" %in% lint_linters(lint_project(configured)))
  expect_true("assignment_linter" %in% lint_linters(lint_project(plain)))
})

test_that("lint_project falls back to CLAUDE_PROJECT_DIR", {
  skip_if_not_installed("lintr")

  proj <- make_project(withr::local_tempdir())
  elsewhere <- withr::local_tempdir()
  withr::local_dir(elsewhere)
  withr::local_envvar(CLAUDE_PROJECT_DIR = proj)

  expect_equal(lint_filenames(lint_project()), "R/foo.R")
})

test_that("lint_project falls back to the process working directory", {
  skip_if_not_installed("lintr")

  proj <- make_project(withr::local_tempdir())
  withr::local_dir(proj)
  withr::local_envvar(CLAUDE_PROJECT_DIR = NA)

  expect_equal(lint_filenames(lint_project()), "R/foo.R")
})

test_that("lint_project errors on a directory that does not exist", {
  withr::local_envvar(CLAUDE_PROJECT_DIR = NA)

  expect_error(
    lint_project(file.path(tempdir(), "definitely-not-here")),
    "does not exist"
  )
})

test_that("lint_project writes nothing to stdout", {
  skip_if_not_installed("lintr")

  proj <- make_project(withr::local_tempdir())
  withr::local_envvar(CLAUDE_PROJECT_DIR = NA)

  stdout <- capture.output(result <- lint_project(proj))

  expect_length(stdout, 0)
  expect_length(result, 1)
})

test_that("the MCP wrapper serialises project diagnostics as compact JSON", {
  skip_if_not_installed("ellmer")
  skip_if_not_installed("jsonlite")

  proj <- make_project(withr::local_tempdir())
  withr::local_envvar(CLAUDE_PROJECT_DIR = NA)

  result <- mcp_lint_project(proj)
  expect_true(inherits(result, "ellmer::ContentToolResult"))

  parsed <- jsonlite::fromJSON(
    asNamespace("ellmer")[["tool_string"]](result),
    simplifyVector = FALSE
  )

  expect_length(parsed, 1)
  expect_equal(parsed[[1]]$filename, "R/foo.R")
  expect_setequal(names(parsed[[1]]$lints[[1]]), DIAGNOSTIC_FIELDS)
  expect_type(parsed[[1]]$lints[[1]]$line, "integer")
})

test_that("a clean project serialises to an empty JSON array", {
  skip_if_not_installed("ellmer")

  proj <- make_project(withr::local_tempdir(), code = "x <- 1")
  withr::local_envvar(CLAUDE_PROJECT_DIR = NA)

  json <- asNamespace("ellmer")[["tool_string"]](mcp_lint_project(proj))

  expect_equal(as.character(json), "[]")
})

test_that("the MCP wrapper reports a bad project dir as a tool error", {
  skip_if_not_installed("ellmer")

  withr::local_envvar(CLAUDE_PROJECT_DIR = NA)

  bad_anchor <- mcp_lint_project("/no/such/dir")
  expect_true(inherits(bad_anchor, "ellmer::ContentToolResult"))
  expect_false(is.null(bad_anchor@error))
  expect_match(
    asNamespace("ellmer")[["tool_string"]](bad_anchor),
    "does not exist"
  )
})

test_that("mcp_tools exposes read-only lint_file and lint_project tools", {
  skip_if_not_installed("ellmer")

  tools <- mcp_tools()
  names(tools) <- vapply(tools, function(tool) tool@name, character(1))

  expect_setequal(names(tools), c("lint_file", "lint_project"))
  expect_true(all(vapply(
    tools,
    function(tool) isTRUE(tool@annotations$read_only_hint),
    logical(1)
  )))
  expect_named(
    tools[["lint_file"]]@arguments@properties,
    c("path", "project_dir")
  )
  expect_named(tools[["lint_project"]]@arguments@properties, "dir")
})

test_that("start_mcp_server gates on the suggested packages", {
  local_mocked_bindings(
    check_mcp_deps = function() stop("The package `mcptools` is required.")
  )

  expect_error(start_mcp_server(), "mcptools")
})

test_that("check_mcp_deps passes when the suggested packages are installed", {
  skip_if_not_installed("mcptools")
  skip_if_not_installed("ellmer")

  expect_no_error(check_mcp_deps())
})
