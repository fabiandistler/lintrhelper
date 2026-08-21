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

git_quiet <- function(dir, args) {
  system2("git", c("-C", shQuote(dir), args), stdout = FALSE, stderr = FALSE)
}

make_git_repo <- function(dir) {
  git_quiet(dir, "init")
  git_quiet(dir, c("config", "user.name", "lintrhelper"))
  git_quiet(dir, c("config", "user.email", "lintrhelper@example.com"))
  git_quiet(dir, c("config", "commit.gpgsign", "false"))
  dir
}

commit_all <- function(dir) {
  git_quiet(dir, c("add", "--all"))
  git_quiet(dir, c("commit", "--message", "commit"))
  dir
}

skip_without_git <- function() {
  testthat::skip_if(!nzchar(Sys.which("git")), "git is not on the PATH")
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

test_that("mcp_wrap carries a value through and an error back as one", {
  skip_if_not_installed("ellmer")

  ok <- mcp_wrap(list(a = 1))
  expect_null(ok@error)
  expect_equal(ok@value, list(a = 1))

  failed <- mcp_wrap(stop("boom", call. = FALSE))
  expect_equal(failed@error, "boom")
})

test_that("one normaliser backs every tool argument", {
  flag <- function(value) {
    normalise_argument(
      value,
      mode = "logical",
      message = "`changed_only` must be TRUE or FALSE.",
      empty = FALSE
    )
  }
  rule <- function(value) {
    normalise_argument(
      value,
      mode = "character",
      message = "`name` must be a single linter name."
    )
  }

  expect_false(flag(NULL))
  expect_false(flag(list()))
  expect_true(flag(list(TRUE)))
  expect_error(flag("yes"), "TRUE or FALSE")
  expect_error(flag(c(TRUE, TRUE)), "TRUE or FALSE")
  expect_error(flag(NA), "TRUE or FALSE")

  expect_equal(rule(" assignment_linter "), "assignment_linter")
  expect_equal(rule(list("assignment_linter")), "assignment_linter")
  expect_error(rule(NULL), "single linter name")
  expect_error(rule(""), "single linter name")
  expect_error(rule(1), "single linter name")
  expect_error(rule(NA_character_), "single linter name")
  expect_error(rule(c("seq_linter", "seq_linter")), "single linter name")
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

test_that("is_package_dir only accepts a DESCRIPTION that names a package", {
  describe_dir <- function(contents) {
    dir <- withr::local_tempdir(.local_envir = parent.frame())
    if (!is.null(contents)) {
      writeLines(contents, file.path(dir, "DESCRIPTION"))
    }
    dir
  }

  expect_true(is_package_dir(describe_dir(c("Package: probe", "Version: 1"))))

  expect_false(is_package_dir(describe_dir(NULL)))
  expect_false(is_package_dir(describe_dir(character(0))))
  expect_false(is_package_dir(describe_dir(c("Version: 1", "Title: X"))))
  expect_false(is_package_dir(describe_dir(c("Package:", "Version: 1"))))
  expect_false(is_package_dir(describe_dir(c("not dcf at all", "%%%%"))))

  a_directory <- withr::local_tempdir()
  dir.create(file.path(a_directory, "DESCRIPTION"))
  expect_false(is_package_dir(a_directory))
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

test_that("changed_only narrows the lint to the files git reports as changed", {
  skip_if_not_installed("lintr")
  skip_without_git()

  proj <- add_file(make_project(withr::local_tempdir()), "scratch/bar.R")
  commit_all(make_git_repo(proj))
  writeLines("z = 3", file.path(proj, "scratch", "bar.R"))
  withr::local_envvar(CLAUDE_PROJECT_DIR = NA)

  expect_equal(
    lint_filenames(lint_project(proj, changed_only = TRUE)),
    "scratch/bar.R"
  )
})

test_that("changed_only counts untracked and staged files as changed", {
  skip_if_not_installed("lintr")
  skip_without_git()

  proj <- commit_all(make_git_repo(make_project(withr::local_tempdir())))
  add_file(proj, "scratch/untracked.R")
  add_file(proj, "scratch/staged.R")
  git_quiet(proj, c("add", "scratch/staged.R"))
  withr::local_envvar(CLAUDE_PROJECT_DIR = NA)

  expect_setequal(
    lint_filenames(lint_project(proj, changed_only = TRUE)),
    c("scratch/untracked.R", "scratch/staged.R")
  )
})

test_that("changed_only leaves files git ignores out of the lint", {
  skip_if_not_installed("lintr")
  skip_without_git()

  proj <- make_project(withr::local_tempdir())
  writeLines("ignored/", file.path(proj, ".gitignore"))
  commit_all(make_git_repo(proj))
  add_file(proj, "ignored/hidden.R")
  withr::local_envvar(CLAUDE_PROJECT_DIR = NA)

  expect_length(lint_project(proj, changed_only = TRUE), 0)
})

test_that("changed_only lints everything in a repository without commits", {
  skip_if_not_installed("lintr")
  skip_without_git()

  proj <- make_git_repo(make_project(withr::local_tempdir()))
  withr::local_envvar(CLAUDE_PROJECT_DIR = NA)

  expect_equal(
    lint_filenames(lint_project(proj, changed_only = TRUE)),
    "R/foo.R"
  )
})

test_that("changed_only returns an empty result when nothing changed", {
  skip_if_not_installed("lintr")
  skip_without_git()

  proj <- commit_all(make_git_repo(make_project(withr::local_tempdir())))
  withr::local_envvar(CLAUDE_PROJECT_DIR = NA)

  result <- lint_project(proj, changed_only = TRUE)

  expect_type(result, "list")
  expect_length(result, 0)
  expect_null(attr(result, "message"))
})

test_that("changed_only ignores changed files lintr would not lint", {
  skip_if_not_installed("lintr")
  skip_without_git()

  proj <- commit_all(make_git_repo(make_project(withr::local_tempdir())))
  writeLines("notes", file.path(proj, "README.md"))
  withr::local_envvar(CLAUDE_PROJECT_DIR = NA)

  expect_length(lint_project(proj, changed_only = TRUE), 0)
})

test_that("changed_only skips a file the change deleted", {
  skip_if_not_installed("lintr")
  skip_without_git()

  proj <- add_file(make_project(withr::local_tempdir()), "scratch/bar.R")
  commit_all(make_git_repo(proj))
  unlink(file.path(proj, "scratch", "bar.R"))
  withr::local_envvar(CLAUDE_PROJECT_DIR = NA)

  expect_length(lint_project(proj, changed_only = TRUE), 0)
})

test_that("changed_only leaves out what a directory lint never walks", {
  skip_if_not_installed("lintr")
  skip_without_git()

  proj <- make_git_repo(make_project(withr::local_tempdir()))
  add_file(proj, ".github/ci.R")
  add_file(proj, "renv/activate.R")
  add_file(proj, "R/.hidden.R")
  withr::local_envvar(CLAUDE_PROJECT_DIR = NA)

  expect_equal(
    lint_filenames(lint_project(proj, changed_only = TRUE)),
    "R/foo.R"
  )
  expect_setequal(
    lint_filenames(lint_project(proj, changed_only = TRUE)),
    lint_filenames(lint_project(proj))
  )
})

test_that("changed_only ignores a change that is already committed", {
  skip_if_not_installed("lintr")
  skip_without_git()

  proj <- commit_all(make_git_repo(make_project(withr::local_tempdir())))
  commit_all(add_file(proj, "R/new.R"))
  withr::local_envvar(CLAUDE_PROJECT_DIR = NA)

  expect_length(lint_project(proj, changed_only = TRUE), 0)
  expect_length(lint_project(proj), 2)
})

test_that("changed_only keeps a package's unlinted directories out", {
  skip_if_not_installed("lintr")
  skip_without_git()

  pkg <- commit_all(make_git_repo(make_package(withr::local_tempdir())))
  add_file(pkg, "scratch/bar.R")
  add_file(pkg, "R/.hidden.R")
  add_file(pkg, "R/new.R")
  withr::local_envvar(CLAUDE_PROJECT_DIR = NA)

  expect_equal(
    lint_filenames(lint_project(pkg, changed_only = TRUE)),
    "R/new.R"
  )
  expect_true(all(
    lint_filenames(lint_project(pkg, changed_only = TRUE)) %in%
      lint_filenames(lint_project(pkg))
  ))
})

test_that("a changed file is reported exactly as a full lint reports it", {
  skip_if_not_installed("lintr")
  skip_without_git()

  proj <- add_file(make_project(withr::local_tempdir()), "R/bar.R")
  commit_all(make_git_repo(proj))
  writeLines("z = 3", file.path(proj, "R", "bar.R"))
  add_file(proj, "renv/activate.R")
  add_file(proj, ".github/ci.R")
  add_file(proj, "R/.hidden.R")
  add_file(proj, "notes.md", code = "notes")
  withr::local_envvar(CLAUDE_PROJECT_DIR = NA)

  narrowed <- lint_project(proj, changed_only = TRUE)
  whole <- lint_project(proj)

  expect_equal(lint_filenames(narrowed), "R/bar.R")
  expect_true(all(vapply(
    narrowed,
    function(group) any(vapply(whole, identical, logical(1), group)),
    logical(1)
  )))
})

test_that("changed_only reports paths relative to the anchor, not the repo", {
  skip_if_not_installed("lintr")
  skip_without_git()

  repo <- commit_all(make_git_repo(make_project(withr::local_tempdir())))
  add_file(repo, "sub/R/inner.R")
  writeLines("z = 3", file.path(repo, "R", "foo.R"))
  withr::local_envvar(CLAUDE_PROJECT_DIR = NA)

  expect_equal(
    lint_filenames(lint_project(file.path(repo, "sub"), changed_only = TRUE)),
    "R/inner.R"
  )
})

test_that("changed_only honours the project's .lintr config", {
  skip_if_not_installed("lintr")
  skip_without_git()

  configured <- commit_all(make_git_repo(make_project(
    withr::local_tempdir(),
    lintr_config = "linters: linters_with_defaults(assignment_linter = NULL)"
  )))
  plain <- commit_all(make_git_repo(make_project(withr::local_tempdir())))
  add_file(configured, "R/new.R")
  add_file(plain, "R/new.R")
  withr::local_envvar(CLAUDE_PROJECT_DIR = NA)

  expect_false(
    "assignment_linter" %in%
      lint_linters(lint_project(configured, changed_only = TRUE))
  )
  expect_true(
    "assignment_linter" %in%
      lint_linters(lint_project(plain, changed_only = TRUE))
  )
})

test_that("changed_only degrades to a full lint outside a git repository", {
  skip_if_not_installed("lintr")

  proj <- add_file(make_project(withr::local_tempdir()), "scratch/bar.R")
  withr::local_envvar(CLAUDE_PROJECT_DIR = NA)

  result <- lint_project(proj, changed_only = TRUE)

  expect_setequal(lint_filenames(result), c("R/foo.R", "scratch/bar.R"))
  expect_match(attr(result, "message"), "not a git repository")
  expect_match(attr(result, "message"), "whole project")
})

test_that("changed_only degrades when git is not on the PATH", {
  skip_if_not_installed("lintr")
  skip_on_os("windows")

  proj <- make_project(withr::local_tempdir())
  withr::local_envvar(CLAUDE_PROJECT_DIR = NA, PATH = "")

  result <- lint_project(proj, changed_only = TRUE)

  expect_equal(lint_filenames(result), "R/foo.R")
  expect_match(attr(result, "message"), "not on the PATH")
})

test_that("changed_only degrades when git cannot answer", {
  skip_if_not_installed("lintr")

  proj <- make_project(withr::local_tempdir())
  withr::local_envvar(CLAUDE_PROJECT_DIR = NA)
  local_mocked_bindings(
    run_git = function(dir, args) {
      list(ok = !identical(args[[1]], "diff"), lines = character(0))
    }
  )

  result <- lint_project(proj, changed_only = TRUE)

  expect_equal(lint_filenames(result), "R/foo.R")
  expect_match(attr(result, "message"), "could not report the changed files")
})

test_that("lint_project lints the whole project unless changed_only is set", {
  skip_if_not_installed("lintr")
  skip_without_git()

  proj <- add_file(make_project(withr::local_tempdir()), "scratch/bar.R")
  commit_all(make_git_repo(proj))
  writeLines("z = 3", file.path(proj, "scratch", "bar.R"))
  withr::local_envvar(CLAUDE_PROJECT_DIR = NA)

  expect_setequal(
    lint_filenames(lint_project(proj)),
    c("R/foo.R", "scratch/bar.R")
  )
})

test_that("changed_only rejects a value that is not a flag", {
  skip_if_not_installed("lintr")

  proj <- make_project(withr::local_tempdir())
  withr::local_envvar(CLAUDE_PROJECT_DIR = NA)

  expect_error(lint_project(proj, changed_only = "yes"), "TRUE or FALSE")
  expect_error(
    lint_project(proj, changed_only = c(TRUE, TRUE)),
    "TRUE or FALSE"
  )
})

test_that("the payload of a result with nothing to say is bare", {
  skip_if_not_installed("lintr")

  proj <- make_project(withr::local_tempdir())
  withr::local_envvar(CLAUDE_PROJECT_DIR = NA)

  payload <- tool_payload(lint_project(proj))

  expect_null(names(payload))
  expect_equal(payload[[1]]$filename, "R/foo.R")
})

test_that("the payload moves a degrade note beside the lints", {
  skip_if_not_installed("lintr")

  proj <- make_project(withr::local_tempdir())
  withr::local_envvar(CLAUDE_PROJECT_DIR = NA)

  payload <- tool_payload(lint_project(proj, changed_only = TRUE))

  expect_named(payload, c("message", "lints"))
  expect_match(payload$message, "not a git repository")
  expect_equal(payload$lints[[1]]$filename, "R/foo.R")
})

test_that("the MCP wrapper passes changed_only through", {
  skip_if_not_installed("ellmer")
  skip_if_not_installed("jsonlite")
  skip_if_not_installed("lintr")
  skip_without_git()

  proj <- add_file(make_project(withr::local_tempdir()), "scratch/bar.R")
  commit_all(make_git_repo(proj))
  writeLines("z = 3", file.path(proj, "scratch", "bar.R"))
  withr::local_envvar(CLAUDE_PROJECT_DIR = NA)

  result <- mcp_lint_project(proj, changed_only = TRUE)

  parsed <- jsonlite::fromJSON(
    asNamespace("ellmer")[["tool_string"]](result),
    simplifyVector = FALSE
  )

  expect_length(parsed, 1)
  expect_equal(parsed[[1]]$filename, "scratch/bar.R")
  expect_setequal(names(parsed[[1]]$lints[[1]]), DIAGNOSTIC_FIELDS)
})

test_that("the MCP wrapper reports a degraded changed_only beside the lints", {
  skip_if_not_installed("ellmer")
  skip_if_not_installed("jsonlite")
  skip_if_not_installed("lintr")

  proj <- make_project(withr::local_tempdir())
  withr::local_envvar(CLAUDE_PROJECT_DIR = NA)

  result <- mcp_lint_project(proj, changed_only = TRUE)

  parsed <- jsonlite::fromJSON(
    asNamespace("ellmer")[["tool_string"]](result),
    simplifyVector = FALSE
  )

  expect_match(parsed$message, "not a git repository")
  expect_equal(parsed$lints[[1]]$filename, "R/foo.R")
  expect_setequal(names(parsed$lints[[1]]$lints[[1]]), DIAGNOSTIC_FIELDS)
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

test_that("mcp_tools exposes the read-only lint and rule tools", {
  skip_if_not_installed("ellmer")

  tools <- mcp_tools()
  names(tools) <- vapply(tools, function(tool) tool@name, character(1))

  expect_setequal(
    names(tools),
    c("lint_file", "lint_project", "list_rules", "explain_rule")
  )
  expect_true(all(vapply(
    tools,
    function(tool) isTRUE(tool@annotations$read_only_hint),
    logical(1)
  )))
  expect_named(
    tools[["lint_file"]]@arguments@properties,
    c("path", "project_dir")
  )
  lint_project_args <- tools[["lint_project"]]@arguments@properties
  expect_named(lint_project_args, c("dir", "changed_only"))
  expect_false(lint_project_args$changed_only@required)
  expect_named(tools[["list_rules"]]@arguments@properties, "tags")
  expect_false(tools[["list_rules"]]@arguments@properties$tags@required)
  expect_named(tools[["explain_rule"]]@arguments@properties, "name")
  expect_true(tools[["explain_rule"]]@arguments@properties$name@required)
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


rule_names <- function(result) {
  vapply(result$rules, function(rule) rule$linter, character(1))
}

rule_tags <- function(result, name) {
  match <- Filter(function(rule) rule$linter == name, result$rules)
  as.character(unlist(match[[1]]$tags))
}

test_that("list_rules returns every available linter when unfiltered", {
  skip_if_not_installed("lintr")

  result <- list_rules()
  available <- lintr::available_linters()

  expect_named(result, c("count", "rules"))
  expect_equal(result$count, nrow(available))
  expect_length(result$rules, nrow(available))
  expect_setequal(rule_names(result), available$linter)

  for (rule in result$rules) {
    expect_named(rule, c("linter", "package", "tags"))
    expect_type(rule$linter, "character")
    expect_type(rule$package, "character")
    expect_type(rule$tags, "list")
  }

  expect_true("assignment_linter" %in% rule_names(result))
  expect_true("default" %in% rule_tags(result, "assignment_linter"))
})

test_that("list_rules filters on a single tag", {
  skip_if_not_installed("lintr")

  result <- list_rules("default")
  everything <- list_rules()

  expect_null(result$message)
  expect_gt(result$count, 0)
  expect_lt(result$count, everything$count)
  expect_setequal(rule_names(result), names(lintr::default_linters))

  for (rule in result$rules) {
    expect_true("default" %in% as.character(unlist(rule$tags)))
  }
})

test_that("list_rules treats several tags as a union, not an intersection", {
  skip_if_not_installed("lintr")

  first <- list_rules("style")
  second <- list_rules("efficiency")
  both <- list_rules(c("style", "efficiency"))

  expect_setequal(
    rule_names(both),
    union(rule_names(first), rule_names(second))
  )
  expect_gte(both$count, max(first$count, second$count))
})

test_that("an unknown tag returns an empty result with a message", {
  skip_if_not_installed("lintr")

  result <- expect_no_error(list_rules("no_such_tag"))

  expect_equal(result$count, 0)
  expect_length(result$rules, 0)
  expect_match(result$message, "no_such_tag")
  expect_match(result$message, "Available tags")
  expect_match(result$message, "default")
})

test_that("an unknown tag alongside a known one keeps the known matches", {
  skip_if_not_installed("lintr")

  result <- list_rules(c("default", "no_such_tag"))

  expect_setequal(rule_names(result), names(lintr::default_linters))
  expect_match(
    result$message,
    "^No linter carries the tag\\(s\\): no_such_tag\\."
  )
})

test_that("list_rules treats an empty or blank filter as no filter", {
  skip_if_not_installed("lintr")

  everything <- list_rules()

  expect_equal(list_rules(character(0))$count, everything$count)
  expect_equal(list_rules("")$count, everything$count)
  expect_equal(list_rules(list())$count, everything$count)
})

test_that("list_rules accepts the list of strings an MCP client sends", {
  skip_if_not_installed("lintr")

  expect_setequal(
    rule_names(list_rules(list("default"))),
    names(lintr::default_linters)
  )
})

test_that("a tag filter is trimmed, blanks dropped, and deduplicated", {
  tags <- function(value) {
    normalise_argument(
      value,
      mode = "character",
      message = "`tags` must be a character vector of linter tags.",
      scalar = FALSE,
      empty = character(0)
    )
  }

  expect_equal(tags(NULL), character(0))
  expect_equal(tags(character(0)), character(0))
  expect_equal(tags(c("", "  ")), character(0))
  expect_equal(tags(c(" style ", "style")), "style")
  expect_equal(tags(c("style", NA)), "style")
  expect_equal(tags(list("style", "default")), c("style", "default"))
  expect_error(tags(1), "character vector")
})

test_that("list_rules writes nothing to stdout", {
  skip_if_not_installed("lintr")

  stdout <- capture.output(result <- list_rules("default"))

  expect_length(stdout, 0)
  expect_gt(result$count, 0)
})

test_that("the MCP wrapper serialises rule metadata as compact JSON", {
  skip_if_not_installed("ellmer")
  skip_if_not_installed("jsonlite")
  skip_if_not_installed("lintr")

  result <- mcp_list_rules("default")
  expect_true(inherits(result, "ellmer::ContentToolResult"))
  expect_null(result@error)

  parsed <- jsonlite::fromJSON(
    asNamespace("ellmer")[["tool_string"]](result),
    simplifyVector = FALSE
  )

  expect_setequal(names(parsed), c("count", "rules"))
  expect_equal(parsed$count, length(parsed$rules))
  expect_setequal(names(parsed$rules[[1]]), c("linter", "package", "tags"))

  # Tags stay a JSON array even for a linter carrying exactly one tag,
  # so the client never has to branch on the shape.
  for (rule in parsed$rules) {
    expect_type(rule$tags, "list")
  }
})

test_that("the MCP wrapper reports an unknown tag as a result, not an error", {
  skip_if_not_installed("ellmer")
  skip_if_not_installed("jsonlite")
  skip_if_not_installed("lintr")

  result <- mcp_list_rules("no_such_tag")

  expect_null(result@error)

  parsed <- jsonlite::fromJSON(
    asNamespace("ellmer")[["tool_string"]](result),
    simplifyVector = FALSE
  )

  expect_equal(parsed$count, 0)
  expect_length(parsed$rules, 0)
  expect_match(parsed$message, "no_such_tag")
})

test_that("the MCP wrapper reports a malformed tag filter as a tool error", {
  skip_if_not_installed("ellmer")

  result <- mcp_list_rules(1)

  expect_false(is.null(result@error))
  expect_match(
    asNamespace("ellmer")[["tool_string"]](result),
    "character vector"
  )
})


test_that("explain_rule returns the documentation of a known linter", {
  skip_if_not_installed("lintr")

  result <- explain_rule("assignment_linter")

  expect_true(result$found)
  expect_equal(result$linter, "assignment_linter")
  expect_equal(result$package, "lintr")
  expect_true("default" %in% as.character(unlist(result$tags)))
  expect_match(result$title, "ssignment")
  expect_match(result$description, "assignment")
  expect_match(result$usage, "^assignment_linter\\(")
  expect_equal(result$help, "?lintr::assignment_linter")

  # Which arguments assignment_linter takes is lintr's business and has
  # changed between releases, so the assertion is that the names come
  # from the signature rather than that any particular one is there. One
  # \item can name several arguments at once.
  documented <- vapply(result$arguments, function(x) x$name, character(1))
  documented <- trimws(unlist(strsplit(documented, ",", fixed = TRUE)))

  expect_gt(length(documented), 0)
  expect_true(any(documented %in% names(formals(lintr::assignment_linter))))

  for (argument in result$arguments) {
    expect_named(argument, c("name", "description"))
    expect_type(argument$description, "character")
  }
})

test_that("explain_rule trims the page to what an agent acts on", {
  skip_if_not_installed("lintr")

  result <- explain_rule("assignment_linter")

  expect_setequal(
    names(result),
    c(
      "found", "linter", "package", "tags",
      "title", "description", "usage", "arguments", "help"
    )
  )

  # The examples and the see-also links are what a human browses; none of
  # the rendered sections may carry them.
  rendered <- paste(
    c(result$title, result$description, result$usage),
    collapse = "\n"
  )
  expect_no_match(rendered, "Examples")
  expect_no_match(rendered, "See Also")
  expect_lt(nchar(rendered), 1000)
})

test_that("explain_rule covers a linter that takes no arguments", {
  skip_if_not_installed("lintr")

  result <- explain_rule("T_and_F_symbol_linter")

  expect_true(result$found)
  expect_match(result$usage, "T_and_F_symbol_linter\\(")
  expect_equal(result$arguments, list())
})

test_that("explain_rule explains every linter list_rules reports", {
  skip_if_not_installed("lintr")

  for (name in rule_names(list_rules("default"))) {
    result <- explain_rule(name)

    expect_true(result$found)
    expect_true(nzchar(result$description))
    expect_true(nzchar(result$usage))
  }
})

test_that("explain_rule leaves no unevaluated \\Sexpr markup in the reply", {
  skip_if_not_installed("lintr")

  # object_name_linter builds part of its argument text at render time,
  # which tools::Rd_db() hands back unevaluated.
  result <- explain_rule("object_name_linter")
  rendered <- paste(unlist(result), collapse = "\n")

  expect_no_match(rendered, "Sexpr", fixed = TRUE)
  expect_no_match(rendered, ":::", fixed = TRUE)
})

test_that("an unknown rule name returns the closest matches, not an error", {
  skip_if_not_installed("lintr")

  result <- expect_no_error(explain_rule("assigment_linter"))

  expect_false(result$found)
  expect_equal(result$linter, "assigment_linter")
  expect_true("assignment_linter" %in% as.character(unlist(result$suggestions)))
  expect_match(result$message, "No linter named \"assigment_linter\"")
  expect_match(result$message, "assignment_linter")
  expect_match(result$message, "list_rules")
  expect_null(result$description)
})

test_that("a rule name missing its suffix suggests the full linter name", {
  skip_if_not_installed("lintr")

  result <- explain_rule("assignment")

  expect_false(result$found)
  expect_equal(
    as.character(unlist(result$suggestions))[[1]],
    "assignment_linter"
  )
})

test_that("explain_rule suggests something even for a name nothing resembles", {
  skip_if_not_installed("lintr")

  result <- explain_rule("zzzzzzzzzz")

  expect_false(result$found)
  expect_length(result$suggestions, 5)
})

test_that("explain_rule covers deprecated linters list_rules leaves out", {
  skip_if_not_installed("lintr")

  deprecated <- lintr::available_linters(exclude_tags = NULL)
  deprecated <- deprecated[
    vapply(deprecated$tags, function(x) "deprecated" %in% x, logical(1)),
  ]
  skip_if(nrow(deprecated) == 0, "this lintr deprecates no linter")

  name <- as.character(deprecated$linter[[1]])
  result <- explain_rule(name)

  expect_true(result$found)
  expect_true("deprecated" %in% as.character(unlist(result$tags)))
  expect_false(name %in% rule_names(list_rules()))
})

test_that("explain_rule rejects a name that is not a single string", {
  expect_error(explain_rule(1), "single linter name")
  expect_error(explain_rule(character(0)), "single linter name")
  expect_error(explain_rule(c("a", "b")), "single linter name")
  expect_error(explain_rule(NA_character_), "single linter name")
  expect_error(explain_rule("  "), "single linter name")
})

test_that("explain_rule accepts the string an MCP client sends", {
  skip_if_not_installed("lintr")

  expect_equal(
    explain_rule(list(" assignment_linter "))$linter,
    "assignment_linter"
  )
})

test_that("explain_rule reports metadata when the help pages are missing", {
  skip_if_not_installed("lintr")

  local_mocked_bindings(rule_help = function(name, package) NULL)

  result <- explain_rule("assignment_linter")

  expect_true(result$found)
  expect_equal(result$linter, "assignment_linter")
  expect_null(result$description)
  expect_match(result$message, "without help pages")
})

test_that("explain_rule writes nothing to stdout", {
  skip_if_not_installed("lintr")

  stdout <- capture.output(result <- explain_rule("assignment_linter"))

  expect_length(stdout, 0)
  expect_true(result$found)
})

test_that("closest_rules ranks a prefix ahead of a nearer edit distance", {
  candidates <- c("assignment_linter", "seq_linter", "any_is_na_linter")

  expect_equal(
    closest_rules("assignment", candidates)[[1]],
    "assignment_linter"
  )
  expect_equal(closest_rules("seq_lintr", candidates)[[1]], "seq_linter")
  expect_length(closest_rules("x", candidates), 3)
  expect_equal(closest_rules("x", character(0)), character(0))
  expect_length(closest_rules("x", candidates, n = 2), 2)
})

test_that("help_sections cuts a rendered page at its headings", {
  page <- c(
    "Assignment linter",
    "",
    "Description:",
    "",
    "     Check that the operator is used.",
    "",
    "Arguments:",
    "",
    "operator: Character vector of valid operators.",
    "",
    "Tags:",
    "",
    "     style"
  )

  sections <- help_sections(page)

  expect_named(sections, c("Title", "Description", "Arguments", "Tags"))
  expect_equal(help_text(sections[["Title"]]), "Assignment linter")
  expect_equal(
    help_text(sections[["Description"]]),
    "Check that the operator is used."
  )
  expect_null(help_text(sections[["No such section"]]))
  expect_equal(
    help_arguments(sections[["Arguments"]]),
    list(list(
      name = "operator",
      description = "Character vector of valid operators."
    ))
  )
  expect_equal(help_arguments(sections[["Tags"]]), list())
})

test_that("a wrapped description is not mistaken for the next argument", {
  arguments <- c(
    "packages: Character vector of packages. Defaults to the most",
    "          common default",
    "          packages: base, stats, and utils.",
    "",
    "allow_names: Character vector of object names to ignore."
  )

  parsed <- help_arguments(arguments)

  expect_equal(
    vapply(parsed, function(x) x$name, character(1)),
    c("packages", "allow_names")
  )
  expect_match(parsed[[1]]$description, "packages: base, stats, and utils.")
})

test_that("a whole-page render keeps a usage's own line breaks", {
  skip_if_not_installed("lintr")

  page <- rule_help("return_linter", "lintr")
  usage <- help_text(help_sections(page)[["Usage"]])

  expect_match(usage, "^return_linter\\(")
  expect_match(usage, "\n  return_style", fixed = TRUE)
})

test_that("rule_help returns NULL for a package with no help database", {
  expect_null(rule_help("assignment_linter", "no_such_package_at_all"))
})

test_that("rule_help returns NULL for a name the package does not document", {
  skip_if_not_installed("lintr")

  expect_null(rule_help("no_such_linter_at_all", "lintr"))
})

test_that("the MCP wrapper serialises rule documentation as compact JSON", {
  skip_if_not_installed("ellmer")
  skip_if_not_installed("jsonlite")
  skip_if_not_installed("lintr")

  result <- mcp_explain_rule("assignment_linter")

  expect_true(inherits(result, "ellmer::ContentToolResult"))
  expect_null(result@error)

  parsed <- jsonlite::fromJSON(
    asNamespace("ellmer")[["tool_string"]](result),
    simplifyVector = FALSE
  )

  expect_true(parsed$found)
  expect_equal(parsed$linter, "assignment_linter")
  expect_type(parsed$tags, "list")
  expect_type(parsed$arguments, "list")
  expect_setequal(names(parsed$arguments[[1]]), c("name", "description"))
})

test_that("the MCP wrapper reports an unknown rule as a result, not an error", {
  skip_if_not_installed("ellmer")
  skip_if_not_installed("jsonlite")
  skip_if_not_installed("lintr")

  result <- mcp_explain_rule("assigment_linter")

  expect_null(result@error)

  parsed <- jsonlite::fromJSON(
    asNamespace("ellmer")[["tool_string"]](result),
    simplifyVector = FALSE
  )

  expect_false(parsed$found)
  expect_type(parsed$suggestions, "list")
  expect_match(parsed$message, "assignment_linter")
})

test_that("the MCP wrapper reports a malformed rule name as a tool error", {
  skip_if_not_installed("ellmer")

  result <- mcp_explain_rule(1)

  expect_false(is.null(result@error))
  expect_match(
    asNamespace("ellmer")[["tool_string"]](result),
    "single linter name"
  )
})
