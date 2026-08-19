#' MCP Tool Definitions
#'
#' Builds the list of [ellmer::tool()] definitions served by
#' [start_mcp_server()]. The underlying functions are plain R functions, so
#' they can be called directly from R as well as through any MCP client.
#'
#' @return A list of `ellmer::tool()` objects.
#'
#' @keywords internal
#' @noRd
mcp_tools <- function() {
  list(
    ellmer::tool(
      mcp_lint_file,
      name = "lint_file",
      description = paste(
        "Lint a single R file with lintr and return the resulting diagnostics.",
        "lintr discovers the .lintr configuration from the file's own",
        "directory upwards, so the diagnostics match what a human running",
        "lintr on that file would see."
      ),
      arguments = list(
        path = ellmer::type_string(
          paste(
            "Path to the R file to lint, either absolute or relative to the",
            "project directory."
          ),
          required = TRUE
        ),
        project_dir = ellmer::type_string(
          paste(
            "Project root that relative paths are resolved against, and the",
            "working directory lintr runs in. Defaults to the",
            "CLAUDE_PROJECT_DIR environment variable, or the working",
            "directory of the server process."
          ),
          required = FALSE
        )
      ),
      annotations = ellmer::tool_annotations(
        title = "Lint an R file",
        read_only_hint = TRUE,
        open_world_hint = FALSE,
        idempotent_hint = TRUE
      )
    ),
    ellmer::tool(
      mcp_lint_project,
      name = "lint_project",
      description = paste(
        "Lint a whole R project or package with lintr and return every",
        "diagnostic, grouped by file. A directory holding a DESCRIPTION is",
        "linted as a package, so the files lintr keeps out of a package lint",
        "stay out of the result; any other directory is linted recursively.",
        "Use this instead of repeated lint_file calls when the target is the",
        "project rather than one file."
      ),
      arguments = list(
        dir = ellmer::type_string(
          paste(
            "Project root to lint. Defaults to the CLAUDE_PROJECT_DIR",
            "environment variable, or the working directory of the server",
            "process."
          ),
          required = FALSE
        )
      ),
      annotations = ellmer::tool_annotations(
        title = "Lint an R project",
        read_only_hint = TRUE,
        open_world_hint = FALSE,
        idempotent_hint = TRUE
      )
    )
  )
}


#' MCP Wrapper Around lint_file
#'
#' Wraps [lint_file()] in an [ellmer::ContentToolResult()] so the
#' diagnostics reach the client as compact JSON rather than a deparsed R
#' list.
#'
#' @inheritParams lint_file
#'
#' @return An `ellmer::ContentToolResult` carrying the grouped diagnostics.
#'
#' @keywords internal
#' @noRd
mcp_lint_file <- function(path, project_dir = NULL) {
  tryCatch(
    ellmer::ContentToolResult(value = lint_file(path, project_dir)),
    error = function(cnd) {
      ellmer::ContentToolResult(error = conditionMessage(cnd))
    }
  )
}


#' Lint a Single R File
#'
#' Runs [lintr::lint()] on one file and returns compact structured
#' diagnostics grouped by file. This is the implementation behind the MCP
#' `lint_file` tool.
#'
#' @param path Path to the R file, absolute or relative to `project_dir`.
#' @param project_dir Project root. Relative paths resolve against it and
#'   lintr runs with it as the working directory, which is what makes the
#'   `exclusions` in a project's `.lintr` resolve. Config discovery itself
#'   is lintr's own: it searches upwards from the linted file's directory,
#'   so an absolute path outside the anchor picks up its own project's
#'   `.lintr`, not this one. When `NULL`, the `CLAUDE_PROJECT_DIR`
#'   environment variable is used, falling back to the working directory.
#'
#' @return An unnamed list of groups, one per file, each with a `filename`
#'   and a `lints` list. Every lint carries `filename`, `line`, `column`,
#'   `type`, `message`, and `linter`. Empty list when the file is clean.
#'
#' @keywords internal
#' @noRd
lint_file <- function(path, project_dir = NULL) {
  anchor <- resolve_project_dir(project_dir)

  full_path <- if (is_absolute_path(path)) {
    path
  } else {
    file.path(anchor, path)
  }

  if (!file.exists(full_path)) {
    stop(sprintf("File not found: %s", full_path), call. = FALSE)
  }
  if (dir.exists(full_path)) {
    stop(
      sprintf("Not a file but a directory: %s", full_path),
      call. = FALSE
    )
  }

  old_dir <- setwd(anchor)
  on.exit(setwd(old_dir), add = TRUE)

  lints <- lintr::lint(relative_to(full_path, anchor))

  group_lints(lints, anchor)
}


#' MCP Wrapper Around lint_project
#'
#' Wraps [lint_project()] in an [ellmer::ContentToolResult()] so the
#' diagnostics reach the client as compact JSON rather than a deparsed R
#' list.
#'
#' @inheritParams lint_project
#'
#' @return An `ellmer::ContentToolResult` carrying the grouped diagnostics.
#'
#' @keywords internal
#' @noRd
mcp_lint_project <- function(dir = NULL) {
  tryCatch(
    ellmer::ContentToolResult(value = lint_project(dir)),
    error = function(cnd) {
      ellmer::ContentToolResult(error = conditionMessage(cnd))
    }
  )
}


#' Lint a Whole R Project
#'
#' Runs lintr over an entire project and returns the same compact
#' diagnostics as [lint_file()], grouped by file. This is the
#' implementation behind the MCP `lint_project` tool.
#'
#' A directory carrying a `DESCRIPTION` with a `Package` field is linted
#' with [lintr::lint_package()], which is what keeps files outside a
#' package's linted directories out of the result. Everything else goes
#' through [lintr::lint_dir()]. The check looks at `dir` itself and never
#' upwards, unlike `lint_package()`, which would otherwise walk up and lint
#' an enclosing package instead of the directory that was asked for.
#'
#' @param dir Project root to lint, and the anchor relative filenames are
#'   reported against. When `NULL`, the `CLAUDE_PROJECT_DIR` environment
#'   variable is used, falling back to the working directory.
#'
#' @return An unnamed list of groups, one per file, each with a `filename`
#'   and a `lints` list. Every lint carries `filename`, `line`, `column`,
#'   `type`, `message`, and `linter`. Empty list when the project is clean.
#'
#' @keywords internal
#' @noRd
lint_project <- function(dir = NULL) {
  anchor <- resolve_project_dir(dir)

  old_dir <- setwd(anchor)
  on.exit(setwd(old_dir), add = TRUE)

  lints <- if (is_package_dir(anchor)) {
    lintr::lint_package(anchor)
  } else {
    lintr::lint_dir(anchor)
  }

  group_lints(lints, anchor)
}


#' Is This Directory the Root of an R Package?
#'
#' @param dir Directory to inspect.
#'
#' @return `TRUE` when `dir` holds a readable `DESCRIPTION` naming a
#'   package, `FALSE` otherwise.
#'
#' @keywords internal
#' @noRd
is_package_dir <- function(dir) {
  description <- file.path(dir, "DESCRIPTION")

  if (!file.exists(description) || dir.exists(description)) {
    return(FALSE)
  }

  package <- tryCatch(
    read.dcf(description, fields = "Package")[1L, "Package"],
    error = function(cnd) NA_character_
  )

  !is.na(package) && nzchar(package)
}


#' Resolve the Project Anchor
#'
#' Implements the precedence tool argument, then `CLAUDE_PROJECT_DIR`, then
#' the process working directory.
#'
#' @param project_dir Project root supplied by the caller, or `NULL`.
#'
#' @return A normalised absolute path to an existing directory.
#'
#' @keywords internal
#' @noRd
resolve_project_dir <- function(project_dir = NULL) {
  candidate <- project_dir

  if (is.null(candidate) || length(candidate) != 1L || !nzchar(candidate)) {
    candidate <- Sys.getenv("CLAUDE_PROJECT_DIR", unset = "")
  }
  if (!nzchar(candidate)) {
    candidate <- getwd()
  }

  if (!dir.exists(candidate)) {
    stop(
      sprintf("Project directory does not exist: %s", candidate),
      call. = FALSE
    )
  }

  normalizePath(candidate, winslash = "/", mustWork = FALSE)
}


is_absolute_path <- function(path) {
  grepl("^(/|~|[A-Za-z]:[/\\\\]|\\\\\\\\)", path)
}


relative_to <- function(path, dir) {
  path <- normalizePath(path, winslash = "/", mustWork = FALSE)
  dir <- normalizePath(dir, winslash = "/", mustWork = FALSE)
  prefix <- paste0(sub("/+$", "", dir), "/")

  if (startsWith(path, prefix)) {
    substring(path, nchar(prefix) + 1L)
  } else {
    path
  }
}


group_lints <- function(lints, anchor) {
  if (length(lints) == 0) {
    return(list())
  }

  diagnostics <- lapply(lints, function(lint) {
    list(
      filename = relative_to(lint$filename, anchor),
      line = as.integer(lint$line_number),
      column = as.integer(lint$column_number),
      type = as.character(lint$type),
      message = as.character(lint$message),
      linter = as.character(lint$linter)
    )
  })

  filenames <- vapply(diagnostics, function(x) x$filename, character(1))

  unname(lapply(unique(filenames), function(file) {
    list(
      filename = file,
      lints = unname(diagnostics[filenames == file])
    )
  }))
}
