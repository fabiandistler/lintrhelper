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
        ),
        changed_only = ellmer::type_boolean(
          paste(
            "Report only the diagnostics of what git reports as changed",
            "(the edits in the working tree and the index, plus untracked",
            "files) rather than the whole project, so fixing a branch does",
            "not mean reading the whole repository's lints. Changes that",
            "are already committed are not included. A directory git cannot",
            "report on, an unversioned one above all, is reported whole and",
            "the reply says why. Defaults to false."
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
    ),
    ellmer::tool(
      mcp_list_rules,
      name = "list_rules",
      description = paste(
        "List the lintr linters available in this installation together with",
        "their tags, so the agent and the human work from the same ruleset.",
        "Optionally filter by tag: a linter is returned when it carries any",
        "of the requested tags, and omitting them lists everything. The",
        "result is metadata only: linter name, providing package, tags. Read",
        "the linter's own help page for the prose."
      ),
      arguments = list(
        tags = ellmer::type_array(
          items = ellmer::type_string(),
          description = paste(
            "Tags to filter by, for example \"default\", \"style\", or",
            "\"best_practices\". A linter matches when it carries any one of",
            "them. Omit to list every available linter. Unknown tags are",
            "reported back with the list of tags that do exist."
          ),
          required = FALSE
        )
      ),
      annotations = ellmer::tool_annotations(
        title = "List lintr rules",
        read_only_hint = TRUE,
        open_world_hint = FALSE,
        idempotent_hint = TRUE
      )
    ),
    ellmer::tool(
      mcp_explain_rule,
      name = "explain_rule",
      description = paste(
        "Explain one lintr linter: what it checks and how it is called,",
        "read from the installed help page. Use it before rewriting code to",
        "satisfy a lint, so the rewrite follows the rule rather than a guess",
        "at what the rule wants. The reply carries the linter's title,",
        "description, usage, and arguments: the part needed to act, not the",
        "whole man page. A name no linter carries is not an error, the reply",
        "names the closest matches instead."
      ),
      arguments = list(
        name = ellmer::type_string(
          paste(
            "Name of the linter to explain, as it appears in a lint or in",
            "list_rules, for example \"assignment_linter\"."
          ),
          required = TRUE
        )
      ),
      annotations = ellmer::tool_annotations(
        title = "Explain a lintr rule",
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
mcp_lint_project <- function(dir = NULL, changed_only = FALSE) {
  tryCatch(
    ellmer::ContentToolResult(
      value = tool_payload(lint_project(dir, changed_only))
    ),
    error = function(cnd) {
      ellmer::ContentToolResult(error = conditionMessage(cnd))
    }
  )
}


#' The Reply Payload for a lint_project Result
#'
#' [lint_project()] carries the note that `changed_only` could not be
#' honoured as an attribute, which JSON has no room for. The note matters
#' to the agent — it explains why it asked for the changed files and got
#' the whole project — so it becomes a field of the reply, with the
#' diagnostics under `lints` beside it. A result with nothing to say stays
#' the bare array of groups the other lint tool returns.
#'
#' @param result The result of [lint_project()].
#'
#' @return The groups as they came, or a list with `message` and `lints`.
#'
#' @keywords internal
#' @noRd
tool_payload <- function(result) {
  message <- attr(result, "message")
  attributes(result) <- NULL

  if (is.null(message)) {
    return(result)
  }

  list(message = message, lints = result)
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
#' `changed_only = TRUE` narrows the *reply* to what git reports as changed
#' — the edits in the working tree and the index, plus untracked files — so
#' an agent fixing a branch reads only the diagnostics for what it touched
#' instead of the whole repository's. Changes that are already committed are
#' not included: the question asked is "what have I touched and not
#' finished", which is the state an agent's own edits leave behind.
#'
#' The project is still linted whole and the changed paths are then kept
#' out of the result. That is what makes the narrowed set a subset of the
#' full lint by construction: lintr applies its own exclusions — the
#' directories a package lint walks, hidden paths, `renv` and `packrat`
#' libraries, a project's `.lintr` — so nothing here has to restate them
#' and fall out of step on the next lintr release. It costs a whole-project
#' lint per call; correctness is worth more than the saving.
#'
#' A directory git cannot report on — an unversioned one above all — is not
#' an error. It is linted whole, and the result carries a `message`
#' attribute saying why, which [tool_payload()] moves into the reply.
#'
#' @param dir Project root to lint, and the anchor relative filenames are
#'   reported against. When `NULL`, the `CLAUDE_PROJECT_DIR` environment
#'   variable is used, falling back to the working directory.
#' @param changed_only Lint only the files git reports as changed rather
#'   than the whole project. `FALSE`, the default, lints everything.
#'
#' @return An unnamed list of groups, one per file, each with a `filename`
#'   and a `lints` list. Every lint carries `filename`, `line`, `column`,
#'   `type`, `message`, and `linter`. Empty list when the project is clean,
#'   or when `changed_only = TRUE` and nothing changed. A `message`
#'   attribute is present only when `changed_only` was asked for and could
#'   not be honoured.
#'
#' @keywords internal
#' @noRd
lint_project <- function(dir = NULL, changed_only = FALSE) {
  changed_only <- normalise_changed_only(changed_only)
  anchor <- resolve_project_dir(dir)

  old_dir <- setwd(anchor)
  on.exit(setwd(old_dir), add = TRUE)

  changed <- if (changed_only) changed_files(anchor) else NULL

  result <- group_lints(lint_whole_project(anchor), anchor)

  if (!is.null(changed) && is.null(changed$message)) {
    result <- keep_changed_groups(result, changed$files)
  }

  # NULL when git answered, and then the assignment leaves no attribute.
  attr(result, "message") <- changed$message

  result
}


#' Keep Only the Groups Belonging to Changed Files
#'
#' @param groups The grouped diagnostics of a whole-project lint.
#' @param files Changed paths relative to the project root, as git
#'   reported them.
#'
#' @return The groups whose `filename` git named, in the order the lint
#'   produced them.
#'
#' @keywords internal
#' @noRd
keep_changed_groups <- function(groups, files) {
  filenames <- vapply(groups, function(group) group$filename, character(1))

  unname(groups[filenames %in% files])
}


#' Lint Every File in a Project
#'
#' A directory carrying a `DESCRIPTION` is linted as a package, everything
#' else as a plain directory. See [lint_project()] for why the check never
#' looks upwards.
#'
#' @param anchor Normalised project root, already the working directory.
#'
#' @return The `lints` object lintr produced.
#'
#' @keywords internal
#' @noRd
lint_whole_project <- function(anchor) {
  if (is_package_dir(anchor)) {
    lintr::lint_package(anchor)
  } else {
    lintr::lint_dir(anchor)
  }
}


#' Normalise the changed_only Flag
#'
#' Accepts what an MCP client can send for an optional boolean — `NULL`, a
#' logical scalar, or a list holding one — and returns a plain flag.
#'
#' @param changed_only The `changed_only` argument as received.
#'
#' @return `TRUE` or `FALSE`.
#'
#' @keywords internal
#' @noRd
normalise_changed_only <- function(changed_only) {
  if (is.null(changed_only)) {
    return(FALSE)
  }

  changed_only <- unlist(changed_only, use.names = FALSE)

  if (length(changed_only) == 0L) {
    return(FALSE)
  }
  if (
    !is.logical(changed_only) ||
      length(changed_only) != 1L ||
      is.na(changed_only)
  ) {
    stop("`changed_only` must be TRUE or FALSE.", call. = FALSE)
  }

  changed_only
}


#' The Files Git Reports as Changed
#'
#' Asks git for the paths that differ from `HEAD` in the working tree or
#' the index, plus the untracked files it does not ignore. Paths come back
#' relative to `dir`, which is what makes an anchor inside a larger
#' repository report its own files and no others.
#'
#' The paths are not narrowed to what lintr would read: [lint_project()]
#' intersects them with the groups of a whole-project lint, so a path lintr
#' never walks simply matches nothing.
#'
#' A repository without a commit has no `HEAD` to diff against, so
#' everything it tracks counts as changed — which for a fresh `git init` is
#' the whole project, and rightly so.
#'
#' @param dir Normalised project root.
#'
#' @return A list carrying either `files`, the changed paths relative to
#'   `dir`, or `message`, saying why git could not be asked and the whole
#'   project was linted instead.
#'
#' @keywords internal
#' @noRd
changed_files <- function(dir) {
  ignored <- function(reason) {
    list(message = sprintf(
      "changed_only was ignored and the whole project linted: %s.",
      reason
    ))
  }

  if (!nzchar(Sys.which("git"))) {
    return(ignored("git is not on the PATH"))
  }
  if (!run_git(dir, c("rev-parse", "--git-dir"))$ok) {
    return(ignored(sprintf("%s is not a git repository", dir)))
  }

  has_head <- run_git(dir, c("rev-parse", "--verify", "--quiet", "HEAD"))$ok

  tracked <- if (has_head) {
    run_git(dir, c("diff", "--name-only", "--relative", "HEAD", "--"))
  } else {
    run_git(dir, c("ls-files", "--cached", "--exclude-standard"))
  }
  untracked <- run_git(dir, c("ls-files", "--others", "--exclude-standard"))

  if (!tracked$ok || !untracked$ok) {
    return(ignored(
      sprintf("git could not report the changed files in %s", dir)
    ))
  }

  files <- c(tracked$lines, untracked$lines)

  list(files = sort(unique(files[nzchar(files)])))
}


#' Run Git and Collect Its Output
#'
#' stderr is dropped rather than merged into the output: a warning about a
#' detached head is not a changed file. The exit status is what says
#' whether the answer can be trusted.
#'
#' @param dir Directory to run in, passed to git as `-C`.
#' @param args Character vector of arguments.
#'
#' @return A list with `ok`, whether git exited cleanly, and `lines`, the
#'   lines it wrote to stdout.
#'
#' @keywords internal
#' @noRd
run_git <- function(dir, args) {
  output <- suppressWarnings(system2(
    "git",
    c("-c", "core.quotepath=false", "-C", shQuote(dir), args),
    stdout = TRUE,
    stderr = FALSE
  ))

  status <- attr(output, "status")

  list(
    ok = is.null(status) || identical(as.integer(status), 0L),
    lines = as.character(output)
  )
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


#' MCP Wrapper Around list_rules
#'
#' Wraps [list_rules()] in an [ellmer::ContentToolResult()] so the rule
#' metadata reaches the client as compact JSON rather than a deparsed R
#' list.
#'
#' @inheritParams list_rules
#'
#' @return An `ellmer::ContentToolResult` carrying the rule metadata.
#'
#' @keywords internal
#' @noRd
mcp_list_rules <- function(tags = NULL) {
  tryCatch(
    ellmer::ContentToolResult(value = list_rules(tags)),
    error = function(cnd) {
      ellmer::ContentToolResult(error = conditionMessage(cnd))
    }
  )
}


#' List the Available lintr Rules
#'
#' Returns the linters [lintr::available_linters()] knows about, optionally
#' narrowed to a set of tags. This is the implementation behind the MCP
#' `list_rules` tool, and it is what lets an agent see the ruleset before
#' it starts changing code.
#'
#' Only metadata comes back — the linter name, the package providing it,
#' and its tags. The tags carry more than they look like they do: lintr
#' tags its default linters `"default"`, so `list_rules("default")` is the
#' set that runs when a project has no `.lintr` of its own.
#'
#' A tag no linter carries is not an error. The result is empty and
#' `message` names the offending tags alongside the ones that do exist, so
#' the agent can correct itself in one step instead of guessing.
#'
#' @param tags Character vector of tags to filter by, or `NULL`. A linter
#'   is kept when it carries any one of the tags — the filter is a union,
#'   not an intersection. `NULL` (or an empty vector) returns every linter.
#'
#' @return A list with `count`, the number of linters returned, and
#'   `rules`, an unnamed list of linters each carrying `linter`, `package`,
#'   and `tags`. A `message` element is present only when something needs
#'   saying, currently when a requested tag does not exist.
#'
#' @keywords internal
#' @noRd
list_rules <- function(tags = NULL) {
  requested <- normalise_tags(tags)

  available <- lintr::available_linters()
  all_tags <- sort(unique(unlist(available[["tags"]], use.names = FALSE)))
  unknown <- setdiff(requested, all_tags)
  known <- setdiff(requested, unknown)

  if (length(requested) > 0L) {
    matches <- vapply(
      available[["tags"]],
      function(x) any(known %in% x),
      logical(1)
    )
    available <- available[matches, , drop = FALSE]
  }

  rules <- lapply(seq_len(nrow(available)), function(i) {
    list(
      linter = as.character(available[["linter"]][[i]]),
      package = as.character(available[["package"]][[i]]),
      tags = as.list(as.character(available[["tags"]][[i]]))
    )
  })

  result <- list(count = length(rules), rules = unname(rules))

  if (length(unknown) > 0L) {
    result$message <- sprintf(
      "No linter carries the tag(s): %s. Available tags: %s.",
      paste(unknown, collapse = ", "),
      paste(all_tags, collapse = ", ")
    )
  }

  result
}


#' Normalise a Tag Filter
#'
#' Accepts what an MCP client can send for an optional string array —
#' `NULL`, a character vector, or a list of strings — and returns a plain
#' character vector. Blanks are dropped so a client sending `""` for "no
#' filter" is not read as asking for a tag named `""`.
#'
#' @param tags The `tags` argument as received.
#'
#' @return A unique character vector, empty when no filter was requested.
#'
#' @keywords internal
#' @noRd
normalise_tags <- function(tags) {
  if (is.null(tags)) {
    return(character(0))
  }

  tags <- unlist(tags, use.names = FALSE)

  if (length(tags) == 0L) {
    return(character(0))
  }
  if (!is.character(tags)) {
    stop("`tags` must be a character vector of linter tags.", call. = FALSE)
  }

  tags <- trimws(tags)
  unique(tags[!is.na(tags) & nzchar(tags)])
}


#' MCP Wrapper Around explain_rule
#'
#' Wraps [explain_rule()] in an [ellmer::ContentToolResult()] so the rule
#' documentation reaches the client as compact JSON rather than a deparsed
#' R list.
#'
#' @inheritParams explain_rule
#'
#' @return An `ellmer::ContentToolResult` carrying the rule documentation.
#'
#' @keywords internal
#' @noRd
mcp_explain_rule <- function(name) {
  tryCatch(
    ellmer::ContentToolResult(value = explain_rule(name)),
    error = function(cnd) {
      ellmer::ContentToolResult(error = conditionMessage(cnd))
    }
  )
}


#' Explain a Single lintr Rule
#'
#' Reads the installed help page of one linter and returns the part an
#' agent needs before it rewrites code: what the rule checks, how the
#' linter is called, and what its arguments do. This is the implementation
#' behind the MCP `explain_rule` tool.
#'
#' The man page is deliberately not returned whole. Examples, the tag
#' section, and the see-also links are what a human browses; an agent
#' acting on a lint needs the description and the usage, so those are what
#' comes back.
#'
#' A name no linter carries is not an error, the same way an unknown tag is
#' not one in [list_rules()]. The result reports `found = FALSE` and names
#' the closest linters, so a near miss — `"assignment"` for
#' `"assignment_linter"`, or a typo — is corrected in one step rather than
#' by retrying blind.
#'
#' Deprecated linters are explainable even though [list_rules()] leaves
#' them out of its listing: a name only turns up in a `.lintr` or an old
#' lint after it has been deprecated, which is exactly when the agent needs
#' the help page to say so. The `tags` in the result carry `"deprecated"`.
#'
#' @param name Name of the linter to explain, for example
#'   `"assignment_linter"`.
#'
#' @return A list with `found`, `linter`, and, when the linter exists,
#'   `package`, `tags`, `title`, `description`, `usage`, `arguments` (an
#'   unnamed list of `name`/`description` pairs), and `help`, the R
#'   expression that opens the full page. When the linter does not exist,
#'   `suggestions` and `message` name the closest matches instead. A
#'   `message` is also present when the linter exists but its package is
#'   installed without help pages.
#'
#' @keywords internal
#' @noRd
explain_rule <- function(name) {
  name <- normalise_rule_name(name)

  available <- lintr::available_linters(exclude_tags = NULL)
  index <- match(name, as.character(available[["linter"]]))

  if (is.na(index)) {
    suggestions <- closest_rules(name, as.character(available[["linter"]]))

    return(list(
      found = FALSE,
      linter = name,
      suggestions = as.list(suggestions),
      message = sprintf(
        paste(
          "No linter named \"%s\". Closest matches: %s.",
          "Call list_rules for the full set."
        ),
        name,
        paste(suggestions, collapse = ", ")
      )
    ))
  }

  package <- as.character(available[["package"]][[index]])

  result <- list(
    found = TRUE,
    linter = name,
    package = package,
    tags = as.list(as.character(available[["tags"]][[index]]))
  )

  rd <- rule_help(name, package)

  if (is.null(rd)) {
    result$message <- sprintf(
      paste(
        "%s exists but %s is installed without help pages,",
        "so only its metadata is available."
      ),
      name,
      package
    )

    return(result)
  }

  c(
    result,
    list(
      title = rd_text(rd_section(rd, "\\title"), "\\title"),
      description = rd_text(rd_section(rd, "\\description"), "\\description"),
      usage = rd_text(rd_section(rd, "\\usage"), "\\usage"),
      arguments = rd_arguments(rd_section(rd, "\\arguments")),
      help = sprintf("?%s::%s", package, name)
    )
  )
}


#' Normalise a Requested Rule Name
#'
#' Accepts what an MCP client can send for a required string — a character
#' scalar or a list holding one — and returns a plain string.
#'
#' @param name The `name` argument as received.
#'
#' @return A length-one character vector.
#'
#' @keywords internal
#' @noRd
normalise_rule_name <- function(name) {
  name <- unlist(name, use.names = FALSE)

  if (!is.character(name) || length(name) != 1L || is.na(name)) {
    stop("`name` must be a single linter name.", call. = FALSE)
  }

  name <- trimws(name)

  if (!nzchar(name)) {
    stop("`name` must be a single linter name.", call. = FALSE)
  }

  name
}


#' Linters Closest to a Name That Does Not Exist
#'
#' Ranks the known linters against a name no linter carries. Names that
#' contain the request as a substring come first — that is the
#' `"assignment"` for `"assignment_linter"` case, where edit distance alone
#' ranks poorly — and the rest follow in order of edit distance.
#'
#' @param name The requested name.
#' @param candidates Character vector of known linter names.
#' @param n Most suggestions to return.
#'
#' @return A character vector of at most `n` linter names.
#'
#' @keywords internal
#' @noRd
closest_rules <- function(name, candidates, n = 5L) {
  if (length(candidates) == 0L) {
    return(character(0))
  }

  needle <- tolower(name)
  haystack <- tolower(candidates)

  contained <- sort(candidates[
    startsWith(haystack, needle) | startsWith(needle, haystack)
  ])
  substrings <- sort(candidates[grepl(needle, haystack, fixed = TRUE)])
  distances <- utils::adist(needle, haystack)[1L, ]
  nearest <- candidates[order(distances, candidates)]

  utils::head(unique(c(contained, substrings, nearest)), n)
}


#' Find the Help Page Documenting a Linter
#'
#' @param name Linter name.
#' @param package Package providing the linter.
#'
#' @return The parsed `Rd` object whose aliases contain `name`, or `NULL`
#'   when the package ships no help database or documents no such alias.
#'
#' @keywords internal
#' @noRd
rule_help <- function(name, package) {
  db <- tryCatch(
    tools::Rd_db(package),
    error = function(cnd) NULL,
    warning = function(cnd) NULL
  )

  if (length(db) == 0L) {
    return(NULL)
  }

  for (rd in db) {
    if (name %in% rd_aliases(rd)) {
      return(rd)
    }
  }

  NULL
}


#' Aliases of a Parsed Help Page
#'
#' @param rd A parsed `Rd` object.
#'
#' @return A character vector of the page's `\alias` entries.
#'
#' @keywords internal
#' @noRd
rd_aliases <- function(rd) {
  aliases <- rd[rd_tags(rd) == "\\alias"]

  vapply(
    aliases,
    function(alias) trimws(paste0(unlist(alias), collapse = "")),
    character(1)
  )
}


#' Section Tags of a Parsed Help Page
#'
#' @param rd A parsed `Rd` object, or any list of Rd fragments.
#'
#' @return A character vector of Rd tags, one per element.
#'
#' @keywords internal
#' @noRd
rd_tags <- function(rd) {
  vapply(
    rd,
    function(part) {
      tag <- attr(part, "Rd_tag")
      if (is.null(tag)) NA_character_ else tag
    },
    character(1)
  )
}


#' One Section of a Parsed Help Page
#'
#' @param rd A parsed `Rd` object.
#' @param tag The Rd tag to extract, for example `"\\description"`.
#'
#' @return The first matching section, or `NULL` when the page has none.
#'
#' @keywords internal
#' @noRd
rd_section <- function(rd, tag) {
  index <- which(rd_tags(rd) == tag)

  if (length(index) == 0L) {
    return(NULL)
  }

  rd[[index[[1L]]]]
}


#' Render an Rd Fragment as Plain Text
#'
#' Wraps the fragment in a minimal help page and hands it to
#' [tools::Rd2txt()], which is what turns `\code{}`, `\link{}`, and item
#' lists into readable prose. The synthetic title and the section heading
#' `Rd2txt()` prints are then stripped back off, along with the indent it
#' adds, leaving the section's own text.
#'
#' The fragment is rendered under its own tag rather than a generic one so
#' `\usage` keeps its verbatim line breaks instead of being reflowed as
#' prose.
#'
#' @param fragment An Rd fragment, or `NULL`.
#' @param tag The Rd tag to render the fragment under.
#'
#' @return A single string, or `NULL` when `fragment` is `NULL` or renders
#'   to nothing.
#'
#' @keywords internal
#' @noRd
rd_text <- function(fragment, tag) {
  if (is.null(fragment)) {
    return(NULL)
  }

  placeholder <- function(section) {
    structure(list(structure("x", Rd_tag = "TEXT")), Rd_tag = section)
  }

  # \title is the one section Rd2txt prints without a heading, so the
  # page's own title would be indistinguishable from the synthetic one.
  # Rendering it as prose instead keeps the stripping below uniform.
  rendered_as <- if (identical(tag, "\\title")) "\\description" else tag

  page <- structure(
    list(
      placeholder("\\name"),
      placeholder("\\title"),
      structure(drop_rd_sexpr(fragment), Rd_tag = rendered_as)
    ),
    class = "Rd"
  )

  file <- tempfile()
  on.exit(unlink(file), add = TRUE)

  tools::Rd2txt(
    page,
    out = file,
    options = list(underline_titles = FALSE, width = 76L)
  )

  lines <- sub("[[:space:]]+$", "", readLines(file, warn = FALSE))

  # Drop the synthetic title, then the section heading, then the blank
  # lines around either of them.
  lines <- drop_leading_blanks(lines[-1L])
  lines <- drop_leading_blanks(lines[-1L])

  while (length(lines) > 0L && !nzchar(lines[[length(lines)]])) {
    lines <- lines[-length(lines)]
  }

  if (length(lines) == 0L) {
    return(NULL)
  }

  indents <- nchar(sub("[^ ].*$", "", lines[nzchar(lines)]))
  lines <- substring(lines, min(indents) + 1L)

  paste(lines, collapse = "\n")
}


#' Replace \Sexpr Nodes With a Pointer to the Help Page
#'
#' Help pages can build part of their text by running R code at render
#' time. [tools::Rd_db()] hands back that code unevaluated, and rendering
#' it would leak markup such as `\Sexpr[stage=render]{pkg:::helper}` into
#' the reply — or, worse, invite the agent to run a package's internals.
#' The node is replaced by a pointer to the page instead.
#'
#' @param x An Rd fragment, or any node inside one.
#'
#' @return `x` with every `\Sexpr` node replaced by plain text.
#'
#' @keywords internal
#' @noRd
drop_rd_sexpr <- function(x) {
  if (identical(attr(x, "Rd_tag"), "\\Sexpr")) {
    return(structure("(see the full help page)", Rd_tag = "TEXT"))
  }

  if (is.list(x)) {
    attributes_of_x <- attributes(x)
    x <- lapply(x, drop_rd_sexpr)
    attributes(x) <- attributes_of_x
  }

  x
}


drop_leading_blanks <- function(lines) {
  while (length(lines) > 0L && !nzchar(lines[[1L]])) {
    lines <- lines[-1L]
  }

  lines
}


#' Argument Documentation of a Help Page
#'
#' @param fragment The page's `\arguments` section, or `NULL`.
#'
#' @return An unnamed list of arguments, each with `name` and
#'   `description`. Empty when the linter takes no arguments.
#'
#' @keywords internal
#' @noRd
rd_arguments <- function(fragment) {
  if (is.null(fragment)) {
    return(list())
  }

  items <- fragment[rd_tags(fragment) == "\\item"]

  # An \item in \arguments carries the argument name and its
  # description, but a page can document a name and leave it at that.
  unname(lapply(items, function(item) {
    list(
      name = trimws(paste0(unlist(item[[1L]]), collapse = "")),
      description = if (length(item) > 1L) {
        rd_text(item[[2L]], "\\description")
      }
    )
  }))
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
