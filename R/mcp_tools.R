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


#' Carry a Tool's Reply Back to the Client
#'
#' Wraps a reply in an [ellmer::ContentToolResult()] so it reaches the
#' client as compact JSON rather than a deparsed R list, and turns a
#' failure the agent can correct itself — an unknown path, a malformed
#' argument — into a tool error carrying the message, rather than a
#' JSON-RPC internal error that clients surface as a hard failure.
#'
#' `reply` is evaluated inside the `tryCatch()`, so the tools below can
#' pass the call itself and leave the error handling here.
#'
#' @param reply The tool's reply, unevaluated until it is needed.
#'
#' @return An `ellmer::ContentToolResult` carrying the reply, or the error
#'   message when producing it failed.
#'
#' @keywords internal
#' @noRd
mcp_wrap <- function(reply) {
  tryCatch(
    ellmer::ContentToolResult(value = reply),
    error = function(cnd) {
      ellmer::ContentToolResult(error = conditionMessage(cnd))
    }
  )
}


#' Normalise an Argument as an MCP Client Sent It
#'
#' A client can send a scalar as a length-one list and an array as a list
#' of scalars, so every tool argument is unlisted before it is looked at.
#' Strings are then trimmed, and blanks dropped — a client sending `""` for
#' "no filter" is not asking for a tag named `""`.
#'
#' @param value The argument as received.
#' @param mode The [typeof()] the argument must have once unlisted.
#' @param message The error to raise when it does not.
#' @param scalar Whether exactly one value is expected. A vector argument
#'   is deduplicated; a scalar one is not, so two of the same value stay
#'   two values and are rejected.
#' @param empty What to return when nothing is left. Omitted when the
#'   argument is required, and then nothing left is an error.
#'
#' @return The normalised value, or `empty`.
#'
#' @keywords internal
#' @noRd
normalise_argument <- function(value,
                               mode,
                               message,
                               scalar = TRUE,
                               empty = NULL) {
  value <- unlist(value, use.names = FALSE)

  if (identical(mode, "character") && is.character(value)) {
    value <- trimws(value)
    value <- value[!is.na(value) & nzchar(value)]

    if (!scalar) {
      value <- unique(value)
    }
  }

  if (length(value) == 0L) {
    if (is.null(empty)) {
      stop(message, call. = FALSE)
    }

    return(empty)
  }

  wrong_length <- scalar && length(value) != 1L

  if (!identical(typeof(value), mode) || anyNA(value) || wrong_length) {
    stop(message, call. = FALSE)
  }

  value
}


#' MCP Wrapper Around lint_file
#'
#' @inheritParams lint_file
#'
#' @return An `ellmer::ContentToolResult` carrying the grouped diagnostics.
#'
#' @keywords internal
#' @noRd
mcp_lint_file <- function(path, project_dir = NULL) {
  mcp_wrap(lint_file(path, project_dir))
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
#' @inheritParams lint_project
#'
#' @return An `ellmer::ContentToolResult` carrying the grouped diagnostics.
#'
#' @keywords internal
#' @noRd
mcp_lint_project <- function(dir = NULL, changed_only = FALSE) {
  mcp_wrap(tool_payload(lint_project(dir, changed_only)))
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
  changed_only <- normalise_argument(
    changed_only,
    mode = "logical",
    message = "`changed_only` must be TRUE or FALSE.",
    empty = FALSE
  )
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
#' @inheritParams list_rules
#'
#' @return An `ellmer::ContentToolResult` carrying the rule metadata.
#'
#' @keywords internal
#' @noRd
mcp_list_rules <- function(tags = NULL) {
  mcp_wrap(list_rules(tags))
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
  requested <- normalise_argument(
    tags,
    mode = "character",
    message = "`tags` must be a character vector of linter tags.",
    scalar = FALSE,
    empty = character(0)
  )

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


#' MCP Wrapper Around explain_rule
#'
#' @inheritParams explain_rule
#'
#' @return An `ellmer::ContentToolResult` carrying the rule documentation.
#'
#' @keywords internal
#' @noRd
mcp_explain_rule <- function(name) {
  mcp_wrap(explain_rule(name))
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
  name <- normalise_argument(
    name,
    mode = "character",
    message = "`name` must be a single linter name."
  )

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

  page <- rule_help(name, package)

  if (is.null(page)) {
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

  sections <- help_sections(page)

  c(
    result,
    list(
      title = help_text(sections[["Title"]]),
      description = help_text(sections[["Description"]]),
      usage = help_text(sections[["Usage"]]),
      arguments = help_arguments(sections[["Arguments"]]),
      help = sprintf("?%s::%s", package, name)
    )
  )
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


#' Render the Help Page Documenting a Linter
#'
#' Looks the name up in the package's installed alias index — the map from
#' alias to help page that `?` itself searches — and renders the page it
#' names once with [tools::Rd2txt()], which lays the page out the way `?`
#' does: the title on the first line, then a heading per section with its
#' body indented under it. [help_sections()] cuts the reply out of that
#' text, so nothing here walks the parsed `Rd` tree.
#'
#' `\Sexpr` nodes are replaced before rendering. A help page can build part
#' of its text by running R code at render time, and `Rd2txt()` would run
#' it — that is what `stage = "render"` means — putting whatever a
#' package's internals produced into the reply.
#'
#' @param name Linter name.
#' @param package Package providing the linter.
#'
#' @return The rendered page as a character vector of lines, trailing
#'   whitespace removed, or `NULL` when the package ships no help database
#'   or documents no such alias.
#'
#' @keywords internal
#' @noRd
rule_help <- function(name, package) {
  quietly <- function(expr) {
    tryCatch(expr, error = function(cnd) NULL, warning = function(cnd) NULL)
  }

  index <- quietly(
    readRDS(system.file("help", "aliases.rds", package = package))
  )
  db <- quietly(tools::Rd_db(package))
  page <- paste0(index[name], ".Rd")

  if (!page %in% names(db)) {
    return(NULL)
  }

  file <- tempfile()
  on.exit(unlink(file), add = TRUE)

  tools::Rd2txt(
    drop_rd_sexpr(db[[page]]),
    out = file,
    options = list(underline_titles = FALSE, width = 76L)
  )

  sub("[[:space:]]+$", "", readLines(file, warn = FALSE))
}


#' Replace \Sexpr Nodes With a Pointer to the Help Page
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


#' Split a Rendered Help Page Into Its Sections
#'
#' A heading is a line that starts in the first column, ends in a colon,
#' and is followed by a blank line — `Description:`, `Usage:`,
#' `Arguments:`, and the rest. Everything up to the next heading is that
#' section's body. The title comes before the first heading and is returned
#' as `Title`, which is not a section `Rd2txt()` ever prints.
#'
#' @param page A rendered help page, as [rule_help()] returns it.
#'
#' @return A named list of character vectors, one per section, named
#'   without the trailing colon.
#'
#' @keywords internal
#' @noRd
help_sections <- function(page) {
  heading <- grepl("^[^[:space:]].*:$", page) & c(!nzchar(page[-1L]), TRUE)
  starts <- c(0L, which(heading))
  ends <- c(starts[-1L] - 1L, length(page))

  sections <- Map(
    function(start, end) if (end > start) page[(start + 1L):end] else "",
    starts,
    ends
  )
  names(sections) <- c("Title", sub(":$", "", page[starts[-1L]]))

  sections
}


#' One Section's Text, Without the Indent Rd2txt Added
#'
#' @param lines A section body, or `NULL` when the page has no such
#'   section.
#'
#' @return A single string, or `NULL` when the section is absent or blank.
#'
#' @keywords internal
#' @noRd
help_text <- function(lines) {
  filled <- which(nzchar(lines))

  if (length(filled) == 0L) {
    return(NULL)
  }

  lines <- lines[min(filled):max(filled)]
  indents <- nchar(sub("[^ ].*$", "", lines[nzchar(lines)]))

  paste(substring(lines, min(indents) + 1L), collapse = "\n")
}


#' Argument Documentation Cut Out of a Rendered Arguments Section
#'
#' `Rd2txt()` lays each argument out as `name: description`, wrapping the
#' description under a hanging indent. An item therefore starts where a
#' line opens with argument names and a colon left of that indent; a page
#' documenting two names in one item keeps both, as the help page prints
#' them.
#'
#' The indent is what makes the name a name. Wrapped description text lands
#' on the hanging indent and can open with a word and a colon of its own —
#' `object_overwrite_linter` wraps onto "packages: base, stats, ..." —
#' which would otherwise start an argument that does not exist.
#'
#' @param lines The page's `Arguments` section, or `NULL`.
#'
#' @return An unnamed list of arguments, each with `name` and
#'   `description`. Empty when the linter takes no arguments.
#'
#' @keywords internal
#' @noRd
help_arguments <- function(lines) {
  name <- "[A-Za-z._][A-Za-z0-9._]*|\\.\\.\\."
  labelled <- grepl(sprintf("^ *(%s)(, (%s))*: ", name, name), lines)
  indents <- nchar(sub("[^ ].*$", "", lines))
  wrapped <- nzchar(lines) & !labelled

  hanging <- if (any(wrapped)) min(indents[wrapped]) else max(0L, indents) + 1L
  opens_item <- labelled & indents < hanging

  if (!any(opens_item)) {
    return(list())
  }

  items <- split(lines, cumsum(opens_item))

  unname(lapply(items[names(items) != "0"], function(item) {
    label <- sub("^( *[^:]*: ).*$", "\\1", item[[1L]])

    list(
      name = trimws(sub(":.*$", "", item[[1L]])),
      description = paste(
        c(substring(item[[1L]], nchar(label) + 1L), help_text(item[-1L])),
        collapse = "\n"
      )
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
