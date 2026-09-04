# Characterization tests for R/mcp_server.R.
#
# The server loop itself blocks the process and needs the suggested MCP
# stack, so only the dependency guard in front of it is exercised here; see
# BLOCKED.md for what could not be reached.

test_that("check_mcp_deps names both suggested packages when they are absent", {
  skip_if(
    requireNamespace("mcptools", quietly = TRUE) &&
      requireNamespace("ellmer", quietly = TRUE),
    "the MCP stack is installed, so the missing-dependency path is unreachable"
  )
  rlang::local_interactive(FALSE)

  condition <- expect_error(check_mcp_deps())

  expect_s3_class(condition, "rlib_error_package_not_found")
  expect_match(conditionMessage(condition), "mcptools")
  expect_match(conditionMessage(condition), "ellmer")
  # rlang wraps the reason to the console width, so match it without the
  # line break it may insert.
  expect_match(
    gsub("[[:space:]]+", " ", conditionMessage(condition)),
    "required to start the lintrhelper MCP server.",
    fixed = TRUE
  )
})


test_that("start_mcp_server refuses before it blocks when the MCP stack is absent", {
  skip_if(
    requireNamespace("mcptools", quietly = TRUE) &&
      requireNamespace("ellmer", quietly = TRUE),
    "the MCP stack is installed, so start_mcp_server() would block"
  )
  rlang::local_interactive(FALSE)

  # The guard runs first, so the call returns instead of serving forever.
  expect_error(start_mcp_server(), class = "rlib_error_package_not_found")
})
