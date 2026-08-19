#' Start the lintrhelper MCP Server
#'
#' Starts a Model Context Protocol (MCP) server over stdio that exposes
#' lintr diagnostics to coding agents such as Claude Code, Codex, and
#' opencode. The server currently provides a single read-only tool,
#' `lint_file`, which lints one R file under whichever `.lintr`
#' configuration lintr finds for it.
#'
#' The function blocks the R process indefinitely and is not intended for
#' interactive use. Register it with an MCP client instead — see the
#' examples for a Claude Code `.mcp.json` entry.
#'
#' `mcptools` and `ellmer` are only suggested dependencies, so authors who
#' merely write linters need not install the MCP stack. Calling this
#' function without them raises an installation prompt.
#'
#' This package writes only JSON-RPC to stdout; its own diagnostics use
#' [message()] so they land on stderr and cannot corrupt the protocol.
#' Startup files are outside its control, though: anything an `.Rprofile`
#' or `Rprofile.site` prints arrives ahead of the handshake and breaks it.
#' Launch the server with `Rscript --no-init-file --no-site-file`, as the
#' bundled `.mcp.json` does — `mcptools` itself tells users to put
#' [mcptools::mcp_session()] in their `.Rprofile`, so the collision is a
#' live one.
#'
#' @return `NULL`, invisibly, once the client closes the connection.
#'   Called for its side effect: until then the process is blocked serving
#'   the protocol.
#'
#' @examples
#' \dontrun{
#' # Started by an MCP client, not by hand:
#' # Rscript -e 'lintrhelper::start_mcp_server()'
#' start_mcp_server()
#' }
#'
#' @export
start_mcp_server <- function() {
  check_mcp_deps()

  mcptools::mcp_server(tools = mcp_tools(), session_tools = FALSE)

  invisible(NULL)
}


check_mcp_deps <- function() {
  rlang::check_installed(
    c("mcptools", "ellmer"),
    reason = "to start the lintrhelper MCP server."
  )
}
