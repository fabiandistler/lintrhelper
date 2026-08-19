# Start the lintrhelper MCP Server

Starts a Model Context Protocol (MCP) server over stdio that exposes
lintr diagnostics to coding agents such as Claude Code, Codex, and
opencode. The server currently provides two read-only tools:
`lint_file`, which lints one R file, and `lint_project`, which lints a
whole project or package in one call. Both run under whichever `.lintr`
configuration lintr finds for the code they lint.

## Usage

``` r
start_mcp_server()
```

## Value

`NULL`, invisibly, once the client closes the connection. Called for its
side effect: until then the process is blocked serving the protocol.

## Details

The function blocks the R process indefinitely and is not intended for
interactive use. Register it with an MCP client instead — see the
examples for a Claude Code `.mcp.json` entry.

`mcptools` and `ellmer` are only suggested dependencies, so authors who
merely write linters need not install the MCP stack. Calling this
function without them raises an installation prompt.

This package writes only JSON-RPC to stdout; its own diagnostics use
[`message()`](https://rdrr.io/r/base/message.html) so they land on
stderr and cannot corrupt the protocol. Startup files are outside its
control, though: anything an `.Rprofile` or `Rprofile.site` prints
arrives ahead of the handshake and breaks it. Launch the server with
`Rscript --no-init-file --no-site-file`, as the bundled `.mcp.json` does
— `mcptools` itself tells users to put
[`mcptools::mcp_session()`](https://posit-dev.github.io/mcptools/reference/server.html)
in their `.Rprofile`, so the collision is a live one.

## Examples

``` r
if (FALSE) { # \dontrun{
# Started by an MCP client, not by hand:
# Rscript -e 'lintrhelper::start_mcp_server()'
start_mcp_server()
} # }
```
