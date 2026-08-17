# Context

## Glossary

- **Linter rule**: a single automated check on R source code (e.g. “no
  `T`/`F` literals”).
- **lintr**: the de-facto R linting package that this repo wraps and
  extends.
- **Rule authoring**: writing new linter rules without requiring
  XPath/XML knowledge — the package’s current core value.
- **Rule distribution**: bundling, versioning, and sharing a set of
  linter rules across a team so everyone (humans and agents) runs the
  same rules.
- **Coding agent**: an AI coding assistant (Claude Code, opencode) that
  consumes project conventions, including linter rules, via
  CLAUDE.md/AGENTS.md and similar mechanisms.
- **Agent integration**: the mechanisms by which coding agents receive
  and obey linter rules.
- **MCP server**: an MCP (Model Context Protocol) server exposing lintr
  lint results and rule metadata as tools to coding agents; the chosen
  v0.2 differentiation wedge.
- **Agent config export**: generating instruction-file entries
  (AGENTS.md/CLAUDE.md) from a rule set; advisory, deliberately deferred
  beyond v0.2.
