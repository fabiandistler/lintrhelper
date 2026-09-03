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
- **Coding agent**: an AI coding assistant (Claude Code, opencode,
  Codex) that consumes project conventions, including linter rules, via
  CLAUDE.md/AGENTS.md and similar mechanisms, or by calling an MCP
  server.
- **Agent integration**: the mechanisms by which coding agents receive
  and obey linter rules.
- **MCP server**: an MCP (Model Context Protocol) server exposing lintr
  lint results, rule metadata, and rule documentation as tools to coding
  agents; the chosen v0.2 differentiation wedge.
- **Agent config export**: generating instruction-file entries
  (AGENTS.md/CLAUDE.md) from a rule set; advisory, deliberately deferred
  beyond v0.2.
- **Candidate rule**: a linter rule being authored, considered against
  the rules that already exist.
- **Rule corpus**: the set of existing rules a candidate rule is
  measured against — every linter lintr ships, not only the ones active
  by default.
- **Rule overlap**: the relation where a candidate rule and an existing
  rule flag some of the same code. Weaker than *subsumption*, where an
  existing rule flags everything the candidate flags and more. Overlap
  is the relation reported to the author, because a near-duplicate is
  worth surfacing even when neither rule contains the other.
