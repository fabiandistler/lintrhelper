## Agent skills

### Issue tracker

Issues and specs for this repo live as GitHub issues, managed with the `gh` CLI. See `docs/agents/issue-tracker.md`.

### Triage labels

Issues move through five triage labels: needs-triage, needs-info, ready-for-agent, ready-for-human, wontfix. See `docs/agents/triage-labels.md`.

### Domain docs

Single-context: `CONTEXT.md` at the repo root plus `docs/adr/` for decisions. See `docs/agents/domain.md`.

### Version bumps

Every PR bumps the package version, so `DESCRIPTION` and `NEWS.md` always say
which release the work in flight belongs to.

Write the PR's `NEWS.md` entries under the topmost heading — the previous PR
left it empty for you. Then, as the last change before pushing, run
`usethis::use_version()` with the component spelled out (it prompts when called
with no argument, which fails under `Rscript`):

```r
usethis::use_version("minor")  # or "patch"
```

That sets `Version:` in `DESCRIPTION` and opens a fresh empty heading on top of
`NEWS.md` for the next PR. Your entries stay under the heading you wrote them
under.

Pick the component from what the PR does. The package is pre-1.0, so `"minor"`
covers a breaking change or a new exported function, and `"patch"` covers a bug
fix or a docs-only change. Never edit `Version:` by hand — `use_version()` keeps
the `DESCRIPTION` and `NEWS.md` sides in step.
