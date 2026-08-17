# lintr state in 2026: activity, flexibility, config, extensibility

Research for [issue #4](https://github.com/fabiandistler/lintrhelper/issues/4) (wayfinder ticket).
All claims cite primary sources: the lintr GitHub repo (`r-lib/lintr`), CRAN, the official
docs site (`lintr.r-lib.org`), the lintr NEWS, and the JOSS paper.

## Activity and roadmap

- **Latest release: v3.4.0, published 2026-07-16** on GitHub and CRAN (CRAN "Published: 2026-07-16",
  current CRAN version 3.4.0, depends on R >= 4.1.0). The repo was pushed to on 2026-08-17 (today),
  i.e. the project is under active development, not dormant.
- **Release cadence** (GitHub releases / CRAN): 3.0.0 (2022-06), 3.0.1 (2022-09), 3.0.2 (2022-10),
  3.1.0 (2023-07), 3.1.1 (2023-11), 3.1.2 (2024-03), 3.2.0 (2025-02), 3.3.0-1 (2025-11), 3.4.0 (2026-07).
  Roughly 1-2 releases/year in the last two years, with 3.4.0 representing a deliberate ~8-month gap
  after 3.3.0-1.
- **Commit velocity**: ~107 commits to the default branch in the 52 weeks before 2026-08-17
  (~2/week), including 8 commits in the week of the 3.4.0 release. Development between releases
  continues on `main` (NEWS has an "in development" section, e.g. new `expect_shape_linter()`
  and `lint()` gaining `filename`+`text` support for IDE/LSP integrations).
- **Scale**: ~1.3k stars, 202 forks, ~300 open issues; maintainer is Michael Chirico (took over
  CRAN maintainership from Jim Hester in 3.1.2, who had done 10+ years / 15 releases). The package
  has a JOSS paper since April 2025 (Hester et al., JOSS 10(108), 7240, DOI 10.21105/joss.07240).
- **Roadmap**: no formal public roadmap document. Development is issue-driven; the biggest visible
  workstreams are the Google-linter backlog (issue #884), Tidyverse Style Guide alignment, and an
  explicit `air-compatibility` label for tracking conflicts with Posit's Rust formatter "air"
  (see below).

## Writing custom linters: XPath is required, with helpers to soften it

- lintr parses R code into an XML parse tree (via `xmlparsedata`) and nearly all linters are XPath
  queries on that tree. The official vignette "Creating new linters" states: "Most linters in lintr
  are built using XPath because it is a powerful language for computation on the abstract syntax
  tree". So yes - non-trivial custom rule authoring requires XPath knowledge, and xml2 implements
  **XPath 1.0 only** (the vignette notes the missing XPath 2.0 features as a limitation).
- The docs explicitly discourage writing linters against raw source text (false positives in
  comments/strings; false negatives across lines), so there is no first-class "regex/text linter"
  escape hatch.
- However, the API has lowered the barrier considerably:
  - `make_linter_from_xpath()` (3.1.1) and `make_linter_from_function_xpath()`: turn a single XPath
    + a static message into a complete linter.
  - `xp_call_name()` (3.1.1), `get_r_string()`, `xml_nodes_to_lints()`, `is_lint_level()`: helper
    utilities exported specifically for custom linter authors.
  - `xml_find_function_calls()` (3.2.0): cached function-call node lookup that avoids writing the
    slow `//SYMBOL_FUNCTION_CALL[text() = 'myfun']` idiom.
  - `Linter()` factory + `linter_level` argument (3.2.0) replace the old manual
    `if (!is_lint_level(...)) return(list())` boilerplate.
- The vignette itself concedes the learning curve: "`expr` nodes in particular take some practice
  to get accustomed to - use the plentiful XPath-based linters in lintr as a guide".
  A simple "ban calls to function X" linter is a one-liner; anything structural (nesting,
  indentation, pipes) requires real XPath skill.

## Config system

- Canonical config is the `.lintr` file (DCF format) in the project root, with keys `linters`,
  `exclusions`, `exclude`, `exclude_start`, `exclude_end`, `encoding`, `error_on_lint`, etc.
  Since 3.1.1 an **experimental R-script config (`.lintr.R`)** is also supported (plain R
  assignments instead of DCF key-values), with `.lintr.R` taking precedence over `.lintr`.
- Config discovery is a 5-level search: `options("lintr.linter_file")` / `R_LINTR_LINTER_FILE`
  -> project-local file or `.github/linters/` child dir -> nearest parent dirs upward -> `$HOME`
  -> user config dir. R options override the file; function arguments override everything.
- Linter selection: `linters_with_defaults()` (the modern replacement for the removed
  `with_defaults()`), `linters_with_tags()` (tag-based selection, e.g. `"style"`, `"correctness"`,
  `"efficiency"`, `"package_development"`, `"pkg_testthat"`, `"default"`), and `all_linters()`
  for the full set.
- Scale: ~35 default linters (mapped to the Tidyverse Style Guide) and ~117 linters total
  (from `all_linters()` output in the official docs). Per-linter parameters are extensive,
  e.g. `assignment_linter(operator = "=")`, `brace_linter(function_bodies = "never")`.
- Exclusions are fine-grained: `# nolint`, `# nolint: <linter>.` (selective), `# nolint next`
  (3.1.1), `# nolint start/end` ranges, plus config-level `exclusions` by file/line/linter.

## Extensibility

- Custom linters are first-class: they can live in a `.lintr`/`.lintr.R` config, in a package,
  and the `available_linters()`/`available_tags()` metadatabase is explicitly documented as
  extensible to linters defined in other packages ("These databases can be extended to include
  custom linters in your package").
- The ecosystem confirms lintr is an engine others build on: reverse imports on CRAN include
  `goodpractice`, `languageserver`, `rhino`, `adaptalint`, `box.linters`, `scilintr`, `dupree`,
  `PaRe`, `gDRstyle`, `quartify`, `autoharp`, `artma`, `rjd3production`.
- Contribution path upstream exists (docs describe adding linters to `default_linters` and
  submitting PRs); the 3.x releases accepted many community linters (e.g. ~20 new linters in
  3.2.0, 4 in 3.3.0-1, one in-development `expect_shape_linter()`).

## What lintr 3.x offers that older (2.x/early 3.x) versions lacked

- 3.0.0 (2022): all linters converted to function factories (`Linter()`), selective
  `# nolint: <linter>.` exclusions, per-linter documentation pages, `available_linters()` tags
  metadatabase, pkgdown site, `use_lintr()`, 30+ Google linters, opening the door to
  non-tidyverse style guides (Bioconductor etc.).
- 3.1.x: `.lintr.R` R-script configs, `make_linter_from_xpath()`, `xp_call_name()`, `# nolint next`,
  Quarto (`.qmd`) support by default.
- 3.2.0: function-call node caching (`xml_find_function_calls`, 14-30% `lint_package()` speed-up),
  `linter_level`, `return_linter()` as default, ~20 new linters, `expect_no_lint()`.
- 3.3.0-1: `pipe_consistency_linter()` default changed to native pipe `|>` (aligning with the
  updated Tidyverse Style Guide), new linters (`coalesce_linter()`, `all_equal_linter()`,
  `download_file_linter()`, `list2df_linter()`), `interpret_extensions` for glue/rlang in
  `object_usage_linter()`, removal of the pre-3.0 API (`with_defaults()`, name-based linter
  passing), `lint()` settings-discovery tightened, JOSS paper.
- 3.4.0: removal of 6 deprecated linters, `assignment_linter(operator=)` consolidation,
  `namespace_linter()` now flags redundant `::`/`:::` on already-imported symbols,
  `backport_linter()` exhaustive back to R 3.0.0, Rmd `eval=FALSE` chunks skipped, internal
  refactor dropping `xml2::xml_ns()` (5-30% end-to-end speed-up in `lint_package()`),
  R >= 4.1.0 requirement.
- API discipline: deprecate -> fully deprecate -> remove over successive releases, so the public
  surface stays lean and the 3.x API is stable since 3.0.0.

## air (Posit's Rust formatter) - integration plans and conflicts

- air is a **formatter**, not a linter; there are no plans for lintr to adopt air as an engine or
  vice versa. The two tools are complementary and expected to be used together.
- lintr tracks compatibility explicitly: tracking issue #2703 "Check that there is no conflict
  between {lintr} and air" (opened 2025-01, now closed, superseded by an `air-compatibility`
  label; 1 open issue with that label as of 2026-08: #3053 `commas_linter()` spaces-before-commas
  vs air).
- Known friction points: `indentation_linter()` conflicts with air's default hanging-indent styles
  (issue #2960 asks for a fourth `hanging_indent_style` "to match air", still open); air can move
  `# nolint` comments to other lines, breaking exclusions (posit-dev/air #256; `# nolint next`
  from 3.1.1 is the documented workaround); air's stance is that "if you are using a formatter,
  then any indentation or line-width related details should only be handled by the formatter"
  (posit-dev/air #444) - i.e. users are expected to disable lintr's formatting linters when air
  formats the code.
- lintr itself keeps moving toward air-compatible defaults where style-guide-aligned (e.g.
  `pipe_consistency_linter()` -> `|>` in 3.3.0-1).

## Where "inflexible / outdated" is defensible vs not

**Not defensible:**

- "Outdated": 3.4.0 was released 2026-07-16, development is continuous (pushed today), the API is
  modern (function factories, tags, cli-based messages, R 4.1+), and there is an active
  maintenance/community pipeline (JOSS paper, many contributors).
- "Inflexible" as a blanket statement: the config system (`.lintr`/`.lintr.R`, options/env
  overrides, tag-based selection, per-linter parameters, fine-grained exclusions) and the
  extensible linter API (custom linters in configs or external packages, metadatabase) are
  genuinely flexible.

**Defensible:**

- Custom rule authoring does require XPath (XPath 1.0, at that), and the docs admit the learning
  curve. The helper functions lower the bar but do not remove it - this is the factual core of the
  "inflexible" criticism and precisely the gap this repo (lintrhelper, "rule authoring without
  requiring XPath/XML knowledge") is positioned to close.
- There is no formal roadmap document, and formatting-related linters can conflict with the
  increasingly popular air formatter (users must manually disable/align `indentation_linter`
  etc.), which is a real current friction point.
- Release cadence (~1-2/year) and a backlog of ~300 open issues mean feature requests (e.g. the
  air-matching `hanging_indent_style`) can take a long time.

## Sources

- GitHub releases: https://github.com/r-lib/lintr/releases (v3.4.0 notes, dates)
- CRAN: https://cran.r-project.org/web/packages/lintr/index.html (version 3.4.0, 2026-07-16,
  reverse imports)
- NEWS: https://raw.githubusercontent.com/r-lib/lintr/main/NEWS.md (3.x changelog, in-development section)
- Docs "Creating new linters": https://lintr.r-lib.org/articles/creating_linters.html
- Docs "Using lintr": https://lintr.r-lib.org/articles/lintr.html (config, defaults, exclusions)
- lintr 3.0.0 announcement: https://www.tidyverse.org/blog/2022/07/lintr-3-0-0/
- JOSS paper: https://doi.org/10.21105/joss.07240 (Hester et al., 2025)
- GitHub API: repo stats (stars/forks/pushed_at), commit activity, releases, issue search
- air compatibility: r-lib/lintr#2703, r-lib/lintr#2960, r-lib/lintr#3053,
  posit-dev/air#256, posit-dev/air#444
