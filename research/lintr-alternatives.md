# Linting Alternatives to lintr (2026 Ecosystem Landscape)

Research for [lintrhelper issue #5](https://github.com/fabiandistler/lintrhelper/issues/5) (wayfinder research ticket).
All claims below are sourced from primary sources (project repos, official docs, Posit/rOpenSci/R Consortium blog posts). Facts current as of 2026-08-17.

## Summary

The 2026 R linting/formatting landscape has shifted decisively toward Rust-based CLIs:

- **air** (Posit) is the new standard *formatter*, not a linter — it has zero lint rules.
- **lintr** remains the dominant *linter*, but its XPath-based rule authoring is unchanged.
- **Jarl** (Etienne Bacher, R Consortium-funded) is the emerging Rust *linter* with auto-fixes, built directly on air's tree-sitter infrastructure, porting lintr rules one by one.
- **flir** (Etienne Bacher, formerly "flint") *already solves* rule authoring without XPath (declarative YAML) and rule distribution across teams (`from-package` mechanism). Its author has stopped adding new built-in rules in favor of Jarl.
- **Posit `{lint}` does not exist.** No `posit-dev/lint` or `r-lib/lint` repository exists; the CRAN package `lint` was archived in 2016 and is unrelated. Posit's linting story is lintr.

## 1. air (posit-dev/air) — formatter + language server, no linting

Source: [GitHub repo](https://github.com/posit-dev/air), [announcement blog](https://opensource.posit.co/blog/2025-02-21_air/), [Air 0.10.0 blog](https://opensource.posit.co/blog/2026-06-26_air-0-10-0/), [docs](https://posit-dev.github.io/air/)

- Rust formatter and language server by Davis Vaughan & Lionel Henry (Posit), built on Biome formatting infrastructure and a tree-sitter R grammar. Current release: **0.11.0** (2026); ~442 stars, very active (660 commits).
- **Speed**: formats dplyr's ~150 files instantly, all ~900 R files of base R in under 2 seconds. Enables format-on-save and whole-project formatting in CI.
- **No R runtime required**: distributed as pre-compiled binaries; install via installer scripts, uv (`air-formatter` on PyPI), Homebrew, conda-forge (pixi/mise).
- **Editor integration**: bundled in Positron, native support in RStudio (from RStudio 2026.06.0), plus VS Code, Zed, Helix, Neovim, Emacs extensions.
- **CI/CLI**: `air format . --check` mode, official GitHub Action (`posit-dev/setup-air`), pre-commit hook (`posit-dev/air-pre-commit`), stdin support, shell completions.
- **Configuration**: `air.toml` (TOML) — line width, indent, assignment style (`assignment-style = "arrow"/"equal"/"preserve"`, added 0.10.0), etc. Opinionated: implements the tidyverse style guide with some deviations.
- **Linting**: none. Air has no lint rules and no lint command; the maintainers defer linting to other tools (formatting-adjacent lint concerns, e.g. indentation/whitespace, are deliberately considered "the formatter's job" by the Jarl author as well).
- Known friction with lintr: lintr keeps an `air-compatibility` label (e.g. [r-lib/lintr#2703](https://github.com/r-lib/lintr/issues/2703), [#2960](https://github.com/r-lib/lintr/issues/2960) — `indentation_linter()` flags air's hanging-indent output) and editor diagnostics conflicts ([posit-dev/air#338](https://github.com/posit-dev/air/issues/338)).

## 2. Posit `{lint}` — does not exist

- No `posit-dev/lint` and no `r-lib/lint` GitHub repository (`gh search repos` over both orgs returns only `lintr`).
- The CRAN package `lint` ([cran.r-project.org/package=lint](https://cran.r-project.org/web/packages/lint/index.html)) was archived in 2016 at the maintainer's request; unrelated to Posit.
- Posit's linting tool is lintr itself (r-lib, Jim Hester → Michael Chirico). The question's premise appears to be based on a rumor or confusion with Jarl/air.

## 3. lintr (r-lib) — the incumbent linter

Source: [GitHub](https://github.com/r-lib/lintr), [CRAN](https://cran.r-project.org/web/packages/lintr/), [vignette](https://lintr.r-lib.org/articles/lintr.html), [JOSS paper](https://doi.org/10.21105/joss.07240)

- **v3.4.0** on CRAN (2026-07-16); ~1291 stars, 300+ open issues, stable lifecycle; JOSS paper; used by Super-Linter and MegaLinter for R.
- ~110 linters, organized by tags (`linters_with_tags()`, `all_linters()`); defaults follow the tidyverse style guide.
- Config via `.lintr` file (DCF format, values evaluated as R code) or experimental `.lintr.R`; also `.github/linters/` for Super-Linter compatibility.
- **Rule authoring requires XPath**: custom linters are R functions running XPath queries against the xml2 parse tree (see `vignette("creating_linters")`). This is the pain point lintrhelper targets.
- **Rule distribution**: only via config files (`.lintr`/`.lintr.R`); external packages can contribute linters to `all_linters(packages = ...)`, but there is no package-level distribution mechanism for custom rule sets.
- **No auto-fixes** — lintr only reports. This is its biggest functional gap vs. Jarl/flir.
- Performance degrades on large codebases (Jarl benchmark: 18.5s vs 0.131s on dplyr).

## 4. Jarl — the emerging Rust linter (built on air)

Source: [jarl.etiennebacher.com](https://jarl.etiennebacher.com/), [GitHub](https://github.com/etiennebacher/jarl), [R Consortium announcement](https://r-consortium.org/posts/jarl-just-another-r-linter/)

- "Just Another R Linter" by Etienne Bacher, **stable v0.5.0** (2026), Rust CLI built on air's tree-sitter infrastructure; R Consortium-funded.
- **55+ rules** (ported from lintr, tracked in [issue #8](https://github.com/etiennebacher/jarl/issues/8) "Equivalence with lintr"), including code-smell rules lintr lacks (`unused_function`, `duplicated_function_definition`, `unreachable_code`, `outer_negation`, ...).
- **Automatic fixes**: `jarl check . --fix` (safe fixes; `--unsafe-fixes` opt-in). Formatting rules are explicitly out of scope ("that's the formatter's job" — delegated to air).
- **No R required**, single binary; IDE extensions for VS Code, Positron, Zed, Helix, Neovim; `setup-jarl` GitHub Action; pre-commit support; `jarl.toml` config; JSON output.
- Benchmark (20 rules, dplyr ~25k lines): **Jarl 0.131s vs flir 4.5s vs lintr 18.5s** (9s cached).
- Rule authoring: Rust code (contributor tutorial), not user-facing. Semantic analysis (e.g. object usage) not yet possible.
- This is the most credible long-term threat to any R linter, including a hypothetical lintrhelper.

## 5. flir — declarative YAML rules + fixes + team distribution (already solves the lintrhelper niche)

Source: [flir.etiennebacher.com](https://flir.etiennebacher.com/), [GitHub](https://github.com/etiennebacher/flir), [sharing-rules vignette](https://flir.etiennebacher.com/articles/sharing_rules.html), CRAN (v0.6.0)

- R package (formerly "flint", renamed to avoid CRAN namespace clash with `flint`), uses **ast-grep** (tree-sitter) for pattern search & replace.
- **Rule authoring without XPath**: rules are declarative YAML files — `pattern:` (ast-grep pattern language), optional `fix:` replacement, `message:`, `severity:`; `export_new_rule()` scaffolds templates.
- **Auto-fixes**: `fix()`/`fix_dir()`/`fix_package()` rewrite code automatically; positioned as "detect and rewrite code patterns" (refactoring tool as much as linter).
- **Rule distribution across teams — solved**: packages can ship rules in `inst/flir/rules/`; users import them via `from-package:` in `flir/config.yml` (vignette: "Sharing rules across packages"; example: tidypolars ships rules to replace its deprecated `describe()`/`describe_plan()`). Built-in rules live in `flir/rules/builtin`, custom ones in `flir/rules/custom`.
- **Status**: the author states flir will receive **no new built-in rules** — development effort moved to Jarl. flir remains useful as a custom-pattern refactoring tool.
- Benchmark: 4.5s on dplyr vs Jarl's 0.131s.

## 6. styler — legacy formatter (R-based)

Source: [GitHub](https://github.com/r-lib/styler), air README acknowledgements

- R package by Lorenz Walthert & Kirill Müller; for years the primary implementation of the tidyverse style guide.
- air explicitly acknowledges styler as its inspiration. styler still works and remains relevant where R-only tooling is preferred, but is slow and R-dependent; air is positioned as its successor for formatting.
- Not a linter.

## 7. Peripheral players

- **flint (CRAN)**: unrelated newer package that forced the flir→flint rename; not a lintr alternative.
- **Panache** (jolars/panache): LSP/formatter/linter for Quarto/Pandoc/R Markdown; delegates R formatting to air and R linting to jarl — a consumer of the ecosystem, not a competitor.
- **Super-Linter / MegaLinter**: multi-language CI linters that invoke lintr for R files.
- **usethis**: ships GitHub Action templates (`use_github_action("lint")`, `use_github_action("lint-project")`) wrapping lintr.

## Answers to the ticket's questions

1. **Alternatives/complements in 2026**: air (formatting), Jarl (linting + fixes), flir (custom pattern linting/fixing), styler (legacy formatting), Panache (Quarto/Rmd). A Posit `{lint}` package does not exist.
2. **Better than lintr**: Jarl (speed, auto-fixes, no R runtime, IDE UX), air (speed, no R runtime), flir (auto-fixes, declarative rules), all three have better CI/editor ergonomics than lintr's R-package-based workflow.
3. **Rule authoring without XPath**: solved by flir (declarative YAML patterns). Jarl requires Rust; lintr requires XPath.
4. **Rule distribution across teams**: solved by flir (`from-package` in `flir/config.yml` + `inst/flir/rules`). lintr has no equivalent; Jarl does not yet (rules are compiled into the binary).
5. **Competitive landscape for lintrhelper**: the exact niche lintrhelper would occupy — easy rule authoring plus team-level rule distribution on top of lintr's rule engine — is already occupied by flir for the declarative/refactoring angle, and is being pulled toward Jarl (Rust) for the performance/fixes angle. lintr itself remains dominant but static in its authoring model. A lintrhelper-style package would compete with (a) flir's YAML approach for custom rules, (b) Jarl for any new tooling investment, while adding value only in lintr-specific integration (e.g. distributing lintr-compatible rule sets, XPath-free authoring for lintr).