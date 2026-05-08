# Changelog

## lintrhelper 0.1.0.9000 (development)

### Bug fixes

- [`forbid_symbols()`](https://fabiandistler.github.io/lintrhelper/reference/forbid_symbols.md),
  [`create_function_call_linter()`](https://fabiandistler.github.io/lintrhelper/reference/create_function_call_linter.md),
  [`require_naming_pattern()`](https://fabiandistler.github.io/lintrhelper/reference/require_naming_pattern.md),
  and
  [`require_function_naming_pattern()`](https://fabiandistler.github.io/lintrhelper/reference/require_function_naming_pattern.md)
  now correctly return multiple lints when several locations match.
  Previously the per-node messages were assembled with
  `unlist(recursive = FALSE)`, which broke the `lints` object structure
  and silently dropped all results.
- [`create_assignment_linter()`](https://fabiandistler.github.io/lintrhelper/reference/create_assignment_linter.md)
  (and therefore
  [`enforce_assignment_operator()`](https://fabiandistler.github.io/lintrhelper/reference/enforce_assignment_operator.md))
  now matches `=` assignments. The previous XPath assumed the assignment
  was always wrapped in `<expr>`, but `=` lives under
  `<expr_or_assign_or_help>`.
- [`require_naming_pattern()`](https://fabiandistler.github.io/lintrhelper/reference/require_naming_pattern.md)
  and
  [`require_function_naming_pattern()`](https://fabiandistler.github.io/lintrhelper/reference/require_function_naming_pattern.md)
  honour the `invert` argument again — both branches of the conditional
  were returning the same value.
- [`require_function_arguments()`](https://fabiandistler.github.io/lintrhelper/reference/require_function_arguments.md)
  now inspects the full call expression (the grandparent of
  `SYMBOL_FUNCTION_CALL`) when looking for named arguments, so calls
  that do supply the required argument no longer trigger.
- [`limit_line_length()`](https://fabiandistler.github.io/lintrhelper/reference/limit_line_length.md)
  is now scoped to the file level (it used to run once per expression
  and rely on a non-existent `$lines` field).

### Tooling

- Added GitHub Actions workflow for `R CMD check` across macOS, Windows,
  and Ubuntu (R devel/release/oldrel-1).
- Added pkgdown workflow that deploys the documentation site to
  <https://fabiandistler.github.io/lintrhelper/>.
- Added `_pkgdown.yml` configuration grouping the reference index by
  user-facing categories.
- Generated `man/` pages from existing roxygen comments (`R CMD check`
  previously failed because no Rd files were committed).
- `DESCRIPTION`: real author, declared `utils` as an import (for
  `capture.output`), dropped the unused `rlang` import, dropped
  `LazyData` (no `data/` directory).

## lintrhelper 0.1.0

### Initial Release

#### High-Level Functions (No XPath Required!)

The main feature of lintrhelper is that **you don’t need to know XPath**
to create linters:

- Added
  [`forbid_symbols()`](https://fabiandistler.github.io/lintrhelper/reference/forbid_symbols.md) -
  Ban specific variable names
- Added
  [`forbid_functions()`](https://fabiandistler.github.io/lintrhelper/reference/forbid_functions.md) -
  Ban function calls with auto-generated alternative suggestions
- Added
  [`require_naming_pattern()`](https://fabiandistler.github.io/lintrhelper/reference/require_naming_pattern.md) -
  Enforce naming conventions with regex (no XPath!)
- Added
  [`require_function_naming_pattern()`](https://fabiandistler.github.io/lintrhelper/reference/require_function_naming_pattern.md) -
  Enforce function naming with regex
- Added
  [`enforce_assignment_operator()`](https://fabiandistler.github.io/lintrhelper/reference/enforce_assignment_operator.md) -
  Prefer `<-`, `=`, or `->`
- Added
  [`require_function_arguments()`](https://fabiandistler.github.io/lintrhelper/reference/require_function_arguments.md) -
  Ensure functions are called with specific arguments
- Added
  [`limit_line_length()`](https://fabiandistler.github.io/lintrhelper/reference/limit_line_length.md) -
  Enforce maximum line length

#### Testing Utilities

- Added
  [`test_linter()`](https://fabiandistler.github.io/lintrhelper/reference/test_linter.md)
  for simplified linter testing
- Added
  [`quick_test()`](https://fabiandistler.github.io/lintrhelper/reference/quick_test.md)
  for rapid development

#### Advanced (XPath-Based) Functions

For users who want more control:

- Added
  [`create_simple_linter()`](https://fabiandistler.github.io/lintrhelper/reference/create_simple_linter.md)
  for XPath-based linter creation
- Added
  [`create_function_call_linter()`](https://fabiandistler.github.io/lintrhelper/reference/create_function_call_linter.md)
  for XPath-based function call linters
- Added
  [`create_assignment_linter()`](https://fabiandistler.github.io/lintrhelper/reference/create_assignment_linter.md)
  for XPath-based assignment operator linters
- Added
  [`linter_template()`](https://fabiandistler.github.io/lintrhelper/reference/linter_template.md)
  for code templates
- Added
  [`xpath_patterns()`](https://fabiandistler.github.io/lintrhelper/reference/xpath_patterns.md)
  for XPath reference

#### Ready-to-Use Examples

- Included example linters:
  [`no_t_f_linter()`](https://fabiandistler.github.io/lintrhelper/reference/example_linters.md),
  [`no_attach_linter()`](https://fabiandistler.github.io/lintrhelper/reference/example_linters.md),
  [`prefer_arrow_assignment_linter()`](https://fabiandistler.github.io/lintrhelper/reference/example_linters.md),
  [`no_sapply_linter()`](https://fabiandistler.github.io/lintrhelper/reference/example_linters.md),
  [`no_one_length_linter()`](https://fabiandistler.github.io/lintrhelper/reference/example_linters.md)

#### Documentation

- Added comprehensive README emphasizing no-XPath approach
- Added “Getting Started” vignette
- Added “Creating Linters Without XPath” vignette with extensive
  examples
- Added unit tests for all functions
