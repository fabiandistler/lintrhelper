# lintrhelper 0.1.0.9000 (development)

## Bug fixes

* `forbid_symbols()`, `create_function_call_linter()`, `require_naming_pattern()`, and `require_function_naming_pattern()` now correctly return multiple lints when several locations match. Previously the per-node messages were assembled with `unlist(recursive = FALSE)`, which broke the `lints` object structure and silently dropped all results.
* `create_assignment_linter()` (and therefore `enforce_assignment_operator()`) now matches `=` assignments. The previous XPath assumed the assignment was always wrapped in `<expr>`, but `=` lives under `<expr_or_assign_or_help>`.
* `require_naming_pattern()` and `require_function_naming_pattern()` honour the `invert` argument again — both branches of the conditional were returning the same value.
* `require_function_arguments()` now inspects the full call expression (the grandparent of `SYMBOL_FUNCTION_CALL`) when looking for named arguments, so calls that do supply the required argument no longer trigger.
* `limit_line_length()` is now scoped to the file level (it used to run once per expression and rely on a non-existent `$lines` field).

## Tooling

* Added GitHub Actions workflow for `R CMD check` across macOS, Windows, and Ubuntu (R devel/release/oldrel-1).
* Added pkgdown workflow that deploys the documentation site to <https://fabiandistler.github.io/lintrhelper/>.
* Added `_pkgdown.yml` configuration grouping the reference index by user-facing categories.
* Generated `man/` pages from existing roxygen comments (`R CMD check` previously failed because no Rd files were committed).
* `DESCRIPTION`: real author, declared `utils` as an import (for `capture.output`), dropped the unused `rlang` import, dropped `LazyData` (no `data/` directory).

# lintrhelper 0.1.0

## Initial Release

### High-Level Functions (No XPath Required!)

The main feature of lintrhelper is that **you don't need to know XPath** to create linters:

* Added `forbid_symbols()` - Ban specific variable names
* Added `forbid_functions()` - Ban function calls with auto-generated alternative suggestions
* Added `require_naming_pattern()` - Enforce naming conventions with regex (no XPath!)
* Added `require_function_naming_pattern()` - Enforce function naming with regex
* Added `enforce_assignment_operator()` - Prefer `<-`, `=`, or `->`
* Added `require_function_arguments()` - Ensure functions are called with specific arguments
* Added `limit_line_length()` - Enforce maximum line length

### Testing Utilities

* Added `test_linter()` for simplified linter testing
* Added `quick_test()` for rapid development

### Advanced (XPath-Based) Functions

For users who want more control:

* Added `create_simple_linter()` for XPath-based linter creation
* Added `create_function_call_linter()` for XPath-based function call linters
* Added `create_assignment_linter()` for XPath-based assignment operator linters
* Added `linter_template()` for code templates
* Added `xpath_patterns()` for XPath reference

### Ready-to-Use Examples

* Included example linters: `no_t_f_linter()`, `no_attach_linter()`, `prefer_arrow_assignment_linter()`, `no_sapply_linter()`, `no_one_length_linter()`

### Documentation

* Added comprehensive README emphasizing no-XPath approach
* Added "Getting Started" vignette
* Added "Creating Linters Without XPath" vignette with extensive examples
* Added unit tests for all functions
