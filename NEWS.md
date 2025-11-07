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
