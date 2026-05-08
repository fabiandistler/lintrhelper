# List Common XPath Patterns for R Code

Provides a reference of useful XPath patterns for linting R code.

## Usage

``` r
xpath_patterns(pattern = NULL)
```

## Arguments

- pattern:

  Optional. If specified, shows only the requested pattern. Options:
  "symbols", "functions", "operators", "literals", "comments".

## Value

Prints XPath patterns to console.

## Examples

``` r
# See all patterns
xpath_patterns()
#> 
#> === SYMBOLS ===
#> All symbols: //SYMBOL
#> Specific symbol: //SYMBOL[text() = 'my_var']
#> Symbols matching pattern: //SYMBOL[contains(text(), 'temp')] 
#> 
#> === FUNCTIONS ===
#> Function calls: //SYMBOL_FUNCTION_CALL
#> Specific function: //SYMBOL_FUNCTION_CALL[text() = 'mean']
#> Function definitions: //FUNCTION 
#> 
#> === OPERATORS ===
#> All assignments: //LEFT_ASSIGN | //RIGHT_ASSIGN | //EQ_ASSIGN
#> Left assign (<-): //LEFT_ASSIGN
#> Equals assign (=): //EQ_ASSIGN
#> Arithmetic ops: //OP-PLUS | //OP-MINUS | //OP-TIMES | //OP-DIVIDE 
#> 
#> === LITERALS ===
#> Numeric constants: //NUM_CONST
#> String constants: //STR_CONST
#> NULL values: //NULL_CONST 
#> 
#> === COMMENTS ===
#> All comments: //COMMENT 
#> 
#> === COMMON_PATTERNS ===
#> Arguments in function call: //SYMBOL_FUNCTION_CALL/following-sibling::expr
#> Assignment target: //LEFT_ASSIGN/preceding-sibling::expr
#> Assignment value: //LEFT_ASSIGN/following-sibling::expr
#> If conditions: //IF/following-sibling::expr[1] 

# See just function-related patterns
xpath_patterns("functions")
#> === FUNCTIONS ===
#> Function calls: //SYMBOL_FUNCTION_CALL
#> Specific function: //SYMBOL_FUNCTION_CALL[text() = 'mean']
#> Function definitions: //FUNCTION 
```
