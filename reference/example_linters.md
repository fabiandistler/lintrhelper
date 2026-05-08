# Example Linters

A collection of example linters that demonstrate common patterns. These
can be used as-is or as templates for your own linters.

## Usage

``` r
no_t_f_linter()

no_attach_linter()

prefer_arrow_assignment_linter()

no_one_length_linter()

no_sapply_linter()
```

## Functions

- `no_t_f_linter()`: Warns against using T/F instead of TRUE/FALSE

- `no_attach_linter()`: Warns against using attach()

- `prefer_arrow_assignment_linter()`: Suggests using \<- instead of =
  for assignment

- `no_one_length_linter()`: Warns against 1:length(x) pattern

- `no_sapply_linter()`: Style guide: warns against sapply
