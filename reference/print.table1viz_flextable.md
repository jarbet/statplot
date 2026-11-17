# Print a table1viz() flextable

In an interactive session, shows the table with its graphics (as SVG) in
the viewer; otherwise falls back to flextable's print method.

## Usage

``` r
# S3 method for class 'table1viz_flextable'
print(x, ...)
```

## Arguments

- x:

  A `table1viz_flextable` from
  [`table1viz()`](https://github.com/jarbet/statplot/reference/table1viz.md).

- ...:

  Passed to flextable's [`print()`](https://rdrr.io/r/base/print.html)
  method.

## Value

`x`, invisibly.
