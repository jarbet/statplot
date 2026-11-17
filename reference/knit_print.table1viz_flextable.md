# Knit a table1viz() flextable

Draws the graphics of a
[`table1viz()`](https://github.com/jarbet/statplot/reference/table1viz.md)
flextable (`output = "flextable"`): as native Word vector shapes when
knitting to `.docx`, as SVG when knitting to HTML. Other formats show
the table without graphics.

## Usage

``` r
# S3 method for class 'table1viz_flextable'
knit_print(x, ...)
```

## Arguments

- x:

  A `table1viz_flextable` from
  [`table1viz()`](https://github.com/jarbet/statplot/reference/table1viz.md).

- ...:

  Passed to flextable's `knit_print()` method.

## Value

A `knit_asis` object.
