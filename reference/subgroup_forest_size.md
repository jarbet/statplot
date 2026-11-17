# Figure size for a table-style subgroup forest plot

The width and height, in inches, to draw
[`plot_subgroup_forest()`](https://github.com/jarbet/statplot/reference/plot_subgroup_forest.md)
at: the width fits its text columns and forest panel, and the height
gives each row `row_height`. In Quarto, compute it in an earlier chunk
and pass it as `fig-width: !expr size[["width"]]` and
`fig-height: !expr size[["height"]]` (chunk options are evaluated before
the chunk's own code runs).

## Usage

``` r
subgroup_forest_size(
  data,
  effect_size,
  ci_low,
  ci_high,
  ...,
  text_size = 8,
  row_height = 0.115 * text_size/7,
  axis_height = 0.45 * text_size/7
)
```

## Arguments

- data, effect_size, ci_low, ci_high:

  As for
  [`plot_subgroup_forest()`](https://github.com/jarbet/statplot/reference/plot_subgroup_forest.md).

- ...:

  Other arguments passed to
  [`plot_subgroup_forest()`](https://github.com/jarbet/statplot/reference/plot_subgroup_forest.md).
  Those that change the layout (`subgroup`, `level`, `group_col`,
  `n_col`, `events_col`, `pvalue_col`, `ref_col`, `estimate_digits`,
  `ref_label`, `labels`, `font_family`, `forest_width`, `gap_level`,
  `gap_subgroup`) must match; the others are ignored, so the same
  arguments can be passed to both functions.

- text_size:

  Numeric. Text size, in points, as for
  [`plot_subgroup_forest()`](https://github.com/jarbet/statplot/reference/plot_subgroup_forest.md).
  Default `8`.

- row_height:

  Numeric. Height of one row, in inches. Default
  `0.115 * text_size / 7`, scaling with the text.

- axis_height:

  Numeric. Height of the estimate axis and the plot margins, in inches.
  Default `0.45 * text_size / 7`, scaling with the text.

## Value

Named numeric vector: `width` and `height`, in inches.

## Examples

``` r
df <- data.frame(
    subgroup = c("Overall", "Age", "Age"),
    level = c("", "<65", "65+"),
    hr = c(0.80, 0.75, 0.85),
    lcl = c(0.70, 0.60, 0.72),
    ucl = c(0.92, 0.94, 1.01)
)
size <- subgroup_forest_size(df, "hr", "lcl", "ucl")
p <- plot_subgroup_forest(df, "hr", "lcl", "ucl")
# ggplot2::ggsave("forest.png", p, width = size[["width"]],
#     height = size[["height"]])
```
