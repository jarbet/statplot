# Dot-and-whisker plot of estimates with confidence intervals

Create a horizontal dot-and-whisker plot showing point estimates and
confidence intervals for labeled rows. Optionally offset points and use
different shapes or colors when a grouping column is supplied.
Optionally append a p-value barplot to the right via patchwork.

## Usage

``` r
plot_confidence_intervals(
  data,
  effect_size,
  ci_low,
  ci_high,
  id,
  group_col = NULL,
  dodge_width = 0.3,
  color_col = NULL,
  color_values = NULL,
  show_separators = TRUE,
  shape_col = NULL,
  sep_linetype = "solid",
  sep_linewidth = 0.4,
  sep_color = "black",
  vline_xintercept = 0,
  vline_linetype = "dashed",
  vline_color = "black",
  point_shapes = c(21, 24, 22, 25, 23),
  pvalue_col = NULL,
  combine_pvalue_method = c("fisher", "CMC", "MCM", "cauchy", "minp_bonferroni"),
  pvalue_plot_width = 0.3,
  pvalue_plot_margin = c(5.5, 12, 5.5, 0),
  ...
)
```

## Arguments

- data:

  A data.frame or tibble containing the columns referenced by
  `effect_size`, `ci_low`, `ci_high`, and `id`.

- effect_size:

  String name of the effect size/estimate column.

- ci_low:

  String name of the lower confidence interval column.

- ci_high:

  String name of the upper confidence interval column.

- id:

  String name of the label/row identifier column. Must be a **factor**;
  factor levels control the top-to-bottom row ordering (first level
  appears at the top).

- group_col:

  Optional string name of a **factor** grouping column. When provided,
  points are offset by group for visibility. Factor levels control the
  ordering and direction of the dodge offset: the first level is plotted
  at the top of each row.

- dodge_width:

  Numeric. Total vertical spread across groups. Default `0.3`.

- color_col:

  Optional string name of a column to use for coloring segments and
  points. When `NULL` (default), no colors are applied. Common choices
  are `group_col` to distinguish groups or `id` to color by row. Must be
  a factor or character column.

- color_values:

  Optional named character vector specifying custom colors for the
  levels in `color_col`. Names should match the factor levels and values
  should be valid R color names or hex codes (e.g.,
  `c(g1 = "#FF0000", g2 = "#0000FF")`). If `NULL` (default), ggplot2's
  default color scale is used. Ignored if `color_col` is `NULL`.

- show_separators:

  Logical. Whether to draw horizontal separator lines between rows when
  `group_col` is supplied. Default `TRUE`.

- shape_col:

  Optional string name of a column to use for point shapes. When `NULL`
  (default), no shape encoding is applied. Can be a factor or character
  column; character columns are coerced to factor with sorted level
  ordering for stable shape assignment. Can be used independently or in
  combination with `color_col`.

- sep_linetype:

  Line type for row separator lines. Default `"solid"`.

- sep_linewidth:

  Line width for row separator lines. Default `0.4`.

- sep_color:

  Color for row separator lines. Default `"black"`.

- vline_xintercept:

  Numeric. Position of the vertical reference line. Default `0`.

- vline_linetype:

  Line type for the vertical reference line. Default `"dashed"`.

- vline_color:

  Color for the vertical reference line. Default `"black"`.

- point_shapes:

  Integer vector of point shapes to use when `shape_col` is specified.
  Must have at least as many elements as there are levels in
  `shape_col`. Defaults to `c(21, 24, 22, 25, 23)` (up to 5 groups). See
  [`graphics::points()`](https://rdrr.io/r/graphics/points.html) for
  shape codes.

- pvalue_col:

  Optional string name of a column in `data` containing p-values (one
  per row). When supplied, a
  [`plot_pvalue_barplot()`](https://github.com/jarbet/statplot/reference/plot_pvalue_barplot.md)
  is appended to the right using patchwork. When `group_col` is
  specified, p-values are combined across groups for each `id` level
  using the method specified by `combine_pvalue_method`. When
  `group_col` is `NULL`, individual p-values are displayed directly.
  Requires the patchwork package.

- combine_pvalue_method:

  Character; method for combining p-values when `group_col` is supplied.
  One of: "fisher" (default), "CMC", "MCM", "cauchy", "minp_bonferroni".
  See
  [`combine_pvalues()`](https://github.com/jarbet/statplot/reference/combine_pvalues.md)
  for details. Ignored if `group_col` is `NULL`.

- pvalue_plot_width:

  Relative width of the p-value panel passed to
  [`patchwork::wrap_plots()`](https://patchwork.data-imaginist.com/reference/wrap_plots.html)
  `widths`. Default `0.3`.

- pvalue_plot_margin:

  Numeric vector of length 4 giving the p-value panel plot margin in
  points: `c(top, right, bottom, left)`. Default `c(5.5, 12, 5.5, 0)`
  which increases right margin so x-axis labels are not clipped when the
  panel is appended to the right.

- ...:

  Additional arguments passed to
  [`plot_pvalue_barplot()`](https://github.com/jarbet/statplot/reference/plot_pvalue_barplot.md)
  when `pvalue_col` is supplied.

## Value

A `ggplot2` object, or a `patchwork` object when `pvalue_col` is
supplied.

## Examples

``` r
df <- data.frame(
  cell_line = factor(c("A", "B", "C", "A", "B", "C"), levels = c('A', 'B', 'C')),
  est       = c(0.2, -0.1, 0.5, 0.35, 0.05, 0.3),
  conf.low  = c(0.0, -0.3, 0.2, 0.10, -0.10, 0.1),
  conf.high = c(0.4, 0.1, 0.8, 0.60, 0.20, 0.5),
  group     = factor(c("g1", "g1", "g1", "g2", "g2", "g2"), levels = c("g1", "g2")),
  pvalue    = c(0.01, 0.4, 0.001, 0.02, 0.3, 0.0001)
)

# Simplest: no grouping, no color
plot_confidence_intervals(
  df[1:3, ],
  effect_size = "est",
  ci_low = "conf.low",
  ci_high = "conf.high",
  id = "cell_line"
)


# With grouping: separate rows with color by default
plot_confidence_intervals(
  df,
  effect_size = "est",
  ci_low = "conf.low",
  ci_high = "conf.high",
  id = "cell_line",
  group_col = "group",
  color_col = "group"
)


# With grouping: different shapes for groups
plot_confidence_intervals(
  df,
  effect_size = "est",
  ci_low = "conf.low",
  ci_high = "conf.high",
  id = "cell_line",
  group_col = "group",
  shape_col = "group",
  show_separators = FALSE
)


# With grouping: color by group with custom colors
plot_confidence_intervals(
  df,
  effect_size = "est",
  ci_low = "conf.low",
  ci_high = "conf.high",
  id = "cell_line",
  group_col = "group",
  color_col = "group",
  color_values = c(g1 = "#E69F00", g2 = "#56B4E9")
)
#> Scale for colour is already present.
#> Adding another scale for colour, which will replace the existing scale.


# Color by label, shapes by group
plot_confidence_intervals(
  df,
  effect_size = "est",
  ci_low = "conf.low",
  ci_high = "conf.high",
  id = "cell_line",
  group_col = "group",
  shape_col = "group",
  color_col = "cell_line"
)


# Grouping with p-value barplot using Fisher's combined p-value
plot_confidence_intervals(
  df,
  effect_size = "est",
  ci_low = "conf.low",
  ci_high = "conf.high",
  id = "cell_line",
  group_col = "group",
  color_col = "group",
  pvalue_col = "pvalue",
  combine_pvalue_method = "fisher",
  mlog10_transform_pvalue = TRUE
)
#> Scale for y is already present.
#> Adding another scale for y, which will replace the existing scale.


# No grouping, but with p-value barplot on the right
plot_confidence_intervals(
  df[1:3, ],
  effect_size = "est",
  ci_low = "conf.low",
  ci_high = "conf.high",
  id = "cell_line",
  pvalue_col = "pvalue",
  mlog10_transform_pvalue = TRUE
)
#> Scale for y is already present.
#> Adding another scale for y, which will replace the existing scale.
```
