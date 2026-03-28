# Dot-and-whisker plot of estimates with confidence intervals

Create a horizontal dot-and-whisker plot showing point estimates and
confidence intervals for labeled rows. Optionally offset points and use
different shapes or colors when a grouping column is supplied.
Optionally append a p-value barplot to the right via patchwork.

## Usage

``` r
plot_dot_whiskers(
  data,
  group_col = NULL,
  x = "est",
  xmin = "conf.low",
  xmax = "conf.high",
  label_col = "cell_line",
  dodge_width = 0.3,
  style = c("color", "shape"),
  sep_linetype = "solid",
  sep_linewidth = 0.4,
  sep_color = "black",
  vline_xintercept = 0,
  vline_linetype = "dashed",
  vline_color = "black",
  point_shapes = c(21, 24, 22, 25, 23),
  pvalue_col = NULL,
  pvalue_plot_width = 0.3,
  pvalue_plot_margin = c(5.5, 12, 5.5, 0),
  ...
)
```

## Arguments

- data:

  A data.frame or tibble containing the columns referenced by `x`,
  `xmin`, `xmax`, and `label_col`.

- group_col:

  Optional string name of a **factor** grouping column. When provided,
  points are offset by group for visibility. Factor levels control the
  ordering and direction of the dodge offset: the first level is plotted
  at the top of each row.

- x:

  String name of the estimate column. Default `"est"`.

- xmin:

  String name of the lower-interval column. Default `"conf.low"`.

- xmax:

  String name of the upper-interval column. Default `"conf.high"`.

- label_col:

  String name of the label/row identifier column. Must be a **factor**;
  factor levels control the top-to-bottom row ordering (first level
  appears at the top). Default `"cell_line"`.

- dodge_width:

  Numeric. Total vertical spread across groups. Default `0.3`.

- style:

  String. Controls group encoding when `group_col` is supplied.
  `"color"` (default) uses different colors per group and draws
  horizontal separator lines between rows. `"shape"` uses the same color
  per `label_col` row and different point shapes per group.

- sep_linetype:

  Line type for row separator lines when `style = "color"`. Default
  `"solid"`.

- sep_linewidth:

  Line width for row separator lines when `style = "color"`. Default
  `0.4`.

- sep_color:

  Color for row separator lines when `style = "color"`. Default
  `"black"`.

- vline_xintercept:

  Numeric. Position of the vertical reference line. Default `0`.

- vline_linetype:

  Line type for the vertical reference line. Default `"dashed"`.

- vline_color:

  Color for the vertical reference line. Default `"black"`.

- point_shapes:

  Integer vector of point shapes to use when `style = "shape"`. Must
  have at least as many elements as there are levels in `group_col`.
  Defaults to `c(21, 24, 22, 25, 23)` (up to 5 groups). See
  [`graphics::points()`](https://rdrr.io/r/graphics/points.html) for
  shape codes.

- pvalue_col:

  Optional string name of a column in `data` containing one p-value per
  `label_col` row. When supplied, a
  [`plot_pvalue_barplot()`](https://github.com/jarbet/statplot/reference/plot_pvalue_barplot.md)
  is appended to the right using patchwork. The p-value used for each
  row is taken from the first occurrence within that `label_col` group,
  so the column must be constant within each row. Requires the patchwork
  package.

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
  cell_line = factor(c("A", "A", "B", "B", "C", "C"), levels = c('A', 'B', 'C')),
  est       = c(0.2, 0.35, -0.1, 0.05, 0.5, 0.3),
  conf.low  = c(0.0, 0.10, -0.3, -0.10, 0.2, 0.1),
  conf.high = c(0.4, 0.60,  0.1,  0.20, 0.8, 0.5),
  group     = factor(c("g1", "g2", "g1", "g2", "g1", "g2"), levels = c("g1", "g2")),
  pvalue    = c(0.01, 0.01, 0.4, 0.4, 0.001, 0.001)
)

# color style: groups distinguished by color, separator lines between rows
plot_dot_whiskers(df, group_col = "group", style = "color")


# shape style: groups distinguished by point shape, rows colored by label
plot_dot_whiskers(df, group_col = "group", style = "shape")


# with p-value barplot appended on the right
plot_dot_whiskers(
  df,
  group_col  = "group",
  style      = "color",
  pvalue_col = "pvalue",
  mlog10_transform_pvalue = TRUE
)
#> Scale for y is already present.
#> Adding another scale for y, which will replace the existing scale.


# no grouping, with p-value barplot appended on the right
df_single <- data.frame(
  cell_line = factor(c("A", "B", "C"), levels = c("A", "B", "C")),
  est       = c(0.2, -0.1, 0.5),
  conf.low  = c(0.0, -0.3, 0.2),
  conf.high = c(0.4,  0.1, 0.8),
  pvalue    = c(0.01, 0.4, 0.001)
)

plot_dot_whiskers(
  df_single,
  pvalue_col = "pvalue",
  mlog10_transform_pvalue = TRUE
)
#> Scale for y is already present.
#> Adding another scale for y, which will replace the existing scale.
```
