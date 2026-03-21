# Stacked percent bar chart for two categorical variables

Create a stacked percent bar chart of an x categorical variable broken
down by a second categorical variable. The plot includes within-group
percentages, group sample sizes above each bar, and (optionally) a
subtitle showing Cramer's V and the test p-value.

## Usage

``` r
plot_2_categorical_vars(
  d,
  xvar,
  yvar,
  xvar_label = NULL,
  yvar_label = NULL,
  yvar_colors = NULL,
  title = NULL,
  title_nchar_wrap = 30,
  show_effect_size = TRUE,
  n_pct_size = 3.5,
  inside_bar_stats = c("pct", "n", "n_and_pct", "none"),
  pct_digits = 0,
  plot_horizontal = FALSE,
  flip = FALSE,
  xaxis_labels_nchar_wrap = 20,
  y_max = 110,
  n_ypos = y_max - 5
)
```

## Arguments

- d:

  A data.frame containing the variables.

- xvar:

  Character scalar, name of the categorical x variable (column in d).

- yvar:

  Character scalar, name of the categorical y variable (column in d).

- xvar_label:

  Optional character scalar to use for the x axis label.

- yvar_label:

  Optional character scalar to use for the legend label.

- yvar_colors:

  Optional character vector of colours to use for the y-variable levels.
  Must be length equal to `nlevels(d[[yvar]])`. Provide colour names
  (e.g. "red") or hex codes (e.g. "#FF0000"). If NULL (default)
  ggplot2's default palette is used.

- title:

  Optional character scalar for the plot title.

- title_nchar_wrap:

  Integer scalar. Maximum number of characters per line of title.

- show_effect_size:

  Logical; if TRUE the subtitle will include Cramer's V and p-value.

- n_pct_size:

  Numeric scalar. Point size used for the percent labels inside the
  stacked bars and for the group N labels above bars. Must be a single
  positive numeric value.

- inside_bar_stats:

  Character scalar controlling what statistics are printed inside the
  stacked bars. One of `"pct"` (default; shows within-group percentage),
  `"n"` (shows count only), `"n_and_pct"` (shows count with percentage
  below it), or `"none"` (no labels inside bars).

- pct_digits:

  Integer scalar (default 0). Number of decimal places to show for the
  within-group percent labels (e.g. 0 =\> "12%", 1 =\> "12.3%"). Must be
  a single non-negative numeric value.

- plot_horizontal:

  Logical scalar. Deprecated/alias for `flip`. If TRUE the plot will be
  shown with horizontal bars. Prefer using `flip`.

- flip:

  Logical scalar (default FALSE). If TRUE the plot is flipped to show
  horizontal bars.

- xaxis_labels_nchar_wrap:

  Integer scalar. Maximum number of characters per line for x-axis group
  labels. Longer labels will be wrapped to multiple lines.

- y_max:

  Numeric scalar. Maximum y-axis limit for the plot.

- n_ypos:

  Numeric scalar. y-axis position for the group N labels.

## Value

A list of the ggplot2 object, Cramer's V effect size, p-value.

## Examples

``` r
data(mtcars)
mtcars$cyl <- factor(mtcars$cyl)
mtcars$gear <- factor(mtcars$gear)
p <- plot_2_categorical_vars(
  d = mtcars,
  xvar = "cyl",
  yvar = "gear",
  xvar_label = 'Cylinders',
  yvar_label = 'Gears'
)
p$ggplot

```
