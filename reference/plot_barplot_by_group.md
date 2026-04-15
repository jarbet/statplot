# Bar plot comparing a numeric outcome between two conditions

Draws a grouped bar chart (one facet per group) comparing a numeric
outcome between exactly two conditions. Bar height = mean (or any effect
size); error bars span +/- 1 error unit (SE, SD, CI half-width, etc.). A
significance bracket with optional label is drawn above bars when the
corresponding p-value falls below `p_cutoff`.

## Usage

``` r
plot_barplot_by_group(
  df,
  group_col = "group",
  condition_col = "condition",
  mean_col = "mean",
  error_col = "se",
  error_direction = "up",
  p_col = "p_value",
  label_col = NULL,
  condition_order = NULL,
  p_cutoff = 0.05,
  show_text_groups = NULL,
  y_label = "Outcome",
  bar_colors = c("black", "grey70"),
  bar_width = 0.4,
  bar_gap = 0.6,
  bar_padding = 0.5,
  text_size = 3.5,
  bracket_offset = 1,
  bracket_gap = 0.04,
  bracket_text_gap = 1,
  strip_position = "top"
)
```

## Arguments

- df:

  Data frame in **long format** - one row per group x condition
  combination.

- group_col:

  Column name for independent groups shown as facets. Default `"group"`.

- condition_col:

  Column name for the two conditions to compare. Default `"condition"`.

- mean_col:

  Column name for bar heights (means or effect sizes). Default `"mean"`.

- error_col:

  Column name for error-bar half-widths (SE, SD, CI half-width, etc.).
  Default `"se"`.

- error_direction:

  Direction of error bars. `"both"` draws `mean +/- error`; `"up"` draws
  only the upper whisker (`mean` to `mean + error`). Default `"up"`.

- p_col:

  Column name for p-values. The value should be the same for both rows
  belonging to a group (i.e. repeated). Set to `NULL` to suppress
  brackets entirely. Default `"p_value"`.

- label_col:

  Optional column name supplying custom bracket label text (e.g.
  `"OR = 1.5 [1.1-2.0], p = 0.012"`). When `NULL` labels are
  auto-formatted as `"p = <value>"` using
  [`signif`](https://rdrr.io/r/base/Round.html).

- condition_order:

  Length-2 character vector setting the left-to-right display order of
  the two conditions. Defaults to the existing factor level order or
  alphabetical.

- p_cutoff:

  Significance threshold; brackets appear only when `p < p_cutoff`.
  Default `0.05`.

- show_text_groups:

  Optional character vector of group names for which text should be
  displayed above brackets. When specified, this argument overrides
  `p_cutoff`: only groups listed in `show_text_groups` will display text
  labels. When `NULL` (default), text display is determined solely by
  `p_cutoff`.

- y_label:

  Y-axis label. Default `"Outcome"`.

- bar_colors:

  Length-2 fill colour vector applied to the two conditions in the order
  given by `condition_order`. Default `c("black", "grey70")`.

- bar_width:

  Width of the bars passed to
  [`geom_col`](https://ggplot2.tidyverse.org/reference/geom_bar.html).
  Default `0.4`.

- bar_gap:

  Gap between the two bars within each facet, in x-axis units. Default
  `0.6` (matches the original discrete-axis spacing).

- bar_padding:

  White space added to the left and right of the bars within each facet
  panel, in x-axis units. Passed to
  `ggplot2::expansion(add = bar_padding)`. Default `0.5`.

- text_size:

  Size of bracket label text (ggplot2 `size` units). Default `3.5`.

- bracket_offset:

  Absolute distance (in data units) used as vertical spacing above bar
  tops for bracket placement. Default `1.0`.

- bracket_gap:

  Fraction of the y range inserted as white space between the top of
  each error bar and the start of the significance bracket tick. Default
  `0.04`.

- bracket_text_gap:

  Absolute distance (in data units) used as white space between the
  horizontal bracket line and the label text above it. Default `1.0`.

- strip_position:

  Controls where the facet strip label is placed. One of `"top"`
  (default), `"bottom"`, `"left"`, or `"right"`.

## Value

A [`ggplot`](https://ggplot2.tidyverse.org/reference/ggplot.html)
object.

## Examples

``` r
library(ggplot2)
df <- data.frame(
    group     = rep(c("Group A", "Group B"), each = 2),
    condition = rep(c("Exercise", "Control"), 2),
    mean      = c(10.2, 14.8, 12.5, 13.1),
    se        = c(0.9, 1.0, 1.1, 1.0),
    p_value   = c(0.004, 0.004, 0.18, 0.18)
)
df$condition <- factor(df$condition, levels = c("Exercise", "Control"))
ggplot2::theme_set(theme_bw2())

# Default: auto-formatted p-value label (text shown if p < 0.05)
plot_barplot_by_group(df, y_label = "Performance score", p_cutoff = 0.05)


# Custom bracket label from a column
df$label <- ifelse(
    df$p_value < 0.05,
    paste0("d = 1.5 [1.1-2.1]\n", format_pvalue(df$p_value)),
    NA
)
plot_barplot_by_group(df, y_label = "Performance score", label_col = "label", p_cutoff = 0.05) +
    ggplot2::coord_cartesian(ylim = c(0, 20))


# Show text for specific groups only (Group B shown even though p = 0.18)
plot_barplot_by_group(
    df,
    y_label = "Performance score",
    show_text_groups = "Group B"
)


# Show text for all groups by setting p_cutoff = 1
plot_barplot_by_group(
    df,
    y_label = "Performance score",
    p_cutoff = 1
)


# Position strip below x-axis labels
plot_barplot_by_group(
    df,
    y_label = "Performance score",
    p_cutoff = 1,
    strip_position = "bottom"
) +
    ggplot2::theme(
        axis.text.x = ggplot2::element_text(margin = ggplot2::margin(b = 5)),
        strip.placement = "outside"
    )
```
