# Visual "Table 1": summary table with inline distribution graphics

Summarizes numeric and categorical variables for a cohort (optionally by
group) like
[`table_overall()`](https://github.com/jarbet/statplot/reference/table_overall.md)
/
[`table_overall_and_group()`](https://github.com/jarbet/statplot/reference/table_overall_and_group.md),
but adds compact inline graphics inspired by Furmaga et al. 2026:

## Usage

``` r
table1viz(
  d,
  name_groupvar = NULL,
  overall = TRUE,
  group_header = "**Group**",
  stats_col_label = "Overall",
  missing = c("ifany", "no", "always"),
  missing_group = c("exclude", "show"),
  show_denominator = c("ifmissing", "always", "no"),
  statistic = list(gtsummary::all_continuous() ~ c("{mean} ({sd})", "{min}; {max}"),
    gtsummary::all_categorical() ~ c("{n} ({p}%)")),
  digits = NULL,
  binary_01_only_show1 = TRUE,
  continuous_plot = c("violin_box", "boxplot", "density"),
  show_outliers = TRUE,
  show_axis = TRUE,
  axis_breaks = NULL,
  bar_axis = FALSE,
  bar_axis_breaks = c(0, 50, 100),
  colors = NULL,
  overall_color = "#7F7F7F",
  plot_width = 140,
  plot_height = 30,
  bar_width = 60,
  plot_layout = c("combined", "columns"),
  distribution_label = "**Distribution**",
  header_align = c("left", "center"),
  overall_divider = TRUE,
  variable_divider = TRUE,
  row_lines = FALSE,
  output = c("auto", "gt", "flextable")
)
```

## Arguments

- d:

  data.frame. Dataset containing variables to summarize.

- name_groupvar:

  character(1) or `NULL`. Optional name of a grouping variable in `d`.
  If `NULL` (default) the entire cohort is summarized. Rows with a
  missing group are dropped (and counted in a table note) unless
  `missing_group = "show"`.

- overall:

  logical(1). If grouping, also show an Overall column? Default `TRUE`.

- group_header:

  character(1). Spanning header over the group columns.

- stats_col_label:

  character(1). Header label for the overall column.

- missing:

  character(1). When to show an "N missing" row for each variable:
  "ifany" (default), "no", or "always". The row shows `n / N (%)`
  missing (`n (%)` if `show_denominator = "no"`), where N and % are out
  of all observations in the column, with a hatched bar on the same
  0-100% scale as the category bars.

- missing_group:

  character(1). Rows with a missing `name_groupvar`: `"exclude"`
  (default) drops them and reports how many in a table note; `"show"`
  keeps them as an extra "Missing" group column (colored brown,
  `#8C564B`, unless `colors` is named and includes `"Missing"`).

- show_denominator:

  character(1). When to show category counts as `n / N (%)` instead of
  `n (%)`, where N is the number of non-missing values in the column
  (the percentage's denominator): `"ifmissing"` (default) for
  categorical variables with missing values, so the denominator is
  visible when it differs from the column N; `"always"` for all
  categorical variables; or `"no"`. Only applies to variables whose
  `statistic` is the default `"{n} ({p}%)"`. Unless `"no"`, "N missing"
  rows also show `n / N (%)`, with N the total number of observations in
  the column.

- statistic, digits:

  Passed to
  [`gtsummary::tbl_summary()`](https://www.danieldsjoberg.com/gtsummary/reference/tbl_summary.html).
  By default continuous variables show mean (SD) and min; max; the
  graphic already shows the median and quartiles. For skewed variables,
  consider adding a "Median (Q1, Q3)" row with e.g.
  `statistic = list(gtsummary::all_continuous() ~ c("{mean} ({sd})", "{median} ({p25}, {p75})", "{min}; {max}"), gtsummary::all_categorical() ~ "{n} ({p}%)")`.
  `digits = NULL` (default) lets gtsummary choose the decimals for each
  variable from its spread, so variables on a small scale (e.g. 0.03)
  are not rounded to 0.0; pass e.g.
  `list(gtsummary::all_continuous() ~ 1)` to fix them.

- binary_01_only_show1:

  logical(1). If `TRUE`, binary variables only show one row (e.g. the
  "1"/"Yes" level). Default `TRUE`.

- continuous_plot:

  character(1). Graphic for continuous variables: `"violin_box"` (violin
  with inner boxplot and outlier dots; default), `"boxplot"` (boxplot
  with outlier dots), or `"density"` (density sparkline with a tick at
  the median).

- show_outliers:

  logical(1). Draw outliers (values beyond 1.5 x IQR from the box) as
  dots? Default `TRUE`. Whiskers still stop at the most extreme values
  within 1.5 x IQR when `FALSE`. Has no effect for
  `continuous_plot = "density"`.

- show_axis:

  logical(1). Add a small x-axis with a few tick labels under each
  continuous-variable graphic to show its range? Default `TRUE`. The
  axis is shared by all columns/groups of a variable.

- axis_breaks:

  Optional named list giving tick values for specific continuous
  variables when `show_axis = TRUE`, e.g.
  `list(age = c(20, 40, 60, 80))`. Names are variable names in `d`;
  variables not listed get automatic ticks. Ticks outside the data range
  widen that variable's (shared) scale so they are shown. If a vector
  has names, they are used as the tick labels, e.g.
  `c("0" = 0, "1 yr" = 365)`.

- bar_axis:

  logical(1). Add a small 0-100% x-axis under the percentage bars?
  Default `FALSE`. The axis is drawn once per variable, under its last
  bar (the "N missing" row if shown, otherwise the last level).

- bar_axis_breaks:

  numeric. Tick positions, in percent (0-100), for the bar axis when
  `bar_axis = TRUE`. Default `c(0, 50, 100)`, labelled "0%", "50%",
  "100%". If the vector has names, they are used as the tick labels,
  e.g. `c("0" = 0, "1/4" = 25, "1/2" = 50)`.

- colors:

  Optional character vector of colors for the group columns (unnamed in
  group order, or named by group level). Defaults to the Okabe-Ito
  palette.

- overall_color:

  character(1). Color used for the overall column.

- plot_width, plot_height:

  numeric(1). Size of continuous-variable graphics in pixels.

- bar_width:

  numeric(1). Width of percentage bars in pixels
  (`plot_layout = "columns"` only).

- plot_layout:

  character(1). Where graphics go when grouping: `"combined"` (default)
  adds one "Distribution" column on the right in which the groups (and
  Overall) are drawn together on a shared axis, one lane per group, so
  they can be compared directly. Continuous variables get stacked
  violins/boxplots/densities spanning the variable's label and statistic
  rows; each categorical level gets a set of side-by-side bars.
  `"columns"` instead draws a separate graphic in each group's column.
  Ignored when `name_groupvar` is `NULL`.

- distribution_label:

  character(1). Header of the combined distribution column.

- header_align:

  character(1). Alignment of the column headers for the Overall, group
  and Distribution columns: `"left"` (default) or `"center"`.

- overall_divider:

  logical(1). Draw a vertical line to the right of the Overall column to
  set it apart from the group columns? Default `TRUE`. Only used when
  grouping with `overall = TRUE`.

- variable_divider:

  logical(1). Draw a thicker horizontal line above each variable (except
  the first) so it is clearer where a new variable starts? Default
  `TRUE`.

- row_lines:

  logical(1). Draw the thin horizontal lines between the rows within a
  variable (e.g. between its statistic rows or levels)? Default `FALSE`,
  so only the lines between variables are drawn. Applies to gt output;
  the Word (flextable) table has no lines within a variable.

- output:

  character(1). Table type to return: `"auto"` (default) returns a
  flextable when knitting to Word (`.docx`, e.g. Quarto `format: docx`)
  and a gt table otherwise; `"gt"` or `"flextable"` force one. Requires
  the flextable and xml2 packages for `"flextable"`.

## Value

A `gt::gt_tbl` (`output = "gt"`) or a `flextable` with class
`"table1viz_flextable"` (`output = "flextable"`).

The gt table draws graphics as inline SVG, so it renders in HTML output
(Quarto/R Markdown HTML, RStudio viewer, `gt::gtsave(".html")`).

The flextable is for Word. When knitted to `.docx` it becomes a native,
editable Word table: text, fonts, borders and widths can be edited in
Word, and the graphics are Word vector shapes (drawing groups) inside
the cells, so no image files or post-processing are needed. Graphics are
stored as placeholders until knitting, so print it or knit it as is;
flextable functions that restyle the table (e.g.
[`flextable::fontsize()`](https://davidgohel.github.io/flextable/reference/fontsize.html))
keep them, but
[`flextable::save_as_docx()`](https://davidgohel.github.io/flextable/reference/save_as_docx.html)
and other officer exports do not draw them. When printed or knitted to
HTML, the graphics are drawn as SVG.

## Details

- **Continuous** variables: by default a horizontal violin plot with an
  inner boxplot (box = IQR, thick line = median, whiskers to the most
  extreme values within 1.5 x IQR, dots = outliers beyond that) in the
  variable's label row, above the numeric summaries. All columns share
  the same x-axis (and violin width) scale so groups are directly
  comparable. Each violin has the same area (like
  [`ggplot2::geom_violin()`](https://ggplot2.tidyverse.org/reference/geom_violin.html)'s
  default), so a violin's width shows the shape of that group's
  distribution, not its sample size; N is given in each column header.

- **Categorical** and **binary** variables: a percentage bar next to
  each `n (%)`.

Percentages are column percentages (within each group), so bars are
comparable across groups. Category percentages exclude missing values,
so for variables with missing values counts are shown as `n / N (%)`
with N the non-missing count (see `show_denominator`); the "N missing"
row gives % missing out of all observations in the column. A table note
states this whenever it matters.

## References

Furmaga J, et al. A new type of table one: showing instead of telling.
*Journal of Clinical Epidemiology*. 2026;196.

## Examples

``` r
# Simulated cohort: group, two continuous, two categorical variables
set.seed(1)
df <- data.frame(
  grp = sample(c("A", "B", "C"), 200, TRUE),
  age = rnorm(200, 60, 15),
  los = rlnorm(200, 5, 0.8),
  sex = sample(c("Female", "Male"), 200, TRUE),
  diabetes = rbinom(200, 1, 0.4)
)

# Same data with some missing values
df_miss <- df
df_miss$los[sample(200, 30)] <- NA
df_miss$sex[sample(200, 12)] <- NA

# ---- Defaults ----

# Whole cohort: one column of summaries with inline graphics
table1viz(df)


  

Variable
```
