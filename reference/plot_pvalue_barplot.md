# Plot p-value barplot

Create a horizontal barplot of p-values (optionally -log10 transformed)
with an optional significance vertical line and optional fill mapping.

## Usage

``` r
plot_pvalue_barplot(
  data,
  x,
  y,
  fill = NULL,
  alpha = 0.05,
  width = 0.6,
  xlim = NULL,
  xbreaks = NULL,
  xlab = "p-value",
  vline = TRUE,
  vline_linetype = "dashed",
  vline_color = "red",
  show_y_labels = FALSE,
  mlog10_transform_pvalue = FALSE,
  also_show_qvalue = TRUE,
  color_qvalue = "black",
  color_pvalue = ifelse(also_show_qvalue, "grey", "black")
)
```

## Arguments

- data:

  A data.frame or tibble containing the variables.

- x:

  Character, name of the column with raw p-values.

- y:

  Character, name of the column for y-axis categories.

- fill:

  Character or NULL, column name to use for fill; if NULL draw solid
  black bars.

- alpha:

  Numeric significance threshold for the vertical line (default 0.05).

- width:

  Numeric bar width.

- xlim:

  Numeric vector of length 2 giving x-axis limits; computed if NULL.

- xbreaks:

  Numeric vector of x-axis breaks; computed if NULL.

- xlab:

  Character, x-axis label.

- vline:

  Logical, whether to draw a vertical line at alpha (or -log10(alpha)).

- vline_linetype:

  Character, linetype for the vertical line.

- vline_color:

  Character, color for the vertical line.

- show_y_labels:

  Logical, whether to show y-axis labels (default FALSE).

- mlog10_transform_pvalue:

  Logical; when TRUE compute -log10(p) for plotting/order.

- also_show_qvalue:

  Logical; when TRUE compute FDR-adjusted q-values (Benjamini-Hochberg)
  and draw two overlapping bars per row: the q-value bar (black) on top
  of the p-value bar (darkgrey). When TRUE, the 'fill' argument is
  ignored and fixed colors are used for p/q bars.

- color_qvalue:

  Character, color for q-value bars when also_show_qvalue = TRUE.

- color_pvalue:

  Character, color for p-value bars when also_show_qvalue = TRUE.

## Value

A ggplot2 object.

## Examples

``` r
set.seed(123)
n <- 4
example_df <- tibble::tibble(
  cell_line = paste0("Cell", sprintf("%02d", 1:n)),
  pvalue = 10^(-runif(n, 0.2, 2.8)),
  group = rep(c("A", "B"), length.out = n)
)
example_df$cell_line <- factor(
  example_df$cell_line,
  levels = rev(example_df$cell_line)
)
plot_pvalue_barplot(
  data = example_df,
  x = "pvalue",
  y = "cell_line",
  fill = NULL,
  mlog10_transform_pvalue = TRUE,
  show_y_labels = TRUE,
  also_show_qvalue = TRUE
)
```
