# Create a dotmap showing effect size (dot size & color) and p-value (tile fill)

A combined tile + point "dotmap" that visualizes an effect (size and
direction) and a p-value (tile fill). The function returns a ggplot
object.

## Usage

``` r
plot_dotmap(
  data,
  x,
  y,
  effect,
  p,
  dot_size_vals = c(-2, -1, -0.5, -0.25, 0.25, 0.5, 1, 2),
  dot_size_labels = as.character(dot_size_vals),
  dot_range = c(5, 30),
  palette = c(positive = "darkorange1", negative = "dodgerblue2"),
  xlab_angle = 0,
  mlog10_transform_pvalue = TRUE,
  fill_limits = NULL,
  legend_pvalue_title = NULL,
  legend_dotsize_title = expression(bold("Effect size")),
  add_combined_pvalue_barplot = FALSE,
  combined_qvalue = FALSE,
  combine_pvalue_method = c("fisher", "cauchy", "hm"),
  patchwork_widths = c(3, 1)
)
```

## Arguments

- data:

  data.frame or tibble containing the plotting variables

- x:

  Character; name of variable in `data` to use for x-axis/columns

- y:

  Character; name of variable in `data` to use for y-axis/rows

- effect:

  Character; column name of numeric variable in `data` to use for dot
  size and color (direction)

- p:

  Character; column name of numeric variable in `data` to use for tile
  fill (p-value)

- dot_size_vals:

  Numeric vector of reference effect values used for the size legend
  (signed to indicate direction)

- dot_size_labels:

  Character vector of labels for the size legend

- dot_range:

  Numeric(2) range of point sizes (min, max)

- palette:

  Named character vector with elements "positive" and "negative"
  specifying dot colors

- xlab_angle:

  Numeric angle to rotate x-axis labels (degrees)

- mlog10_transform_pvalue:

  Logical; when TRUE the fill uses -log10(p) instead of raw p

- fill_limits:

  Numeric(2) or NULL; limits for the fill scale

- legend_pvalue_title:

  Character or NULL; override title for p-value/color legend

- legend_dotsize_title:

  Character; title for the dot-size legend

- add_combined_pvalue_barplot:

  Logical; when TRUE adds a combined p-value barplot to the right of the
  dotmap

- combined_qvalue:

  if TRUE then the combined pvalue barplot will show q-values instead of
  p-values (only applies if add_combined_pvalue_barplot = TRUE)

- combine_pvalue_method:

  Character; method for combining p-values in the barplot ("fisher",
  "cauchy", or "hm")

- patchwork_widths:

  Numeric(2); widths passed to patchwork::plot_layout when adding the
  combined p-value barplot (default c(3, 1))

## Value

A ggplot2::ggplot object when `add_combined_pvalue_barplot = FALSE`, or
a patchwork composition object (from `patchwork`) when
`add_combined_pvalue_barplot = TRUE`.

## Examples

``` r
set.seed(42)
genes <- paste0("gene", 1:6)
df <- expand.grid(col = c("A", "B", "C"), row = genes, stringsAsFactors = FALSE)
df$effect <- rnorm(nrow(df), mean = 0, sd = 1.2)         # realistic effect sizes
df$mlog10_p <- runif(nrow(df), min = 0, max = 3)         # -log10(p) between 0 and 3
df$p <- 10^(-df$mlog10_p)
df$row <- factor(df$row, levels = rev(genes))
plot_dotmap(df, x = "col", y = "row", effect = "effect", p = "p",
            mlog10_transform_pvalue = TRUE)
#> Scale for size is already present.
#> Adding another scale for size, which will replace the existing scale.

# Add Fisher's combination pvalue barplot on the right which combines p-values across columns for each row category
plot_dotmap(
  df,
  x = "col",
  y = "row",
  effect = "effect",
  p = "p",
  mlog10_transform_pvalue = TRUE,
  add_combined_pvalue_barplot = TRUE,
  combine_pvalue_method = "fisher", # can also use "cauchy" or "hm" methods for combining potentially correlated p-values
  combined_qvalue = FALSE # set to TRUE to show q-values instead of p-values in the combined barplot (only applies if add_combined_pvalue_barplot = TRUE)
  )
#> Scale for size is already present.
#> Adding another scale for size, which will replace the existing scale.
#> Scale for y is already present.
#> Adding another scale for y, which will replace the existing scale.
#> Scale for y is already present.
#> Adding another scale for y, which will replace the existing scale.


```
