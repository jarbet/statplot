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
  mlog10_transform_pvalue = FALSE,
  fill_limits = NULL,
  legend_pvalue_title = NULL,
  legend_dotsize_title = "Effect size"
)
```

## Arguments

- data:

  data.frame or tibble containing the plotting variables

- x:

  Character; column name for x axis

- y:

  Character; column name for y axis

- effect:

  Character; numeric column name used for dot size and direction

- p:

  Character; numeric column name used for tile fill (p-value)

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

## Value

A ggplot2::ggplot object

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

```
