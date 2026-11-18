# Compare a numeric variable between two groups with violins/boxplots and Wilcoxon test

Compare a numeric variable between two groups with violins/boxplots and
Wilcoxon test. Returns the ggplot object and a tidy Wilcoxon result.
Optionally you can facet by additional variables.

## Usage

``` r
plot_numeric_by_2groups(
  yvar,
  group,
  d,
  colors = c("white", "white"),
  digits = ifelse(effect_size == "median_difference", 1, 2),
  alpha = 0.7,
  effect_size = c("median_difference"),
  facet_cols = NULL,
  facet_pvalue = "both",
  text_effectsize_vjust = 1.5,
  text_n_vjust = -0.4,
  text_effectsize_prefix = ifelse(effect_size == "median_difference", "Median diff: ",
    "C-index: ")
)
```

## Arguments

- yvar:

  character(1) Name of the numeric outcome column in `d`.

- group:

  character(1) Name of the factor column in `d` (must have 2 levels).

- d:

  data.frame Data containing `yvar` and `group`.

- colors:

  character vector Length-2 vector of fill colours (default c('white',
  'white')); if NULL the default ggplot2 fill scale is used. When
  supplied, must be a character vector with length equal to the number
  of groups (two after filtering); if it has names, they must exactly
  match the factor levels.

- digits:

  numeric(1) Number of decimal places to use in effect size results.

- alpha:

  numeric(1) Fill transparency for violin and boxplot geoms, passed to
  the `alpha` argument of
  [`ggplot2::geom_violin()`](https://ggplot2.tidyverse.org/reference/geom_violin.html)
  and
  [`ggplot2::geom_boxplot()`](https://ggplot2.tidyverse.org/reference/geom_boxplot.html).
  Values range from 0 (fully transparent) to 1 (fully opaque). Default
  `0.7`.

- effect_size:

  character(1) Type of effect size to compute. Either
  `"median_difference"` (default; Hodges-Lehmann estimate of location
  shift) or `"c_index"` (concordance probability using
  [`asht::wmwTest`](https://rdrr.io/pkg/asht/man/wmwTest.html); requires
  the suggested package asht).

- facet_cols:

  character vector of column names to facet by. Default `NULL` (single
  panel).

- facet_pvalue:

  character(1) When `facet_cols` is supplied, which p-value information
  to display in annotations. One of "pvalue", "qvalue", or "both"
  (default). Q-values are always computed when `facet_cols` is supplied
  using FDR correction.

- text_effectsize_vjust:

  numeric(1) Vertical justification for the effect size annotation text
  (used when `facet_cols` is supplied). Default `1.5`.

- text_n_vjust:

  numeric(1) Vertical justification for the sample size annotation text
  (used when `facet_cols` is supplied). Default `-0.4`.

- text_effectsize_prefix:

  character(1) Prefix text for the effect size annotation. Default
  `"Median diff: "`.

## Value

A list with elements:

- ggplot:

  A ggplot2 object (violin + boxplot). Add `+ ggplot2::facet_wrap()` or
  `+ ggplot2::facet_grid()` to create multi-panel layouts; annotations
  facet automatically.

- wilcox:

  A data.frame with the Wilcoxon test results, one row per test, with
  columns `estimate`, `statistic`, `p.value`, `conf.low`, `conf.high`,
  `method`, `alternative` and `outcome`. When `facet_cols` is supplied
  it also includes the faceting column(s) identifying which facet each
  row belongs to, and a `qvalue` column (FDR-adjusted p-values).

## Details

When `facet_cols` is supplied, a separate Wilcoxon test is run within
each unique combination of those columns and per-panel p-value and
sample size annotations are prepared automatically. The annotation
layers retain the faceting columns so that adding
`+ ggplot2::facet_wrap()` or `+ ggplot2::facet_grid()` after the
function call correctly splits both violins and annotations across
panels.

## Examples

``` r
ggplot2::theme_set(theme_bw2())
mtcars$vs <- factor(mtcars$vs)

# Basic example
res <- plot_numeric_by_2groups("mpg", "vs", mtcars)
res$ggplot

res$wilcox
#>   estimate statistic      p.value conf.low conf.high
#> 1    -7.35      22.5 1.947244e-05    -11.4      -4.1
#>                         method alternative outcome
#> 1 Wilcoxon rank sum exact test   two.sided     mpg

# Show C-index effect size instead of median difference (requires asht)
if (requireNamespace("asht", quietly = TRUE)) {
  plot_numeric_by_2groups(
    yvar = "mpg",
    group = "vs",
    d = mtcars,
    effect_size = "c_index"
  )
}
#> $ggplot

#> 
#> $wilcox
#>    estimate statistic      p.value conf.low conf.high
#> 1 0.9107143        NA 9.034472e-05 0.731417 0.9725219
#>                                                                                                                                   method
#> 1 Wilcoxon-Mann-Whitney test with continuity correction\n (confidence interval requires proportional odds assumption, but test does not)
#>                       alternative outcome
#> 1 two distributions are not equal     mpg
#> 

# Faceted example: compare a "score" between exercisers and non-exercisers,
# faceted by group (A/B/C/D)
set.seed(42)
n <- 200
df <- data.frame(
  score    = c(rnorm(n / 2, mean = 5), rnorm(n / 2, mean = 6)),
  exercise = factor(rep(c("No", "Yes"), each = n / 2)),
  group    = factor(sample(LETTERS[1:4], n, replace = TRUE))
)
res_facet <- plot_numeric_by_2groups(
  yvar       = "score",
  group      = "exercise",
  d          = df,
  facet_cols = "group",
  colors     = c("No" = "grey80", "Yes" = "steelblue"),
  alpha      = 0.6
)
res_facet$ggplot + ggplot2::facet_wrap(~group, ncol = 2)

res_facet$wilcox
#>     estimate statistic      p.value  conf.low   conf.high
#> 1 -0.6113676       105 0.0444305696 -1.345396 -0.05543786
#> 2 -1.0237538       149 0.0008043605 -1.608435 -0.48862601
#> 3 -0.7148098       284 0.0053209725 -1.227443 -0.23339073
#> 4 -0.8802748       137 0.0010031844 -1.432571 -0.36064269
#>                         method alternative outcome group      qvalue
#> 1 Wilcoxon rank sum exact test   two.sided   score     D 0.044430570
#> 2 Wilcoxon rank sum exact test   two.sided   score     A 0.002006369
#> 3 Wilcoxon rank sum exact test   two.sided   score     B 0.007094630
#> 4 Wilcoxon rank sum exact test   two.sided   score     C 0.002006369
```
