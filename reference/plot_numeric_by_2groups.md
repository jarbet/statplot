# Violin + boxplot with Wilcoxon rank-sum test

Create a violin plot overlaid with a narrow boxplot for two groups and
run a Wilcoxon rank-sum test. Returns the ggplot object and a tidy
Wilcoxon result.

## Usage

``` r
plot_numeric_by_2groups(
  yvar,
  group,
  d,
  title = NULL,
  colors = c("white", "white"),
  digits = 1
)
```

## Arguments

- yvar:

  character(1) Name of the numeric outcome column in `d`.

- group:

  character(1) Name of the factor column in `d` (must have 2 levels).

- d:

  data.frame Data containing `yvar` and `group`.

- title:

  character Optional plot title. If NULL, a default is generated.

- colors:

  character vector Length-2 vector of fill colours (default c('white',
  'white')); if NULL the default ggplot2 fill scale is used. When
  supplied, must be a character vector with length equal to the number
  of groups (two after filtering); if it has names, they must exactly
  match the factor levels.

- digits:

  numeric(1) Number of decimal places to use in the subtitle statistics.

## Value

A list with elements:

- ggplot:

  A ggplot2 object (violin + boxplot).

- wilcox:

  A tibble with the Wilcoxon test results (from broom::tidy).

## Examples

``` r
mtcars$am <- factor(mtcars$am)
res <- plot_numeric_by_2groups("mpg", "am", mtcars)
#> Warning: cannot compute exact p-value with ties
#> Warning: cannot compute exact confidence intervals with ties
res$ggplot

res$wilcox
#> # A tibble: 1 × 8
#>   estimate statistic p.value conf.low conf.high method       alternative outcome
#>      <dbl>     <dbl>   <dbl>    <dbl>     <dbl> <chr>        <chr>       <chr>  
#> 1    -6.80        42 0.00187    -11.7     -2.90 Wilcoxon ra… two.sided   mpg    
```
