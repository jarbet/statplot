# Combine p-values using Fisher, Cauchy (ACAT), or Harmonic Mean methods

Combines a numeric vector of p-values into a single p-value using one of
three supported methods: "fisher" (poolr::fisher), "cauchy"
(ACAT::ACAT), or "hm" (harmonicmeanp::p.hmp). NAs are removed and all
p-values must be strictly between 0 and 1.

## Usage

``` r
combine_pvalues(p, method = c("fisher", "cauchy", "hm"))
```

## Arguments

- p:

  Numeric vector of p-values.

- method:

  One of "fisher", "cauchy", or "hm".

## Value

A numeric p-value

## Examples

``` r
pvals <- c(0.001, 0.005, 0.2)
combine_pvalues(pvals, "fisher")
#> [1] 0.0001102497
combine_pvalues(pvals, "cauchy")
#> [1] 0.002491014
combine_pvalues(pvals, "hm")
#> [1] 0.002534232
```
