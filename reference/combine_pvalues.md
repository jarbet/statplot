# Combine p-values

Combines a numeric vector of p-values into a single p-value using a
variety of methods. Note CMC or MCM are recommended by Chen 2022.

## Usage

``` r
combine_pvalues(p)
```

## Arguments

- p:

  Numeric vector of p-values.

## Value

A named vector of combined p-values for each method

## References

Chen, Z. Robust tests for combining p-values under arbitrary dependency
structures. Sci Rep 12, 3158 (2022).
https://doi.org/10.1038/s41598-022-07094-7

## Examples

``` r
pvals <- c(0.001, 0.005, 0.2)
combine_pvalues(pvals)
#>          fisher             CMC             MCM          cauchy minp_bonferroni 
#>    0.0001102497    0.0027219175    0.0049820278    0.0024910139    0.0030000000 
```
