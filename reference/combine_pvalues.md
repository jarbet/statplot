# Combine p-values

Combines a numeric vector of p-values into a single p-value using a
variety of methods. Note CMC or MCM are recommended by Chen 2022.

## Usage

``` r
combine_pvalues(p, methods = "CMC")
```

## Arguments

- p:

  Numeric vector of p-values.

- methods:

  Character, either 'CMC' (default) to return the CMC combined p-value,
  `'all'` to return all combined p-values, or the name of one or more
  methods (fisher, CMC, MCM, cauchy, minp_bonferroni; note CMC or MCM
  are recommended by Chen 2022) to return only those p-values.

## Value

A named vector of combined p-values for each method, or a single numeric
value when `methods` names one method

## References

Chen, Z. Robust tests for combining p-values under arbitrary dependency
structures. Sci Rep 12, 3158 (2022).
https://doi.org/10.1038/s41598-022-07094-7

## Examples

``` r
pvals <- c(0.001, 0.005, 0.2)
# default returns CMC
combine_pvalues(pvals)
#>         CMC 
#> 0.002721917 
# get all methods
combine_pvalues(pvals, methods = "all")
#>          fisher             CMC             MCM          cauchy minp_bonferroni 
#>    0.0001102497    0.0027219175    0.0049820278    0.0024910139    0.0030000000 
```
