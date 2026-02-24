# Create a table that summarizes the entire cohort.

Convenience wrapper that builds a gtsummary::tbl_summary for the overall
dataset.

## Usage

``` r
table_overall(d)
```

## Arguments

- d:

  data.frame Dataset containing variables to summarize.

## Value

A gtsummary::tbl_summary object for the overall cohort.

## Examples

``` r
df <- data.frame(age = rnorm(20, 50, 10), sex = sample(c("M","F"), 20, TRUE))
table_overall(df)
#> Setting theme "language: en"
#> Setting theme "Compact"


  

Characteristic
```

**Overall**  
N = 20

age

  

    Median (Q1, Q3)

45.4 (41.2, 54.3)

    Mean +/- SD

47.4 +/- 9.5

    Min, Max

30.3, 67.9

sex

  

    F

7 (35%)

    M

13 (65%)
