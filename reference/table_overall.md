# Create a table that summarizes the entire cohort.

Convenience wrapper that builds a gtsummary::tbl_summary for the overall
dataset.

## Usage

``` r
table_overall(
  d,
  missing = c("ifany", "no", "always"),
  stats_col_label = "Summary",
  statistic = list(gtsummary::all_continuous() ~ c("{median} ({p25}, {p75})",
    "{mean} +/- {sd}", "{min}, {max}"), gtsummary::all_categorical() ~ c("{n} ({p}%)")),
  digits = list(gtsummary::all_continuous() ~ c(rep(1, 7))),
  binary_01_only_show1 = TRUE
)
```

## Arguments

- d:

  data.frame Dataset containing variables to summarize.

- missing:

  character(1) How to display missing data. Options are "ifany", "no",
  or "always". Default is "ifany".

- stats_col_label:

  character(1) Label for the summary column in the table. Default is
  "Summary".

- statistic:

  Specifies summary statistics to display for each variable. See
  `gtsummary::tbl_summmary` documentation for details.

- digits:

  Specifies the number of decimal places to display for each summary
  statistic in `statistic`.

- binary_01_only_show1:

  logical(1) If TRUE, binary variables with values 0 and 1 (or No and
  Yes) will only show the summary for the "1" (or Yes) value. Default is
  TRUE.

## Value

A
[`gtsummary::tbl_summary`](https://www.danieldsjoberg.com/gtsummary/reference/tbl_summary.html)
object for the overall cohort.

## Examples

``` r
df <- data.frame(age = rnorm(20, 50, 10), sex = sample(c("M","F"), 20, TRUE))
table_overall(df)
#> Setting theme "language: en"
#> Setting theme "Compact"


  

Characteristic
```
