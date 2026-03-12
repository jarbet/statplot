# Create overall and by-group summary table

The Overall column reports percentages for the overall cohort. The group
columns report row-wise percentages.

## Usage

``` r
table_overall_and_group(d, name_groupvar, group_header = "**Group**")
```

## Arguments

- d:

  data.frame. Dataset containing variables to summarize.

- name_groupvar:

  character(1). Name of the grouping variable (column name) in `d` used
  to produce the grouped summary.

- group_header:

  character(1). Label used as the tab spanner for the grouped columns in
  the merged table.

## Value

A gtsummary table object

## Details

The function builds two tbl_summary objects: one for the overall dataset
(with the grouping column removed) and one stratified by the specified
grouping variable. It then merges them into a single table with a custom
tab spanner for the grouped columns.

## Examples

``` r
df <- data.frame(
  grp = rep(c("A","B"), each = 10),
  age = rnorm(20, 50, 10),
  sex = sample(c("M","F"), 20, TRUE)
)
table_overall_and_group(df, "grp")
#> Setting theme "language: en"
#> Setting theme "Compact"


  


Characteristic
```

**Overall**  
N = 20

**Group**

**A**  
n = 10

**B**  
n = 10

age

  

  

  

    Median (Q1, Q3)

45.7 (42.3, 50.8)

42.9 (42.2, 47.2)

48.8 (43.7, 52.1)

    Min, Max

35.7, 56.6

35.7, 56.2

41.0, 56.6

sex

  

  

  

    F

11 (55%)

4 (36%)

7 (64%)

    M

9 (45%)

6 (67%)

3 (33%)
