# Format p-values for display

Vectorised helper to format numeric p-values into readable strings.

## Usage

``` r
format_pvalue(x, p_symbol = "p", include_p_symbol = TRUE)
```

## Arguments

- x:

  Numeric vector of p-values

- p_symbol:

  Character; symbol to prefix p-values with (default 'p')

- include_p_symbol:

  Logical; whether to include the p_symbol prefix (default TRUE)

## Value

Character vector of formatted p-values, optionally prefixed with
`p_symbol` when `include_p_symbol` is TRUE.

## Examples

``` r
format_pvalue(0.0005)
#> [1] "p<0.001"
format_pvalue(c(0.0005, 0.005, 0.02, 0.236))
#> [1] "p<0.001" "p=0.005" "p=0.020" "p=0.24" 
```
