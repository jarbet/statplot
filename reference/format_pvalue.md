# Format p-values for display in text, ggplot2 plots, or tables.

Vectorised helper to format numeric p-values into readable strings.

## Usage

``` r
format_pvalue(
  x,
  p_text = "p",
  p_symbol = "= ",
  truncate_pvalue = FALSE,
  html = TRUE,
  format = c("text", "plotmath")
)
```

## Arguments

- x:

  Numeric vector of p-values

- p_text:

  Character; text prefix for p-values (default "p"). Use "" to omit.

- p_symbol:

  Character; symbol/operator to display including spacing (default "=
  "). When truncate_pvalue=TRUE and p\<0.001, "\<" is used instead.

- truncate_pvalue:

  Logical; whether to truncate pvalues \<0.001 to "p\<0.001". If FALSE,
  scientific notation is used (default FALSE).

- html:

  Logical; whether to format scientific notation as HTML for small
  p-values (default TRUE). HTML output is suitable for markdown
  documents, Quarto, or with
  [`ggtext::geom_richtext()`](https://wilkelab.org/ggtext/reference/geom_richtext.html),
  but not with standard
  [`ggplot2::geom_text()`](https://ggplot2.tidyverse.org/reference/geom_text.html).
  Ignored when `format = "plotmath"`.

- format:

  Character; output format for small p-values. One of "text" (default;
  plain scientific notation), or "plotmath" (suitable for ggplot2 with
  parse=TRUE). When "plotmath", returns a quoted string with plotmath
  notation (e.g., `"p =" ~ 5.0 %*% 10^{-5}`).

## Value

Character vector of formatted p-values. For p \>= 0.001, returns values
formatted to 2-3 decimal places. For p \< 0.001 with truncate_pvalue =
TRUE, returns "p\<0.001". For p \< 0.001 with truncate_pvalue = FALSE,
returns scientific notation in the format specified by `format`.

## Examples

``` r
# Default behavior: plain text scientific notation
format_pvalue(0.0005)
#> [1] "p= 5.0 × 10<sup>-4</sup>"
format_pvalue(c(0.0005, 0.005, 0.02, 0.236))
#> [1] "p= 5.0 × 10<sup>-4</sup>" "p= 0.005"                
#> [3] "p= 0.020"                 "p= 0.24"                 

# HTML formatting for markdown/Quarto or ggtext::geom_richtext()
format_pvalue(0.0005, html = TRUE)
#> [1] "p= 5.0 × 10<sup>-4</sup>"

# Truncate small p-values<0.001 to "p<0.001"
format_pvalue(0.0005, truncate_pvalue = TRUE)
#> [1] "p<0.001"
format_pvalue(c(0.0005, 0.005, 0.02, 0.236), truncate_pvalue = TRUE)
#> [1] "p<0.001"  "p= 0.005" "p= 0.020" "p= 0.24" 

# Plotmath notation for use with geom_text(parse = TRUE)
format_pvalue(0.0005, format = "plotmath")
#> [1] "\"p=\" ~ 5.0 %*% 10^{-4}"

# Custom p_text and p_symbol
format_pvalue(0.0005, p_text = "q")
#> [1] "q= 5.0 × 10<sup>-4</sup>"
format_pvalue(c(0.0005, 0.005), p_text = "p", p_symbol = ": ")
#> [1] "p: 5.0 × 10<sup>-4</sup>" "p: 0.005"                

# Just the p-value without prefix
format_pvalue(0.0005, p_text = "", p_symbol = "")
#> [1] "5.0 × 10<sup>-4</sup>"
```
