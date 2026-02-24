# Render a named list as a Quarto tabset

Print each element of a list with a level-2 heading (##) suitable for
Quarto tabsets. If `title` is NULL the list names are used. Typical use
is printing ggplot objects or other printable plot objects so each
becomes a separate tab.

## Usage

``` r
quarto_html_tabset_list(list_obj, title = NULL)
```

## Arguments

- list_obj:

  A list of printable R objects (e.g. ggplot objects).

- title:

  Optional character vector of titles to use for each element; if NULL
  names(list_obj) are used.

## Value

The input list, returned invisibly.

## Examples

``` r
if (FALSE) { # \dontrun{
library(ggplot2)
plots <- list(
  One = ggplot(mtcars, aes(mpg, wt)) + geom_point(),
  Two = ggplot(mtcars, aes(hp, qsec)) + geom_point()
)
quarto_tabset_list(plots)
} # }
```
