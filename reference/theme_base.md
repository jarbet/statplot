# Base theme with common bold styling elements

Internal utility function that creates the common ggplot2 theme
modifications used by theme_bw2 and theme_classic2.

## Usage

``` r
theme_base(markdown = TRUE)
```

## Arguments

- markdown:

  Logical; if `TRUE` (default), the plot title, subtitle, caption, axis
  titles, and legend title use
  [`ggtext::element_markdown()`](https://wilkelab.org/ggtext/reference/element_markdown.html)
  so they can contain markdown/HTML. Axis tick labels and legend text
  are left as plain
  [`ggplot2::element_text()`](https://ggplot2.tidyverse.org/reference/element.html),
  since those are often populated from data values rather than authored
  by the user.

## Value

A ggplot2 theme object with bold text elements
