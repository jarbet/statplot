# Bold-styled Classic Theme

A modified ggplot2 classic theme with bold text elements. By default,
the plot title, subtitle, caption, axis titles, and legend title support
markdown/HTML via
[`ggtext::element_markdown()`](https://wilkelab.org/ggtext/reference/element_markdown.html).

## Usage

``` r
theme_classic2(markdown = TRUE)
```

## Arguments

- markdown:

  Logical; if `TRUE` (default), the plot title, subtitle, caption, axis
  titles, and legend title use
  [`ggtext::element_markdown()`](https://wilkelab.org/ggtext/reference/element_markdown.html),
  so labels can contain markdown/HTML such as `"*italic*"`,
  `"**bold**"`, or `"log<sub>2</sub> FC"`. Use `"<br>"` for line breaks
  in these labels instead of `"\n"`, which markdown/HTML rendering
  ignores. Axis tick labels and legend text are left as plain
  [`ggplot2::element_text()`](https://ggplot2.tidyverse.org/reference/element.html)
  (they can still be switched to
  [`ggtext::element_markdown()`](https://wilkelab.org/ggtext/reference/element_markdown.html)
  manually, e.g.
  `theme(axis.text.x = ggtext::element_markdown(angle = 45))`). Note
  that when `TRUE`, further overriding one of the markdown elements
  (plot title/subtitle/caption, axis titles, or legend title) with
  [`ggplot2::element_text()`](https://ggplot2.tidyverse.org/reference/element.html)
  errors in ggplot2 \>= 4.0 ("Only elements of the same class can be
  merged"); use
  [`ggtext::element_markdown()`](https://wilkelab.org/ggtext/reference/element_markdown.html)
  for that override instead, or set `markdown = FALSE` for plain
  [`ggplot2::element_text()`](https://ggplot2.tidyverse.org/reference/element.html)
  elements throughout.

## Value

A ggplot2 theme object

## Examples

``` r
library(ggplot2)
ggplot(mtcars, aes(x = wt, y = mpg)) +
  geom_point() +
  labs(tag = "A") +
  theme_classic2()
```
