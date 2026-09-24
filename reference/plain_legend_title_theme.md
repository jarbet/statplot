# Force a plot's legend title to plain text

Internal helper for functions (e.g. `plot_dotmap`, `plot_pathways`) that
allow a legend title to be supplied as a plotmath
[`expression()`](https://rdrr.io/r/base/expression.html).
[`ggtext::element_markdown()`](https://wilkelab.org/ggtext/reference/element_markdown.html)
cannot render an
[`expression()`](https://rdrr.io/r/base/expression.html) (it would be
coerced to a literal, deparsed character string, e.g.
`"bold(-log[\"10\"] ~ \"pvalue\")"`, rather than parsed as plotmath);
and a later `theme(legend.title = ggplot2::element_text(...))` cannot
simply be added on top of a plot whose (possibly global, via
`theme_set()`) legend title element is `element_markdown()`, since
ggplot2 \>= 4.0 errors when merging theme elements of different classes
for the same slot. This resolves the plot's current theme (its own local
overrides plus the current global default from
[`ggplot2::theme_get()`](https://ggplot2.tidyverse.org/reference/get_theme.html))
into a single, complete theme with a plain-text, bold legend title,
using
[ggplot2::%+replace%](https://ggplot2.tidyverse.org/reference/get_theme.html)
to swap the element in directly rather than merging it, which sidesteps
the class-merge restriction entirely.

## Usage

``` r
plain_legend_title_theme(p)
```

## Arguments

- p:

  A ggplot object.

## Value

`p` with its theme resolved to a complete theme using a plain
[`ggplot2::element_text()`](https://ggplot2.tidyverse.org/reference/element.html)
`legend.title`.
