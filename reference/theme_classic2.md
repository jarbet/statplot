# Bold-styled Classic Theme

A modified ggplot2 classic theme with bold text elements.

## Usage

``` r
theme_classic2()
```

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
