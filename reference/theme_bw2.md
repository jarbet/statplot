# Bold-styled Black and White Theme

A modified ggplot2 black and white theme with bold text elements.

## Usage

``` r
theme_bw2()
```

## Value

A ggplot2 theme object

## Examples

``` r
library(ggplot2)
ggplot(mtcars, aes(x = wt, y = mpg)) +
  geom_point() +
  labs(tag = "A") +
  theme_bw2()
```
