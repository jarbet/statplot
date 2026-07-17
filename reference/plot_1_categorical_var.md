# Plot Distribution of a Categorical Variable as a Stacked Bar

Create a single 100% stacked bar displaying the distribution of a
categorical variable.

## Usage

``` r
plot_1_categorical_var(
  data,
  var,
  text_inside_bars = c("count_and_percent", "none", "count", "percent"),
  fill_palette = NULL,
  bar_width = 0.8,
  border_color = "white",
  text_size = 4
)
```

## Arguments

- data:

  A data.frame.

- var:

  A categorical variable in `data`.

- text_inside_bars:

  Character specifying labels displayed within bar segments. One of
  `"count_and_percent"`, `"count"`, `"percent"`, or `"none"`.

- fill_palette:

  Optional named vector of fill colors passed to
  [`ggplot2::scale_fill_manual()`](https://ggplot2.tidyverse.org/reference/scale_manual.html).

- bar_width:

  Width of the stacked bar.

- border_color:

  Color of borders separating bar segments.

- text_size:

  Size of text labels displayed within bar segments.

## Value

A ggplot object.

## Examples

``` r
set.seed(123)

d <- data.frame(
    smoking = factor(
        sample(
            c("Never", "Former", "Current"),
            size = 1000,
            replace = TRUE,
            prob = c(0.55, 0.30, 0.15)
        ),
        levels = c("Never", "Former", "Current")
    )
)

plot_1_categorical_var(
    data = d,
    var = smoking,
    text_inside_bars = "count_and_percent"
)

```
