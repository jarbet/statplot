# Plot a covariate heatmap

Displays one vertical strip per covariate, where each strip is an
independent
[`Heatmap`](https://rdrr.io/pkg/ComplexHeatmap/man/Heatmap.html)
combined into a
[`HeatmapList`](https://rdrr.io/pkg/ComplexHeatmap/man/HeatmapList.html).
Both categorical (named-vector colors) and continuous
([`colorRamp2`](https://rdrr.io/pkg/circlize/man/colorRamp2.html)
function) variables are supported.

## Usage

``` r
plot_covariate_heatmap(
  dataset,
  color_map,
  row_id_var = NULL,
  row_split_var = NULL,
  cluster_rows = FALSE,
  show_row_names = TRUE,
  row_names_side = "left",
  ht_gap = grid::unit(0.2, "mm"),
  row_gap = grid::unit(0.2, "mm"),
  legend_side = "left",
  merge_legends = FALSE,
  return_details = FALSE,
  ...
)
```

## Arguments

- dataset:

  A data frame. Must contain all columns named in `color_map` and, if
  supplied, `row_id_var` and `row_split_var`.

- color_map:

  A named list specifying which covariates to plot and their color
  mappings. Each name must be a column in `dataset`. Each element is
  either:

  - A **named character vector** mapping factor levels to hex colors
    (for categorical/character/factor columns). Any levels present in
    the data but absent from the vector receive auto-generated colors.

  - A **colorRamp2 function** (for continuous/numeric columns), e.g. one
    created by
    [`colorRamp2`](https://rdrr.io/pkg/circlize/man/colorRamp2.html).

- row_id_var:

  Character. Column in `dataset` used as row labels (matrix rownames).
  Default `NULL` uses `1:nrow(dataset)`.

- row_split_var:

  Character. Column in `dataset` used to split rows into groups. Must
  have \\\geq 2\\ distinct levels. Default `NULL`.

- cluster_rows:

  Logical. Whether to cluster rows. When `TRUE`, clustering is performed
  on the first covariate strip and the resulting row order is shared
  across all strips. Default `FALSE`.

- show_row_names:

  Logical. Default `TRUE`.

- row_names_side:

  `"left"` or `"right"`. Row names are shown on the first strip when
  `"left"` and the last strip when `"right"`. Default `"left"`.

- ht_gap:

  A [`unit`](https://rdrr.io/r/grid/unit.html) object controlling the
  gap between adjacent covariate strips. Passed as `ht_gap` to
  [`draw`](https://rdrr.io/pkg/ComplexHeatmap/man/draw-dispatch.html).
  Default `grid::unit(0.2, "mm")`. Set to `grid::unit(0, "mm")` to
  remove all gaps.

- row_gap:

  A [`unit`](https://rdrr.io/r/grid/unit.html) object controlling the
  gap between row-split groups. Passed as `row_gap` to each
  [`Heatmap`](https://rdrr.io/pkg/ComplexHeatmap/man/Heatmap.html) call.
  Default `grid::unit(0.2, "mm")`. Set to `grid::unit(0, "mm")` to
  remove row gaps entirely.

- legend_side:

  Character. Position of the legends. One of `"left"`, `"right"`,
  `"top"`, or `"bottom"`. Passed as both `heatmap_legend_side` and
  `annotation_legend_side` to
  [`draw`](https://rdrr.io/pkg/ComplexHeatmap/man/draw-dispatch.html).
  Default `"left"`.

- merge_legends:

  Logical. When `TRUE`: (1) strips sharing the exact same color mapping
  are collapsed into one legend whose title joins the covariate names
  with `"\n"`; (2) the value is forwarded as `merge_legends` to
  [`draw`](https://rdrr.io/pkg/ComplexHeatmap/man/draw-dispatch.html).
  Default `FALSE`.

- return_details:

  Logical. If `TRUE`, returns a named list with elements `ht` (the drawn
  `HeatmapList`) and `final_colors` (the resolved color map). Default
  `FALSE`.

- ...:

  Additional arguments passed to every
  [`Heatmap`](https://rdrr.io/pkg/ComplexHeatmap/man/Heatmap.html) call
  (e.g. `column_names_rot`, `width`, `column_title_gp`).

## Value

Invisibly returns the drawn
[`HeatmapList`](https://rdrr.io/pkg/ComplexHeatmap/man/HeatmapList.html)
object, or a named list with elements `ht` and `final_colors` when
`return_details = TRUE`.

## Examples

``` r
data(ex_data_heatmap)

# Build a one-row-per-sample metadata frame
sample_meta <- ex_data_heatmap |>
  dplyr::select(sample, group, condition, sample_type, qc_score) |>
  dplyr::distinct()

rng_qc <- range(sample_meta$qc_score, na.rm = TRUE)
col_fun_qc <- circlize::colorRamp2(
  c(rng_qc[1], mean(rng_qc), rng_qc[2]),
  c("#ffffcc", "#41b6c4", "#0c2c84")
)

# Multiple covariates (categorical + continuous)
plot_covariate_heatmap(
  dataset = sample_meta,
  color_map = list(
    group       = c(G1 = "#1b9e77", G2 = "#d95f02"),
    condition   = c(healthy = "#b3de69", EAE = "#fccde5"),
    sample_type = c(input = "#8dd3c7", IP = "#80b1d3"),
    qc_score    = col_fun_qc
  ),
  row_id_var    = "sample",
  row_split_var = NULL,
  cluster_rows  = FALSE
)


# Single categorical covariate bar
plot_covariate_heatmap(
  dataset    = sample_meta,
  color_map  = list(group = c(G1 = "#1b9e77", G2 = "#d95f02")),
  row_id_var = "sample"
)


# Single continuous covariate bar
plot_covariate_heatmap(
  dataset    = sample_meta,
  color_map  = list(qc_score = col_fun_qc),
  row_id_var = "sample"
)
```
