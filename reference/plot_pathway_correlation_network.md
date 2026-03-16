# Gene–Gene correlation network for a single pathway

Computes pairwise Pearson correlations among genes belonging to a chosen
pathway and draws the resulting network. Edges are drawn only for gene
pairs whose absolute correlation meets `cor_thresh`. Node size encodes
**degree** (number of connections), so hub genes appear larger. Node
color encodes the gene-level statistic supplied via `log2fc` (e.g. log2
fold change).

## Usage

``` r
plot_pathway_correlation_network(
  expr,
  pathway,
  gene_sets,
  log2fc,
  top_n_genes = 30,
  cor_thresh = 0.6,
  cor_low = "steelblue",
  cor_mid = "grey85",
  cor_high = "firebrick",
  fc_low = RColorBrewer::brewer.pal(11, "PiYG")[10],
  fc_mid = RColorBrewer::brewer.pal(11, "PiYG")[6],
  fc_high = RColorBrewer::brewer.pal(11, "PiYG")[2],
  title = sprintf("Gene-Gene correlation network for pathway: %s", pathway),
  label_size = 3,
  label_bold = TRUE,
  show_size_legend = TRUE,
  seed = 42L
)
```

## Arguments

- expr:

  matrix Normalised expression matrix with **genes as rows** and
  **samples as columns**. `rownames` must be gene symbols.

- pathway:

  character(1) Name of the pathway to visualise. Must match a value in
  the `term` column of `gene_sets`.

- gene_sets:

  data.frame Two-column data frame with columns `"term"` and `"gene"`
  mapping pathway names to gene symbols. Typically the `term2gene`
  element returned by
  [`run_gsea()`](https://statgencore.github.io/statplot/reference/run_gsea.md).

- log2fc:

  named numeric vector Gene-level statistics (e.g. log2 fold change)
  used to color nodes. Names must be gene symbols. Missing genes are
  treated as 0.

- top_n_genes:

  integer(1) Before computing correlations, retain only the top
  `top_n_genes` genes ranked by `abs(log2fc)`. Reduces visual clutter on
  large pathways. Set to `Inf` to use all pathway genes (default `30`).
  Must be a single positive whole number (≥ 1) or `Inf`.

- cor_thresh:

  numeric(1) Minimum absolute Pearson correlation required to draw an
  edge between two genes (default `0.6`). Must be a single finite value
  in \[0, 1\].

- cor_low:

  character(1) Edge color for strongly negative correlations (default
  `"steelblue"`).

- cor_mid:

  character(1) Edge color at correlation = 0 (default `"grey85"`).

- cor_high:

  character(1) Edge color for strongly positive correlations (default
  `"firebrick"`).

- fc_low:

  character(1) Node color for strongly negative `log2fc` values.
  Defaults to the 10th color of the `PiYG` palette.

- fc_mid:

  character(1) Node color at `log2fc = 0`. Defaults to the neutral
  midpoint of the `PiYG` palette.

- fc_high:

  character(1) Node color for strongly positive `log2fc` values.
  Defaults to the 2nd color of the `PiYG` palette.

- title:

  character(1) Plot title. Defaults to
  `"Gene-Gene correlation network for pathway: <pathway>"`.

- label_size:

  numeric(1) Gene label font size (default `3`).

- label_bold:

  logical(1) Whether gene labels are bold (default `TRUE`).

- show_size_legend:

  logical(1) Whether to display the node-size legend ("# connections")
  (default `TRUE`).

- seed:

  integer(1) or `NULL`. Random seed used for the Fruchterman-Reingold
  layout so the graph is drawn the same way each time. The caller's RNG
  state is saved before the seed is set and fully restored on exit, so
  using this function does **not** affect subsequent random operations
  in the session. Pass `NULL` to skip seeding entirely (default `42L`).

## Value

A ggplot2 object, or `NULL` (invisibly) when fewer than 3 pathway genes
are present in `expr` or no gene pairs pass `cor_thresh`.

## Details

Unlike
[`plot_pathways()`](https://statgencore.github.io/statplot/reference/plot_pathways.md),
which uses network layout to reflect shared pathway membership, this
plot shows actual co-expression structure among genes within a single
pathway.

## Examples

``` r
data(ex_expr_pathway)
data(ex_log2fc_pathway)
data(hallmark_t2g)

plot_pathway_correlation_network(
    expr      = ex_expr_pathway,
    pathway   = "MYC_TARGETS_V1",
    gene_sets = hallmark_t2g,
    log2fc    = ex_log2fc_pathway
)

```
