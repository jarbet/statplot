# Gene–Pathway network plot (cnetplot)

Visualises the top GSEA pathways as a bipartite network where pathway
nodes are connected to their constituent gene nodes. Gene nodes are
colored by the gene-level statistic used for GSEA (e.g. log2 fold
change). Node proximity reflects **shared pathway membership** (network
layout), *not* gene–gene correlation — see
[`plot_pathway_correlation_network()`](https://github.com/jarbet/statplot/reference/plot_pathway_correlation_network.md)
for co-expression structure.

## Usage

``` r
plot_pathways(
  gsea_result,
  fold_change,
  show_category = 5,
  fc_threshold = 1.5,
  size_item = 0.7,
  size_edge = 0.5,
  category_color = "black",
  category_size = 4,
  item_color = "grey30",
  item_size = 2.5,
  title = "Gene-Pathway network (GSEA)"
)
```

## Arguments

- gsea_result:

  A `gseaResult` object returned by
  [`run_gsea()`](https://github.com/jarbet/statplot/reference/run_gsea.md)
  or
  [`clusterProfiler::GSEA()`](https://rdrr.io/pkg/clusterProfiler/man/GSEA.html).

- fold_change:

  named numeric vector Gene-level statistics used to color gene nodes
  (e.g. log2 fold change, t-statistic). Names must be gene symbols
  matching those in `gsea_result`. Typically the `gene_vec` element
  returned by
  [`run_gsea()`](https://github.com/jarbet/statplot/reference/run_gsea.md).

- show_category:

  integer(1) Number of top pathways to display (default `5`). Must be a
  single positive whole number.

- fc_threshold:

  numeric(1) Only show gene nodes whose
  `abs(fold_change) >= fc_threshold` (default `1.5`). Set to `0` to show
  all genes. Must be a single finite non-negative value.

- size_item:

  numeric(1) Relative size of gene circles/nodes (default `0.7`). Must
  be a single positive value.

- size_edge:

  numeric(1) Relative thickness of edges (default `0.5`). Must be a
  single positive value.

- category_color:

  character(1) Color of pathway label text (default `"black"`).

- category_size:

  numeric(1) Font size of pathway labels (default `4`). Must be a single
  positive value.

- item_color:

  character(1) Color of gene label text (default `"grey30"`).

- item_size:

  numeric(1) Font size of gene labels (default `2.5`). Must be a single
  positive value.

- title:

  character(1) Plot title (default `"Gene-Pathway network (GSEA)"`).

## Value

A ggplot2 object.

## Examples

``` r
# hallmark_t2g is bundled with the package (columns: term, gene)
data(hallmark_t2g)
set.seed(1)
all_genes <- unique(hallmark_t2g$gene)
gene_vec  <- setNames(rnorm(length(all_genes)), all_genes)

res <- run_gsea(gene_vec, term2gene = hallmark_t2g)

plot_pathways(
    gsea_result   = res$gsea_result,
    fold_change   = res$gene_vec,
    show_category = 5
)

```
