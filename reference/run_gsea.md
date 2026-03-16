# Run Gene Set Enrichment Analysis (GSEA)

A wrapper around
[`clusterProfiler::GSEA()`](https://rdrr.io/pkg/clusterProfiler/man/GSEA.html)
that accepts a pre-ranked gene vector and a gene-set data frame, runs
GSEA, and returns the result together with the inputs needed by
downstream plotting functions
([`plot_pathways()`](https://statgencore.github.io/statplot/reference/plot_pathways.md)
and
[`plot_pathway_correlation_network()`](https://statgencore.github.io/statplot/reference/plot_pathway_correlation_network.md)).

## Usage

``` r
run_gsea(
  gene_vec,
  term2gene,
  sort = TRUE,
  p_adjust_method = "BH",
  p_cutoff = 1,
  min_gs_size = 10,
  max_gs_size = 500,
  seed = 1,
  verbose = FALSE,
  ...
)
```

## Arguments

- gene_vec:

  named numeric vector Gene-level statistics (e.g. log2 fold change,
  t-statistic) named by gene symbol. The vector **must be sorted in
  decreasing order** before being passed in (or set `sort = TRUE`).

- term2gene:

  data.frame Two-column data frame mapping gene-set names to gene
  symbols. Column 1 must be the gene-set / term name; column 2 must be
  the gene symbol. Column names are coerced to `"term"` and `"gene"`
  internally.

- sort:

  logical(1) If `TRUE` (default), `gene_vec` is sorted in decreasing
  order before being passed to
  [`clusterProfiler::GSEA()`](https://rdrr.io/pkg/clusterProfiler/man/GSEA.html).

- p_adjust_method:

  character(1) Method passed to
  [`clusterProfiler::GSEA()`](https://rdrr.io/pkg/clusterProfiler/man/GSEA.html)
  for multiple-testing correction (default `"BH"`).

- p_cutoff:

  numeric(1) `pvalueCutoff` passed to
  [`clusterProfiler::GSEA()`](https://rdrr.io/pkg/clusterProfiler/man/GSEA.html)
  (default `1`, i.e. return all gene sets). Must be a single finite
  value in \[0, 1\].

- min_gs_size:

  integer(1) Minimum gene-set size (default `10`). Must be a single
  positive integer and no greater than `max_gs_size`.

- max_gs_size:

  integer(1) Maximum gene-set size (default `500`). Must be a single
  positive integer and no less than `min_gs_size`.

- seed:

  integer(1) Random seed set before calling
  [`clusterProfiler::GSEA()`](https://rdrr.io/pkg/clusterProfiler/man/GSEA.html)
  for reproducibility (default `1`). The caller's RNG state is saved
  before the seed is applied and fully restored on exit, so calling this
  function does **not** affect subsequent random operations in the
  session.

- verbose:

  logical(1) Passed to
  [`clusterProfiler::GSEA()`](https://rdrr.io/pkg/clusterProfiler/man/GSEA.html)
  (default `FALSE`).

- ...:

  Additional arguments forwarded to
  [`clusterProfiler::GSEA()`](https://rdrr.io/pkg/clusterProfiler/man/GSEA.html).

## Value

A list with three elements:

- `gsea_result`:

  A `gseaResult` object returned by
  [`clusterProfiler::GSEA()`](https://rdrr.io/pkg/clusterProfiler/man/GSEA.html).

- `gene_vec`:

  The (possibly sorted) named numeric vector that was passed to GSEA —
  use this as the `foldChange` argument of
  [`plot_pathways()`](https://statgencore.github.io/statplot/reference/plot_pathways.md).

- `term2gene`:

  The gene-set data frame (columns `term` and `gene`) that was passed to
  GSEA — use this as the `gene_sets` argument of
  [`plot_pathway_correlation_network()`](https://statgencore.github.io/statplot/reference/plot_pathway_correlation_network.md).

## Examples

``` r
if (FALSE) { # \dontrun{
# hallmark_t2g is bundled with the package (columns: term, gene)
data(hallmark_t2g)
set.seed(1)
all_genes <- unique(hallmark_t2g$gene)
gene_vec  <- setNames(rnorm(length(all_genes)), all_genes)

res <- run_gsea(gene_vec, term2gene = hallmark_t2g)
head(as.data.frame(res$gsea_result))
} # }
```
