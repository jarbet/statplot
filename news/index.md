# Changelog

## statplot (development version)

### Added

- …

### Changed

- …

### Fixed

- …

## statplot 0.3.0 - 2026-03-20

### Added

- `run_gsea`: wrapper around
  [`clusterProfiler::GSEA()`](https://rdrr.io/pkg/clusterProfiler/man/GSEA.html)
  for gene set enrichment analysis on a pre-ranked gene vector; returns
  results alongside the inputs needed by the pathway plotting functions
  below.
- `plot_pathways`: gene–pathway bipartite network (cnetplot) showing the
  top GSEA pathways and their constituent genes, with nodes colored by
  fold change.
- `plot_pathway_correlation_network`: gene–gene Pearson correlation
  network for a single pathway, with edges filtered by a correlation
  threshold and nodes colored by fold change and sized by degree (hub
  genes appear larger).
- Example datasets `ex_expr_pathway`, `ex_log2fc_pathway`, and
  `hallmark_t2g` bundled to support pathway analysis examples and
  vignettes.
- `plot_numeric_by_3plusgroups` for creating violin + boxplots of a
  numeric variable by a grouping factor, with optional effect size and
  pairwise Wilcoxon test letters.
- `plot_numeric_by_2groups` for creating violin + boxplots of a numeric
  variable by a grouping factor with exactly 2 levels

### Changed

- `plot_2_categorical_vars`: new `inside_bar_stats` argument controls
  what is printed inside the stacked bars — `"pct"` (default,
  within-group percentage), `"n"` (count only), `"n_and_pct"` (count and
  percentage formatted as `n (pct%)`), or `"none"` (no labels).
- `plot_2_categorical_vars`: new `include_overall_bar` argument (default
  `FALSE`); when `TRUE`, a pooled bar showing the marginal distribution
  of `yvar` across all observations is prepended to the left of the
  per-group bars and separated by a vertical line. The bar respects
  `inside_bar_stats` and displays the total N label above it like all
  other bars. The bar label defaults to `"Overall"` and can be
  customised via the new `overall_label` argument.
- `plot_dotmap`: new `q` argument accepts an optional column name of
  per-cell q-values (e.g. FDR-adjusted p-values) to use for cell shading
  instead of raw p-values. When both `p` and `q` are supplied the
  combined p-value barplot (right panel) is still computed from `p`, so
  Fisher/CMC combination uses raw p-values while cells reflect adjusted
  values.
- `plot_pvalue_barplot`: `NA` p-values are now handled gracefully — rows
  with missing p-values are silently dropped rather than causing an
  error, and BH correction for q-values is applied only to the
  non-missing subset so adjusted values are not distorted.
- Use
  [`stats::reformulate()`](https://rdrr.io/r/stats/delete.response.html)
  with backticks when building formulas in numeric-by-group plotting
  functions to support non-syntactic column names (fixes issue with
  spaces/hyphens).

### Fixed

- …

## statplot 0.2.0 - 2026-03-06

### Added

- `plot_dot_whiskers` for plotting effect sizes and confidence
  intervals, optionally by group and with pvalue barplot
- `plot_heatmap`
  - By default, white gridlines around cells
  - Add support for categorical heatmaps
  - Support for clustering categorical data
- `quarto_html_tabset_list`: convert a named list object to a tabset
  when knitting to HTML
- `format_pvalue`: function to format pvalues to display in plots
- `combine_pvalues` function to combine pvalues to produce a single
  overall pvalue
  - Updated methods for contemporary best practices
- `plot_pvalue_barplot`: horizontal barplot of pvalues
  - By default now shows both pvalues and qvalues stacked
- `plot_dotmap`: plot heatmap of effect sizes (dots) with cells shaded
  by pvalue
  - option to add combined pvalue barplot on the right
    - Changed default combo method to CMC as recommended by literature
  - option to show q-values for the combined pvalue barplot
  - 5 methods are supported when combining pvalues
  - show an x symbol if dot is missing

## statplot 0.1.0 - 2026-01-01

Setup first working draft.
