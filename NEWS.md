# statplot (development version)

## Added

* `plot_dot_whiskers`: added `pvalue_plot_margin` argument to parameterize
  the p-value panel plot margin (points), default `c(5.5, 12, 5.5, 0)`. This
  prevents x-axis labels on the appended p-value barplot from being clipped
  when the panel is composed to the right using `patchwork`.

* `hallmark_pathway_categories`: new internal dataset mapping Hallmark
pathway names (the `term` column, with the `HALLMARK_` prefix removed) to
biological process categories as defined in Table 1 of Liberzon et al.
(2015). The dataset includes a runtime validation step against the bundled
`hallmark_t2g` data to ensure pathway names align with MSigDB.


## Changed

* `plot_dotmap`: added a short example demonstrating `only_show_top_sig` (shows top N rows in the combined p-value barplot) and updated the `combine_pvalue_method` parameter documentation to reference `combine_pvalues` for method details.

* `combine_pvalues`: default `methods` changed from `'all'` to `'CMC'`; documentation and tests updated to reflect the new default.

## Fixed

* `plot_dotmap`: `combine_pvalues()` is now called with `methods = combine_pvalue_method` instead of subsetting the result by name. Previously, `combine_pvalues()` defaulted to computing only `"CMC"`, so requesting any other method (e.g. `"fisher"`) returned `NA`, causing the combined p-value barplot to draw no bars.

# statplot 0.4.0 - 2026-03-26

## Added

* `plot_data_avail_by_group`: tile plot showing availability (0/1) of data types across groups, with a black border, no whitespace padding, and optional bold axis labels.
* `plot_pathways`: new `legend_fixed_dot_size` argument (numeric vector) fixes the size-legend breaks and limits to user-supplied gene-count values so that multiple plots composed with `patchwork` share an identical visual size scale. Values outside the specified range are squished to the nearest extreme rather than dropped.

## Changed

* `plot_pathways`: the `fold_change` argument has been renamed to `effect_size`. Code using `fold_change = ...` must be updated.
* `plot_pathways`: default `effect_size_threshold` changed from `1.5` to `0` (show all genes by default).
* `plot_pathways`: out-of-bounds gene values are now squished (via `scales::squish`) rather than censored to `NA` for all color scale paths (diverging, sequential, and in-place `colorkey_limits`), and for the size scale when `legend_fixed_dot_size` is set. This prevents genes outside `colorkey_limits` from rendering as grey.
* `plot_heatmap`: when `anno_colors` is supplied for a categorical covariate, all user-specified color levels now always appear in the annotation legend—including levels absent from the current data (e.g. when plotting a subset). To show only levels present in the data, omit the unwanted levels from the `anno_colors` named vector.
* `plot_heatmap`: added a new `merge_legends` argument (default `FALSE`). When `TRUE` the function:
  - deduplicates annotation legends whose color mappings are semantically equivalent (named vectors are compared after sorting by name; `circlize::colorRamp2` functions are compared by their breakpoints and colours), combining their titles (e.g. `"pvalue\nqvalue"`) and suppressing redundant legends;
  - forwards the flag to `ComplexHeatmap::draw(..., merge_legends = TRUE)` so heatmap and annotation legends are packed together.
  Unit tests covering the deduplication and semantic comparison behaviour were added.

## Fixed

* `plot_dotmap`: `custom_qvalues` passed via `...` now correctly drives the q-value bars in the combined p-value barplot. Previously the argument was stripped before being forwarded to `plot_pvalue_barplot()`, causing the barplot to always display internally computed BH-adjusted q-values instead of the user-supplied ones. The supplied column is now joined from the input data into the combined-p data frame and forwarded as `custom_qvalues`; when not supplied, the BH fallback is used as before.

# statplot 0.3.0 - 2026-03-20

## Added

* `run_gsea`: wrapper around `clusterProfiler::GSEA()` for gene set enrichment
  analysis on a pre-ranked gene vector; returns results alongside the inputs
  needed by the pathway plotting functions below.
* `plot_pathways`: gene–pathway bipartite network (cnetplot) showing the top
  GSEA pathways and their constituent genes, with nodes colored by fold change.
* `plot_pathway_correlation_network`: gene–gene Pearson correlation network for
  a single pathway, with edges filtered by a correlation threshold and nodes
  colored by fold change and sized by degree (hub genes appear larger).
* Example datasets `ex_expr_pathway`, `ex_log2fc_pathway`, and `hallmark_t2g`
  bundled to support pathway analysis examples and vignettes.
* `plot_numeric_by_3plusgroups` for creating violin + boxplots of a numeric variable by a grouping factor, with optional effect size and pairwise Wilcoxon test letters.
* `plot_numeric_by_2groups` for creating violin + boxplots of a numeric variable by a grouping factor with exactly 2 levels

## Changed

* `plot_pathways`: several arguments renamed for clarity:
    + `fc_threshold` → `effect_size_threshold`
    + `fc_threshold_label` → `subtitle_effect_size_label`
    + `show_category` → `show_pathways`
    + `size_item` → `gene_node_size`
    + `size_edge` → `line_size`
    + `category_color` / `category_size` → `pathway_color` / `pathway_label_size`
    + `item_color` / `item_size` → `gene_color` / `gene_label_size`
* `plot_pathways`: new `max_genes_shown` argument adaptively raises
  `effect_size_threshold` so that at most `max_genes_shown` gene nodes are
  displayed; the chosen cutoff is reported in an auto-generated plot subtitle
  (subtitle is suppressed when `max_genes_shown` is `NULL`).
* `plot_pathways`: new `subtitle_effect_size_label` argument controls the
  effect-size label shown inside `abs()` in the subtitle (default
  `"effect size"`).
* `plot_pathways`: new `legend_pathway_size_title` (default `"Num. genes"`) and
  `legend_color_title` (default `"Effect size"`) arguments to customise legend
  titles for node size and color, respectively.
* `plot_pathways`: `legend_color_title` now accepts plotmath expressions via `expression()`; validation and documentation updated.
* `plot_pathways`: new `colorkey_breaks` and `colorkey_limits` arguments to
  control the tick-mark values and range shown on the color legend.
* `plot_pathways`: new `color_low`, `color_mid`, and `color_high` arguments to
  customise the color scale.  Supplying all three produces a 3-color diverging
  scale; omitting `color_mid` produces a 2-color sequential scale; leaving all
  three `NULL` preserves the default cnetplot palette.
* `plot_pathways`: new `plot_margin` argument (`c(top, right, bottom, left)` in
  lines, default `c(0.5, 0.5, 0.5, 0.5)`) to control whitespace around the plot and
  prevent node labels from being clipped at the edges.
* `plot_2_categorical_vars`: new `inside_bar_stats` argument controls what is printed inside the stacked bars — `"pct"` (default, within-group percentage), `"n"` (count only), `"pct_and_n"` (percentage and count formatted as `pct% (n)`), or `"none"` (no labels).
* `plot_2_categorical_vars`: new `include_overall_bar` argument (default `FALSE`); when `TRUE`, a pooled bar showing the marginal distribution of `yvar` across all observations is prepended to the left of the per-group bars and separated by a vertical line. The bar respects `inside_bar_stats` and displays the total N label above it like all other bars. The bar label defaults to `"Overall"` and can be customised via the new `overall_label` argument.
* `plot_dotmap`: new `q` argument accepts an optional column name of per-cell q-values (e.g. FDR-adjusted p-values) to use for cell shading instead of raw p-values. When both `p` and `q` are supplied the combined p-value barplot (right panel) is still computed from `p`, so Fisher/CMC combination uses raw p-values while cells reflect adjusted values.
* `plot_pvalue_barplot`: `NA` p-values are now handled gracefully — rows with missing p-values are silently dropped rather than causing an error, and BH correction for q-values is applied only to the non-missing subset so adjusted values are not distorted.
* Use `stats::reformulate()` with backticks when building formulas in numeric-by-group plotting functions to support non-syntactic column names (fixes issue with spaces/hyphens).

# statplot 0.2.0 - 2026-03-06

## Added

* `plot_dot_whiskers` for plotting effect sizes and confidence intervals, optionally by group and with pvalue barplot
* `plot_heatmap`
    + By default, white gridlines around cells
    + Add support for categorical heatmaps
    + Support for clustering categorical data
* `quarto_html_tabset_list`: convert a named list object to a tabset when knitting to HTML
* `format_pvalue`: function to format pvalues to display in plots
* `combine_pvalues` function to combine pvalues to produce a single overall pvalue
    + Updated methods for contemporary best practices
* `plot_pvalue_barplot`: horizontal barplot of pvalues
    + By default now shows both pvalues and qvalues stacked
* `plot_dotmap`: plot heatmap of effect sizes (dots) with cells shaded by pvalue
    + option to add combined pvalue barplot on the right
        + Changed default combo method to CMC as recommended by literature
    + option to show q-values for the combined pvalue barplot
    + 5 methods are supported when combining pvalues
    + show an x symbol if dot is missing

# statplot 0.1.0 - 2026-01-01

Setup first working draft.
