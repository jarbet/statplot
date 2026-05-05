# statplot (development version)

## Added

* `data(hallmark_pathway_categories)` has 2 new columns that give more human-readable labels that can be used for plots or in-text reference

## Changed

...

## Fixed

...

# statplot 0.7.0 - 2026-05-04

## Added

* `patchwork_rowtitle`: new function to create row titles for patchwork compositions. Produces a ggplot object with centered text in a customizable box, suitable for use as either an entire row spanning all columns or as a narrow left-side column. Supports customization of text position/size (`x`, `y`, `size`, `hjust`, `vjust`), background styling (`fill`, `color`, `linewidth`), and plot margins (`margin_top`, `margin_right`, `margin_bottom`, `margin_left`).

* `quarto_note`: new function to render colored reviewer or collaborator notes in Quarto documents. Produces colored text in HTML, colored LaTeX in PDF, and custom paragraph styles in Word documents. Supports customizable text color and Word style names.

## Changed

* `theme_bw2` and `theme_classic2`: refactored to use an internal `theme_base()` helper function
  that consolidates common bold styling (titles, axis labels, legend titles, and plot tags).
  Both themes now apply `theme_base()` consistently, reducing code duplication.

* `plot_pvalue_barplot`: new `exponentiate_labels` argument (default `FALSE`) allows formatting
  x-axis labels with exponential notation and superscripts (e.g., 1, 10^-1, 10^-2, 10^-3, <10^-x)
  when `mlog10_transform_pvalue = TRUE`. Uses R expressions via `bquote()` for proper rendering of
  mathematical notation in plots. When `exponentiate_labels = TRUE` without `mlog10_transform_pvalue = TRUE`,
  a warning is issued.

* `plot_barplot_by_group`: major refactoring for more flexible faceting support:
  - **Breaking changes**: removed `group_col` parameter (no longer auto-facets by group); removed `show_text_groups` parameter; removed `strip_position` parameter
  - **New parameter**: `facet_cols` (optional character vector) supports multi-level faceting via user-supplied column names; when supplied, significance brackets are computed per unique combination of those columns
  - **API changes**: `condition_col`, `mean_col`, `error_col`, and `p_col` are now required arguments (previously had defaults)
  - **Faceting approach**: users now explicitly add `+ ggplot2::facet_wrap()` or `+ ggplot2::facet_grid()` after the function call; bracket annotations facet automatically via retained grouping columns
  - **Updated examples**: demonstrate single-panel, single-faceting, multi-faceting, and custom label workflows

* `plot_heatmap`: added `row_title_gp` and `column_title_gp` parameters to allow customization of row and column split label styling. Both parameters accept `grid::gpar()` objects and default to `grid::gpar(fontface = "bold")` for bold split labels.

* `plot_heatmap`: added `annotation_legend_param` to customize annotation legend titles and appearance for row and column covariates, including support for overriding auto-generated titles when `merge_legends = TRUE`.

## Fixed

* `plot_heatmap`: fixed factor covariate legend rendering when a factor covariate
  exists only on one side (column-only or row-only). Previously, combining a `NULL`
  value with a factor vector via `c(NULL, factor_vector)` would drop factor
  attributes and convert the result to integer codes (`"1"`, `"2"`, `"3"`),
  which then appeared in the legend and color mapping instead of proper factor
  labels. Now converts each side (row and column) to character independently
  before combining, preserving factor label names in all cases.

* `plot_confidence_intervals`: fixed y-axis alignment when displaying grouped
  data with a p-value barplot. Previously, the y-axis limits of the confidence
  interval plot were calculated based on the dodged `y_pos` values, causing
  misalignment with the p-value plot's fixed limits. Now uses fixed y-axis limits
  that match the p-value panel, ensuring perfect alignment of tick marks
  regardless of dodging.

# statplot 0.6.0 - 2026-04-15

## Added

* `table_basic`: new function for creating clean, formatted tables using
  tinytable. Supports column-level rounding control via named lists or vectors,
  optional comma formatting for large numbers, and configurable handling of
  missing values. By default uses bold header rows and can respect column
  `label` attributes.

## Changed

* `plot_pvalue_barplot`: expanded examples to demonstrate 6 use cases including
  default p/q-value overlay, basic transformation with vline, raw p-value scale,
  fill mapping by group, custom significance thresholds, and custom q-values.

* `plot_pvalue_barplot`: default `-log10(p)` x-axis breaks now use meaningful
  p-value ticks (`1, 0.1, 0.01, 0.001`) instead of equally-spaced
  integer breaks; the smallest p-value tick is labeled with a `"<"` prefix
  (e.g., `"<0.001"`) to indicate that values below this threshold are represented,
  and the label is determined by the largest break on the -log10 scale
  (not hard-coded to `0.001`) for robust support of custom breaks.

* `plot_pathways`: default `color_low`, `color_mid`, and `color_high` changed
  from `NULL` to `"blue"`, `"white"`, and `"red"`, respectively, providing a
  sensible default diverging color palette without requiring explicit user
  specification.

* `plot_pathways`, `plot_dotmap`: new `legend_position` argument (default `"right"`) allows
  users to control the legend position via `ggplot2::theme(legend.position = ...)`.
  Common values include `"right"`, `"left"`, `"top"`, `"bottom"`, or `"none"`
  to hide the legend.

* `combine_pvalues`: now allows p-values equal to 1 (previously rejected);
  validation changed from `p > 0 & p < 1` to `p > 0 & p <= 1`.

* `plot_confidence_intervals`: refactor — replace the monolithic `style`
  parameter with independent arguments for visual encoding and layout:
  `color_col`, `color_values`, `shape_col`, and `point_shapes` (for
  shape-based grouping), plus separator and reference-line controls
  (`show_separators`, `sep_linetype`, `sep_linewidth`, `sep_color`,
  `vline_xintercept`, `vline_linetype`, `vline_color`) and `dodge_width`.
  This makes styling explicit and composable and simplifies legend
  handling.

* `plot_confidence_intervals`: ensure `shape_col` is coerced to a factor
  with a stable, sorted level ordering; validate `point_shapes` length
  against the number of shape levels and provide clearer legend
  overrides when `color_col` and/or `shape_col` are used.


* `plot_2_categorical_vars`: added `inside_bar_text_bold` argument to allow
  inside-bar labels to be drawn in bold when requested; documentation and
  examples updated to demonstrate `yvar_text_colors` combined with the new
  bold-text option.
* `plot_2_categorical_vars`: updated examples

* All roxygen2 examples for plot functions now begin with `ggplot2::theme_set(theme_bw2())`
  to apply the package theme consistently across all example code blocks.

## Fixed

* `plot_pvalue_barplot`: fixed legend linetype to remove unwanted line rendering
  for fill-mapped bars by applying `override.aes = list(linetype = 0)` to the
  fill legend so legend keys display as solid/filled swatches.

* `plot_covariate_heatmap`: removed whitespace padding between covariate bars and plot borders by applying `expand = ggplot2::expansion(0)` to both applicable scales in vertical and horizontal layouts.


# statplot 0.5.0 - 2026-04-10

## Added

* `theme_bw2` and `theme_classic2`: commonly used themes

* `plot_confidence_intervals`: added `pvalue_plot_margin` argument to parameterize
  the p-value panel plot margin (points), default `c(5.5, 12, 5.5, 0)`. This
  prevents x-axis labels on the appended p-value barplot from being clipped
  when the panel is composed to the right using `patchwork`.

* `hallmark_pathway_categories`: new internal dataset mapping Hallmark
pathway names (the `term` column, with the `HALLMARK_` prefix removed) to
biological process categories as defined in Table 1 of Liberzon et al.
(2015). The dataset includes a runtime validation step against the bundled
`hallmark_t2g` data to ensure pathway names align with MSigDB.

* `plot_covariate_heatmap`: new function to draw vertical covariate strips
  (one strip per covariate). Supports categorical covariates (named color
  vectors) only.

* `plot_barplot_by_group`: new two-condition bar plot function for comparing a
  numeric outcome across groups. Supports `error_direction = c("both", "up")`,
  configurable `bar_width`, `bar_gap`, and `bar_padding`, optional facet layout
  via `facet`/`strip_position`, and custom bracket labels via `label_col`.

* `plot_pathways`: new `pathway_cats` and `pathway_cat_colors` arguments let
  users fill pathway nodes by biological process category. The accompanying
  `legend_pathway_fill_title` and `legend_pathway_fill_dot_size` arguments
  customise the category fill legend, while the gene effect-size colour scale
  remains independent.


## Changed

* `plot_dot_whiskers` has been renamed to `plot_confidence_intervals`. The function signature and behavior have been significantly enhanced:
  - **Parameter renames for clarity**: `x` → `effect_size`, `xmin` → `ci_low`, `xmax` → `ci_high`, `label_col` → `id`
  - **New parameters**:
    + `effect_size`, `ci_low`, `ci_high`, and `id` are now required arguments (previously optional with defaults)
    + `color_col`: optional column for coloring segments and points (can be group-based or id-based)
    + `color_values`: optional named character vector for custom colors
    + `combine_pvalue_method`: method for combining p-values when `group_col` is supplied (default `"fisher"`)
  - **Enhanced p-value panel logic**: When `group_col` is specified, p-values are now automatically combined across groups for each `id` using the specified method, eliminating the need for pre-computed combined p-values. When `group_col` is `NULL`, individual p-values are displayed directly.
  - **Improved styling**: Supports shape-based and color-based group distinction, with flexible color and shape customization options

* `plot_barplot_by_group`: changed `bracket_offset` default from `0.08` to `1.0` and updated the parameter documentation to clarify that it is now an absolute distance in data units (formerly a fraction of the y range).

* `plot_dotmap`: added a short example demonstrating `only_show_top_sig` (shows top N rows in the combined p-value barplot) and updated the `combine_pvalue_method` parameter documentation to reference `combine_pvalues` for method details.

* `combine_pvalues`: default `methods` changed from `'all'` to `'CMC'`; documentation and tests updated to reflect the new default.

* `plot_dotmap`: the default `combine_pvalue_method` is now `"fisher"` (was previously `"CMC"` when not supplied via `match.arg()`); documentation and examples updated to reflect the new default choice.

* `plot_pvalue_barplot`: improved legend behaviour — when a vertical significance line legend is requested (`vline = TRUE, vline_legend = TRUE`) the colour scale now stores an explicit named `values` vector so code and tests can programmatically find the named alpha entry; also the fill legend uses `override.aes` to remove line linetype in legend keys when showing p/q bars so legend keys render as solid/filled swatches.
* `plot_pvalue_barplot`: added `vline_legend` to optionally include or suppress the vertical significance line legend key.
* `plot_pathways`: default `legend_pathway_size_title` changed to `"Num. genes\n in pathway"` and default `legend_color_title` changed to `"Gene effect size"`.

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
