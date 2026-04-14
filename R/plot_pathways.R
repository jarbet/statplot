#' Gene–Pathway network plot (cnetplot)
#'
#' Visualises the top GSEA pathways as a bipartite network where pathway nodes
#' are connected to their constituent gene nodes.  Gene nodes are colored by
#' the gene-level statistic used for GSEA (e.g. log2 fold change).  Node
#' proximity reflects **shared pathway membership** (network layout), *not*
#' gene–gene correlation — see [plot_pathway_correlation_network()] for
#' co-expression structure.
#'
#' @param gsea_result A `gseaResult` object returned by [run_gsea()] or
#'   [clusterProfiler::GSEA()].
#' @param effect_size named numeric vector. Gene-level statistics used to color
#'   gene nodes (e.g. log2 fold change, t-statistic).  Names must be gene
#'   symbols matching those in `gsea_result`.  Typically the `gene_vec`
#'   element returned by [run_gsea()].
#' @param show_pathways integer(1) Number of top pathways to display (default
#'   `5`).  Must be a single positive whole number.
#' @param effect_size_threshold numeric(1) Only show gene nodes whose
#'   `abs(effect_size) >= effect_size_threshold` (default `0`).  Set to `0`
#'   to show all genes.  Must be a single finite non-negative value.
#' @param subtitle_effect_size_label character(1) String placed inside `abs()` in the
#'   auto-generated subtitle when a threshold is applied
#'   (default `"effect size"`).  Change to match your effect-size metric, e.g.
#'   `"log2FC"`, `"t-statistic"`, or `"z-score"`.
#' @param max_genes_shown integer(1) Maximum number of gene nodes to display
#'   (default `NULL`, no limit).  If the number of genes belonging to the top
#'   `show_pathways` pathways and passing `effect_size_threshold` exceeds this value,
#'   the threshold is raised adaptively (via quantile of `abs(effect_size)`
#'   among pathway genes) until at most `max_genes_shown` genes remain.  The
#'   effective threshold will never drop below `effect_size_threshold`.  Must be
#'   a single positive whole number.
#' @param gene_node_size numeric(1) Relative size of gene circles/nodes (default
#'   `0.7`).  Must be a single positive value.
#' @param line_size numeric(1) Relative thickness of edges (default `0.5`).
#'   Must be a single positive value.
#' @param pathway_color character(1) Color of pathway label text (default
#'   `"black"`).
#' @param pathway_label_size numeric(1) Font size of pathway labels (default `4`).
#'   Must be a single positive value.
#' @param pathway_cats named character vector or `NULL` (default `NULL`).
#'   Optionally maps pathway IDs to a biological process category name
#'   (e.g. `c(MTORC1_SIGNALING = "Signaling", UV_RESPONSE_DN = "DNA damage")`).
#'   Partial mappings are allowed: only pathway nodes whose IDs appear in
#'   `names(pathway_cats)` receive a category fill color; any displayed pathway
#'   not present in `names(pathway_cats)` is left unfilled (transparent overlay).
#'   Must be used together with `pathway_cat_colors`.
#'   `data(hallmark_pathway_categories)` provides a ready-to-use
#'   term-to-category mapping for MSigDB Hallmark gene sets.
#' @param pathway_cat_colors named character vector or `NULL` (default `NULL`).
#'   Maps each category name to a color string (hex or named R color),
#'   e.g. `c(Signaling = "#e41a1c", "DNA damage" = "#ff7f00")`.  Must cover
#'   every category value that appears in `pathway_cats`.  Node shape is set
#'   to `21` (filled circle with border) so `fill` and `colour` remain
#'   independent aesthetics — the gene fold-change gradient on `colour` is
#'   unaffected.  Must be used together with `pathway_cats`.
#' @param legend_pathway_fill_title character(1) or `NULL`.  Title for the
#'   pathway-fill legend when `pathway_cats` is non-`NULL`
#'   (default `"Pathway category"`).  Set to `NULL` for no legend title.
#' @param legend_pathway_fill_dot_size numeric(1) Size of the dot/point keys in
#'   the pathway-category fill legend (default `5`).  Increase this value if the
#'   colored dots in the legend appear too small.  Must be a single positive
#'   numeric value.  Ignored when `pathway_cats` is `NULL`.
#' @param gene_color character(1) Color of gene label text (default
#'   `"grey30"`).
#' @param gene_label_size numeric(1) Font size of gene labels (default `2.5`).
#'   Must be a single positive value.
#' @param title character(1) Plot title (default `"Effect sizes of genes in selected pathways"`).
#' @param legend_pathway_size_title character(1) Title for the node-size legend (default
#'   `"Num. genes\n in pathway"`).  Set to `NULL` to show the legend without a title.
#' @param legend_fixed_dot_size numeric vector of gene-count values whose dot
#'   sizes should appear as keys in the size legend (default `NULL`,
#'   automatic).  For example, `c(50, 100, 200)` causes exactly those three
#'   dot sizes to be shown.  The supplied values also become the scale limits
#'   (using their range), so the visual size mapping is identical across
#'   multiple plots combined with `patchwork`.  Values outside the range are
#'   squished to the nearest extreme rather than dropped.  All values must be
#'   finite and positive.
#' @param legend_color_title character(1) or expression() Title for the color scale legend
#'   (default `"Gene effect size"`).  Set to `NULL` to show the legend without a
#'   title. Use `expression()` to supply plotmath expressions.
#' @param colorkey_breaks numeric vector of values at which tick marks and
#'   labels are drawn on the color legend (default `NULL`, automatic).  For
#'   example, `c(-2, -1, 0, 1, 2)` to show five labeled ticks.  When
#'   supplied without any `color_*` arguments, the existing cnetplot palette
#'   is preserved and only the break positions are updated.
#' @param colorkey_limits numeric vector of length 2 giving the lower and
#'   upper bounds of the color scale (default `NULL`, automatic).  Values
#'   outside this range are mapped to the nearest extreme color.  Most
#'   useful together with `colorkey_breaks`.  Like `colorkey_breaks`,
#'   this preserves the cnetplot palette when no `color_*` arguments are set.
#' @param color_low character(1) Color for the low end of the scale (default
#'   `"blue"`).  Combine with `color_high` for a 2-color sequential scale, or
#'   also set `color_mid` for a 3-color diverging scale.  Set to `NULL` to
#'   use cnetplot's default palette (only when no `color_*` arguments are
#'   specified).
#' @param color_mid character(1) Color for the midpoint of the scale (default
#'   `"white"`).  When non-`NULL`, a 3-color diverging
#'   [ggplot2::scale_color_gradient2()] is used.  Set to `NULL` to use a
#'   2-color [ggplot2::scale_color_gradient()] when `color_low` or
#'   `color_high` are set.  To restore cnetplot's original palette, set all
#'   `color_*` arguments to `NULL`.
#' @param color_high character(1) Color for the high end of the scale (default
#'   `"red"`).  Set to `NULL` (with `color_low` and `color_mid` also `NULL`)
#'   to use cnetplot's default palette.
#' @param plot_margin numeric vector of length 4 giving the plot margin in
#'   lines: `c(top, right, bottom, left)` (default `c(0.5, 0.5, 0.5, 0.5)`).
#'   All values must be finite and non-negative.  Increase
#'   the left/right values if node labels are being clipped at the edges.
#'
#' @return A ggplot2 object.
#'
#' @examples
#' # hallmark_t2g is bundled with the package (columns: term, gene)
#' data(hallmark_t2g)
#' set.seed(1)
#' all_genes <- unique(hallmark_t2g$gene)
#' gene_vec  <- setNames(rnorm(length(all_genes)), all_genes)
#'
#' res <- run_gsea(gene_vec, term2gene = hallmark_t2g)
#'
#' # Basic usage
#' plot_pathways(
#'     gsea_result           = res$gsea_result,
#'     effect_size           = res$gene_vec,
#'     show_pathways         = 5,
#'     effect_size_threshold = 1.5
#' )
#'
#' # Adaptively cap gene nodes at 50: effect_size_threshold is raised automatically
#' # so at most 50 genes appear; the subtitle reports the effective threshold used
#' plot_pathways(
#'     gsea_result        = res$gsea_result,
#'     effect_size        = res$gene_vec,
#'     show_pathways      = 5,
#'     max_genes_shown    = 50,
#'     subtitle_effect_size_label  = "log2FC"
#' )
#'
#' # 3-color diverging scale (purple -> white -> orange)
#' plot_pathways(
#'     gsea_result   = res$gsea_result,
#'     effect_size   = res$gene_vec,
#'     show_pathways = 5,
#'     color_low     = "purple",
#'     color_mid     = "white",
#'     color_high    = "orange",
#'     effect_size_threshold = 1.5
#' )
#'
#' # 2-color sequential scale (white -> red)
#' plot_pathways(
#'     gsea_result   = res$gsea_result,
#'     effect_size   = res$gene_vec,
#'     show_pathways = 5,
#'     color_low     = "white",
#'     color_high    = "red",
#'     effect_size_threshold = 1.5
#' )
#'
#' # Custom colors with explicit breaks and limits
#' plot_pathways(
#'     gsea_result     = res$gsea_result,
#'     effect_size     = res$gene_vec,
#'     show_pathways   = 5,
#'     color_low       = "blue",
#'     color_mid       = "white",
#'     color_high      = "red",
#'     colorkey_breaks = c(-2, -1, 0, 1, 2),
#'     colorkey_limits = c(-3, 3),
#'     effect_size_threshold = 1.5
#' )
#'
#' # Two plots with a shared dot-size legend for use with patchwork.
#' # Both plots use the same legend_fixed_dot_size so the size keys are
#' # identical across panels, making visual comparisons meaningful.
#' shared_dot_sizes <- c(50, 100, 200)
#'
#' p1 <- plot_pathways(
#'     gsea_result           = res$gsea_result,
#'     effect_size           = res$gene_vec,
#'     show_pathways         = 3,
#'     legend_fixed_dot_size = shared_dot_sizes,
#'     effect_size_threshold = 1.5
#' )
#' p2 <- plot_pathways(
#'     gsea_result           = res$gsea_result,
#'     effect_size           = res$gene_vec,
#'     show_pathways         = 3,
#'     legend_fixed_dot_size = shared_dot_sizes,
#'     effect_size_threshold = 1.5
#' )
#'
#' patchwork::wrap_plots(p1, p2, guides = "collect")
#'
#' # Color pathway nodes by biological process category using hallmark_pathway_categories
#' data(hallmark_pathway_categories)
#' top_ids <- utils::head(res$gsea_result@result$ID, 5)
#' # Look up the process category for each displayed pathway
#' pathway_cats <- setNames(
#'     hallmark_pathway_categories$process_category[
#'         match(top_ids, hallmark_pathway_categories$term)
#'     ],
#'     top_ids
#' )
#' # One color per category; covers all eight Hallmark categories
#' cat_palette <- c(
#'     Signaling = "#e41a1c", Development = "#377eb8", Immune = "#4daf4a",
#'     Metabolic = "#984ea3", "DNA damage" = "#ff7f00", Proliferation = "#a65628",
#'     "Cellular component" = "#f781bf", Pathway = "#999999"
#' )
#' plot_pathways(
#'     gsea_result               = res$gsea_result,
#'     effect_size               = res$gene_vec,
#'     show_pathways             = 5,
#'     pathway_cats              = pathway_cats,
#'     pathway_cat_colors        = cat_palette,
#'     legend_pathway_fill_title = "Pathway category",
#'     effect_size_threshold     = 1.5
#' )
#'
#' @importFrom enrichplot cnetplot
#' @importFrom ggplot2 ggtitle geom_point scale_fill_identity
#' @export
plot_pathways <- function(
    gsea_result,
    effect_size,
    show_pathways = 5,
    effect_size_threshold = 0,
    subtitle_effect_size_label = "effect size",
    max_genes_shown = NULL,
    gene_node_size = 0.7,
    line_size = 0.5,
    pathway_color = "black",
    pathway_label_size = 4,
    pathway_cats = NULL,
    pathway_cat_colors = NULL,
    legend_pathway_fill_title = "Pathway category",
    legend_pathway_fill_dot_size = 5,
    gene_color = "grey30",
    gene_label_size = 2.5,
    title = "Effect sizes of genes in selected pathways",
    legend_pathway_size_title = "Num. genes\n in pathway",
    legend_fixed_dot_size = NULL,
    legend_color_title = "Gene effect size",
    colorkey_breaks = NULL,
    colorkey_limits = NULL,
    color_low = "blue",
    color_mid = "white",
    color_high = "red",
    plot_margin = c(0.5, 0.5, 0.5, 0.5)
) {
    stopifnot(
        "effect_size must be a named numeric vector" = is.numeric(
            effect_size
        ) &&
            !is.null(names(effect_size)),
        "show_pathways must be a single positive whole number" = is.numeric(
            show_pathways
        ) &&
            length(show_pathways) == 1 &&
            is.finite(show_pathways) &&
            show_pathways >= 1 &&
            show_pathways == floor(show_pathways),
        "effect_size_threshold must be a single non-negative numeric value" = is.numeric(
            effect_size_threshold
        ) &&
            length(effect_size_threshold) == 1 &&
            is.finite(effect_size_threshold) &&
            effect_size_threshold >= 0,
        "subtitle_effect_size_label must be a single character string" = is.character(
            subtitle_effect_size_label
        ) &&
            length(subtitle_effect_size_label) == 1,
        "max_genes_shown must be a single positive whole number or NULL" = is.null(
            max_genes_shown
        ) ||
            (is.numeric(max_genes_shown) &&
                length(max_genes_shown) == 1 &&
                is.finite(max_genes_shown) &&
                max_genes_shown >= 1 &&
                max_genes_shown == floor(max_genes_shown)),
        "gene_node_size must be a single positive numeric value" = is.numeric(
            gene_node_size
        ) &&
            length(gene_node_size) == 1 &&
            is.finite(gene_node_size) &&
            gene_node_size > 0,
        "line_size must be a single positive numeric value" = is.numeric(
            line_size
        ) &&
            length(line_size) == 1 &&
            is.finite(line_size) &&
            line_size > 0,
        "pathway_color must be a single character string" = is.character(
            pathway_color
        ) &&
            length(pathway_color) == 1,
        "pathway_label_size must be a single positive numeric value" = is.numeric(
            pathway_label_size
        ) &&
            length(pathway_label_size) == 1 &&
            is.finite(pathway_label_size) &&
            pathway_label_size > 0,
        "gene_color must be a single character string" = is.character(
            gene_color
        ) &&
            length(gene_color) == 1,
        "gene_label_size must be a single positive numeric value" = is.numeric(
            gene_label_size
        ) &&
            length(gene_label_size) == 1 &&
            is.finite(gene_label_size) &&
            gene_label_size > 0,
        "title must be a single character string or NULL" = is.null(title) ||
            (is.character(title) && length(title) == 1),
        "legend_pathway_size_title must be a single character string or NULL" = is.null(
            legend_pathway_size_title
        ) ||
            (is.character(legend_pathway_size_title) &&
                length(legend_pathway_size_title) == 1),
        "legend_fixed_dot_size must be a positive numeric vector or NULL" = is.null(
            legend_fixed_dot_size
        ) ||
            (is.numeric(legend_fixed_dot_size) &&
                length(legend_fixed_dot_size) >= 1 &&
                all(is.finite(legend_fixed_dot_size)) &&
                all(legend_fixed_dot_size > 0)),
        "legend_color_title must be a single character string, an expression, or NULL" = is.null(
            legend_color_title
        ) ||
            (is.character(legend_color_title) &&
                length(legend_color_title) == 1) ||
            is.expression(legend_color_title),
        "colorkey_breaks must be a numeric vector or NULL" = is.null(
            colorkey_breaks
        ) ||
            (is.numeric(colorkey_breaks) && length(colorkey_breaks) >= 1),
        "colorkey_limits must be a numeric vector of length 2 or NULL" = is.null(
            colorkey_limits
        ) ||
            (is.numeric(colorkey_limits) && length(colorkey_limits) == 2),
        "color_low must be a single character string or NULL" = is.null(
            color_low
        ) ||
            (is.character(color_low) && length(color_low) == 1),
        "color_mid must be a single character string or NULL" = is.null(
            color_mid
        ) ||
            (is.character(color_mid) && length(color_mid) == 1),
        "color_high must be a single character string or NULL" = is.null(
            color_high
        ) ||
            (is.character(color_high) && length(color_high) == 1),
        "plot_margin must be a numeric vector of length 4 with finite non-negative values" = is.numeric(
            plot_margin
        ) &&
            length(plot_margin) == 4 &&
            all(is.finite(plot_margin)) &&
            all(plot_margin >= 0),
        "pathway_cats must be a named character vector or NULL" = is.null(
            pathway_cats
        ) ||
            (is.character(pathway_cats) &&
                !is.null(names(pathway_cats)) &&
                length(pathway_cats) >= 1),
        "names(pathway_cats) must not contain empty strings" = is.null(
            pathway_cats
        ) ||
            all(nzchar(names(pathway_cats))),
        "names(pathway_cats) must not contain NAs" = is.null(
            pathway_cats
        ) ||
            !anyNA(names(pathway_cats)),
        "names(pathway_cats) must be unique (no duplicate pathway IDs)" = is.null(
            pathway_cats
        ) ||
            anyDuplicated(names(pathway_cats)) == 0L,
        "pathway_cat_colors must be a named character vector or NULL" = is.null(
            pathway_cat_colors
        ) ||
            (is.character(pathway_cat_colors) &&
                !is.null(names(pathway_cat_colors)) &&
                length(pathway_cat_colors) >= 1),
        "names(pathway_cat_colors) must not contain empty strings" = is.null(
            pathway_cat_colors
        ) ||
            all(nzchar(names(pathway_cat_colors))),
        "names(pathway_cat_colors) must not contain NAs" = is.null(
            pathway_cat_colors
        ) ||
            !anyNA(names(pathway_cat_colors)),
        "names(pathway_cat_colors) must be unique (no duplicate category names)" = is.null(
            pathway_cat_colors
        ) ||
            anyDuplicated(names(pathway_cat_colors)) == 0L,
        "pathway_cats and pathway_cat_colors must both be provided or both NULL" = is.null(
            pathway_cats
        ) ==
            is.null(pathway_cat_colors),
        "all values in pathway_cats must appear in names(pathway_cat_colors)" = is.null(
            pathway_cats
        ) ||
            all(pathway_cats %in% names(pathway_cat_colors)),
        "legend_pathway_fill_title must be a single character string or NULL" = is.null(
            legend_pathway_fill_title
        ) ||
            (is.character(legend_pathway_fill_title) &&
                length(legend_pathway_fill_title) == 1),
        "legend_pathway_fill_dot_size must be a single positive numeric value" = is.numeric(
            legend_pathway_fill_dot_size
        ) &&
            length(legend_pathway_fill_dot_size) == 1 &&
            is.finite(legend_pathway_fill_dot_size) &&
            legend_pathway_fill_dot_size > 0
    )

    # Determine genes that could appear in the plot (all genes in the
    # top `show_pathways` pathway gene sets) and compute an adaptive
    # threshold over those if `max_genes_shown` is set.
    top_ids <- utils::head(gsea_result@result$ID, show_pathways)
    pathway_genes <- unique(unlist(gsea_result@geneSets[top_ids]))
    genes_in_plot <- intersect(pathway_genes, names(effect_size))
    abs_fc_path <- abs(effect_size[genes_in_plot])
    abs_fc_path <- abs_fc_path[is.finite(abs_fc_path)]
    effective_threshold <- effect_size_threshold
    if (!is.null(max_genes_shown) && length(abs_fc_path) > 0) {
        if (sum(abs_fc_path >= effective_threshold) > max_genes_shown) {
            effective_threshold <- max(
                unname(quantile(
                    abs_fc_path,
                    probs = 1 - max_genes_shown / length(abs_fc_path)
                )),
                effect_size_threshold
            )
        }
    }

    # Build subtitle:
    # - max_genes_shown set: report the adaptive threshold chosen
    # - effect_size_threshold > 0 (no cap): report how many genes are shown
    # - effect_size_threshold == 0: no subtitle needed (all genes shown)
    n_genes_shown <- if (effective_threshold == 0) {
        length(genes_in_plot)
    } else {
        sum(abs_fc_path >= effective_threshold)
    }
    if (!is.null(max_genes_shown)) {
        subtitle <- paste0(
            "Showing at most ",
            max_genes_shown,
            " gene nodes: abs(",
            subtitle_effect_size_label,
            ") \u2265 ",
            round(effective_threshold, 2)
        )
    } else if (effective_threshold > 0) {
        subtitle <- paste0(
            "Showing ",
            n_genes_shown,
            " gene nodes with abs(",
            subtitle_effect_size_label,
            ") \u2265 ",
            round(effective_threshold, 2)
        )
    } else {
        subtitle <- NULL
    }

    p <- enrichplot::cnetplot(
        gsea_result,
        showCategory = show_pathways,
        foldChange = effect_size,
        size_item = gene_node_size,
        size_edge = line_size,
        node_label = "none", # labels added separately below
        fc_threshold = effective_threshold
    ) +
        # gene labels: smaller, subdued (draw first)
        ggtangle::geom_cnet_label(
            node_label = "item",
            color = gene_color,
            size = gene_label_size
        ) +
        ggplot2::guides(
            size = ggplot2::guide_legend(title = legend_pathway_size_title)
        ) +
        ggplot2::ggtitle(title) +
        ggplot2::labs(subtitle = subtitle) +
        ggplot2::theme(
            plot.margin = ggplot2::margin(
                t = plot_margin[1],
                r = plot_margin[2],
                b = plot_margin[3],
                l = plot_margin[4],
                unit = "lines"
            )
        )

    # Size-legend customisation: fix the breaks (and limits) shown in the
    # dot-size legend so that multiple plots composed with patchwork share the
    # same visual size scale.
    if (!is.null(legend_fixed_dot_size)) {
        idx_size <- which(vapply(
            p$scales$scales,
            function(s) "size" %in% s$aesthetics,
            logical(1)
        ))
        if (length(idx_size) > 0) {
            p$scales$scales[[idx_size[1]]]$breaks <- legend_fixed_dot_size
            p$scales$scales[[idx_size[1]]]$limits <- range(
                legend_fixed_dot_size
            )
            p$scales$scales[[idx_size[1]]]$oob <- scales::squish
        }
    }

    # Colour scale customisation:
    # - If any colours are specified, replace the scale entirely.
    # - If only breaks/limits are changed, modify the existing scale in-place
    #   so the palette set by cnetplot is preserved.
    any_colors <- !is.null(color_low) ||
        !is.null(color_mid) ||
        !is.null(color_high)
    any_key <- !is.null(colorkey_breaks) || !is.null(colorkey_limits)

    if (any_colors) {
        if (!is.null(color_mid)) {
            # 3-colour diverging scale
            p <- p +
                ggplot2::scale_color_gradient2(
                    name = legend_color_title,
                    low = if (!is.null(color_low)) color_low else "blue",
                    mid = color_mid,
                    high = if (!is.null(color_high)) color_high else "red",
                    breaks = if (!is.null(colorkey_breaks)) {
                        colorkey_breaks
                    } else {
                        ggplot2::waiver()
                    },
                    limits = colorkey_limits,
                    oob = scales::squish,
                    guide = ggplot2::guide_colorbar(title = legend_color_title)
                )
        } else {
            # 2-colour sequential scale
            p <- p +
                ggplot2::scale_color_gradient(
                    name = legend_color_title,
                    low = if (!is.null(color_low)) color_low else "white",
                    high = if (!is.null(color_high)) color_high else "red",
                    breaks = if (!is.null(colorkey_breaks)) {
                        colorkey_breaks
                    } else {
                        ggplot2::waiver()
                    },
                    limits = colorkey_limits,
                    oob = scales::squish,
                    guide = ggplot2::guide_colorbar(title = legend_color_title)
                )
        }
    } else {
        # No colour customisation — apply the colorbar guide via guides()
        p <- p +
            ggplot2::guides(
                color = ggplot2::guide_colorbar(title = legend_color_title)
            )
        if (any_key) {
            # Preserve the existing palette; only update breaks/limits in-place.
            # Use idx[1] to safely handle the unlikely case of multiple colour
            # scales (cnetplot adds exactly one, but guard defensively).
            idx <- which(vapply(
                p$scales$scales,
                function(s) "colour" %in% s$aesthetics,
                logical(1)
            ))
            if (length(idx) > 0) {
                if (!is.null(colorkey_breaks)) {
                    p$scales$scales[[idx[1]]]$breaks <- colorkey_breaks
                }
                if (!is.null(colorkey_limits)) {
                    p$scales$scales[[idx[1]]]$limits <- colorkey_limits
                    p$scales$scales[[idx[1]]]$oob <- scales::squish
                }
            }
        }
    }

    # Pathway-category fill: access the ggraph layout (p$data is the layout
    # data.frame for ggtangle/ggraph plots, with x, y, name, .isCategory, size)
    # to get pathway node positions, then add a plain geom_point overlay with
    # pre-computed fill colors.  Using geom_point with explicit data avoids the
    # ggraph filter-evaluation issues that prevent environment-variable lookup
    # in geom_node_point's filter aesthetic.  colour = "transparent" (not NA)
    # ensures shape 21 renders visibly: NA colour makes shape 21 invisible in
    # grid because the border defines the shape's rendering boundary.
    if (!is.null(pathway_cats)) {
        pathway_ids_fill <- names(pathway_cats)

        node_layout <- as.data.frame(p$data)

        # Identify pathway (category) nodes via .isCategory when available
        is_pathway <- if (".isCategory" %in% names(node_layout)) {
            node_layout$.isCategory
        } else {
            rep(TRUE, nrow(node_layout))
        }

        pathway_node_df <- node_layout[
            is_pathway & (node_layout$name %in% pathway_ids_fill),
            c("x", "y", "name", "size"),
            drop = FALSE
        ]
        # Color each node by looking up its category then the category's color
        pathway_node_df$node_fill <- unname(
            pathway_cat_colors[pathway_cats[pathway_node_df$name]]
        )

        # Overlay with explicit coordinates; exclude from size legend (which is
        # already populated by the cnetplot pathway node layer).
        p <- p +
            ggplot2::geom_point(
                data = pathway_node_df,
                ggplot2::aes(x = x, y = y, size = size, fill = node_fill),
                shape = 21,
                colour = "transparent",
                inherit.aes = FALSE,
                show.legend = c(size = FALSE, fill = NA)
            )

        # Legend: one entry per unique category among displayed pathway nodes
        cats_used <- unique(pathway_cats[pathway_node_df$name])
        cats_used <- cats_used[!is.na(cats_used)]
        legend_colors <- pathway_cat_colors[cats_used]
        p <- p +
            ggplot2::scale_fill_identity(
                name = legend_pathway_fill_title,
                guide = ggplot2::guide_legend(
                    override.aes = list(size = legend_pathway_fill_dot_size)
                ),
                breaks = unname(legend_colors),
                labels = names(legend_colors)
            )
    }

    # Pathway labels drawn last so they always sit on top of any fill overlay.
    p <- p +
        ggtangle::geom_cnet_label(
            node_label = "category",
            fontface = "bold",
            color = pathway_color,
            size = pathway_label_size
        )

    p
}
