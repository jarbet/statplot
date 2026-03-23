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
#' @param fold_change named numeric vector. Gene-level statistics used to color
#'   gene nodes (e.g. log2 fold change, t-statistic).  Names must be gene
#'   symbols matching those in `gsea_result`.  Typically the `gene_vec`
#'   element returned by [run_gsea()].
#' @param show_category integer(1) Number of top pathways to display (default
#'   `5`).  Must be a single positive whole number.
#' @param fc_threshold numeric(1) Only show gene nodes whose
#'   `abs(fold_change) >= fc_threshold` (default `1.5`).  Set to `0` to show
#'   all genes.  Must be a single finite non-negative value.
#' @param fc_threshold_label character(1) String placed inside `abs()` in the
#'   auto-generated subtitle when a threshold is applied
#'   (default `"log2FC"`).  Change to match your effect-size metric, e.g.
#'   `"t-statistic"` or `"z-score"`.
#' @param max_genes_shown integer(1) Maximum number of gene nodes to display
#'   (default `NULL`, no limit).  If the number of genes belonging to the top
#'   `show_category` pathways and passing `fc_threshold` exceeds this value,
#'   the threshold is raised adaptively (via quantile of `abs(fold_change)`
#'   among pathway genes) until at most `max_genes_shown` genes remain.  The
#'   effective threshold will never drop below `fc_threshold`.  Must be a
#'   single positive whole number.
#' @param size_item numeric(1) Relative size of gene circles/nodes (default
#'   `0.7`).  Must be a single positive value.
#' @param size_edge numeric(1) Relative thickness of edges (default `0.5`).
#'   Must be a single positive value.
#' @param category_color character(1) Color of pathway label text (default
#'   `"black"`).
#' @param category_size numeric(1) Font size of pathway labels (default `4`).
#'   Must be a single positive value.
#' @param item_color character(1) Color of gene label text (default
#'   `"grey30"`).
#' @param item_size numeric(1) Font size of gene labels (default `2.5`).
#'   Must be a single positive value.
#' @param title character(1) Plot title (default `"Gene-Pathway Network
#'   (GSEA)"`).
#' @param legend_pathway_size_title character(1) Title for the node-size legend (default
#'   `"Num. genes"`).  Set to `NULL` to suppress the size legend entirely.
#' @param legend_color_title character(1) Title for the color scale legend
#'   (default `"Effect size"`).  Set to `NULL` to suppress the color legend
#'   entirely.
#' @param colorkey_breaks numeric vector of values at which tick marks and
#'   labels are drawn on the color legend (default `NULL`, automatic).  For
#'   example, `c(-2, -1, 0, 1, 2)` to show five labeled ticks.  When
#'   supplied, the color scale is overridden with [ggplot2::scale_color_gradient2()].
#' @param colorkey_limits numeric vector of length 2 giving the lower and
#'   upper bounds of the color scale (default `NULL`, automatic).  Values
#'   outside this range are mapped to the nearest extreme color.  Most
#'   useful together with `colorkey_breaks`.
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
#'     gsea_result   = res$gsea_result,
#'     fold_change   = res$gene_vec,
#'     show_category = 5
#' )
#'
#' # Adaptively cap gene nodes at 50: fc_threshold is raised automatically
#' # so at most 50 genes appear; the subtitle reports the chosen cutoff
#' plot_pathways(
#'     gsea_result      = res$gsea_result,
#'     fold_change      = res$gene_vec,
#'     show_category    = 5,
#'     max_genes_shown  = 50,
#'     fc_threshold_label = "log2FC"
#' )
#'
#' @importFrom enrichplot cnetplot
#' @importFrom ggplot2 ggtitle
#' @export
plot_pathways <- function(
    gsea_result,
    fold_change,
    show_category = 5,
    fc_threshold = 1.5,
    fc_threshold_label = "log2FC",
    max_genes_shown = NULL,
    size_item = 0.7,
    size_edge = 0.5,
    category_color = "black",
    category_size = 4,
    item_color = "grey30",
    item_size = 2.5,
    title = "Effect sizes of genes in selected pathways",
    legend_pathway_size_title = "Num. genes",
    legend_color_title = "Effect size",
    colorkey_breaks = NULL,
    colorkey_limits = NULL
) {
    stopifnot(
        "fold_change must be a named numeric vector" = is.numeric(
            fold_change
        ) &&
            !is.null(names(fold_change)),
        "show_category must be a single positive whole number" = is.numeric(
            show_category
        ) &&
            length(show_category) == 1 &&
            is.finite(show_category) &&
            show_category >= 1 &&
            show_category == floor(show_category),
        "fc_threshold must be a single non-negative numeric value" = is.numeric(
            fc_threshold
        ) &&
            length(fc_threshold) == 1 &&
            is.finite(fc_threshold) &&
            fc_threshold >= 0,
        "max_genes_shown must be a single positive whole number or NULL" = is.null(
            max_genes_shown
        ) ||
            (is.numeric(max_genes_shown) &&
                length(max_genes_shown) == 1 &&
                is.finite(max_genes_shown) &&
                max_genes_shown >= 1 &&
                max_genes_shown == floor(max_genes_shown)),
        "size_item must be a single positive numeric value" = is.numeric(
            size_item
        ) &&
            length(size_item) == 1 &&
            is.finite(size_item) &&
            size_item > 0,
        "size_edge must be a single positive numeric value" = is.numeric(
            size_edge
        ) &&
            length(size_edge) == 1 &&
            is.finite(size_edge) &&
            size_edge > 0,
        "category_size must be a single positive numeric value" = is.numeric(
            category_size
        ) &&
            length(category_size) == 1 &&
            is.finite(category_size) &&
            category_size > 0,
        "item_size must be a single positive numeric value" = is.numeric(
            item_size
        ) &&
            length(item_size) == 1 &&
            is.finite(item_size) &&
            item_size > 0,
        "colorkey_breaks must be a numeric vector or NULL" = is.null(
            colorkey_breaks
        ) ||
            (is.numeric(colorkey_breaks) && length(colorkey_breaks) >= 1),
        "colorkey_limits must be a numeric vector of length 2 or NULL" = is.null(
            colorkey_limits
        ) ||
            (is.numeric(colorkey_limits) && length(colorkey_limits) == 2)
    )

    # Determine genes that could appear in the plot (all genes in the
    # top `show_category` pathway gene sets) and compute an adaptive
    # threshold over those if `max_genes_shown` is set.
    top_ids <- utils::head(gsea_result@result$ID, show_category)
    pathway_genes <- unique(unlist(gsea_result@geneSets[top_ids]))
    genes_in_plot <- intersect(pathway_genes, names(fold_change))
    abs_fc_path <- abs(fold_change[genes_in_plot])

    effective_threshold <- fc_threshold
    if (!is.null(max_genes_shown) && length(abs_fc_path) > 0) {
        if (sum(abs_fc_path >= effective_threshold) > max_genes_shown) {
            effective_threshold <- max(
                unname(quantile(
                    abs_fc_path,
                    probs = 1 - max_genes_shown / length(abs_fc_path)
                )),
                fc_threshold
            )
        }
    }

    # Build a concise subtitle describing the gene-count / threshold choice
    n_genes_total <- length(genes_in_plot)
    if (effective_threshold == 0 || n_genes_total == 0) {
        subtitle <- paste0(
            "All ",
            n_genes_total,
            " gene nodes shown (no effect size cutoff applied)"
        )
    } else if (!is.null(max_genes_shown)) {
        subtitle <- paste0(
            "An effect size cutoff was chosen to show at most ",
            max_genes_shown,
            " gene nodes: abs(",
            fc_threshold_label,
            ") > ",
            round(effective_threshold, 2)
        )
    } else {
        subtitle <- paste0(
            "An effect size cutoff was applied: abs(",
            fc_threshold_label,
            ") > ",
            round(effective_threshold, 2)
        )
    }

    p <- enrichplot::cnetplot(
        gsea_result,
        showCategory = show_category,
        foldChange = fold_change,
        size_item = size_item,
        size_edge = size_edge,
        node_label = "none", # labels added separately below
        fc_threshold = effective_threshold
    ) +
        # pathway labels: bold, prominent
        ggtangle::geom_cnet_label(
            node_label = "category",
            fontface = "bold",
            color = category_color,
            size = category_size
        ) +
        # gene labels: smaller, subdued
        ggtangle::geom_cnet_label(
            node_label = "item",
            color = item_color,
            size = item_size
        ) +
        (if (!is.null(colorkey_breaks) || !is.null(colorkey_limits)) {
            ggplot2::scale_color_gradient2(
                breaks = colorkey_breaks,
                limits = colorkey_limits
            )
        }) +
        ggplot2::guides(
            size = ggplot2::guide_legend(title = legend_pathway_size_title),
            color = ggplot2::guide_colorbar(title = legend_color_title)
        ) +
        ggplot2::ggtitle(title) +
        ggplot2::labs(subtitle = subtitle)

    p
}
