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
#' @param fold_change named numeric vector Gene-level statistics used to color
#'   gene nodes (e.g. log2 fold change, t-statistic).  Names must be gene
#'   symbols matching those in `gsea_result`.  Typically the `gene_vec`
#'   element returned by [run_gsea()].
#' @param show_category integer(1) Number of top pathways to display (default
#'   `5`).
#' @param fc_threshold numeric(1) Only show gene nodes whose
#'   `abs(fold_change) >= fc_threshold` (default `1.5`).  Set to `0` to show
#'   all genes.
#' @param size_item numeric(1) Relative size of gene circles/nodes (default
#'   `0.7`).
#' @param size_edge numeric(1) Relative thickness of edges (default `0.5`).
#' @param category_color character(1) Color of pathway label text (default
#'   `"black"`).
#' @param category_size numeric(1) Font size of pathway labels (default `4`).
#' @param item_color character(1) Color of gene label text (default
#'   `"grey30"`).
#' @param item_size numeric(1) Font size of gene labels (default `2.5`).
#' @param title character(1) Plot title (default `"Gene-Pathway network
#'   (GSEA)"`).
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
#' plot_pathways(
#'     gsea_result   = res$gsea_result,
#'     fold_change   = res$gene_vec,
#'     show_category = 5
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
    size_item = 0.7,
    size_edge = 0.5,
    category_color = "black",
    category_size = 4,
    item_color = "grey30",
    item_size = 2.5,
    title = "Gene-Pathway network (GSEA)"
) {
    stopifnot(
        "fold_change must be a named numeric vector" = is.numeric(
            fold_change
        ) &&
            !is.null(names(fold_change)),
        "show_category must be a positive integer" = is.numeric(
            show_category
        ) &&
            show_category >= 1
    )

    p <- enrichplot::cnetplot(
        gsea_result,
        showCategory = show_category,
        foldChange = fold_change,
        size_item = size_item,
        size_edge = size_edge,
        node_label = "none", # labels added separately below
        fc_threshold = fc_threshold
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
        ggplot2::ggtitle(title)

    p
}
