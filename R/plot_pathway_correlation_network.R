#' Gene–Gene correlation network for a single pathway
#'
#' Computes pairwise Pearson correlations among genes belonging to a chosen
#' pathway and draws the resulting network.  Edges are drawn only for gene
#' pairs whose absolute correlation meets `cor_thresh`.  Node size encodes
#' **degree** (number of connections), so hub genes appear larger.  Node color
#' encodes the gene-level statistic supplied via `log2fc` (e.g. log2 fold
#' change).
#'
#' Unlike [plot_pathways()], which uses network layout to reflect shared
#' pathway membership, this plot shows actual co-expression structure among
#' genes within a single pathway.
#'
#' @param expr matrix Normalised expression matrix with **genes as rows** and
#'   **samples as columns**.  `rownames` must be gene symbols.
#' @param pathway character(1) Name of the pathway to visualise.  Must match a
#'   value in the `term` column of `gene_sets`.
#' @param gene_sets data.frame Two-column data frame with columns `"term"` and
#'   `"gene"` mapping pathway names to gene symbols.  Typically the `term2gene`
#'   element returned by [run_gsea()].
#' @param log2fc named numeric vector Gene-level statistics (e.g. log2 fold
#'   change) used to color nodes.  Names must be gene symbols.  Missing genes
#'   are treated as 0.
#' @param top_n_genes integer(1) Before computing correlations, retain only the
#'   top `top_n_genes` genes ranked by `abs(log2fc)`.  Reduces visual clutter
#'   on large pathways.  Set to `Inf` to use all pathway genes (default `30`).
#' @param cor_thresh numeric(1) Minimum absolute Pearson correlation required
#'   to draw an edge between two genes (default `0.6`).
#' @param cor_low character(1) Edge color for strongly negative correlations
#'   (default `"steelblue"`).
#' @param cor_mid character(1) Edge color at correlation = 0 (default
#'   `"grey85"`).
#' @param cor_high character(1) Edge color for strongly positive correlations
#'   (default `"firebrick"`).
#' @param fc_low character(1) Node color for strongly negative `log2fc` values.
#'   Defaults to the 10th color of the `PiYG` palette.
#' @param fc_mid character(1) Node color at `log2fc = 0`.  Defaults to the
#'   neutral midpoint of the `PiYG` palette.
#' @param fc_high character(1) Node color for strongly positive `log2fc`
#'   values.  Defaults to the 2nd color of the `PiYG` palette.
#' @param title character(1) Plot title.  Defaults to
#'   `"Gene-Gene correlation network for pathway: <pathway>"`.
#' @param label_size numeric(1) Gene label font size (default `3`).
#' @param label_bold logical(1) Whether gene labels are bold (default `TRUE`).
#'
#' @return A ggplot2 object, or `NULL` (invisibly) when fewer than 3 pathway
#'   genes are present in `expr` or no gene pairs pass `cor_thresh`.
#'
#' @examples
#' data(ex_expr_pathway)
#' data(ex_log2fc_pathway)
#' data(hallmark_t2g)
#'
#' plot_pathway_correlation_network(
#'     expr      = ex_expr_pathway,
#'     pathway   = "MYC_TARGETS_V1",
#'     gene_sets = hallmark_t2g,
#'     log2fc    = ex_log2fc_pathway
#' )
#'
#' @importFrom igraph graph_from_data_frame degree
#' @importFrom ggraph ggraph geom_edge_link geom_node_point geom_node_text
#' @importFrom ggplot2 aes ggtitle scale_color_gradient2 theme_void
#' @importFrom RColorBrewer brewer.pal
#' @importFrom scales squish
#' @importFrom stats cor
#' @export
plot_pathway_correlation_network <- function(
    expr,
    pathway,
    gene_sets,
    log2fc,
    top_n_genes = 30,
    cor_thresh = 0.6,
    # edge (correlation) color scale
    cor_low = "steelblue",
    cor_mid = "grey85",
    cor_high = "firebrick",
    # node (log2FC) color scale
    fc_low = RColorBrewer::brewer.pal(11, "PiYG")[10],
    fc_mid = RColorBrewer::brewer.pal(11, "PiYG")[6],
    fc_high = RColorBrewer::brewer.pal(11, "PiYG")[2],
    title = sprintf("Gene-Gene correlation network for pathway: %s", pathway),
    label_size = 3,
    label_bold = TRUE
) {
    stopifnot(
        "expr must be a matrix with rownames" = is.matrix(expr) &&
            !is.null(rownames(expr)),
        "gene_sets must be a data.frame with columns 'term' and 'gene'" = is.data.frame(
            gene_sets
        ) &&
            all(c("term", "gene") %in% colnames(gene_sets)),
        "log2fc must be a named numeric vector" = is.numeric(log2fc) &&
            !is.null(names(log2fc)),
        "pathway must be a single character string" = is.character(pathway) &&
            length(pathway) == 1
    )

    genes_in_term <- intersect(
        unique(gene_sets$gene[gene_sets$term == pathway]),
        rownames(expr)
    )

    if (length(genes_in_term) < 3) {
        message(
            "Fewer than 3 pathway genes found in expr for pathway: ",
            pathway
        )
        return(invisible(NULL))
    }

    # Optionally restrict to top N genes by |log2fc| to reduce clutter
    if (is.finite(top_n_genes) && length(genes_in_term) > top_n_genes) {
        fc_vals <- abs(replace(
            log2fc[genes_in_term],
            is.na(log2fc[genes_in_term]),
            0
        ))
        genes_in_term <- names(sort(fc_vals, decreasing = TRUE))[seq_len(
            top_n_genes
        )]
    }

    # Correlation matrix — upper triangle only
    cmat <- stats::cor(
        t(expr[genes_in_term, , drop = FALSE]),
        method = "pearson"
    )
    idx <- which(upper.tri(cmat) & abs(cmat) >= cor_thresh, arr.ind = TRUE)

    if (nrow(idx) == 0) {
        message(
            "No gene pairs pass |cor| >= ",
            cor_thresh,
            " for pathway: ",
            pathway
        )
        return(invisible(NULL))
    }

    edge_df <- data.frame(
        from = rownames(cmat)[idx[, 1]],
        to = rownames(cmat)[idx[, 2]],
        cor = cmat[idx]
    )
    node_genes <- unique(c(edge_df$from, edge_df$to))
    node_df <- data.frame(
        gene = node_genes,
        stat = replace(log2fc[node_genes], is.na(log2fc[node_genes]), 0)
    )

    g <- igraph::graph_from_data_frame(
        edge_df,
        vertices = node_df,
        directed = FALSE
    )

    set.seed(42)
    ggraph::ggraph(g, layout = "fr") +
        ggraph::geom_edge_link(
            ggplot2::aes(
                color = cor,
                width = abs(cor)
            )
        ) +
        ggraph::geom_node_point(
            ggplot2::aes(
                size = igraph::degree(g),
                color = stat
            )
        ) +
        ggraph::geom_node_text(
            ggplot2::aes(label = name),
            repel = TRUE,
            size = label_size,
            fontface = if (label_bold) "bold" else "plain",
            bg.colour = "white",
            bg.r = 0.15
        ) +
        ggraph::scale_edge_color_gradient2(
            low = cor_low,
            mid = cor_mid,
            high = cor_high,
            midpoint = 0,
            limits = c(-1, 1),
            name = "Correlation",
            guide = ggraph::guide_edge_colourbar()
        ) +
        ggraph::scale_edge_width(range = c(0.5, 2), guide = "none") +
        ggplot2::scale_size_continuous(name = "# connections", guide = "none") +
        ggplot2::scale_color_gradient2(
            low = fc_low,
            mid = fc_mid,
            high = fc_high,
            midpoint = 0,
            limits = c(-1, 1),
            oob = scales::squish,
            name = expression(log[2] ~ Fold ~ Change)
        ) +
        ggplot2::theme_void() +
        ggplot2::ggtitle(title)
}
