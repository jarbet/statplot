#' Gene–Gene correlation network for a single pathway
#'
#' Computes pairwise Pearson correlations among genes belonging to a chosen
#' pathway and draws the resulting network.  Edges are drawn only for gene
#' pairs whose absolute correlation meets `cor_thresh`.  Node size encodes
#' **degree** (number of connections), so hub genes appear larger.  Node color
#' encodes the gene-level statistic supplied via `effect_size` (e.g. log2 fold
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
#' @param effect_size named numeric vector Gene-level statistics (e.g. log2
#'   fold change, t-statistic) used to color nodes.  Names must be gene
#'   symbols.  Missing genes are treated as 0.
#' @param top_n_genes integer(1) Before computing correlations, retain only the
#'   top `top_n_genes` genes ranked by `abs(effect_size)`.  Reduces visual
#'   clutter on large pathways.  Set to `Inf` to use all pathway genes
#'   (default `30`).  Must be a single positive whole number (≥ 1) or `Inf`.
#' @param cor_thresh numeric(1) Minimum absolute Pearson correlation required
#'   to draw an edge between two genes (default `0.6`).  Must be a single
#'   finite value in \[0, 1\].
#' @param cor_low character(1) Edge color for strongly negative correlations
#'   (default `"steelblue"`).
#' @param cor_mid character(1) Edge color at correlation = 0 (default
#'   `"grey85"`).
#' @param cor_high character(1) Edge color for strongly positive correlations
#'   (default `"firebrick"`).
#' @param color_low character(1) Node color for the low end of the effect size
#'   scale.  Defaults to the 10th color of the `PiYG` palette.
#' @param color_mid character(1) Node color at the midpoint of the scale
#'   (default: 6th color of `PiYG`).  Set to `NULL` for a 2-color sequential
#'   scale ([ggplot2::scale_color_gradient()]) instead of the default 3-color
#'   diverging scale ([ggplot2::scale_color_gradient2()]).
#' @param color_high character(1) Node color for the high end of the effect
#'   size scale.  Defaults to the 2nd color of the `PiYG` palette.
#' @param title character(1) Plot title.  Defaults to
#'   `"Gene-Gene correlation network for pathway: <pathway>"`.
#' @param gene_label_size numeric(1) Gene label font size (default `3`).
#' @param label_bold logical(1) Whether gene labels are bold (default `TRUE`).
#' @param show_size_legend logical(1) Whether to display the node-size legend
#'   ("# connections") (default `TRUE`).
#' @param legend_gene_color_title character(1) Title for the node color (effect
#'   size) legend (default `"Gene effect size"`).
#' @param legend_correlation_title character(1) Title for the edge color
#'   (correlation) legend (default `"Correlation\nbetween genes"`).
#' @param legend_gene_size_title character(1) Title for the node size (# connections)
#'   legend (default `"# connections"`).  Ignored when `show_size_legend =
#'   FALSE`.
#' @param plot_margin numeric vector of length 4 giving the plot margin in
#'   lines: `c(top, right, bottom, left)` (default `c(1, 1, 1, 1)`).  Increase
#'   the left/right values if node labels are being clipped at the edges.
#' @param seed integer(1) or `NULL`.  Random seed used for the
#'   Fruchterman-Reingold layout so the graph is drawn the same way each time.
#'   The caller's RNG state is saved before the seed is set and fully restored
#'   on exit, so using this function does **not** affect subsequent random
#'   operations in the session.  Pass `NULL` to skip seeding entirely
#'   (default `42L`).
#'
#' @return A ggplot2 object, or `NULL` (invisibly) when fewer than 3 pathway
#'   genes are present in `expr` or no gene pairs pass `cor_thresh`.
#'
#' @examples
#' ggplot2::theme_set(theme_bw2())
#' data(ex_expr_pathway)
#' data(ex_log2fc_pathway)
#' data(hallmark_t2g)
#'
#' plot_pathway_correlation_network(
#'     expr      = ex_expr_pathway,
#'     pathway   = "MYC_TARGETS_V1",
#'     gene_sets = hallmark_t2g,
#'     effect_size = ex_log2fc_pathway
#' )
#'
#' @importFrom igraph graph_from_data_frame degree
#' @importFrom ggraph ggraph geom_edge_link geom_node_point geom_node_text
#' @importFrom ggplot2 aes ggtitle scale_color_gradient scale_color_gradient2 theme_void
#' @importFrom RColorBrewer brewer.pal
#' @importFrom scales squish
#' @importFrom stats cor
#' @export
plot_pathway_correlation_network <- function(
    expr,
    pathway,
    gene_sets,
    effect_size,
    top_n_genes = 30,
    cor_thresh = 0.6,
    # edge (correlation) color scale
    cor_low = "steelblue",
    cor_mid = "grey85",
    cor_high = "firebrick",
    # node (effect size) color scale
    color_low = RColorBrewer::brewer.pal(11, "PiYG")[10],
    color_mid = RColorBrewer::brewer.pal(11, "PiYG")[6],
    color_high = RColorBrewer::brewer.pal(11, "PiYG")[2],
    title = sprintf("Gene-Gene correlation network for pathway: %s", pathway),
    gene_label_size = 3,
    label_bold = TRUE,
    show_size_legend = TRUE,
    legend_gene_color_title = "Gene effect size",
    legend_correlation_title = "Correlation\nbetween genes",
    legend_gene_size_title = "# connections",
    plot_margin = c(1, 1, 1, 1),
    seed = 42L
) {
    stopifnot(
        "expr must be a matrix with rownames" = is.matrix(expr) &&
            !is.null(rownames(expr)),
        "gene_sets must be a data.frame with columns 'term' and 'gene'" = is.data.frame(
            gene_sets
        ) &&
            all(c("term", "gene") %in% colnames(gene_sets)),
        "effect_size must be a named numeric vector" = is.numeric(
            effect_size
        ) &&
            !is.null(names(effect_size)),
        "pathway must be a single character string" = is.character(pathway) &&
            length(pathway) == 1,
        "top_n_genes must be a single positive number or Inf" = is.numeric(
            top_n_genes
        ) &&
            length(top_n_genes) == 1 &&
            (is.infinite(top_n_genes) ||
                (is.finite(top_n_genes) && top_n_genes >= 1)),
        "cor_thresh must be a single numeric value in [0, 1]" = is.numeric(
            cor_thresh
        ) &&
            length(cor_thresh) == 1 &&
            is.finite(cor_thresh) &&
            cor_thresh >= 0 &&
            cor_thresh <= 1
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

    # Optionally restrict to top N genes by |effect_size| to reduce clutter
    if (is.finite(top_n_genes) && length(genes_in_term) > top_n_genes) {
        fc_vals <- abs(replace(
            effect_size[genes_in_term],
            is.na(effect_size[genes_in_term]),
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
        stat = replace(
            effect_size[node_genes],
            is.na(effect_size[node_genes]),
            0
        )
    )

    g <- igraph::graph_from_data_frame(
        edge_df,
        vertices = node_df,
        directed = FALSE
    )

    # Preserve the caller's RNG state so this function has no side-effects on
    # subsequent random operations in the session.
    if (!is.null(seed)) {
        old_seed <- if (
            exists(".Random.seed", envir = .GlobalEnv, inherits = FALSE)
        ) {
            .GlobalEnv$.Random.seed
        } else {
            NULL
        }
        on.exit(
            {
                if (is.null(old_seed)) {
                    if (
                        exists(
                            ".Random.seed",
                            envir = .GlobalEnv,
                            inherits = FALSE
                        )
                    ) {
                        rm(".Random.seed", envir = .GlobalEnv)
                    }
                } else {
                    assign(".Random.seed", old_seed, envir = .GlobalEnv)
                }
            },
            add = TRUE
        )
        set.seed(seed)
    }

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
            size = gene_label_size,
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
            name = legend_correlation_title,
            guide = ggraph::guide_edge_colourbar()
        ) +
        ggraph::scale_edge_width(range = c(0.5, 2), guide = "none") +
        ggplot2::scale_size_continuous(
            name = legend_gene_size_title,
            breaks = function(limits) {
                unique(round(scales::breaks_pretty()(limits)))
            },
            guide = if (show_size_legend) "legend" else "none"
        ) +
        ggplot2::theme_void() +
        ggplot2::theme(
            plot.title = ggplot2::element_text(face = "bold"),
            legend.title = ggplot2::element_text(face = "bold"),
            plot.margin = ggplot2::margin(
                t = plot_margin[1],
                r = plot_margin[2],
                b = plot_margin[3],
                l = plot_margin[4],
                unit = "lines"
            )
        ) +
        ggplot2::ggtitle(title) +
        (if (!is.null(color_mid)) {
            ggplot2::scale_color_gradient2(
                low = color_low,
                mid = color_mid,
                high = color_high,
                midpoint = 0,
                limits = c(-1, 1),
                oob = scales::squish,
                name = legend_gene_color_title
            )
        } else {
            ggplot2::scale_color_gradient(
                low = color_low,
                high = color_high,
                name = legend_gene_color_title
            )
        })
}
