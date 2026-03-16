#' Run Gene Set Enrichment Analysis (GSEA)
#'
#' A wrapper around [clusterProfiler::GSEA()] that accepts a pre-ranked gene
#' vector and a gene-set data frame, runs GSEA, and returns the result together
#' with the inputs needed by downstream plotting functions
#' ([plot_pathways()] and [plot_pathway_correlation_network()]).
#'
#' @param gene_vec named numeric vector Gene-level statistics (e.g. log2 fold
#'   change, t-statistic) named by gene symbol.  The vector **must be sorted in
#'   decreasing order** before being passed in (or set `sort = TRUE`).
#' @param term2gene data.frame Two-column data frame mapping gene-set names to
#'   gene symbols.  Column 1 must be the gene-set / term name; column 2 must be
#'   the gene symbol.  Column names are coerced to `"term"` and `"gene"`
#'   internally.
#' @param sort logical(1) If `TRUE` (default), `gene_vec` is sorted in
#'   decreasing order before being passed to [clusterProfiler::GSEA()].
#' @param p_adjust_method character(1) Method passed to
#'   [clusterProfiler::GSEA()] for multiple-testing correction (default
#'   `"BH"`).
#' @param p_cutoff numeric(1) `pvalueCutoff` passed to
#'   [clusterProfiler::GSEA()] (default `1`, i.e. return all gene sets).
#'   Must be a single finite value in \[0, 1\].
#' @param min_gs_size integer(1) Minimum gene-set size (default `10`).  Must
#'   be a single positive integer and no greater than `max_gs_size`.
#' @param max_gs_size integer(1) Maximum gene-set size (default `500`).  Must
#'   be a single positive integer and no less than `min_gs_size`.
#' @param seed integer(1) Random seed set before calling
#'   [clusterProfiler::GSEA()] for reproducibility (default `1`).  The
#'   caller's RNG state is saved before the seed is applied and fully restored
#'   on exit, so calling this function does **not** affect subsequent random
#'   operations in the session.
#' @param verbose logical(1) Passed to [clusterProfiler::GSEA()] (default
#'   `FALSE`).
#' @param ... Additional arguments forwarded to [clusterProfiler::GSEA()].
#'
#' @return A list with three elements:
#' \describe{
#'   \item{`gsea_result`}{A `gseaResult` object returned by
#'     [clusterProfiler::GSEA()].}
#'   \item{`gene_vec`}{The (possibly sorted) named numeric vector that was
#'     passed to GSEA — use this as the `foldChange` argument of
#'     [plot_pathways()].}
#'   \item{`term2gene`}{The gene-set data frame (columns `term` and `gene`)
#'     that was passed to GSEA — use this as the `gene_sets` argument of
#'     [plot_pathway_correlation_network()].}
#' }
#'
#' @examples
#' \dontrun{
#' # hallmark_t2g is bundled with the package (columns: term, gene)
#' data(hallmark_t2g)
#' set.seed(1)
#' all_genes <- unique(hallmark_t2g$gene)
#' gene_vec  <- setNames(rnorm(length(all_genes)), all_genes)
#'
#' res <- run_gsea(gene_vec, term2gene = hallmark_t2g)
#' head(as.data.frame(res$gsea_result))
#' }
#'
#' @importFrom clusterProfiler GSEA
#' @export
run_gsea <- function(
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
) {
    stopifnot(
        "gene_vec must be a named numeric vector" = is.numeric(gene_vec) &&
            !is.null(names(gene_vec)),
        "term2gene must be a data.frame with at least 2 columns" = is.data.frame(
            term2gene
        ) &&
            ncol(term2gene) >= 2,
        "p_cutoff must be a single numeric value in [0, 1]" = is.numeric(
            p_cutoff
        ) &&
            length(p_cutoff) == 1 &&
            is.finite(p_cutoff) &&
            p_cutoff >= 0 &&
            p_cutoff <= 1,
        "min_gs_size must be a single positive integer" = is.numeric(
            min_gs_size
        ) &&
            length(min_gs_size) == 1 &&
            is.finite(min_gs_size) &&
            min_gs_size >= 1,
        "max_gs_size must be a single positive integer" = is.numeric(
            max_gs_size
        ) &&
            length(max_gs_size) == 1 &&
            is.finite(max_gs_size) &&
            max_gs_size >= 1,
        "min_gs_size must be <= max_gs_size" = min_gs_size <= max_gs_size,
        "seed must be a single integer or numeric value" = is.numeric(seed) &&
            length(seed) == 1 &&
            is.finite(seed)
    )

    # Normalise term2gene column names
    t2g <- as.data.frame(term2gene)[, 1:2]
    colnames(t2g) <- c("term", "gene")

    if (sort) {
        gene_vec <- sort(gene_vec, decreasing = TRUE)
    }

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
                    exists(".Random.seed", envir = .GlobalEnv, inherits = FALSE)
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

    gsea_result <- clusterProfiler::GSEA(
        geneList = gene_vec,
        TERM2GENE = t2g,
        pAdjustMethod = p_adjust_method,
        pvalueCutoff = p_cutoff,
        minGSSize = min_gs_size,
        maxGSSize = max_gs_size,
        verbose = verbose,
        ...
    )

    list(
        gsea_result = gsea_result,
        gene_vec = gene_vec,
        term2gene = t2g
    )
}
