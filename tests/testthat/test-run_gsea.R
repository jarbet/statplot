# ---------------------------------------------------------------------------
# Helpers shared across tests
# ---------------------------------------------------------------------------

make_gene_vec <- function(seed = 42) {
    data(hallmark_t2g, package = "statplot", envir = parent.frame())
    all_genes <- unique(hallmark_t2g$gene)
    set.seed(seed)
    stats::setNames(stats::rnorm(length(all_genes)), all_genes)
}

# ---------------------------------------------------------------------------
# Argument validation
# ---------------------------------------------------------------------------

test_that("run_gsea() errors when gene_vec is not numeric", {
    skip_if_not_installed("clusterProfiler")
    data(hallmark_t2g, package = "statplot")

    expect_error(
        run_gsea(
            gene_vec = letters[1:10],
            term2gene = hallmark_t2g
        ),
        "gene_vec must be a named numeric vector"
    )
})

test_that("run_gsea() errors when gene_vec has no names", {
    skip_if_not_installed("clusterProfiler")
    data(hallmark_t2g, package = "statplot")

    expect_error(
        run_gsea(
            gene_vec = 1:10,
            term2gene = hallmark_t2g
        ),
        "gene_vec must be a named numeric vector"
    )
})

test_that("run_gsea() errors when term2gene is not a data.frame", {
    skip_if_not_installed("clusterProfiler")
    gene_vec <- make_gene_vec()

    expect_error(
        run_gsea(gene_vec = gene_vec, term2gene = "not_a_df"),
        "term2gene must be a data.frame with at least 2 columns"
    )
})

test_that("run_gsea() errors when term2gene has fewer than 2 columns", {
    skip_if_not_installed("clusterProfiler")
    gene_vec <- make_gene_vec()

    expect_error(
        run_gsea(
            gene_vec = gene_vec,
            term2gene = data.frame(term = "HALLMARK_FOO")
        ),
        "term2gene must be a data.frame with at least 2 columns"
    )
})

# ---------------------------------------------------------------------------
# Validation: p_cutoff, min_gs_size, max_gs_size, seed
# ---------------------------------------------------------------------------

test_that("run_gsea() errors when p_cutoff is out of [0, 1]", {
    skip_if_not_installed("clusterProfiler")
    data(hallmark_t2g, package = "statplot")
    gene_vec <- make_gene_vec()

    expect_error(
        run_gsea(gene_vec, term2gene = hallmark_t2g, p_cutoff = -0.1),
        "p_cutoff"
    )
    expect_error(
        run_gsea(gene_vec, term2gene = hallmark_t2g, p_cutoff = 1.1),
        "p_cutoff"
    )
    expect_error(
        run_gsea(gene_vec, term2gene = hallmark_t2g, p_cutoff = c(0.05, 0.1)),
        "p_cutoff"
    )
})

test_that("run_gsea() errors when min_gs_size is invalid", {
    skip_if_not_installed("clusterProfiler")
    data(hallmark_t2g, package = "statplot")
    gene_vec <- make_gene_vec()

    expect_error(
        run_gsea(gene_vec, term2gene = hallmark_t2g, min_gs_size = 0),
        "min_gs_size"
    )
    expect_error(
        run_gsea(gene_vec, term2gene = hallmark_t2g, min_gs_size = -5),
        "min_gs_size"
    )
    expect_error(
        run_gsea(gene_vec, term2gene = hallmark_t2g, min_gs_size = c(10, 20)),
        "min_gs_size"
    )
})

test_that("run_gsea() errors when max_gs_size is invalid", {
    skip_if_not_installed("clusterProfiler")
    data(hallmark_t2g, package = "statplot")
    gene_vec <- make_gene_vec()

    expect_error(
        run_gsea(gene_vec, term2gene = hallmark_t2g, max_gs_size = 0),
        "max_gs_size"
    )
    expect_error(
        run_gsea(gene_vec, term2gene = hallmark_t2g, max_gs_size = c(100, 500)),
        "max_gs_size"
    )
})

test_that("run_gsea() errors when min_gs_size > max_gs_size", {
    skip_if_not_installed("clusterProfiler")
    data(hallmark_t2g, package = "statplot")
    gene_vec <- make_gene_vec()

    expect_error(
        run_gsea(
            gene_vec,
            term2gene = hallmark_t2g,
            min_gs_size = 500,
            max_gs_size = 10
        ),
        "min_gs_size must be <= max_gs_size"
    )
})

test_that("run_gsea() errors when seed is invalid", {
    skip_if_not_installed("clusterProfiler")
    data(hallmark_t2g, package = "statplot")
    gene_vec <- make_gene_vec()

    expect_error(
        run_gsea(gene_vec, term2gene = hallmark_t2g, seed = "abc"),
        "seed"
    )
    expect_error(
        run_gsea(gene_vec, term2gene = hallmark_t2g, seed = c(1, 2)),
        "seed"
    )
    expect_error(
        run_gsea(gene_vec, term2gene = hallmark_t2g, seed = Inf),
        "seed"
    )
})

# ---------------------------------------------------------------------------
# Sorting behaviour
# ---------------------------------------------------------------------------

test_that("run_gsea() sorts gene_vec in decreasing order when sort = TRUE", {
    skip_if_not_installed("clusterProfiler")
    data(hallmark_t2g, package = "statplot")
    gene_vec <- make_gene_vec()

    # deliberately pass an unsorted vector
    shuffled <- sample(gene_vec)
    res <- run_gsea(shuffled, term2gene = hallmark_t2g, sort = TRUE)

    expect_identical(res$gene_vec, sort(shuffled, decreasing = TRUE))
})

test_that("run_gsea() leaves gene_vec unsorted when sort = FALSE", {
    skip_if_not_installed("clusterProfiler")
    data(hallmark_t2g, package = "statplot")

    # pass a pre-sorted vector so clusterProfiler does not complain
    gene_vec <- sort(make_gene_vec(), decreasing = TRUE)
    res <- run_gsea(gene_vec, term2gene = hallmark_t2g, sort = FALSE)

    expect_identical(res$gene_vec, gene_vec)
})

# ---------------------------------------------------------------------------
# TERM2GENE column-name normalisation
# ---------------------------------------------------------------------------

test_that("run_gsea() renames term2gene columns to 'term' and 'gene'", {
    skip_if_not_installed("clusterProfiler")
    data(hallmark_t2g, package = "statplot")
    gene_vec <- make_gene_vec()

    # supply a term2gene with non-standard column names
    t2g_renamed <- hallmark_t2g
    colnames(t2g_renamed) <- c("pathway", "symbol")

    res <- run_gsea(gene_vec, term2gene = t2g_renamed)

    expect_named(res$term2gene, c("term", "gene"))
})

test_that("run_gsea() only keeps the first two columns of term2gene", {
    skip_if_not_installed("clusterProfiler")
    data(hallmark_t2g, package = "statplot")
    gene_vec <- make_gene_vec()

    # add an extra column that should be dropped
    t2g_extra <- cbind(hallmark_t2g, extra_col = "junk")

    res <- run_gsea(gene_vec, term2gene = t2g_extra)

    expect_equal(ncol(res$term2gene), 2L)
    expect_named(res$term2gene, c("term", "gene"))
})

# ---------------------------------------------------------------------------
# Return-value structure
# ---------------------------------------------------------------------------

test_that("run_gsea() returns a list with the expected elements", {
    skip_if_not_installed("clusterProfiler")
    data(hallmark_t2g, package = "statplot")
    gene_vec <- make_gene_vec()

    res <- run_gsea(gene_vec, term2gene = hallmark_t2g)

    expect_type(res, "list")
    expect_named(res, c("gsea_result", "gene_vec", "term2gene"))
    expect_s4_class(res$gsea_result, "gseaResult")
    expect_true(is.numeric(res$gene_vec) && !is.null(names(res$gene_vec)))
    expect_true(is.data.frame(res$term2gene))
})
