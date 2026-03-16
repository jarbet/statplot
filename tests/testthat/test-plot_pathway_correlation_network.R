# ---------------------------------------------------------------------------
# Helpers
# ---------------------------------------------------------------------------

skip_network_deps <- function() {
    skip_if_not_installed("igraph")
    skip_if_not_installed("ggraph")
}

make_network_inputs <- function() {
    data(ex_expr_pathway, package = "statplot", envir = parent.frame())
    data(ex_log2fc_pathway, package = "statplot", envir = parent.frame())
    data(hallmark_t2g, package = "statplot", envir = parent.frame())
    list(
        expr = ex_expr_pathway,
        log2fc = ex_log2fc_pathway,
        gene_sets = hallmark_t2g,
        pathway = "MYC_TARGETS_V1"
    )
}

# ---------------------------------------------------------------------------
# RNG state preservation
# ---------------------------------------------------------------------------

test_that("plot_pathway_correlation_network() does not alter the caller's RNG state", {
    skip_network_deps()
    inp <- make_network_inputs()

    set.seed(99)
    # Capture the RNG state immediately after a fixed seed
    before <- .Random.seed

    suppressMessages(
        plot_pathway_correlation_network(
            expr = inp$expr,
            pathway = inp$pathway,
            gene_sets = inp$gene_sets,
            log2fc = inp$log2fc,
            seed = 42L
        )
    )

    # The global RNG state must be identical to what it was before the call
    expect_identical(.Random.seed, before)
})

test_that("plot_pathway_correlation_network() with seed = NULL does not restore RNG", {
    skip_network_deps()
    inp <- make_network_inputs()

    set.seed(99)
    before <- .Random.seed

    suppressMessages(
        plot_pathway_correlation_network(
            expr = inp$expr,
            pathway = inp$pathway,
            gene_sets = inp$gene_sets,
            log2fc = inp$log2fc,
            seed = NULL
        )
    )

    # seed = NULL means no seeding at all; ggraph uses current RNG, so the
    # state afterwards will differ from `before`
    expect_false(identical(.Random.seed, before))
})

test_that("plot_pathway_correlation_network() preserves RNG even when no .Random.seed exists yet", {
    skip_network_deps()
    inp <- make_network_inputs()

    # Remove .Random.seed to simulate a fresh session
    if (exists(".Random.seed", envir = .GlobalEnv, inherits = FALSE)) {
        old <- .Random.seed
        on.exit(assign(".Random.seed", old, envir = .GlobalEnv), add = TRUE)
        rm(".Random.seed", envir = .GlobalEnv)
    }

    expect_false(exists(".Random.seed", envir = .GlobalEnv, inherits = FALSE))

    suppressMessages(
        plot_pathway_correlation_network(
            expr = inp$expr,
            pathway = inp$pathway,
            gene_sets = inp$gene_sets,
            log2fc = inp$log2fc,
            seed = 42L
        )
    )

    # After the call, .Random.seed should again be absent (restored to its
    # pre-call state of non-existence)
    expect_false(exists(".Random.seed", envir = .GlobalEnv, inherits = FALSE))
})

# ---------------------------------------------------------------------------
# Deterministic layout with fixed seed
# ---------------------------------------------------------------------------

test_that("plot_pathway_correlation_network() produces the same layout for the same seed", {
    skip_network_deps()
    inp <- make_network_inputs()

    p1 <- suppressMessages(
        plot_pathway_correlation_network(
            expr = inp$expr,
            pathway = inp$pathway,
            gene_sets = inp$gene_sets,
            log2fc = inp$log2fc,
            seed = 42L
        )
    )
    p2 <- suppressMessages(
        plot_pathway_correlation_network(
            expr = inp$expr,
            pathway = inp$pathway,
            gene_sets = inp$gene_sets,
            log2fc = inp$log2fc,
            seed = 42L
        )
    )

    # Compare node x/y coordinates only — igraph pointers in $data differ per call.
    # Use as.data.frame() to drop the 'graph' attribute that ggraph attaches.
    coords <- function(p) as.data.frame(p$data)[, c("x", "y", "name")]
    expect_equal(coords(p1), coords(p2))
})

# ---------------------------------------------------------------------------
# Simulated-data helpers (no Bioconductor deps required)
# ---------------------------------------------------------------------------

# Build a genes-x-samples matrix whose rows are nearly identical (high |cor|).
# cor_sd: row noise; smaller => more correlated rows.
make_sim_expr <- function(
    n_genes = 5,
    n_samples = 20,
    seed = 1,
    cor_sd = 0.01
) {
    set.seed(seed)
    base <- stats::rnorm(n_samples)
    mat <- t(vapply(
        seq_len(n_genes),
        function(i) base + stats::rnorm(n_samples, sd = cor_sd),
        numeric(n_samples)
    ))
    rownames(mat) <- paste0("G", seq_len(n_genes))
    colnames(mat) <- paste0("S", seq_len(n_samples))
    mat
}

make_sim_gs <- function(n_genes = 5, pathway = "PATHWAY_A") {
    data.frame(term = pathway, gene = paste0("G", seq_len(n_genes)))
}

make_sim_lfc <- function(n_genes = 5, seed = 2) {
    set.seed(seed)
    stats::setNames(stats::rnorm(n_genes), paste0("G", seq_len(n_genes)))
}

# ---------------------------------------------------------------------------
# Argument validation
# ---------------------------------------------------------------------------

test_that("plot_pathway_correlation_network() errors when expr is not a matrix", {
    skip_network_deps()
    expect_error(
        plot_pathway_correlation_network(
            expr = as.data.frame(make_sim_expr()),
            pathway = "PATHWAY_A",
            gene_sets = make_sim_gs(),
            log2fc = make_sim_lfc()
        ),
        "expr must be a matrix with rownames"
    )
})

test_that("plot_pathway_correlation_network() errors when expr has no rownames", {
    skip_network_deps()
    mat <- make_sim_expr()
    rownames(mat) <- NULL
    expect_error(
        plot_pathway_correlation_network(
            expr = mat,
            pathway = "PATHWAY_A",
            gene_sets = make_sim_gs(),
            log2fc = make_sim_lfc()
        ),
        "expr must be a matrix with rownames"
    )
})

test_that("plot_pathway_correlation_network() errors when gene_sets lacks required columns", {
    skip_network_deps()
    bad_gs <- data.frame(pathway = "PATHWAY_A", symbol = "G1")
    expect_error(
        plot_pathway_correlation_network(
            expr = make_sim_expr(),
            pathway = "PATHWAY_A",
            gene_sets = bad_gs,
            log2fc = make_sim_lfc()
        ),
        "gene_sets must be a data.frame with columns 'term' and 'gene'"
    )
})

test_that("plot_pathway_correlation_network() errors when log2fc is unnamed", {
    skip_network_deps()
    expect_error(
        plot_pathway_correlation_network(
            expr = make_sim_expr(),
            pathway = "PATHWAY_A",
            gene_sets = make_sim_gs(),
            log2fc = 1:5
        ),
        "log2fc must be a named numeric vector"
    )
})

test_that("plot_pathway_correlation_network() errors when pathway is not length 1", {
    skip_network_deps()
    expect_error(
        plot_pathway_correlation_network(
            expr = make_sim_expr(),
            pathway = c("PATHWAY_A", "PATHWAY_B"),
            gene_sets = make_sim_gs(),
            log2fc = make_sim_lfc()
        ),
        "pathway must be a single character string"
    )
})

# ---------------------------------------------------------------------------
# NULL-return paths
# ---------------------------------------------------------------------------

test_that("plot_pathway_correlation_network() returns NULL when fewer than 3 pathway genes are in expr", {
    skip_network_deps()
    # gene_sets maps only 2 genes to the pathway
    gs_2 <- data.frame(term = "PATHWAY_A", gene = c("G1", "G2"))
    expect_message(
        result <- plot_pathway_correlation_network(
            expr = make_sim_expr(),
            pathway = "PATHWAY_A",
            gene_sets = gs_2,
            log2fc = make_sim_lfc()
        ),
        "Fewer than 3 pathway genes"
    )
    expect_null(result)
})

test_that("plot_pathway_correlation_network() returns NULL when no gene pairs pass cor_thresh", {
    skip_network_deps()
    # cor_thresh = 1 is the upper bound; no random off-diagonal pair has |cor| == 1
    expect_message(
        result <- plot_pathway_correlation_network(
            expr = make_sim_expr(),
            pathway = "PATHWAY_A",
            gene_sets = make_sim_gs(),
            log2fc = make_sim_lfc(),
            cor_thresh = 1
        ),
        "No gene pairs pass"
    )
    expect_null(result)
})

# ---------------------------------------------------------------------------
# Successful plot output
# ---------------------------------------------------------------------------

test_that("plot_pathway_correlation_network() returns a ggplot when edges exist", {
    skip_network_deps()
    p <- suppressMessages(
        plot_pathway_correlation_network(
            expr = make_sim_expr(),
            pathway = "PATHWAY_A",
            gene_sets = make_sim_gs(),
            log2fc = make_sim_lfc(),
            cor_thresh = 0 # accept all pairs
        )
    )
    expect_s3_class(p, "ggplot")
})

# ---------------------------------------------------------------------------
# top_n_genes filtering
# ---------------------------------------------------------------------------

test_that("plot_pathway_correlation_network() restricts nodes to top_n_genes", {
    skip_network_deps()
    # 5-gene pathway, but ask for only the top 3 by |log2fc|
    p <- suppressMessages(
        plot_pathway_correlation_network(
            expr = make_sim_expr(n_genes = 5),
            pathway = "PATHWAY_A",
            gene_sets = make_sim_gs(n_genes = 5),
            log2fc = make_sim_lfc(n_genes = 5),
            top_n_genes = 3,
            cor_thresh = 0
        )
    )
    expect_s3_class(p, "ggplot")
    # Node table has at most top_n_genes rows
    expect_lte(nrow(as.data.frame(p$data)), 3)
})

test_that("plot_pathway_correlation_network() uses all genes when top_n_genes = Inf", {
    skip_network_deps()
    n <- 5
    p <- suppressMessages(
        plot_pathway_correlation_network(
            expr = make_sim_expr(n_genes = n),
            pathway = "PATHWAY_A",
            gene_sets = make_sim_gs(n_genes = n),
            log2fc = make_sim_lfc(n_genes = n),
            top_n_genes = Inf,
            cor_thresh = 0
        )
    )
    expect_s3_class(p, "ggplot")
    expect_equal(nrow(as.data.frame(p$data)), n)
})

# ---------------------------------------------------------------------------
# Input validation: top_n_genes and cor_thresh
# ---------------------------------------------------------------------------

test_that("plot_pathway_correlation_network() rejects non-positive top_n_genes", {
    base_args <- list(
        expr = make_sim_expr(),
        pathway = "PATHWAY_A",
        gene_sets = make_sim_gs(),
        log2fc = make_sim_lfc()
    )
    expect_error(
        do.call(
            plot_pathway_correlation_network,
            c(base_args, list(top_n_genes = 0))
        ),
        "top_n_genes"
    )
    expect_error(
        do.call(
            plot_pathway_correlation_network,
            c(base_args, list(top_n_genes = -5))
        ),
        "top_n_genes"
    )
    expect_error(
        do.call(
            plot_pathway_correlation_network,
            c(base_args, list(top_n_genes = c(10, 20)))
        ),
        "top_n_genes"
    )
})

test_that("plot_pathway_correlation_network() rejects cor_thresh outside [0, 1]", {
    base_args <- list(
        expr = make_sim_expr(),
        pathway = "PATHWAY_A",
        gene_sets = make_sim_gs(),
        log2fc = make_sim_lfc()
    )
    expect_error(
        do.call(
            plot_pathway_correlation_network,
            c(base_args, list(cor_thresh = -0.1))
        ),
        "cor_thresh"
    )
    expect_error(
        do.call(
            plot_pathway_correlation_network,
            c(base_args, list(cor_thresh = 1.1))
        ),
        "cor_thresh"
    )
    expect_error(
        do.call(
            plot_pathway_correlation_network,
            c(base_args, list(cor_thresh = c(0.5, 0.8)))
        ),
        "cor_thresh"
    )
})
