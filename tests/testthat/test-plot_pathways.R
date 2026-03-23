# ---------------------------------------------------------------------------
# Argument validation: plot_pathways()
# ---------------------------------------------------------------------------

# These tests exercise only the input-validation layer, so they do not need
# enrichplot / clusterProfiler to be installed.

make_fake_gsea <- function() {
    structure(list(), class = "gseaResult")
}

make_fc <- function(n = 10) {
    stats::setNames(stats::rnorm(n), paste0("GENE", seq_len(n)))
}

test_that("plot_pathways() errors when fold_change is not a named numeric", {
    expect_error(
        plot_pathways(make_fake_gsea(), fold_change = 1:5),
        "fold_change must be a named numeric vector"
    )
    expect_error(
        plot_pathways(make_fake_gsea(), fold_change = c(a = "x", b = "y")),
        "fold_change must be a named numeric vector"
    )
})

test_that("plot_pathways() errors when show_pathways is not a positive whole number", {
    fc <- make_fc()
    expect_error(
        plot_pathways(make_fake_gsea(), fc, show_pathways = 0),
        "show_pathways"
    )
    expect_error(
        plot_pathways(make_fake_gsea(), fc, show_pathways = -1),
        "show_pathways"
    )
    expect_error(
        plot_pathways(make_fake_gsea(), fc, show_pathways = 2.5),
        "show_pathways"
    )
    expect_error(
        plot_pathways(make_fake_gsea(), fc, show_pathways = c(3L, 5L)),
        "show_pathways"
    )
    expect_error(
        plot_pathways(make_fake_gsea(), fc, show_pathways = Inf),
        "show_pathways"
    )
})

test_that("plot_pathways() errors when effect_size_threshold is invalid", {
    fc <- make_fc()
    expect_error(
        plot_pathways(make_fake_gsea(), fc, effect_size_threshold = -1),
        "effect_size_threshold"
    )
    expect_error(
        plot_pathways(make_fake_gsea(), fc, effect_size_threshold = c(1, 2)),
        "effect_size_threshold"
    )
    expect_error(
        plot_pathways(make_fake_gsea(), fc, effect_size_threshold = Inf),
        "effect_size_threshold"
    )
})

test_that("plot_pathways() errors when gene_node_size is invalid", {
    fc <- make_fc()
    expect_error(
        plot_pathways(make_fake_gsea(), fc, gene_node_size = 0),
        "gene_node_size"
    )
    expect_error(
        plot_pathways(make_fake_gsea(), fc, gene_node_size = -1),
        "gene_node_size"
    )
    expect_error(
        plot_pathways(make_fake_gsea(), fc, gene_node_size = c(0.5, 1)),
        "gene_node_size"
    )
})

test_that("plot_pathways() errors when line_size is invalid", {
    fc <- make_fc()
    expect_error(
        plot_pathways(make_fake_gsea(), fc, line_size = 0),
        "line_size"
    )
    expect_error(
        plot_pathways(make_fake_gsea(), fc, line_size = -0.1),
        "line_size"
    )
    expect_error(
        plot_pathways(make_fake_gsea(), fc, line_size = c(0.5, 1)),
        "line_size"
    )
})

test_that("plot_pathways() errors when pathway_label_size is invalid", {
    fc <- make_fc()
    expect_error(
        plot_pathways(make_fake_gsea(), fc, pathway_label_size = 0),
        "pathway_label_size"
    )
    expect_error(
        plot_pathways(make_fake_gsea(), fc, pathway_label_size = -2),
        "pathway_label_size"
    )
    expect_error(
        plot_pathways(make_fake_gsea(), fc, pathway_label_size = c(3, 4)),
        "pathway_label_size"
    )
})

test_that("plot_pathways() errors when gene_label_size is invalid", {
    fc <- make_fc()
    expect_error(
        plot_pathways(make_fake_gsea(), fc, gene_label_size = 0),
        "gene_label_size"
    )
    expect_error(
        plot_pathways(make_fake_gsea(), fc, gene_label_size = -1),
        "gene_label_size"
    )
    expect_error(
        plot_pathways(make_fake_gsea(), fc, gene_label_size = c(2, 3)),
        "gene_label_size"
    )
})
