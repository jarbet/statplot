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

test_that("plot_pathways() errors when show_category is not a positive whole number", {
    fc <- make_fc()
    expect_error(
        plot_pathways(make_fake_gsea(), fc, show_category = 0),
        "show_category"
    )
    expect_error(
        plot_pathways(make_fake_gsea(), fc, show_category = -1),
        "show_category"
    )
    expect_error(
        plot_pathways(make_fake_gsea(), fc, show_category = 2.5),
        "show_category"
    )
    expect_error(
        plot_pathways(make_fake_gsea(), fc, show_category = c(3L, 5L)),
        "show_category"
    )
    expect_error(
        plot_pathways(make_fake_gsea(), fc, show_category = Inf),
        "show_category"
    )
})

test_that("plot_pathways() errors when fc_threshold is invalid", {
    fc <- make_fc()
    expect_error(
        plot_pathways(make_fake_gsea(), fc, fc_threshold = -1),
        "fc_threshold"
    )
    expect_error(
        plot_pathways(make_fake_gsea(), fc, fc_threshold = c(1, 2)),
        "fc_threshold"
    )
    expect_error(
        plot_pathways(make_fake_gsea(), fc, fc_threshold = Inf),
        "fc_threshold"
    )
})

test_that("plot_pathways() errors when size_item is invalid", {
    fc <- make_fc()
    expect_error(
        plot_pathways(make_fake_gsea(), fc, size_item = 0),
        "size_item"
    )
    expect_error(
        plot_pathways(make_fake_gsea(), fc, size_item = -1),
        "size_item"
    )
    expect_error(
        plot_pathways(make_fake_gsea(), fc, size_item = c(0.5, 1)),
        "size_item"
    )
})

test_that("plot_pathways() errors when size_edge is invalid", {
    fc <- make_fc()
    expect_error(
        plot_pathways(make_fake_gsea(), fc, size_edge = 0),
        "size_edge"
    )
    expect_error(
        plot_pathways(make_fake_gsea(), fc, size_edge = -0.1),
        "size_edge"
    )
    expect_error(
        plot_pathways(make_fake_gsea(), fc, size_edge = c(0.5, 1)),
        "size_edge"
    )
})

test_that("plot_pathways() errors when category_size is invalid", {
    fc <- make_fc()
    expect_error(
        plot_pathways(make_fake_gsea(), fc, category_size = 0),
        "category_size"
    )
    expect_error(
        plot_pathways(make_fake_gsea(), fc, category_size = -2),
        "category_size"
    )
    expect_error(
        plot_pathways(make_fake_gsea(), fc, category_size = c(3, 4)),
        "category_size"
    )
})

test_that("plot_pathways() errors when item_size is invalid", {
    fc <- make_fc()
    expect_error(
        plot_pathways(make_fake_gsea(), fc, item_size = 0),
        "item_size"
    )
    expect_error(
        plot_pathways(make_fake_gsea(), fc, item_size = -1),
        "item_size"
    )
    expect_error(
        plot_pathways(make_fake_gsea(), fc, item_size = c(2, 3)),
        "item_size"
    )
})
