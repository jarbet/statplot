# ---------------------------------------------------------------------------
# Helpers
# ---------------------------------------------------------------------------

make_cov_df <- function() {
    data(ex_data_heatmap, package = "statplot", envir = environment())
    ex_data_heatmap |>
        dplyr::select(sample, group, condition, sample_type, qc_score) |>
        dplyr::distinct()
}

# ---------------------------------------------------------------------------
# Smoke tests
# ---------------------------------------------------------------------------

test_that("returns a patchwork object invisibly (single categorical)", {
    df <- make_cov_df()
    result <- plot_covariate_heatmap(
        dataset = df,
        color_map = list(group = c(G1 = "#1b9e77", G2 = "#d95f02")),
        row_id_var = "sample"
    )
    expect_s3_class(result, "patchwork")
})

test_that("returns a patchwork object with multiple categorical covariates", {
    df <- make_cov_df()
    result <- plot_covariate_heatmap(
        dataset = df,
        color_map = list(
            group = c(G1 = "#1b9e77", G2 = "#d95f02"),
            condition = c(healthy = "#b3de69", EAE = "#fccde5")
        ),
        row_id_var = "sample"
    )
    expect_s3_class(result, "patchwork")
})

test_that("continuous (colorRamp2) color_map entry errors", {
    df <- make_cov_df()
    rng <- range(df$qc_score, na.rm = TRUE)
    col_fun <- circlize::colorRamp2(
        c(rng[1], mean(rng), rng[2]),
        c("#ffffcc", "#41b6c4", "#0c2c84")
    )
    expect_error(
        plot_covariate_heatmap(
            dataset = df,
            color_map = list(qc_score = col_fun),
            row_id_var = "sample"
        ),
        "not supported"
    )
})

# ---------------------------------------------------------------------------
# return_details
# ---------------------------------------------------------------------------

test_that("return_details = TRUE gives list with ht and final_colors", {
    df <- make_cov_df()
    out <- plot_covariate_heatmap(
        dataset = df,
        color_map = list(group = c(G1 = "#1b9e77", G2 = "#d95f02")),
        row_id_var = "sample",
        return_details = TRUE
    )
    expect_type(out, "list")
    expect_named(out, c("ht", "final_colors"))
    expect_s3_class(out$ht, "patchwork")
})

# ---------------------------------------------------------------------------
# Color resolution
# ---------------------------------------------------------------------------

test_that("user-specified categorical colors are preserved in final_colors", {
    df <- make_cov_df()
    out <- plot_covariate_heatmap(
        dataset = df,
        color_map = list(group = c(G1 = "#1b9e77", G2 = "#d95f02")),
        row_id_var = "sample",
        return_details = TRUE
    )
    expect_equal(unname(out$final_colors$group["G1"]), "#1b9e77")
    expect_equal(unname(out$final_colors$group["G2"]), "#d95f02")
})

test_that("data levels missing from color_map receive auto-generated colors", {
    df <- make_cov_df()
    # Only supply G1; G2 must be auto-filled
    out <- plot_covariate_heatmap(
        dataset = df,
        color_map = list(group = c(G1 = "#1b9e77")),
        row_id_var = "sample",
        return_details = TRUE
    )
    expect_true("G2" %in% names(out$final_colors$group))
    expect_true(nzchar(out$final_colors$group["G2"]))
})


# ---------------------------------------------------------------------------
# row_id_var
# ---------------------------------------------------------------------------

test_that("row_id_var = NULL falls back to sequential integer row labels", {
    df <- make_cov_df()
    expect_no_error(
        plot_covariate_heatmap(
            dataset = df,
            color_map = list(group = c(G1 = "#1b9e77", G2 = "#d95f02")),
            row_id_var = NULL
        )
    )
})

# ---------------------------------------------------------------------------
# Input validation
# ---------------------------------------------------------------------------

test_that("color_map column absent from dataset errors", {
    df <- make_cov_df()
    expect_error(
        plot_covariate_heatmap(
            dataset = df,
            color_map = list(nonexistent = c(a = "#ff0000")),
            row_id_var = "sample"
        ),
        "not found in `dataset`"
    )
})

test_that("unnamed color_map errors", {
    df <- make_cov_df()
    expect_error(
        plot_covariate_heatmap(
            dataset = df,
            color_map = list(c(G1 = "#1b9e77")), # no name on the list element
            row_id_var = "sample"
        ),
        "named"
    )
})

test_that("unnamed character vector inside color_map errors", {
    df <- make_cov_df()
    expect_error(
        plot_covariate_heatmap(
            dataset = df,
            color_map = list(group = c("#1b9e77", "#d95f02")), # no names
            row_id_var = "sample"
        ),
        "named"
    )
})

test_that("row_id_var not in dataset errors", {
    df <- make_cov_df()
    expect_error(
        plot_covariate_heatmap(
            dataset = df,
            color_map = list(group = c(G1 = "#1b9e77", G2 = "#d95f02")),
            row_id_var = "no_such_col"
        ),
        "not found in `dataset`"
    )
})

test_that("non-data-frame dataset errors", {
    expect_error(
        plot_covariate_heatmap(
            dataset = matrix(1:4, 2, 2),
            color_map = list(V1 = c(a = "#ff0000"))
        )
    )
})

# ---------------------------------------------------------------------------
# merge_legends deduplication
# ---------------------------------------------------------------------------

test_that("merge_legends = TRUE with identical categorical color maps does not error", {
    df <- make_cov_df()
    shared_map <- c(G1 = "#1b9e77", G2 = "#d95f02")
    # Add a duplicate column that reuses the same color map
    df$group2 <- df$group
    expect_no_error(
        plot_covariate_heatmap(
            dataset = df,
            color_map = list(group = shared_map, group2 = shared_map),
            row_id_var = "sample",
            merge_legends = TRUE
        )
    )
})

test_that("merge_legends = FALSE (default) with duplicate categorical color maps does not error", {
    df <- make_cov_df()
    shared_map <- c(G1 = "#1b9e77", G2 = "#d95f02")
    df$group2 <- df$group
    expect_no_error(
        plot_covariate_heatmap(
            dataset = df,
            color_map = list(group = shared_map, group2 = shared_map),
            row_id_var = "sample",
            merge_legends = FALSE
        )
    )
})

# ---------------------------------------------------------------------------
# collect_guides
# ---------------------------------------------------------------------------

test_that("collect_guides = FALSE returns a patchwork without internal guide collection", {
    df <- make_cov_df()
    result <- plot_covariate_heatmap(
        dataset = df,
        color_map = list(group = c(G1 = "#1b9e77", G2 = "#d95f02")),
        row_id_var = "sample",
        collect_guides = FALSE
    )
    expect_s3_class(result, "patchwork")
    # The patchwork layout should not contain guides = "collect"
    layout <- result$patches$layout
    expect_false(isTRUE(layout$guides == "collect"))
})

test_that("collect_guides = FALSE allows outer patchwork to collect guides", {
    df <- make_cov_df()
    cov <- plot_covariate_heatmap(
        dataset = df,
        color_map = list(group = c(G1 = "#1b9e77", G2 = "#d95f02")),
        row_id_var = "sample",
        collect_guides = FALSE,
        horizontal = TRUE
    )
    dummy <- ggplot2::ggplot(
        data.frame(x = 1:2, y = c(G1 = "#1b9e77", G2 = "#d95f02")),
        ggplot2::aes(x, x, fill = y)
    ) +
        ggplot2::geom_col()
    expect_no_error(
        patchwork::wrap_plots(dummy, cov, ncol = 1, guides = "collect")
    )
})
