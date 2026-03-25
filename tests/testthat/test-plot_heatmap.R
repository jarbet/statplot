# ---------------------------------------------------------------------------
# Helpers
# ---------------------------------------------------------------------------

# Minimal tidy data: 2 genes x 3 samples, one categorical covariate
# Only direction = "up" is present so we can supply "down" as a phantom level
make_heatmap_df <- function() {
    data(ex_data_heatmap, package = "statplot", envir = environment())
    ex_data_heatmap |>
        dplyr::filter(direction == "up") # only "up" genes present
}

# anno_colors that specifies BOTH "up" and "down", even though "down" is absent
anno_cols_with_phantom <- list(
    direction = c(up = "#1f78b4", down = "#e31a1c"),
    sample_type = c(input = "#7570b3", IP = "#66a61e")
)

# ---------------------------------------------------------------------------
# Basic smoke tests
# ---------------------------------------------------------------------------

test_that("plot_heatmap returns a HeatmapList invisibly", {
    df <- make_heatmap_df()
    result <- plot_heatmap(
        df,
        row_var = external_gene_name,
        col_var = sample,
        value_var = expression,
        scale_rows = FALSE
    )
    expect_s4_class(result, "HeatmapList")
})

test_that("return_details = TRUE gives a list with ht, final_colors, and levels", {
    df <- make_heatmap_df()
    out <- plot_heatmap(
        df,
        row_var = external_gene_name,
        col_var = sample,
        value_var = expression,
        col_covariates = c("direction", "sample_type"),
        scale_rows = FALSE,
        return_details = TRUE
    )
    expect_type(out, "list")
    expect_named(out, c("ht", "final_colors", "levels"))
})

# ---------------------------------------------------------------------------
# phantom anno_color levels always included when anno_colors is supplied
# ---------------------------------------------------------------------------

test_that("phantom anno_color levels are included in final_colors", {
    df <- make_heatmap_df()
    out <- plot_heatmap(
        df,
        row_var = external_gene_name,
        col_var = sample,
        value_var = expression,
        col_covariates = c("direction", "sample_type"),
        anno_colors = anno_cols_with_phantom,
        scale_rows = FALSE,
        return_details = TRUE
    )
    # Both "up" (present) and "down" (absent from data) must be in final_colors
    expect_true("up" %in% names(out$final_colors$direction))
    expect_true("down" %in% names(out$final_colors$direction))
})

test_that("phantom level color value is preserved from anno_colors", {
    df <- make_heatmap_df()
    out <- plot_heatmap(
        df,
        row_var = external_gene_name,
        col_var = sample,
        value_var = expression,
        col_covariates = c("direction", "sample_type"),
        anno_colors = anno_cols_with_phantom,
        scale_rows = FALSE,
        return_details = TRUE
    )
    expect_equal(
        unname(out$final_colors$direction["down"]),
        "#e31a1c"
    )
})

test_that("data-present levels always have a color", {
    df <- make_heatmap_df()
    out <- plot_heatmap(
        df,
        row_var = external_gene_name,
        col_var = sample,
        value_var = expression,
        col_covariates = c("direction", "sample_type"),
        anno_colors = anno_cols_with_phantom,
        scale_rows = FALSE,
        return_details = TRUE
    )
    data_levels <- unique(df$direction) # "up"
    expect_true(all(data_levels %in% names(out$final_colors$direction)))
})

test_that("levels and final_colors are consistent: phantom levels appear in both", {
    df <- make_heatmap_df()
    out <- plot_heatmap(
        df,
        row_var = external_gene_name,
        col_var = sample,
        value_var = expression,
        col_covariates = c("direction", "sample_type"),
        anno_colors = anno_cols_with_phantom,
        scale_rows = FALSE,
        return_details = TRUE
    )
    # levels[[v]] and names(final_colors[[v]]) should contain the same elements
    expect_setequal(
        out$levels$direction,
        names(out$final_colors$direction)
    )
})

# ---------------------------------------------------------------------------
# NULL row_df / col_df robustness
# ---------------------------------------------------------------------------

test_that("col_covariates-only call does not error when row_df is NULL", {
    df <- make_heatmap_df()
    expect_no_error(
        plot_heatmap(
            df,
            row_var = external_gene_name,
            col_var = sample,
            value_var = expression,
            col_covariates = c("direction", "sample_type"),
            row_covariates = NULL,
            anno_colors = anno_cols_with_phantom,
            scale_rows = FALSE,
            cluster_rows = FALSE,
            cluster_columns = FALSE
        )
    )
})

test_that("row_covariates-only call does not error when col_df is NULL", {
    df <- make_heatmap_df()
    expect_no_error(
        plot_heatmap(
            df,
            row_var = external_gene_name,
            col_var = sample,
            value_var = expression,
            row_covariates = c("direction"),
            col_covariates = NULL,
            scale_rows = FALSE,
            cluster_rows = FALSE,
            cluster_columns = FALSE
        )
    )
})
