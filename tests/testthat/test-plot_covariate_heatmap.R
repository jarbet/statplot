# ---------------------------------------------------------------------------
# Helpers
# ---------------------------------------------------------------------------

make_cov_df <- function() {
    data(ex_data_heatmap, package = "statplot", envir = environment())
    ex_data_heatmap |>
        dplyr::select(sample, group, condition, sample_type, qc_score) |>
        dplyr::distinct()
}

make_col_fun <- function(x) {
    rng <- range(x, na.rm = TRUE)
    circlize::colorRamp2(
        c(rng[1], mean(rng), rng[2]),
        c("#ffffcc", "#41b6c4", "#0c2c84")
    )
}

# ---------------------------------------------------------------------------
# Smoke tests
# ---------------------------------------------------------------------------

test_that("returns a HeatmapList invisibly (categorical)", {
    df <- make_cov_df()
    result <- plot_covariate_heatmap(
        dataset = df,
        color_map = list(group = c(G1 = "#1b9e77", G2 = "#d95f02")),
        row_id_var = "sample"
    )
    expect_s4_class(result, "HeatmapList")
})

test_that("returns a HeatmapList invisibly (continuous)", {
    df <- make_cov_df()
    result <- plot_covariate_heatmap(
        dataset = df,
        color_map = list(qc_score = make_col_fun(df$qc_score)),
        row_id_var = "sample"
    )
    expect_s4_class(result, "HeatmapList")
})

test_that("returns a HeatmapList with multiple mixed covariates", {
    df <- make_cov_df()
    result <- plot_covariate_heatmap(
        dataset = df,
        color_map = list(
            group = c(G1 = "#1b9e77", G2 = "#d95f02"),
            qc_score = make_col_fun(df$qc_score)
        ),
        row_id_var = "sample"
    )
    expect_s4_class(result, "HeatmapList")
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
    expect_s4_class(out$ht, "HeatmapList")
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

test_that("continuous covariate passes colorRamp2 function through unchanged", {
    df <- make_cov_df()
    fun <- make_col_fun(df$qc_score)
    out <- plot_covariate_heatmap(
        dataset = df,
        color_map = list(qc_score = fun),
        row_id_var = "sample",
        return_details = TRUE
    )
    expect_true(is.function(out$final_colors$qc_score))
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
# row_split_var
# ---------------------------------------------------------------------------

test_that("row_split_var with 2+ levels does not error", {
    df <- make_cov_df()
    expect_no_error(
        plot_covariate_heatmap(
            dataset = df,
            color_map = list(
                condition = c(healthy = "#b3de69", EAE = "#fccde5")
            ),
            row_id_var = "sample",
            row_split_var = "group"
        )
    )
})

test_that("row_split_var with only 1 level errors", {
    df <- make_cov_df() |> dplyr::filter(group == "G1")
    expect_error(
        plot_covariate_heatmap(
            dataset = df,
            color_map = list(
                condition = c(healthy = "#b3de69", EAE = "#fccde5")
            ),
            row_id_var = "sample",
            row_split_var = "group"
        ),
        "at least 2 levels"
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

test_that("merge_legends = TRUE with identical color maps does not error", {
    df <- make_cov_df()
    fun <- make_col_fun(df$qc_score)
    # Add a second numeric column with same range to reuse the same color fn
    df$qc_score2 <- df$qc_score
    expect_no_error(
        plot_covariate_heatmap(
            dataset = df,
            color_map = list(qc_score = fun, qc_score2 = fun),
            row_id_var = "sample",
            merge_legends = TRUE
        )
    )
})

test_that("merge_legends = FALSE (default) with duplicate color maps does not error", {
    df <- make_cov_df()
    fun <- make_col_fun(df$qc_score)
    df$qc_score2 <- df$qc_score
    expect_no_error(
        plot_covariate_heatmap(
            dataset = df,
            color_map = list(qc_score = fun, qc_score2 = fun),
            row_id_var = "sample",
            merge_legends = FALSE
        )
    )
})
