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

# ---------------------------------------------------------------------------
# merge_legends
# ---------------------------------------------------------------------------

# Shared setup: two row covariates (pvalue, qvalue) with identical color fns
make_merge_legends_df <- function() {
    set.seed(42)
    genes <- paste0("Gene", 1:6)
    samples <- paste0("S", 1:4)
    df <- expand.grid(
        external_gene_name = genes,
        sample = samples,
        stringsAsFactors = FALSE
    )
    df$expression <- stats::rnorm(nrow(df))
    df$pvalue <- rep(stats::runif(length(genes)), each = length(samples))
    df$qvalue <- rep(
        pmin(df$pvalue[seq_along(genes)] * 5, 1),
        each = length(samples)
    )
    df
}

col_fun_p <- circlize::colorRamp2(
    c(0, 0.05, 1),
    c("#e31a1c", "white", "#1f78b4")
)

anno_cols_dup <- list(pvalue = col_fun_p, qvalue = col_fun_p)
anno_cols_diff <- list(
    pvalue = circlize::colorRamp2(
        c(0, 0.05, 1),
        c("#e31a1c", "white", "#1f78b4")
    ),
    qvalue = circlize::colorRamp2(
        c(0, 0.05, 1),
        c("#33a02c", "white", "#ff7f00")
    )
)

test_that("merge_legends = FALSE (default) returns HeatmapList without error", {
    df <- make_merge_legends_df()
    expect_no_error(
        plot_heatmap(
            df,
            row_var = external_gene_name,
            col_var = sample,
            value_var = expression,
            row_covariates = c("pvalue", "qvalue"),
            anno_colors = anno_cols_dup,
            merge_legends = FALSE
        )
    )
})

test_that("merge_legends = TRUE returns HeatmapList without error", {
    df <- make_merge_legends_df()
    expect_no_error(
        plot_heatmap(
            df,
            row_var = external_gene_name,
            col_var = sample,
            value_var = expression,
            row_covariates = c("pvalue", "qvalue"),
            anno_colors = anno_cols_dup,
            merge_legends = TRUE
        )
    )
})

# Local copies of the internal helpers for unit-testing dedup logic directly.
# These must stay in sync with the implementations in R/plot_heatmap.R.
color_key_test <- function(col) {
    if (is.function(col)) {
        env <- environment(col)
        env_names <- ls(env)
        if (all(c("breaks", "colors") %in% env_names)) {
            list(type = "colorRamp2", breaks = env$breaks, colors = env$colors)
        } else {
            list(type = "unknown_fn", fn = col)
        }
    } else if (is.character(col) && !is.null(names(col))) {
        list(type = "named_vector", sorted = col[order(names(col))])
    } else {
        list(type = "other", val = col)
    }
}

dedup_anno_legend_test <- function(covariates, colors) {
    show <- stats::setNames(rep(TRUE, length(covariates)), covariates)
    params <- stats::setNames(vector("list", length(covariates)), covariates)
    group_rep <- character(length(covariates))
    seen_keys <- list()
    seen_reps <- character()
    for (v in covariates) {
        key <- color_key_test(colors[[v]])
        match_idx <- which(vapply(
            seen_keys,
            function(s) identical(s, key),
            logical(1)
        ))
        if (length(match_idx)) {
            group_rep[[which(covariates == v)]] <- seen_reps[[match_idx]]
            show[[v]] <- FALSE
        } else {
            seen_keys <- c(seen_keys, list(key))
            seen_reps <- c(seen_reps, v)
            group_rep[[which(covariates == v)]] <- v
        }
    }
    for (rep_v in unique(group_rep)) {
        members <- covariates[group_rep == rep_v]
        if (length(members) > 1L) {
            params[[rep_v]] <- list(title = paste(members, collapse = "\n"))
        }
    }
    params_clean <- Filter(Negate(is.null), params)
    list(show_legend = show, annotation_legend_param = params_clean)
}

test_that("merge_legends = TRUE: duplicate color fn -> second covariate show_legend is FALSE", {
    res <- dedup_anno_legend_test(c("pvalue", "qvalue"), anno_cols_dup)
    expect_true(res$show_legend[["pvalue"]])
    expect_false(res$show_legend[["qvalue"]])
})

test_that("merge_legends = TRUE: duplicate -> representative gets combined title", {
    res <- dedup_anno_legend_test(c("pvalue", "qvalue"), anno_cols_dup)
    expect_equal(
        res$annotation_legend_param[["pvalue"]][["title"]],
        "pvalue\nqvalue"
    )
    # qvalue is suppressed, so it has no entry in annotation_legend_param
    expect_false("qvalue" %in% names(res$annotation_legend_param))
})

test_that("merge_legends = TRUE: distinct color fns -> both legends shown, no combined title", {
    res <- dedup_anno_legend_test(c("pvalue", "qvalue"), anno_cols_diff)
    expect_true(res$show_legend[["pvalue"]])
    expect_true(res$show_legend[["qvalue"]])
    # No duplicates, so annotation_legend_param should be empty
    expect_length(res$annotation_legend_param, 0L)
})

test_that("merge_legends = TRUE: separately-constructed colorRamp2 with same args are merged", {
    # Two independently constructed functions — would fail with identical() but
    # pass with the semantic color_key() comparison on breaks + colors
    sep_cols <- list(
        pvalue = circlize::colorRamp2(
            c(0, 0.05, 1),
            c("#e31a1c", "white", "#1f78b4")
        ),
        qvalue = circlize::colorRamp2(
            c(0, 0.05, 1),
            c("#e31a1c", "white", "#1f78b4")
        )
    )
    res <- dedup_anno_legend_test(c("pvalue", "qvalue"), sep_cols)
    expect_true(res$show_legend[["pvalue"]])
    expect_false(res$show_legend[["qvalue"]])
    expect_equal(
        res$annotation_legend_param[["pvalue"]][["title"]],
        "pvalue\nqvalue"
    )
})

test_that("merge_legends = TRUE: named vectors merged regardless of element ordering", {
    fwd <- list(pvalue = c(low = "#1f78b4", high = "#e31a1c"))
    rev <- list(pvalue = c(high = "#e31a1c", low = "#1f78b4"))
    # Same mapping, different order — should be treated as equivalent
    res_fwd <- dedup_anno_legend_test("pvalue", fwd)
    res_rev <- dedup_anno_legend_test("pvalue", rev)
    # Single covariate: no duplicate to suppress, but keys should be identical
    expect_true(res_fwd$show_legend[["pvalue"]])
    expect_true(res_rev$show_legend[["pvalue"]])
    # And the keys themselves should match
    expect_identical(color_key_test(fwd$pvalue), color_key_test(rev$pvalue))
})

test_that("merge_legends = TRUE works with col_covariates sharing identical colors", {
    df <- make_merge_legends_df()
    expect_no_error(
        plot_heatmap(
            df,
            row_var = external_gene_name,
            col_var = sample,
            value_var = expression,
            col_covariates = c("pvalue", "qvalue"),
            anno_colors = anno_cols_dup,
            merge_legends = TRUE
        )
    )
})
