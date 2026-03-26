# ---------------------------------------------------------------------------
# Helpers
# ---------------------------------------------------------------------------

make_dotmap_df <- function(n_genes = 4, n_cols = 2, add_q = FALSE, seed = 42) {
    set.seed(seed)
    genes <- paste0("gene", seq_len(n_genes))
    cols <- LETTERS[seq_len(n_cols)]
    df <- expand.grid(col = cols, row = genes, stringsAsFactors = FALSE)
    df$effect <- rnorm(nrow(df))
    df$p <- runif(nrow(df), 0.001, 0.5)
    if (add_q) {
        df$q <- stats::p.adjust(df$p, method = "fdr")
    }
    df
}

# ---------------------------------------------------------------------------
# Basic return types
# ---------------------------------------------------------------------------

test_that("plot_dotmap returns a ggplot without barplot", {
    df <- make_dotmap_df()
    p <- plot_dotmap(df, x = "col", y = "row", effect = "effect", p = "p")
    expect_s3_class(p, "ggplot")
})

test_that("plot_dotmap returns patchwork with add_combined_pvalue_barplot = TRUE", {
    df <- make_dotmap_df()
    p <- plot_dotmap(
        df,
        x = "col",
        y = "row",
        effect = "effect",
        p = "p",
        add_combined_pvalue_barplot = TRUE
    )
    expect_s3_class(p, "patchwork")
})

# ---------------------------------------------------------------------------
# q argument: input validation
# ---------------------------------------------------------------------------

test_that("q must be a character string naming a column in data", {
    df <- make_dotmap_df()
    # non-existent column
    expect_error(
        plot_dotmap(
            df,
            x = "col",
            y = "row",
            effect = "effect",
            p = "p",
            q = "nonexistent"
        ),
        regexp = NULL # stopifnot fires
    )
})

test_that("q must be length-1 character, not a vector", {
    df <- make_dotmap_df(add_q = TRUE)
    expect_error(
        plot_dotmap(
            df,
            x = "col",
            y = "row",
            effect = "effect",
            p = "p",
            q = c("q", "p")
        ),
        regexp = NULL
    )
})

test_that("q column must be numeric", {
    df <- make_dotmap_df(add_q = TRUE)
    df$q_char <- as.character(df$q)
    expect_error(
        plot_dotmap(
            df,
            x = "col",
            y = "row",
            effect = "effect",
            p = "p",
            q = "q_char"
        ),
        regexp = NULL
    )
})

test_that("q column values must be in [0, 1]", {
    df <- make_dotmap_df(add_q = TRUE)
    df$q_bad <- df$q + 2 # out of range
    expect_error(
        plot_dotmap(
            df,
            x = "col",
            y = "row",
            effect = "effect",
            p = "p",
            q = "q_bad"
        ),
        regexp = NULL
    )
})

test_that("q column must not contain Inf or -Inf", {
    df <- make_dotmap_df(add_q = TRUE)
    df$q_inf <- df$q
    df$q_inf[1] <- Inf
    expect_error(
        plot_dotmap(
            df,
            x = "col",
            y = "row",
            effect = "effect",
            p = "p",
            q = "q_inf"
        ),
        regexp = NULL
    )
})

test_that("q column must be strictly positive when mlog10_transform_pvalue = TRUE", {
    df <- make_dotmap_df(add_q = TRUE)
    df$q_zero <- df$q
    df$q_zero[1] <- 0
    expect_error(
        plot_dotmap(
            df,
            x = "col",
            y = "row",
            effect = "effect",
            p = "p",
            q = "q_zero",
            mlog10_transform_pvalue = TRUE
        ),
        regexp = NULL
    )
})

test_that("q column with zero values is allowed when mlog10_transform_pvalue = FALSE", {
    df <- make_dotmap_df(add_q = TRUE)
    df$q_zero <- df$q
    df$q_zero[1] <- 0
    expect_no_error(
        plot_dotmap(
            df,
            x = "col",
            y = "row",
            effect = "effect",
            p = "p",
            q = "q_zero",
            mlog10_transform_pvalue = FALSE
        )
    )
})


test_that("q = NULL (default) produces same result as omitting q", {
    df <- make_dotmap_df()
    p1 <- plot_dotmap(df, x = "col", y = "row", effect = "effect", p = "p")
    p2 <- plot_dotmap(
        df,
        x = "col",
        y = "row",
        effect = "effect",
        p = "p",
        q = NULL
    )
    # Both must be ggplots and their fill_val data should be identical
    expect_s3_class(p1, "ggplot")
    expect_s3_class(p2, "ggplot")
    expect_equal(p1$data$fill_val, p2$data$fill_val)
})

# ---------------------------------------------------------------------------
# q argument: cell fill uses q, not p
# ---------------------------------------------------------------------------

test_that("fill_val reflects q column when q is supplied (raw scale)", {
    df <- make_dotmap_df(add_q = TRUE)
    p <- plot_dotmap(
        df,
        x = "col",
        y = "row",
        effect = "effect",
        p = "p",
        q = "q",
        mlog10_transform_pvalue = FALSE
    )
    # fill_val should equal q, not p
    expect_equal(
        p$data$fill_val,
        df$q[match(paste(p$data$col, p$data$row), paste(df$col, df$row))]
    )
})

test_that("fill_val reflects -log10(q) when q is supplied and mlog10_transform_pvalue = TRUE", {
    df <- make_dotmap_df(add_q = TRUE)
    p <- plot_dotmap(
        df,
        x = "col",
        y = "row",
        effect = "effect",
        p = "p",
        q = "q",
        mlog10_transform_pvalue = TRUE
    )
    matched_q <- df$q[match(
        paste(p$data$col, p$data$row),
        paste(df$col, df$row)
    )]
    expect_equal(p$data$fill_val, -log10(matched_q), tolerance = 1e-10)
})

test_that("fill_val differs between q-based and p-based plots", {
    df <- make_dotmap_df(add_q = TRUE)
    p_fill <- plot_dotmap(
        df,
        x = "col",
        y = "row",
        effect = "effect",
        p = "p",
        mlog10_transform_pvalue = FALSE
    )
    q_fill <- plot_dotmap(
        df,
        x = "col",
        y = "row",
        effect = "effect",
        p = "p",
        q = "q",
        mlog10_transform_pvalue = FALSE
    )
    # p and q values are different (q >= p for BH), so fill_val must differ
    expect_false(isTRUE(all.equal(p_fill$data$fill_val, q_fill$data$fill_val)))
})

# ---------------------------------------------------------------------------
# q argument: combined p-value barplot still uses p, not q
# ---------------------------------------------------------------------------

test_that("combined p-value barplot uses p column even when q is supplied", {
    df <- make_dotmap_df(add_q = TRUE)

    # Build two patchwork plots: one with q (cell fill), one without
    combo_p <- plot_dotmap(
        df,
        x = "col",
        y = "row",
        effect = "effect",
        p = "p",
        add_combined_pvalue_barplot = TRUE
    )
    combo_q <- plot_dotmap(
        df,
        x = "col",
        y = "row",
        effect = "effect",
        p = "p",
        q = "q",
        add_combined_pvalue_barplot = TRUE
    )

    # Extract the barplot panel data (second element of patchwork)
    barplot_p_data <- combo_p[[2]]$data
    barplot_q_data <- combo_q[[2]]$data

    # The barplot's p_combined column must be identical whether or not q is given,
    # because both should compute combined p-values from the p column
    expect_equal(
        barplot_p_data$p_combined,
        barplot_q_data$p_combined,
        tolerance = 1e-10
    )
})

test_that("dotmap cell fill differs between q and p when barplot is added", {
    df <- make_dotmap_df(add_q = TRUE)

    combo_p <- plot_dotmap(
        df,
        x = "col",
        y = "row",
        effect = "effect",
        p = "p",
        add_combined_pvalue_barplot = TRUE,
        mlog10_transform_pvalue = FALSE
    )
    combo_q <- plot_dotmap(
        df,
        x = "col",
        y = "row",
        effect = "effect",
        p = "p",
        q = "q",
        add_combined_pvalue_barplot = TRUE,
        mlog10_transform_pvalue = FALSE
    )

    # Main dotmap is first panel; its fill_val should differ
    expect_false(isTRUE(all.equal(
        combo_p[[1]]$data$fill_val,
        combo_q[[1]]$data$fill_val
    )))
})

# ---------------------------------------------------------------------------
# Legend title reflects q when q is supplied
# ---------------------------------------------------------------------------

test_that("fill scale name reflects qvalue when q is supplied (mlog10 = TRUE)", {
    df <- make_dotmap_df(add_q = TRUE)
    p_q <- plot_dotmap(
        df,
        x = "col",
        y = "row",
        effect = "effect",
        p = "p",
        q = "q",
        mlog10_transform_pvalue = TRUE
    )
    # The first scale (tile fill) should have a name containing "q"
    fill_scale <- p_q$scales$scales[[1]]
    # deparse the expression-based name and check it mentions "q"
    scale_name_str <- paste(deparse(fill_scale$name), collapse = "")
    expect_true(grepl("q", scale_name_str, ignore.case = TRUE))
})

test_that("fill scale name reflects pvalue when q is NULL (mlog10 = TRUE)", {
    df <- make_dotmap_df()
    p_p <- plot_dotmap(
        df,
        x = "col",
        y = "row",
        effect = "effect",
        p = "p",
        mlog10_transform_pvalue = TRUE
    )
    fill_scale <- p_p$scales$scales[[1]]
    scale_name_str <- paste(deparse(fill_scale$name), collapse = "")
    expect_true(grepl("p", scale_name_str, ignore.case = TRUE))
})

test_that("fill scale name is raw column name when q supplied and mlog10 = FALSE", {
    df <- make_dotmap_df(add_q = TRUE)
    p_q <- plot_dotmap(
        df,
        x = "col",
        y = "row",
        effect = "effect",
        p = "p",
        q = "q",
        mlog10_transform_pvalue = FALSE
    )
    fill_scale <- p_q$scales$scales[[1]]
    expect_equal(fill_scale$name, "q")
})

# ---------------------------------------------------------------------------
# NA handling: NAs in p propagate correctly; q NAs handled gracefully
# ---------------------------------------------------------------------------

test_that("NAs in p are tolerated with q supplied (combined barplot)", {
    df <- make_dotmap_df(add_q = TRUE)
    df$p[1] <- NA
    df$q[1] <- NA
    expect_no_error(
        plot_dotmap(
            df,
            x = "col",
            y = "row",
            effect = "effect",
            p = "p",
            q = "q",
            mlog10_transform_pvalue = TRUE,
            add_combined_pvalue_barplot = TRUE
        )
    )
})

# ---------------------------------------------------------------------------
# custom_qvalues forwarding via ... into the combined p-value barplot
# ---------------------------------------------------------------------------

make_dotmap_df_with_combined_q <- function(seed = 42) {
    set.seed(seed)
    genes <- paste0("gene", 1:6)
    df <- expand.grid(
        col = c("A", "B", "C"),
        row = genes,
        stringsAsFactors = FALSE
    )
    df$effect <- rnorm(nrow(df), sd = 1.2)
    df$p <- 10^(-runif(nrow(df), 0, 3))
    df$row <- factor(df$row, levels = rev(genes))
    # Attach one pre-computed combined q-value per row level
    q_per_row <- df |>
        dplyr::group_by(row) |>
        dplyr::summarise(
            my_q = stats::p.adjust(p, method = "bonferroni")[1],
            .groups = "drop"
        )
    df <- dplyr::left_join(df, q_per_row, by = "row")
    df
}

test_that("custom_qvalues forwarded via ... builds patchwork without error", {
    df <- make_dotmap_df_with_combined_q()
    expect_no_error(
        plt <- plot_dotmap(
            df,
            x = "col",
            y = "row",
            effect = "effect",
            p = "p",
            mlog10_transform_pvalue = TRUE,
            add_combined_pvalue_barplot = TRUE,
            combine_pvalue_method = "fisher",
            custom_qvalues = "my_q"
        )
    )
    expect_s3_class(plt, "patchwork")
})

test_that("barplot uses user custom_qvalues, not internal BH q_combined", {
    df <- make_dotmap_df_with_combined_q()

    # Plot with user-supplied custom q-values
    plt_custom <- plot_dotmap(
        df,
        x = "col",
        y = "row",
        effect = "effect",
        p = "p",
        mlog10_transform_pvalue = TRUE,
        add_combined_pvalue_barplot = TRUE,
        combine_pvalue_method = "fisher",
        custom_qvalues = "my_q"
    )
    # Plot without custom q-values (falls back to internal BH)
    plt_bh <- plot_dotmap(
        df,
        x = "col",
        y = "row",
        effect = "effect",
        p = "p",
        mlog10_transform_pvalue = TRUE,
        add_combined_pvalue_barplot = TRUE,
        combine_pvalue_method = "fisher"
    )

    barplot_custom <- plt_custom[[2]]$data
    barplot_bh <- plt_bh[[2]]$data

    # The custom q column must be present and differ from BH q_combined
    expect_true(".custom_q_combined" %in% names(barplot_custom))
    expect_true("q_combined" %in% names(barplot_bh))
    expect_false(isTRUE(all.equal(
        barplot_custom$.custom_q_combined,
        barplot_bh$q_combined[match(barplot_custom$row, barplot_bh$row)]
    )))
})

test_that("custom_qvalues in barplot match the values supplied in data", {
    df <- make_dotmap_df_with_combined_q()

    plt <- plot_dotmap(
        df,
        x = "col",
        y = "row",
        effect = "effect",
        p = "p",
        mlog10_transform_pvalue = TRUE,
        add_combined_pvalue_barplot = TRUE,
        combine_pvalue_method = "fisher",
        custom_qvalues = "my_q"
    )
    barplot_data <- plt[[2]]$data

    # Reconstruct expected per-row q-values from df
    expected <- df |>
        dplyr::select(row, my_q) |>
        dplyr::distinct()

    actual_q <- barplot_data$.custom_q_combined[match(
        expected$row,
        barplot_data$row
    )]
    expect_equal(actual_q, expected$my_q, tolerance = 1e-10)
})

test_that("NULL custom_qvalues falls back to internal BH q_combined without error", {
    df <- make_dotmap_df_with_combined_q()
    expect_no_error(
        plt <- plot_dotmap(
            df,
            x = "col",
            y = "row",
            effect = "effect",
            p = "p",
            mlog10_transform_pvalue = TRUE,
            add_combined_pvalue_barplot = TRUE,
            combine_pvalue_method = "fisher",
            custom_qvalues = NULL
        )
    )
    barplot_data <- plt[[2]]$data
    expect_true("q_combined" %in% names(barplot_data))
    expect_false(".custom_q_combined" %in% names(barplot_data))
})

test_that("custom_qvalues forwarding works with sort_by_pvalue and only_show_top_sig", {
    df <- make_dotmap_df_with_combined_q()
    expect_no_error(
        plt <- plot_dotmap(
            df,
            x = "col",
            y = "row",
            effect = "effect",
            p = "p",
            mlog10_transform_pvalue = TRUE,
            add_combined_pvalue_barplot = TRUE,
            combine_pvalue_method = "fisher",
            sort_by_pvalue = TRUE,
            only_show_top_sig = 3L,
            custom_qvalues = "my_q"
        )
    )
    # Only top 3 rows should appear in the barplot
    barplot_data <- plt[[2]]$data
    expect_equal(nrow(barplot_data), 3L)
})

test_that("custom_qvalues with inconsistent values per y level errors informatively", {
    df <- make_dotmap_df_with_combined_q()
    # Introduce inconsistent q-values within the first row level so that
    # distinct() still returns two rows for that level, which would otherwise
    # silently duplicate rows in the left_join.
    df$my_q[df$row == levels(df$row)[1] & df$col == "A"] <- 0.999
    expect_error(
        plot_dotmap(
            df,
            x = "col",
            y = "row",
            effect = "effect",
            p = "p",
            mlog10_transform_pvalue = TRUE,
            add_combined_pvalue_barplot = TRUE,
            combine_pvalue_method = "fisher",
            custom_qvalues = "my_q"
        ),
        regexp = "inconsistent",
        fixed = FALSE
    )
})
