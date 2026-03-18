make_pval_df <- function(n = 6, with_qvalue = FALSE) {
    df <- tibble::tibble(
        term = paste0("gene", seq_len(n)),
        pvalue = c(0.001, 0.01, 0.05, 0.1, 0.2, 0.5)[seq_len(n)]
    )
    if (with_qvalue) {
        df$qvalue <- stats::p.adjust(df$pvalue, method = "fdr")
    }
    df$term <- factor(df$term, levels = rev(df$term))
    df
}

test_that("plot_pvalue_barplot returns a ggplot (no q-value)", {
    df <- make_pval_df()
    p <- plot_pvalue_barplot(
        df,
        x = "pvalue",
        y = "term",
        also_show_qvalue = FALSE
    )
    expect_s3_class(p, "ggplot")
})

test_that("custom_qvalues column is used without error", {
    df <- make_pval_df(with_qvalue = TRUE)
    p <- plot_pvalue_barplot(
        data = df,
        x = "pvalue",
        y = "term",
        also_show_qvalue = TRUE,
        custom_qvalues = "qvalue"
    )
    expect_s3_class(p, "ggplot")
})

test_that("custom_qvalues works with mlog10 transform", {
    df <- make_pval_df(with_qvalue = TRUE)
    p <- plot_pvalue_barplot(
        data = df,
        x = "pvalue",
        y = "term",
        also_show_qvalue = TRUE,
        custom_qvalues = "qvalue",
        mlog10_transform_pvalue = TRUE
    )
    expect_s3_class(p, "ggplot")
})

test_that("custom_qvalues via plot_dotmap ... does not cause 'matched by multiple actual arguments'", {
    # Regression test: previously, passing custom_qvalues through ... to plot_dotmap
    # forwarded it alongside the hardcoded custom_qvalues = 'q_combined' in the
    # internal plot_pvalue_barplot call, triggering the error.
    set.seed(42)
    genes <- paste0("gene", 1:4)
    df <- expand.grid(col = c("A", "B"), row = genes, stringsAsFactors = FALSE)
    df$effect <- rnorm(nrow(df))
    df$p <- runif(nrow(df), 0.001, 0.49)
    df$row <- factor(df$row, levels = rev(genes))

    expect_no_error(
        plot_dotmap(
            df,
            x = "col",
            y = "row",
            effect = "effect",
            p = "p",
            mlog10_transform_pvalue = TRUE,
            add_combined_pvalue_barplot = TRUE,
            # custom_qvalues passed via ... — previously caused the error
            custom_qvalues = NULL
        )
    )
})

test_that("custom_qvalues must be a column present in data", {
    df <- make_pval_df()
    expect_error(
        plot_pvalue_barplot(
            data = df,
            x = "pvalue",
            y = "term",
            also_show_qvalue = TRUE,
            custom_qvalues = "nonexistent_col"
        )
    )
})

test_that("custom_qvalues column must be numeric", {
    df <- make_pval_df()
    df$qvalue_char <- as.character(seq_len(nrow(df)))
    expect_error(
        plot_pvalue_barplot(
            data = df,
            x = "pvalue",
            y = "term",
            also_show_qvalue = TRUE,
            custom_qvalues = "qvalue_char"
        )
    )
})

# ---------------------------------------------------------------------------
# NA p-value handling
# ---------------------------------------------------------------------------

test_that("NA p-values: plot builds without error and BH uses only non-NA rows", {
    df <- make_pval_df()
    df$pvalue[3] <- NA # inject one NA

    # (1) builds without error
    expect_no_error(
        p <- plot_pvalue_barplot(
            data = df,
            x = "pvalue",
            y = "term",
            also_show_qvalue = TRUE,
            mlog10_transform_pvalue = FALSE
        )
    )

    # (2) returns a ggplot
    expect_s3_class(p, "ggplot")

    # (3) q-values for non-NA rows match p.adjust() on the non-NA subset only
    expected_q <- rep(NA_real_, nrow(df))
    non_na <- !is.na(df$pvalue)
    expected_q[non_na] <- stats::p.adjust(df$pvalue[non_na], method = "fdr")

    plot_data <- p$data
    actual_q <- plot_data[[".qvalue_raw"]][match(
        levels(df$term),
        plot_data$term
    )]
    expect_equal(
        actual_q,
        expected_q[match(levels(df$term), df$term)],
        tolerance = 1e-10
    )
})

test_that("NA p-values: mlog10 transform also builds without error", {
    df <- make_pval_df()
    df$pvalue[1] <- NA
    expect_no_error(
        plot_pvalue_barplot(
            data = df,
            x = "pvalue",
            y = "term",
            also_show_qvalue = TRUE,
            mlog10_transform_pvalue = TRUE
        )
    )
})

test_that("NA custom_qvalues: plot builds without error and NA row has no q bar", {
    df <- make_pval_df(with_qvalue = TRUE)
    df$qvalue[2] <- NA # inject NA into custom q-values

    expect_no_error(
        p <- plot_pvalue_barplot(
            data = df,
            x = "pvalue",
            y = "term",
            also_show_qvalue = TRUE,
            custom_qvalues = "qvalue"
        )
    )
    expect_s3_class(p, "ggplot")
    # The injected NA should be preserved in .qvalue_raw
    expect_true(is.na(p$data[[".qvalue_raw"]][p$data$term == df$term[2]]))
})

test_that("all-NA p-values: plot builds (degenerate but valid input)", {
    df <- make_pval_df()
    df$pvalue <- NA_real_
    expect_no_error(
        plot_pvalue_barplot(
            data = df,
            x = "pvalue",
            y = "term",
            also_show_qvalue = TRUE
        )
    )
})
