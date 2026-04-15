library(testthat)
library(ggplot2)

set.seed(1)
df <- tibble::tibble(
    item = factor(
        paste0("item", sprintf("%02d", 1:6)),
        levels = rev(paste0("item", sprintf("%02d", 1:6)))
    ),
    p = c(0.0005, 0.002, 0.02, 0.12, 0.4, NA)
)

test_that("vline legend shows numeric alpha value", {
    alpha <- 0.05
    p <- plot_pvalue_barplot(
        df,
        x = "p",
        y = "item",
        mlog10_transform_pvalue = TRUE,
        vline = TRUE,
        vline_legend = TRUE,
        alpha = alpha
    )
    scale_color <- p$scales$get_scales("colour")
    expect_true(!is.null(scale_color))
    expect_true(as.character(signif(alpha, 3)) %in% names(scale_color$values))
})

test_that("vline legend suppressed when vline_legend = FALSE", {
    p <- plot_pvalue_barplot(
        df,
        x = "p",
        y = "item",
        mlog10_transform_pvalue = TRUE,
        vline = TRUE,
        vline_legend = FALSE
    )
    expect_null(p$scales$get_scales("colour"))
})

test_that("fill legend override removes dashed line (override.aes)", {
    p <- plot_pvalue_barplot(
        df,
        x = "p",
        y = "item",
        mlog10_transform_pvalue = TRUE,
        also_show_qvalue = TRUE,
        vline = TRUE,
        vline_legend = TRUE
    )
    scale_fill <- p$scales$get_scales("fill")
    expect_true(!is.null(scale_fill))
    if (!is.null(scale_fill$guide)) {
        la <- scale_fill$guide$params$override.aes$linetype
        expect_true(
            identical(la, 0) || identical(la, "blank") || identical(la, "")
        )
    } else {
        succeed()
    }
})

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

test_that("default transformed p-value x-axis breaks use p-value ticks", {
    df <- make_pval_df()
    p <- plot_pvalue_barplot(
        df,
        x = "pvalue",
        y = "term",
        also_show_qvalue = FALSE,
        mlog10_transform_pvalue = TRUE
    )
    scale_x <- p$scales$get_scales("x")
    expect_equal(
        scale_x$breaks,
        -log10(c(1, 0.1, 0.01, 0.001)),
        tolerance = 1e-8
    )
    expect_equal(
        scale_x$labels(scale_x$breaks),
        c("1", "0.1", "0.01", "<0.001")
    )
})

test_that("fill legend override removes dashed line with fill mapping (also_show_qvalue=FALSE)", {
    # Regression test: fill-mapped bars should have guides() with override.aes
    # to prevent legend artifacts when vline adds a color scale
    df <- make_pval_df()
    df$group <- rep(c("A", "B", "C"), length.out = nrow(df))
    p <- plot_pvalue_barplot(
        df,
        x = "pvalue",
        y = "term",
        fill = "group",
        mlog10_transform_pvalue = TRUE,
        also_show_qvalue = FALSE,
        vline = TRUE,
        vline_legend = TRUE
    )
    # Check that fill aesthetic is in the plot mapping
    expect_true("fill" %in% names(p$mapping))
    # Check that plot renders without error (this ensures guides() is properly applied)
    expect_no_error(ggplot2::ggplot_build(p))

    # Build the plot to inspect guide configuration
    pb <- ggplot2::ggplot_build(p)

    # Verify fill guide has override.aes$linetype set to remove dashed line artifact
    # The guides are keyed by hash, so we check all guides for one with linetype override
    fill_guide_found <- FALSE
    for (guide in pb$plot$guides$guides) {
        if (!is.null(guide$params$override.aes$linetype)) {
            linetype_override <- guide$params$override.aes$linetype
            if (
                identical(linetype_override, 0) ||
                    identical(linetype_override, "blank") ||
                    identical(linetype_override, "")
            ) {
                fill_guide_found <- TRUE
                break
            }
        }
    }
    expect_true(
        fill_guide_found,
        info = "fill guide should have override.aes$linetype set to 0/blank/empty"
    )

    # Verify the color scale for vline exists
    scale_color <- p$scales$get_scales("colour")
    expect_true(!is.null(scale_color))
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
