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
