test_that("functions handle non-syntactic column names", {
    # create sample data with spaces and hyphens in column names
    set.seed(123)
    df <- data.frame(
        `y var` = rnorm(30),
        `grp var` = factor(rep(c("a", "b", "c"), each = 10)),
        check.names = FALSE
    )

    # two-group case
    df2 <- df[df[["grp var"]] != "c", , drop = FALSE]
    df2[["grp var"]] <- droplevels(df2[["grp var"]])
    res2 <- plot_numeric_by_2groups("y var", "grp var", df2)
    expect_s3_class(res2$ggplot, "ggplot")
    expect_equal(res2$wilcox$outcome, "y var")

    # three-plus groups case
    res3 <- plot_numeric_by_3plusgroups("y var", "grp var", df)
    expect_s3_class(res3$ggplot, "ggplot")
    expect_equal(res3$stats$outcome[1], "y var")

    # if filtering removes all observations in one group we should fail early
    df_bad <- data.frame(
        y = c(1, NA, NA, NA),
        grp = factor(c("a", "a", "b", "b"))
    )
    expect_error(
        plot_numeric_by_2groups("y", "grp", df_bad),
        "must have observations in both levels"
    )
})
