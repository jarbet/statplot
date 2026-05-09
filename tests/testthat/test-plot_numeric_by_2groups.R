# Test file for plot_numeric_by_2groups

test_that("plot_numeric_by_2groups basic functionality works", {
    set.seed(42)
    df <- data.frame(
        value = rnorm(20),
        group = factor(rep(c("A", "B"), each = 10))
    )

    result <- plot_numeric_by_2groups("value", "group", df)

    expect_true(is.list(result))
    expect_named(result, c("ggplot", "wilcox"))
    expect_s3_class(result$ggplot, "ggplot")
    expect_s3_class(result$wilcox, "tbl_df")
})

test_that("plot_numeric_by_2groups returns correct wilcox columns", {
    set.seed(42)
    df <- data.frame(
        value = rnorm(20),
        group = factor(rep(c("A", "B"), each = 10))
    )

    result <- plot_numeric_by_2groups("value", "group", df)

    expected_cols <- c(
        "estimate",
        "statistic",
        "p.value",
        "conf.low",
        "conf.high",
        "method",
        "alternative",
        "outcome"
    )
    expect_true(all(expected_cols %in% names(result$wilcox)))
})

test_that("plot_numeric_by_2groups requires factor with 2 levels", {
    df <- data.frame(
        value = rnorm(20),
        group = rep(c("A", "B"), each = 10) # character, not factor
    )

    expect_error(
        plot_numeric_by_2groups("value", "group", df),
        "is\\.factor\\(d\\[\\[group\\]\\]\\)"
    )

    # Test with 3 levels
    df2 <- data.frame(
        value = rnorm(30),
        group = factor(rep(c("A", "B", "C"), each = 10))
    )

    expect_error(
        plot_numeric_by_2groups("value", "group", df2),
        "length\\(levels\\(d\\[\\[group\\]\\]\\)\\) == 2"
    )
})

test_that("plot_numeric_by_2groups handles missing values", {
    set.seed(42)
    df <- data.frame(
        value = c(rnorm(18), NA, NA),
        group = factor(rep(c("A", "B"), each = 10))
    )

    result <- plot_numeric_by_2groups("value", "group", df)

    expect_true(is.list(result))
    expect_named(result, c("ggplot", "wilcox"))
})

test_that("plot_numeric_by_2groups errors when one group has no observations", {
    df <- data.frame(
        value = c(1, 2, 3, 4, NA, NA, NA, NA, NA, NA),
        group = factor(rep(c("A", "B"), each = 5))
    )

    expect_error(
        plot_numeric_by_2groups("value", "group", df),
        "must have observations in both levels"
    )
})

test_that("plot_numeric_by_2groups works with named color vector", {
    set.seed(42)
    df <- data.frame(
        value = rnorm(20),
        group = factor(rep(c("A", "B"), each = 10))
    )

    result <- plot_numeric_by_2groups(
        "value",
        "group",
        df,
        colors = c(A = "red", B = "blue")
    )

    expect_true(is.list(result))
})

test_that("plot_numeric_by_2groups works with unnamed color vector", {
    set.seed(42)
    df <- data.frame(
        value = rnorm(20),
        group = factor(rep(c("A", "B"), each = 10))
    )

    result <- plot_numeric_by_2groups(
        "value",
        "group",
        df,
        colors = c("red", "blue")
    )

    expect_true(is.list(result))
})

test_that("plot_numeric_by_2groups works with NULL colors", {
    set.seed(42)
    df <- data.frame(
        value = rnorm(20),
        group = factor(rep(c("A", "B"), each = 10))
    )

    result <- plot_numeric_by_2groups(
        "value",
        "group",
        df,
        colors = NULL
    )

    expect_true(is.list(result))
})

test_that("plot_numeric_by_2groups errors with wrong number of colors", {
    set.seed(42)
    df <- data.frame(
        value = rnorm(20),
        group = factor(rep(c("A", "B"), each = 10))
    )

    expect_error(
        plot_numeric_by_2groups(
            "value",
            "group",
            df,
            colors = c("red", "blue", "green")
        ),
        "length\\(colors\\)"
    )
})

test_that("plot_numeric_by_2groups errors when color names don't match levels", {
    set.seed(42)
    df <- data.frame(
        value = rnorm(20),
        group = factor(rep(c("A", "B"), each = 10))
    )

    expect_error(
        plot_numeric_by_2groups(
            "value",
            "group",
            df,
            colors = c(X = "red", Y = "blue")
        ),
        "setequal\\(names\\(colors\\), group_levels\\)"
    )
})

test_that("plot_numeric_by_2groups applies alpha parameter", {
    set.seed(42)
    df <- data.frame(
        value = rnorm(20),
        group = factor(rep(c("A", "B"), each = 10))
    )

    result <- plot_numeric_by_2groups("value", "group", df, alpha = 0.5)

    expect_true(is.list(result))
    # Check that alpha is in the plot layers
    expect_true(any(sapply(result$ggplot$layers, function(l) {
        !is.null(l$aes_params$alpha) && l$aes_params$alpha == 0.5
    })))
})

test_that("plot_numeric_by_2groups respects digits parameter", {
    set.seed(42)
    df <- data.frame(
        value = rnorm(20),
        group = factor(rep(c("A", "B"), each = 10))
    )

    result1 <- plot_numeric_by_2groups("value", "group", df, digits = 1)
    result2 <- plot_numeric_by_2groups("value", "group", df, digits = 3)

    # Extract text layer data (effect size annotations)
    text1 <- result1$ggplot$layers[[3]]$data$.lbl
    text2 <- result2$ggplot$layers[[3]]$data$.lbl

    # Text labels should be different due to different precision
    expect_false(identical(text1, text2))
})
test_that("plot_numeric_by_2groups works with single facet column", {
    set.seed(42)
    df <- data.frame(
        value = rnorm(40),
        group = factor(rep(c("A", "B"), 20)),
        category = factor(rep(c("X", "Y"), each = 20))
    )

    result <- plot_numeric_by_2groups(
        "value",
        "group",
        df,
        facet_cols = "category"
    )

    expect_true(is.list(result))
    expect_true("category" %in% names(result$wilcox))
    expect_equal(nrow(result$wilcox), 2)
})

test_that("plot_numeric_by_2groups works with multiple facet columns", {
    set.seed(42)
    df <- data.frame(
        value = rnorm(80),
        group = factor(rep(c("A", "B"), 40)),
        category = factor(rep(c("X", "Y"), each = 40)),
        study = factor(rep(c("S1", "S2"), each = 20))
    )

    result <- plot_numeric_by_2groups(
        "value",
        "group",
        df,
        facet_cols = c("category", "study")
    )

    expect_true(is.list(result))
    expect_true(all(c("category", "study") %in% names(result$wilcox)))
    expect_equal(nrow(result$wilcox), 4) # 2 categories x 2 studies
})

test_that("plot_numeric_by_2groups facet results include facet columns", {
    set.seed(42)
    df <- data.frame(
        value = rnorm(40),
        group = factor(rep(c("A", "B"), 20)),
        category = factor(rep(c("X", "Y"), each = 20))
    )

    result <- plot_numeric_by_2groups(
        "value",
        "group",
        df,
        facet_cols = "category"
    )

    expect_true("category" %in% names(result$wilcox))
    expect_equal(nrow(result$wilcox), 2)
    expect_setequal(result$wilcox$category, c("X", "Y"))
})

test_that("plot_numeric_by_2groups faceting produces correct number of tests", {
    set.seed(42)
    df <- data.frame(
        value = rnorm(60),
        group = factor(rep(c("A", "B"), 30)),
        category = factor(rep(c("X", "Y", "Z"), each = 20))
    )

    result <- plot_numeric_by_2groups(
        "value",
        "group",
        df,
        facet_cols = "category"
    )

    expect_equal(nrow(result$wilcox), 3) # One test per category
})

test_that("plot_numeric_by_2groups errors when facet_cols not in data", {
    set.seed(42)
    df <- data.frame(
        value = rnorm(20),
        group = factor(rep(c("A", "B"), each = 10))
    )

    expect_error(
        plot_numeric_by_2groups(
            "value",
            "group",
            df,
            facet_cols = "nonexistent"
        ),
        "`facet_cols` column\\(s\\) not found in `d`"
    )
})

test_that("plot_numeric_by_2groups validates input parameters", {
    df <- data.frame(
        value = rnorm(40),
        group = factor(rep(c("A", "B"), 20))
    )

    # yvar must be length 1
    expect_error(
        plot_numeric_by_2groups(c("value", "value2"), "group", df),
        "length\\(yvar\\) == 1"
    )

    # group must be length 1
    expect_error(
        plot_numeric_by_2groups("value", c("group", "group2"), df),
        "length\\(group\\) == 1"
    )

    # columns must exist in data
    expect_error(
        plot_numeric_by_2groups("nonexistent", "group", df),
        "all\\(c\\(yvar, group\\) %in% names\\(d\\)\\)"
    )

    expect_error(
        plot_numeric_by_2groups("value", "nonexistent", df),
        "all\\(c\\(yvar, group\\) %in% names\\(d\\)\\)"
    )
})

test_that("plot_numeric_by_2groups text adjustment parameters work", {
    set.seed(42)
    df <- data.frame(
        value = rnorm(40),
        group = factor(rep(c("A", "B"), 20)),
        category = factor(rep(c("X", "Y"), each = 20))
    )

    result <- plot_numeric_by_2groups(
        "value",
        "group",
        df,
        facet_cols = "category",
        text_effectsize_vjust = 2,
        text_n_vjust = -1
    )

    expect_true(is.list(result))
    expect_named(result, c("ggplot", "wilcox"))
})


test_that("plot_numeric_by_2groups handles faceting with missing values", {
    set.seed(123)
    df <- data.frame(
        value = c(rnorm(10, 5), rnorm(10, 6), NA, NA),
        group = factor(rep(c("A", "B"), 11)),
        category = c(rep("X", 10), rep("Y", 10), "X", "Y")
    )

    # Should handle NAs in outcome and still produce results for both facets
    suppressWarnings(
        result <- plot_numeric_by_2groups(
            "value",
            "group",
            df,
            facet_cols = "category"
        )
    )

    expect_true(is.list(result))
    expect_named(result, c("ggplot", "wilcox"))

    # Should have results for both X and Y categories
    expect_equal(nrow(result$wilcox), 2)
    expect_true("category" %in% names(result$wilcox))
    expect_setequal(result$wilcox$category, c("X", "Y"))
})
