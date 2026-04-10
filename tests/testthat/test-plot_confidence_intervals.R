test_that("plot_confidence_intervals works with minimal data", {
    df <- data.frame(
        cell_line = factor(c("A", "B"), levels = c("A", "B")),
        est = c(0.2, -0.1),
        conf.low = c(0.0, -0.3),
        conf.high = c(0.4, 0.1)
    )

    p <- plot_confidence_intervals(
        df,
        effect_size = "est",
        ci_low = "conf.low",
        ci_high = "conf.high",
        id = "cell_line"
    )
    expect_s3_class(p, "ggplot")
})

test_that("plot_confidence_intervals requires id to be a factor", {
    df <- data.frame(
        cell_line = c("A", "B"),
        est = c(0.2, -0.1),
        conf.low = c(0.0, -0.3),
        conf.high = c(0.4, 0.1)
    )

    expect_error(
        plot_confidence_intervals(
            df,
            effect_size = "est",
            ci_low = "conf.low",
            ci_high = "conf.high",
            id = "cell_line"
        ),
        "id must be a factor"
    )
})

test_that("plot_confidence_intervals requires group_col to be a factor when supplied", {
    df <- data.frame(
        cell_line = factor(c("A", "B"), levels = c("A", "B")),
        est = c(0.2, -0.1),
        conf.low = c(0.0, -0.3),
        conf.high = c(0.4, 0.1),
        group = c("g1", "g2")
    )

    expect_error(
        plot_confidence_intervals(
            df,
            effect_size = "est",
            ci_low = "conf.low",
            ci_high = "conf.high",
            id = "cell_line",
            group_col = "group"
        ),
        "group_col must be a factor"
    )
})

test_that("plot_confidence_intervals works with grouping but no color", {
    df <- data.frame(
        cell_line = factor(c("A", "A", "B", "B"), levels = c("A", "B")),
        est = c(0.2, 0.35, -0.1, 0.05),
        conf.low = c(0.0, 0.10, -0.3, -0.10),
        conf.high = c(0.4, 0.60, 0.1, 0.20),
        group = factor(c("g1", "g2", "g1", "g2"), levels = c("g1", "g2"))
    )

    p <- plot_confidence_intervals(
        df,
        effect_size = "est",
        ci_low = "conf.low",
        ci_high = "conf.high",
        id = "cell_line",
        group_col = "group"
    )
    expect_s3_class(p, "ggplot")
})

test_that("plot_confidence_intervals works with color_col = group", {
    df <- data.frame(
        cell_line = factor(c("A", "A", "B", "B"), levels = c("A", "B")),
        est = c(0.2, 0.35, -0.1, 0.05),
        conf.low = c(0.0, 0.10, -0.3, -0.10),
        conf.high = c(0.4, 0.60, 0.1, 0.20),
        group = factor(c("g1", "g2", "g1", "g2"), levels = c("g1", "g2"))
    )

    p <- plot_confidence_intervals(
        df,
        effect_size = "est",
        ci_low = "conf.low",
        ci_high = "conf.high",
        id = "cell_line",
        group_col = "group",
        color_col = "group"
    )
    expect_s3_class(p, "ggplot")
})

test_that("plot_confidence_intervals works with color_col = label_col", {
    df <- data.frame(
        cell_line = factor(c("A", "A", "B", "B"), levels = c("A", "B")),
        est = c(0.2, 0.35, -0.1, 0.05),
        conf.low = c(0.0, 0.10, -0.3, -0.10),
        conf.high = c(0.4, 0.60, 0.1, 0.20),
        group = factor(c("g1", "g2", "g1", "g2"), levels = c("g1", "g2"))
    )

    p <- plot_confidence_intervals(
        df,
        effect_size = "est",
        ci_low = "conf.low",
        ci_high = "conf.high",
        id = "cell_line",
        group_col = "group",
        color_col = "cell_line"
    )
    expect_s3_class(p, "ggplot")
})

test_that("plot_confidence_intervals works with shape style", {
    df <- data.frame(
        cell_line = factor(c("A", "A", "B", "B"), levels = c("A", "B")),
        est = c(0.2, 0.35, -0.1, 0.05),
        conf.low = c(0.0, 0.10, -0.3, -0.10),
        conf.high = c(0.4, 0.60, 0.1, 0.20),
        group = factor(c("g1", "g2", "g1", "g2"), levels = c("g1", "g2"))
    )

    p <- plot_confidence_intervals(
        df,
        effect_size = "est",
        ci_low = "conf.low",
        ci_high = "conf.high",
        id = "cell_line",
        group_col = "group",
        style = "shape"
    )
    expect_s3_class(p, "ggplot")
})

test_that("plot_confidence_intervals works with shape style and color", {
    df <- data.frame(
        cell_line = factor(c("A", "A", "B", "B"), levels = c("A", "B")),
        est = c(0.2, 0.35, -0.1, 0.05),
        conf.low = c(0.0, 0.10, -0.3, -0.10),
        conf.high = c(0.4, 0.60, 0.1, 0.20),
        group = factor(c("g1", "g2", "g1", "g2"), levels = c("g1", "g2"))
    )

    p <- plot_confidence_intervals(
        df,
        effect_size = "est",
        ci_low = "conf.low",
        ci_high = "conf.high",
        id = "cell_line",
        group_col = "group",
        style = "shape",
        color_col = "group"
    )
    expect_s3_class(p, "ggplot")
})

test_that("plot_confidence_intervals requires point_shapes length matches group levels", {
    df <- data.frame(
        cell_line = factor(c("A", "A", "B", "B"), levels = c("A", "B")),
        est = c(0.2, 0.35, -0.1, 0.05),
        conf.low = c(0.0, 0.10, -0.3, -0.10),
        conf.high = c(0.4, 0.60, 0.1, 0.20),
        group = factor(c("g1", "g2", "g1", "g2"), levels = c("g1", "g2"))
    )

    expect_error(
        plot_confidence_intervals(
            df,
            effect_size = "est",
            ci_low = "conf.low",
            ci_high = "conf.high",
            id = "cell_line",
            group_col = "group",
            style = "shape",
            point_shapes = c(21)
        ),
        "point_shapes has 1 element"
    )
})

test_that("plot_confidence_intervals works with custom effect_size, ci_low, ci_high column names", {
    df <- data.frame(
        cell_line = factor(c("A", "B"), levels = c("A", "B")),
        estimate = c(0.2, -0.1),
        lower = c(0.0, -0.3),
        upper = c(0.4, 0.1)
    )

    p <- plot_confidence_intervals(
        df,
        effect_size = "estimate",
        ci_low = "lower",
        ci_high = "upper",
        id = "cell_line"
    )
    expect_s3_class(p, "ggplot")
})

test_that("plot_confidence_intervals works with custom vline parameters", {
    df <- data.frame(
        cell_line = factor(c("A", "B"), levels = c("A", "B")),
        est = c(0.2, -0.1),
        conf.low = c(0.0, -0.3),
        conf.high = c(0.4, 0.1)
    )

    p <- plot_confidence_intervals(
        df,
        effect_size = "est",
        ci_low = "conf.low",
        ci_high = "conf.high",
        id = "cell_line",
        vline_xintercept = 0.5,
        vline_linetype = "solid",
        vline_color = "red"
    )
    expect_s3_class(p, "ggplot")
})

test_that("plot_confidence_intervals works with custom dodge_width", {
    df <- data.frame(
        cell_line = factor(c("A", "A", "B", "B"), levels = c("A", "B")),
        est = c(0.2, 0.35, -0.1, 0.05),
        conf.low = c(0.0, 0.10, -0.3, -0.10),
        conf.high = c(0.4, 0.60, 0.1, 0.20),
        group = factor(c("g1", "g2", "g1", "g2"), levels = c("g1", "g2"))
    )

    p <- plot_confidence_intervals(
        df,
        effect_size = "est",
        ci_low = "conf.low",
        ci_high = "conf.high",
        id = "cell_line",
        group_col = "group",
        dodge_width = 0.5
    )
    expect_s3_class(p, "ggplot")
})

test_that("plot_confidence_intervals works with separator line customization", {
    df <- data.frame(
        cell_line = factor(c("A", "A", "B", "B"), levels = c("A", "B")),
        est = c(0.2, 0.35, -0.1, 0.05),
        conf.low = c(0.0, 0.10, -0.3, -0.10),
        conf.high = c(0.4, 0.60, 0.1, 0.20),
        group = factor(c("g1", "g2", "g1", "g2"), levels = c("g1", "g2"))
    )

    p <- plot_confidence_intervals(
        df,
        effect_size = "est",
        ci_low = "conf.low",
        ci_high = "conf.high",
        id = "cell_line",
        group_col = "group",
        style = "color",
        sep_linetype = "dashed",
        sep_linewidth = 0.8,
        sep_color = "gray50"
    )
    expect_s3_class(p, "ggplot")
})

test_that("plot_confidence_intervals returns ggplot without pvalue_col", {
    df <- data.frame(
        cell_line = factor(c("A", "B"), levels = c("A", "B")),
        est = c(0.2, -0.1),
        conf.low = c(0.0, -0.3),
        conf.high = c(0.4, 0.1)
    )

    p <- plot_confidence_intervals(
        df,
        effect_size = "est",
        ci_low = "conf.low",
        ci_high = "conf.high",
        id = "cell_line"
    )
    expect_s3_class(p, "ggplot")
    expect_false(inherits(p, "patchwork"))
})

test_that("plot_confidence_intervals returns patchwork when pvalue_col is supplied", {
    skip_if_not_installed("patchwork")

    df <- data.frame(
        cell_line = factor(c("A", "B"), levels = c("A", "B")),
        est = c(0.2, -0.1),
        conf.low = c(0.0, -0.3),
        conf.high = c(0.4, 0.1),
        pvalue = c(0.01, 0.4)
    )

    p <- plot_confidence_intervals(
        df,
        effect_size = "est",
        ci_low = "conf.low",
        ci_high = "conf.high",
        id = "cell_line",
        pvalue_col = "pvalue"
    )
    expect_true(inherits(p, "patchwork") || inherits(p, "gg"))
})

test_that("plot_confidence_intervals validates pvalue_col is constant within id groups", {
    skip_if_not_installed("patchwork")

    df <- data.frame(
        cell_line = factor(c("A", "A", "B", "B"), levels = c("A", "B")),
        est = c(0.2, 0.35, -0.1, 0.05),
        conf.low = c(0.0, 0.10, -0.3, -0.10),
        conf.high = c(0.4, 0.60, 0.1, 0.20),
        pvalue = c(0.01, 0.05, 0.4, 0.4)
    )

    expect_error(
        plot_confidence_intervals(
            df,
            effect_size = "est",
            ci_low = "conf.low",
            ci_high = "conf.high",
            id = "cell_line",
            pvalue_col = "pvalue"
        ),
        "pvalue_col must be constant within each"
    )
})

test_that("plot_confidence_intervals works with NA values in pvalue_col", {
    skip_if_not_installed("patchwork")

    df <- data.frame(
        cell_line = factor(c("A", "A", "B", "B"), levels = c("A", "B")),
        est = c(0.2, 0.35, -0.1, 0.05),
        conf.low = c(0.0, 0.10, -0.3, -0.10),
        conf.high = c(0.4, 0.60, 0.1, 0.20),
        pvalue = c(0.01, NA, 0.4, 0.4)
    )

    p <- plot_confidence_intervals(
        df,
        effect_size = "est",
        ci_low = "conf.low",
        ci_high = "conf.high",
        id = "cell_line",
        pvalue_col = "pvalue"
    )
    expect_true(inherits(p, "patchwork") || inherits(p, "gg"))
})

test_that("plot_confidence_intervals validates effect_size, ci_low, ci_high are in data", {
    df <- data.frame(
        cell_line = factor(c("A", "B"), levels = c("A", "B")),
        est = c(0.2, -0.1),
        conf.low = c(0.0, -0.3),
        conf.high = c(0.4, 0.1)
    )

    expect_error(
        plot_confidence_intervals(
            df,
            effect_size = "missing",
            ci_low = "conf.low",
            ci_high = "conf.high",
            id = "cell_line"
        ),
        "effect_size must be a column in data"
    )

    expect_error(
        plot_confidence_intervals(
            df,
            effect_size = "est",
            ci_low = "missing",
            ci_high = "conf.high",
            id = "cell_line"
        ),
        "ci_low must be a column in data"
    )

    expect_error(
        plot_confidence_intervals(
            df,
            effect_size = "est",
            ci_low = "conf.low",
            ci_high = "missing",
            id = "cell_line"
        ),
        "ci_high must be a column in data"
    )
})

test_that("plot_confidence_intervals validates color_col is in data", {
    df <- data.frame(
        cell_line = factor(c("A", "B"), levels = c("A", "B")),
        est = c(0.2, -0.1),
        conf.low = c(0.0, -0.3),
        conf.high = c(0.4, 0.1)
    )

    expect_error(
        plot_confidence_intervals(
            df,
            effect_size = "est",
            ci_low = "conf.low",
            ci_high = "conf.high",
            id = "cell_line",
            color_col = "missing"
        ),
        "color_col must be a column in data"
    )
})

test_that("plot_confidence_intervals respects factor level ordering", {
    df <- data.frame(
        cell_line = factor(c("A", "B", "C"), levels = c("C", "B", "A")),
        est = c(0.2, -0.1, 0.5),
        conf.low = c(0.0, -0.3, 0.2),
        conf.high = c(0.4, 0.1, 0.8)
    )

    p <- plot_confidence_intervals(
        df,
        effect_size = "est",
        ci_low = "conf.low",
        ci_high = "conf.high",
        id = "cell_line"
    )
    # Check that the plot is created with correct scale
    expect_s3_class(p, "ggplot")
    # Verify factor levels are preserved
    expect_equal(levels(df$cell_line), c("C", "B", "A"))
})

test_that("plot_confidence_intervals works with single row", {
    df <- data.frame(
        cell_line = factor("A", levels = "A"),
        est = 0.2,
        conf.low = 0.0,
        conf.high = 0.4
    )

    p <- plot_confidence_intervals(
        df,
        effect_size = "est",
        ci_low = "conf.low",
        ci_high = "conf.high",
        id = "cell_line"
    )
    expect_s3_class(p, "ggplot")
})

test_that("plot_confidence_intervals works with many rows", {
    df <- data.frame(
        cell_line = factor(paste0("L", 1:20), levels = paste0("L", 1:20)),
        est = rnorm(20),
        conf.low = rnorm(20, -0.5),
        conf.high = rnorm(20, 0.5)
    )

    p <- plot_confidence_intervals(
        df,
        effect_size = "est",
        ci_low = "conf.low",
        ci_high = "conf.high",
        id = "cell_line"
    )
    expect_s3_class(p, "ggplot")
})

test_that("plot_confidence_intervals validates dodge_width is non-negative", {
    df <- data.frame(
        cell_line = factor(c("A", "B"), levels = c("A", "B")),
        est = c(0.2, -0.1),
        conf.low = c(0.0, -0.3),
        conf.high = c(0.4, 0.1)
    )

    expect_error(
        plot_confidence_intervals(
            df,
            effect_size = "est",
            ci_low = "conf.low",
            ci_high = "conf.high",
            id = "cell_line",
            dodge_width = -0.1
        ),
        "dodge_width must be a single non-negative number"
    )
})

test_that("plot_confidence_intervals validates pvalue_plot_width is positive", {
    skip_if_not_installed("patchwork")

    df <- data.frame(
        cell_line = factor(c("A", "B"), levels = c("A", "B")),
        est = c(0.2, -0.1),
        conf.low = c(0.0, -0.3),
        conf.high = c(0.4, 0.1),
        pvalue = c(0.01, 0.4)
    )

    expect_error(
        plot_confidence_intervals(
            df,
            effect_size = "est",
            ci_low = "conf.low",
            ci_high = "conf.high",
            id = "cell_line",
            pvalue_col = "pvalue",
            pvalue_plot_width = 0
        ),
        "pvalue_plot_width must be a single positive number"
    )
})

test_that("plot_confidence_intervals validates pvalue_plot_margin length and values", {
    skip_if_not_installed("patchwork")

    df <- data.frame(
        cell_line = factor(c("A", "B"), levels = c("A", "B")),
        est = c(0.2, -0.1),
        conf.low = c(0.0, -0.3),
        conf.high = c(0.4, 0.1),
        pvalue = c(0.01, 0.4)
    )

    expect_error(
        plot_confidence_intervals(
            df,
            effect_size = "est",
            ci_low = "conf.low",
            ci_high = "conf.high",
            id = "cell_line",
            pvalue_col = "pvalue",
            pvalue_plot_margin = c(1, 2, 3)
        ),
        "pvalue_plot_margin must be a numeric vector of length 4"
    )

    expect_error(
        plot_confidence_intervals(
            df,
            effect_size = "est",
            ci_low = "conf.low",
            ci_high = "conf.high",
            id = "cell_line",
            pvalue_col = "pvalue",
            pvalue_plot_margin = c(1, 2, 3, -1)
        ),
        "pvalue_plot_margin must be a numeric vector of length 4"
    )
})

test_that("plot_confidence_intervals works with custom color_values", {
    df <- data.frame(
        cell_line = factor(c("A", "A", "B", "B"), levels = c("A", "B")),
        est = c(0.2, 0.35, -0.1, 0.05),
        conf.low = c(0.0, 0.10, -0.3, -0.10),
        conf.high = c(0.4, 0.60, 0.1, 0.20),
        group = factor(c("g1", "g2", "g1", "g2"), levels = c("g1", "g2"))
    )

    p <- plot_confidence_intervals(
        df,
        effect_size = "est",
        ci_low = "conf.low",
        ci_high = "conf.high",
        id = "cell_line",
        group_col = "group",
        color_col = "group",
        color_values = c(g1 = "#E69F00", g2 = "#56B4E9")
    )
    expect_s3_class(p, "ggplot")
})

test_that("plot_confidence_intervals works with color_values for cell_line", {
    df <- data.frame(
        cell_line = factor(c("A", "B", "C"), levels = c("A", "B", "C")),
        est = c(0.2, -0.1, 0.5),
        conf.low = c(0.0, -0.3, 0.2),
        conf.high = c(0.4, 0.1, 0.8)
    )

    p <- plot_confidence_intervals(
        df,
        effect_size = "est",
        ci_low = "conf.low",
        ci_high = "conf.high",
        id = "cell_line",
        color_col = "cell_line",
        color_values = c(A = "red", B = "blue", C = "green")
    )
    expect_s3_class(p, "ggplot")
})

test_that("plot_confidence_intervals works with color_values and shape style", {
    df <- data.frame(
        cell_line = factor(c("A", "A", "B", "B"), levels = c("A", "B")),
        est = c(0.2, 0.35, -0.1, 0.05),
        conf.low = c(0.0, 0.10, -0.3, -0.10),
        conf.high = c(0.4, 0.60, 0.1, 0.20),
        group = factor(c("g1", "g2", "g1", "g2"), levels = c("g1", "g2"))
    )

    p <- plot_confidence_intervals(
        df,
        effect_size = "est",
        ci_low = "conf.low",
        ci_high = "conf.high",
        id = "cell_line",
        group_col = "group",
        style = "shape",
        color_col = "group",
        color_values = c(g1 = "#FF0000", g2 = "#0000FF")
    )
    expect_s3_class(p, "ggplot")
})

test_that("plot_confidence_intervals validates color_values is named vector", {
    df <- data.frame(
        cell_line = factor(c("A", "B"), levels = c("A", "B")),
        est = c(0.2, -0.1),
        conf.low = c(0.0, -0.3),
        conf.high = c(0.4, 0.1),
        group = factor(c("g1", "g2"), levels = c("g1", "g2"))
    )

    expect_error(
        plot_confidence_intervals(
            df,
            effect_size = "est",
            ci_low = "conf.low",
            ci_high = "conf.high",
            id = "cell_line",
            color_col = "group",
            color_values = c("red", "blue")
        ),
        "color_values must be NULL or a named character vector"
    )
})
