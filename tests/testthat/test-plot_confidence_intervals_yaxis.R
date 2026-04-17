test_that("plot_confidence_intervals y-axis limits preserve unused factor levels", {
    # Create data with unused factor level
    df <- data.frame(
        cell_line = factor(c("A", "B"), levels = c("A", "B", "C", "D")),
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

    # Extract the y-scale limits from coord_cartesian
    pb <- ggplot2::ggplot_build(p)
    # With 4 factor levels and dodge_width=NULL (no grouping), limits should be c(0.5, 4.5)
    y_limits <- pb$layout$coord$limits$y
    expect_equal(y_limits[1], 0.5, tolerance = 1e-6)
    expect_equal(y_limits[2], 4.5, tolerance = 1e-6)

    # Check that the y scale has 4 breaks for all factor levels
    y_scale <- pb$layout$panel_scales_y[[1]]
    labels <- y_scale$get_labels()
    expect_equal(length(labels), 4)
    expect_equal(labels, c("D", "C", "B", "A"))
})

test_that("plot_confidence_intervals y-axis limits account for dodge_width", {
    df <- data.frame(
        cell_line = factor(c("A", "A", "B", "B"), levels = c("A", "B")),
        est = c(0.2, 0.35, -0.1, 0.05),
        conf.low = c(0.0, 0.10, -0.3, -0.10),
        conf.high = c(0.4, 0.60, 0.1, 0.20),
        group = factor(c("g1", "g2", "g1", "g2"), levels = c("g1", "g2"))
    )

    # With large dodge_width, verify y-limits are expanded to accommodate offsets
    p <- plot_confidence_intervals(
        df,
        effect_size = "est",
        ci_low = "conf.low",
        ci_high = "conf.high",
        id = "cell_line",
        group_col = "group",
        dodge_width = 1.0
    )

    expect_s3_class(p, "ggplot")

    pb <- ggplot2::ggplot_build(p)
    # Extract the y coordinate limits from coord_cartesian
    # With 2 units and dodge_width=1.0, offsets range ±0.5, limits should be c(0.5-0.5, 2+0.5+0.5) = c(0, 3)
    y_limits <- pb$layout$coord$limits$y
    expect_equal(y_limits[1], 0.0, tolerance = 1e-6)
    expect_equal(y_limits[2], 3.0, tolerance = 1e-6)
})

test_that("plot_confidence_intervals y-axis limits without group_col has no dodge padding", {
    df <- data.frame(
        cell_line = factor(c("A", "B"), levels = c("A", "B", "C")),
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

    pb <- ggplot2::ggplot_build(p)
    # Without group_col, y-limits should be c(0.5, n_units+0.5) = c(0.5, 3.5)
    y_limits <- pb$layout$coord$limits$y
    expect_equal(y_limits[1], 0.5, tolerance = 1e-6)
    expect_equal(y_limits[2], 3.5, tolerance = 1e-6)
})

test_that("plot_confidence_intervals y-scale breaks cover all factor levels", {
    df <- data.frame(
        cell_line = factor(
            c("C", "A"),
            levels = c("A", "B", "C", "D", "E")
        ),
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

    # Build the plot and extract y-axis breaks/labels
    pb <- ggplot2::ggplot_build(p)
    y_scale <- pb$layout$panel_scales_y[[1]]

    # Check that the y-scale has labels for all 5 factor levels
    labels <- y_scale$get_labels()
    expect_equal(length(labels), 5)
    # Labels should be in reverse order of levels
    expect_equal(labels, c("E", "D", "C", "B", "A"))

    # Check that y-limits preserve space for all factor levels
    y_limits <- pb$layout$coord$limits$y
    expect_equal(y_limits[1], 0.5, tolerance = 1e-6)
    expect_equal(y_limits[2], 5.5, tolerance = 1e-6)
})
