test_that('plot_1_categorical_var returns a ggplot object', {
    d <- data.frame(
        smoking = factor(
            c("Never", "Former", "Current")
        )
    )

    p <- plot_1_categorical_var(
        data = d,
        var = smoking
    )

    expect_s3_class(p, "ggplot")
})

test_that('all text_inside_bars options work', {
    d <- data.frame(
        smoking = factor(
            c("Never", "Former", "Current")
        )
    )

    expect_s3_class(
        plot_1_categorical_var(
            d,
            smoking,
            text_inside_bars = "none"
        ),
        "ggplot"
    )

    expect_s3_class(
        plot_1_categorical_var(
            d,
            smoking,
            text_inside_bars = "count"
        ),
        "ggplot"
    )

    expect_s3_class(
        plot_1_categorical_var(
            d,
            smoking,
            text_inside_bars = "percent"
        ),
        "ggplot"
    )

    expect_s3_class(
        plot_1_categorical_var(
            d,
            smoking,
            text_inside_bars = "count_and_percent"
        ),
        "ggplot"
    )
})

test_that('invalid text_inside_bars value errors', {
    d <- data.frame(
        smoking = factor(
            c("Never", "Former", "Current")
        )
    )

    expect_error(
        plot_1_categorical_var(
            d,
            smoking,
            text_inside_bars = "bad_value"
        )
    )
})

test_that('custom bar_width and border_color are accepted', {
    d <- data.frame(
        smoking = factor(
            c("Never", "Former", "Current")
        )
    )

    expect_s3_class(
        plot_1_categorical_var(
            d,
            smoking,
            bar_width = 0.5,
            border_color = "black"
        ),
        "ggplot"
    )
})

test_that('text_size is applied', {
    d <- data.frame(
        smoking = factor(
            c("Never", "Former", "Current")
        )
    )

    p <- plot_1_categorical_var(
        d,
        smoking,
        text_size = 6
    )

    expect_equal(
        p$layers[[2]]$aes_params$size,
        6
    )
})

test_that('custom fill palette is applied', {
    d <- data.frame(
        smoking = factor(
            c("Never", "Former", "Current")
        )
    )

    p <- plot_1_categorical_var(
        d,
        smoking,
        fill_palette = c(
            Never = "red",
            Former = "blue",
            Current = "green"
        )
    )

    expect_true(
        inherits(
            p$scales$scales[[2]],
            "ScaleDiscrete"
        )
    )
})

test_that('function handles missing values', {
    d <- data.frame(
        smoking = factor(
            c(
                "Never",
                "Former",
                NA,
                "Current"
            )
        )
    )

    expect_s3_class(
        plot_1_categorical_var(
            d,
            smoking
        ),
        "ggplot"
    )
})


test_that('one segment per factor level is plotted', {
    d <- data.frame(
        smoking = factor(
            c(
                "Never",
                "Never",
                "Former",
                "Current"
            ),
            levels = c(
                "Never",
                "Former",
                "Current"
            )
        )
    )

    p <- plot_1_categorical_var(
        d,
        smoking
    )

    gb <- ggplot2::ggplot_build(p)

    expect_equal(
        nrow(gb$data[[1]]),
        3
    )
})


test_that('text_size is applied', {
    d <- data.frame(
        smoking = factor(
            c("Never", "Former", "Current")
        )
    )

    p <- plot_1_categorical_var(
        d,
        smoking,
        text_size = 6
    )

    expect_equal(
        p$layers[[2]]$aes_params$size,
        6
    )
})
