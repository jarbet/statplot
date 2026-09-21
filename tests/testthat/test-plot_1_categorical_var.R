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
        any(
            vapply(
                p$scales$scales,
                inherits,
                logical(1),
                "ScaleDiscrete"
            )
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

test_that('include_cat_labels can be turned off', {
    d <- data.frame(
        smoking = factor(
            c("Never", "Former", "Current")
        )
    )

    p <- plot_1_categorical_var(
        d,
        smoking,
        include_cat_labels = FALSE
    )

    expect_s3_class(
        p,
        "ggplot"
    )
})

test_that('include_cat_labels works with text_inside_bars = none', {
    d <- data.frame(
        smoking = factor(
            c("Never", "Former", "Current")
        )
    )

    p <- plot_1_categorical_var(
        d,
        smoking,
        text_inside_bars = "none",
        include_cat_labels = TRUE
    )

    expect_s3_class(
        p,
        "ggplot"
    )

    expect_equal(
        length(p$layers),
        2
    )
})

test_that('no label layer when labels are completely disabled', {
    d <- data.frame(
        smoking = factor(
            c("Never", "Former", "Current")
        )
    )

    p <- plot_1_categorical_var(
        d,
        smoking,
        text_inside_bars = "none",
        include_cat_labels = FALSE
    )

    expect_equal(
        length(p$layers),
        1
    )
})

# helper: labels shown in the richtext layer
get_labels <- function(p) {
    p$layers[[2]]$data$label
}

test_that('category names are shown in bold inside bar segments', {
    d <- data.frame(
        smoking = factor(
            c("Never", "Never", "Former", "Current"),
            levels = c("Never", "Former", "Current")
        )
    )

    labs <- get_labels(
        plot_1_categorical_var(d, smoking, small_pct_threshold = 0)
    )

    expect_true(all(grepl("^<b>[A-Za-z]+</b><br>", labs)))
    expect_true(any(labs == "<b>Never</b><br>2 (50.0%)"))
})

test_that('legend is hidden only when category labels are included', {
    d <- data.frame(
        smoking = factor(c("Never", "Former", "Current"))
    )

    expect_equal(
        plot_1_categorical_var(d, smoking)$theme$legend.position,
        "none"
    )
    expect_equal(
        plot_1_categorical_var(
            d,
            smoking,
            include_cat_labels = FALSE
        )$theme$legend.position,
        "right"
    )
})

test_that('small_pct_threshold puts rare categories on a single line', {
    d <- data.frame(
        smoking = factor(
            c(rep("Never", 96), rep("Current", 4)),
            levels = c("Never", "Current")
        )
    )

    labs <- get_labels(
        plot_1_categorical_var(d, smoking, small_pct_threshold = 0.05)
    )
    expect_true(any(labs == "<b>Current</b>: 4 (4.0%)"))
    expect_true(any(labs == "<b>Never</b><br>96 (96.0%)"))

    # with a lower threshold the rare category is no longer on one line
    labs0 <- get_labels(
        plot_1_categorical_var(d, smoking, small_pct_threshold = 0.01)
    )
    expect_true(any(labs0 == "<b>Current</b><br>4 (4.0%)"))
})

test_that('labels without category names show only the value', {
    d <- data.frame(smoking = factor(c("Never", "Never", "Former")))

    labs <- get_labels(
        plot_1_categorical_var(
            d,
            smoking,
            text_inside_bars = "count",
            include_cat_labels = FALSE
        )
    )
    expect_setequal(labs, c("2", "1"))
})

test_that('invalid small_pct_threshold errors', {
    d <- data.frame(smoking = factor(c("Never", "Former")))
    expect_error(plot_1_categorical_var(d, smoking, small_pct_threshold = 2))
})
