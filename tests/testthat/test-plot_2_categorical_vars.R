# ---------------------------------------------------------------------------
# Helpers
# ---------------------------------------------------------------------------

make_cat_df <- function() {
    d <- mtcars
    d$cyl <- factor(d$cyl)
    d$gear <- factor(d$gear)
    d
}

# ---------------------------------------------------------------------------
# Return structure
# ---------------------------------------------------------------------------

test_that("returns a list with ggplot and stats elements", {
    d <- make_cat_df()
    res <- plot_2_categorical_vars(d, "cyl", "gear")
    expect_type(res, "list")
    expect_named(res, c("ggplot", "stats"))
    expect_s3_class(res$ggplot, "ggplot")
    expect_s3_class(res$stats, "data.frame")
})

test_that("stats data frame has expected columns", {
    d <- make_cat_df()
    res <- plot_2_categorical_vars(d, "cyl", "gear")
    expected_cols <- c(
        "outcome",
        "predictor",
        "value",
        "CI_low",
        "CI_high",
        "effect_type",
        "pvalue",
        "n"
    )
    expect_true(all(expected_cols %in% names(res$stats)))
})

# ---------------------------------------------------------------------------
# inside_bar_stats argument
# ---------------------------------------------------------------------------

test_that("inside_bar_stats = 'pct' (default) runs without error", {
    d <- make_cat_df()
    expect_no_error(
        plot_2_categorical_vars(d, "cyl", "gear", inside_bar_stats = "pct")
    )
})

test_that("inside_bar_stats = 'n' runs without error", {
    d <- make_cat_df()
    expect_no_error(
        plot_2_categorical_vars(d, "cyl", "gear", inside_bar_stats = "n")
    )
})

test_that("inside_bar_stats = 'n_and_pct' runs without error", {
    d <- make_cat_df()
    expect_no_error(
        plot_2_categorical_vars(
            d,
            "cyl",
            "gear",
            inside_bar_stats = "n_and_pct"
        )
    )
})

test_that("inside_bar_stats = 'none' runs without error", {
    d <- make_cat_df()
    expect_no_error(
        plot_2_categorical_vars(d, "cyl", "gear", inside_bar_stats = "none")
    )
})

test_that("invalid inside_bar_stats value is rejected", {
    d <- make_cat_df()
    expect_error(
        plot_2_categorical_vars(
            d,
            "cyl",
            "gear",
            inside_bar_stats = "bad_value"
        )
    )
})

test_that("inside_bar_stats = 'pct' bar labels contain '%'", {
    d <- make_cat_df()
    res <- plot_2_categorical_vars(d, "cyl", "gear", inside_bar_stats = "pct")
    labels <- res$ggplot$data$bar_label
    expect_true(all(grepl("%", labels)))
})

test_that("inside_bar_stats = 'n' bar labels are numeric strings (no '%')", {
    d <- make_cat_df()
    res <- plot_2_categorical_vars(d, "cyl", "gear", inside_bar_stats = "n")
    labels <- res$ggplot$data$bar_label
    expect_false(any(grepl("%", labels)))
    expect_true(all(!is.na(as.numeric(gsub(",", "", labels)))))
})

test_that("inside_bar_stats = 'n_and_pct' bar labels contain both count and '%'", {
    d <- make_cat_df()
    res <- plot_2_categorical_vars(
        d,
        "cyl",
        "gear",
        inside_bar_stats = "n_and_pct"
    )
    labels <- res$ggplot$data$bar_label
    expect_true(all(grepl("%", labels)))
    expect_true(all(grepl("\\(", labels))) # format: "42 (12%)"
})

test_that("inside_bar_stats = 'none' bar labels are all NA", {
    d <- make_cat_df()
    res <- plot_2_categorical_vars(d, "cyl", "gear", inside_bar_stats = "none")
    expect_true(all(is.na(res$ggplot$data$bar_label)))
})

# ---------------------------------------------------------------------------
# Input validation
# ---------------------------------------------------------------------------

test_that("xvar must be a factor", {
    d <- make_cat_df()
    d$cyl <- as.character(d$cyl)
    expect_error(plot_2_categorical_vars(d, "cyl", "gear"))
})

test_that("yvar must be a factor", {
    d <- make_cat_df()
    d$gear <- as.character(d$gear)
    expect_error(plot_2_categorical_vars(d, "cyl", "gear"))
})

test_that("d must be a data.frame", {
    expect_error(plot_2_categorical_vars(list(a = 1), "a", "b"))
})

test_that("xvar must be a column in d", {
    d <- make_cat_df()
    expect_error(plot_2_categorical_vars(d, "nonexistent", "gear"))
})

test_that("yvar must be a column in d", {
    d <- make_cat_df()
    expect_error(plot_2_categorical_vars(d, "cyl", "nonexistent"))
})

test_that("yvar_colors length must equal number of yvar levels", {
    d <- make_cat_df()
    # gear has 3 levels; supplying 2 colors should fail
    expect_error(
        plot_2_categorical_vars(
            d,
            "cyl",
            "gear",
            yvar_colors = c("red", "blue")
        )
    )
})

test_that("pct_digits must be non-negative", {
    d <- make_cat_df()
    expect_error(
        plot_2_categorical_vars(d, "cyl", "gear", pct_digits = -1)
    )
})

# ---------------------------------------------------------------------------
# show_effect_size
# ---------------------------------------------------------------------------

test_that("show_effect_size = FALSE removes subtitle", {
    d <- make_cat_df()
    res <- plot_2_categorical_vars(d, "cyl", "gear", show_effect_size = FALSE)
    subtitle <- res$ggplot$labels$subtitle
    expect_true(is.null(subtitle))
})

test_that("show_effect_size = TRUE adds a subtitle", {
    d <- make_cat_df()
    res <- plot_2_categorical_vars(d, "cyl", "gear", show_effect_size = TRUE)
    subtitle <- res$ggplot$labels$subtitle
    expect_false(is.null(subtitle))
})

# ---------------------------------------------------------------------------
# flip argument
# ---------------------------------------------------------------------------

test_that("flip = TRUE adds coord_flip to the plot", {
    d <- make_cat_df()
    res_flip <- plot_2_categorical_vars(d, "cyl", "gear", flip = TRUE)
    coord_class <- class(res_flip$ggplot$coordinates)
    expect_true(any(grepl("Flip", coord_class)))
})

test_that("flip = FALSE (default) does not flip coordinates", {
    d <- make_cat_df()
    res <- plot_2_categorical_vars(d, "cyl", "gear", flip = FALSE)
    coord_class <- class(res$ggplot$coordinates)
    expect_false(any(grepl("Flip", coord_class)))
})

# ---------------------------------------------------------------------------
# Custom labels
# ---------------------------------------------------------------------------

test_that("xvar_label and yvar_label are used in plot labels", {
    d <- make_cat_df()
    res <- plot_2_categorical_vars(
        d,
        "cyl",
        "gear",
        xvar_label = "Cylinders",
        yvar_label = "Gears"
    )
    expect_equal(res$ggplot$labels$x, "Cylinders")
    expect_equal(res$ggplot$labels$fill, "Gears")
})

test_that("yvar_colors are applied when supplied", {
    d <- make_cat_df()
    colors <- c("red", "blue", "green") # gear has 3 levels
    res <- plot_2_categorical_vars(d, "cyl", "gear", yvar_colors = colors)
    fill_scales <- Filter(
        function(s) "fill" %in% s$aesthetics,
        res$ggplot$scales$scales
    )
    expect_length(fill_scales, 1L)
    expect_equal(fill_scales[[1]]$palette(3), colors)
})
