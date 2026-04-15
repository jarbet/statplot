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

test_that("inside_bar_stats = 'pct_and_n' runs without error", {
    d <- make_cat_df()
    expect_no_error(
        plot_2_categorical_vars(
            d,
            "cyl",
            "gear",
            inside_bar_stats = "pct_and_n"
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

test_that("inside_bar_stats = 'pct_and_n' bar labels contain both count and '%'", {
    d <- make_cat_df()
    res <- plot_2_categorical_vars(
        d,
        "cyl",
        "gear",
        inside_bar_stats = "pct_and_n"
    )
    labels <- res$ggplot$data$bar_label
    expect_true(all(grepl("%", labels)))
    expect_true(all(grepl("\\(", labels))) # format: "12% (42)"
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

# ---------------------------------------------------------------------------
# include_overall_bar
# ---------------------------------------------------------------------------

test_that("include_overall_bar = TRUE runs without error", {
    d <- make_cat_df()
    expect_no_error(
        plot_2_categorical_vars(d, "cyl", "gear", include_overall_bar = TRUE)
    )
})

test_that("include_overall_bar = TRUE prepends 'Overall' as first x level", {
    d <- make_cat_df()
    res <- plot_2_categorical_vars(d, "cyl", "gear", include_overall_bar = TRUE)
    x_levels <- levels(res$ggplot$data$cyl)
    expect_equal(x_levels[1], "Overall")
})

test_that("include_overall_bar = TRUE adds one extra bar vs FALSE", {
    d <- make_cat_df()
    res_no <- plot_2_categorical_vars(
        d,
        "cyl",
        "gear",
        include_overall_bar = FALSE
    )
    res_yes <- plot_2_categorical_vars(
        d,
        "cyl",
        "gear",
        include_overall_bar = TRUE
    )
    n_levels_no <- nlevels(res_no$ggplot$data$cyl)
    n_levels_yes <- nlevels(res_yes$ggplot$data$cyl)
    expect_equal(n_levels_yes, n_levels_no + 1L)
})

test_that("include_overall_bar = TRUE overall pct sums to 100", {
    d <- make_cat_df()
    res <- plot_2_categorical_vars(d, "cyl", "gear", include_overall_bar = TRUE)
    overall_pct <- res$ggplot$data |>
        dplyr::filter(as.character(cyl) == "Overall") |>
        dplyr::pull(pct) |>
        sum()
    expect_equal(overall_pct, 100, tolerance = 1e-6)
})

test_that("include_overall_bar = TRUE adds a vline layer", {
    d <- make_cat_df()
    res <- plot_2_categorical_vars(d, "cyl", "gear", include_overall_bar = TRUE)
    geom_classes <- sapply(res$ggplot$layers, function(l) class(l$geom)[1])
    expect_true(any(geom_classes == "GeomVline"))
})

test_that("include_overall_bar = FALSE does not add a vline layer", {
    d <- make_cat_df()
    res <- plot_2_categorical_vars(
        d,
        "cyl",
        "gear",
        include_overall_bar = FALSE
    )
    geom_classes <- sapply(res$ggplot$layers, function(l) class(l$geom)[1])
    expect_false(any(geom_classes == "GeomVline"))
})

test_that("include_overall_bar must be logical", {
    d <- make_cat_df()
    expect_error(
        plot_2_categorical_vars(d, "cyl", "gear", include_overall_bar = 1)
    )
})

# ---------------------------------------------------------------------------
# overall_label
# ---------------------------------------------------------------------------

test_that("overall_label changes the x-axis label for the overall bar", {
    d <- make_cat_df()
    res <- plot_2_categorical_vars(
        d,
        "cyl",
        "gear",
        include_overall_bar = TRUE,
        overall_label = "All patients"
    )
    x_levels <- levels(res$ggplot$data$cyl)
    expect_equal(x_levels[1], "All patients")
    expect_false("Overall" %in% x_levels)
})

test_that("overall_label must be a character scalar", {
    d <- make_cat_df()
    expect_error(
        plot_2_categorical_vars(
            d,
            "cyl",
            "gear",
            include_overall_bar = TRUE,
            overall_label = 123
        )
    )
    expect_error(
        plot_2_categorical_vars(
            d,
            "cyl",
            "gear",
            include_overall_bar = TRUE,
            overall_label = c("a", "b")
        )
    )
})

# ---------------------------------------------------------------------------
# yvar_text_colors
# ---------------------------------------------------------------------------

test_that("yvar_text_colors accepts a named character vector", {
    d <- make_cat_df()
    expect_no_error(
        plot_2_categorical_vars(
            d,
            "cyl",
            "gear",
            yvar_text_colors = c("3" = "black", "4" = "white"),
            inside_bar_stats = "pct"
        )
    )
})

test_that("yvar_text_colors must have names", {
    d <- make_cat_df()
    expect_error(
        plot_2_categorical_vars(
            d,
            "cyl",
            "gear",
            yvar_text_colors = c("black", "white"),
            inside_bar_stats = "pct"
        )
    )
})

test_that("yvar_text_colors must be a character vector", {
    d <- make_cat_df()
    expect_error(
        plot_2_categorical_vars(
            d,
            "cyl",
            "gear",
            yvar_text_colors = c("3" = 1, "4" = 2),
            inside_bar_stats = "pct"
        )
    )
})

test_that("yvar_text_colors rejects empty string names", {
    d <- make_cat_df()
    # Create vector with empty string name programmatically
    test_vec <- c("red", "black")
    names(test_vec) <- c("", "3")
    expect_error(
        plot_2_categorical_vars(
            d,
            "cyl",
            "gear",
            yvar_text_colors = test_vec,
            inside_bar_stats = "pct"
        ),
        "non-empty, non-NA names"
    )
})

test_that("yvar_text_colors rejects NA names", {
    d <- make_cat_df()
    # Create vector with NA name
    test_vec <- c("red", "black")
    names(test_vec) <- c(NA_character_, "3")
    expect_error(
        plot_2_categorical_vars(
            d,
            "cyl",
            "gear",
            yvar_text_colors = test_vec,
            inside_bar_stats = "pct"
        ),
        "non-empty, non-NA names"
    )
})

test_that("yvar_text_colors = NULL (default) does not add color scale", {
    d <- make_cat_df()
    res <- plot_2_categorical_vars(
        d,
        "cyl",
        "gear",
        yvar_text_colors = NULL,
        inside_bar_stats = "pct"
    )
    color_scales <- Filter(
        function(s) "colour" %in% s$aesthetics,
        res$ggplot$scales$scales
    )
    expect_length(color_scales, 0L)
})

test_that("yvar_text_colors with values adds color scale", {
    d <- make_cat_df()
    res <- plot_2_categorical_vars(
        d,
        "cyl",
        "gear",
        yvar_text_colors = c("3" = "red"),
        inside_bar_stats = "pct"
    )
    color_scales <- Filter(
        function(s) "colour" %in% s$aesthetics,
        res$ggplot$scales$scales
    )
    expect_length(color_scales, 1L)
    # gear has 3 levels; only "3" is specified as red, others should be black
    palette <- unname(color_scales[[1]]$palette(3))
    expect_equal(palette[1], "red")
    expect_equal(palette[2], "black")
    expect_equal(palette[3], "black")
})

test_that("yvar_text_colors only applies when inside_bar_stats != 'none'", {
    d <- make_cat_df()
    # inside_bar_stats = 'none' should not have color scale for text
    res <- plot_2_categorical_vars(
        d,
        "cyl",
        "gear",
        yvar_text_colors = c("3" = "red", "4" = "blue"),
        inside_bar_stats = "none"
    )
    color_scales <- Filter(
        function(s) "colour" %in% s$aesthetics,
        res$ggplot$scales$scales
    )
    expect_length(color_scales, 0L)
})

# ---------------------------------------------------------------------------
# inside_bar_text_bold
# ---------------------------------------------------------------------------

test_that("inside_bar_text_bold = TRUE runs without error", {
    d <- make_cat_df()
    expect_no_error(
        plot_2_categorical_vars(
            d,
            "cyl",
            "gear",
            inside_bar_stats = "pct",
            inside_bar_text_bold = TRUE
        )
    )
})

test_that("inside_bar_text_bold = FALSE (default) runs without error", {
    d <- make_cat_df()
    expect_no_error(
        plot_2_categorical_vars(
            d,
            "cyl",
            "gear",
            inside_bar_stats = "pct",
            inside_bar_text_bold = FALSE
        )
    )
})

test_that("inside_bar_text_bold must be logical", {
    d <- make_cat_df()
    expect_error(
        plot_2_categorical_vars(
            d,
            "cyl",
            "gear",
            inside_bar_stats = "pct",
            inside_bar_text_bold = "yes"
        )
    )
})

test_that("inside_bar_text_bold = TRUE applies bold fontface", {
    d <- make_cat_df()
    res <- plot_2_categorical_vars(
        d,
        "cyl",
        "gear",
        inside_bar_stats = "pct",
        inside_bar_text_bold = TRUE
    )
    # Find the geom_text layer for inside-bar labels
    text_layers <- Filter(
        function(l) class(l$geom)[1] == "GeomText",
        res$ggplot$layers
    )
    expect_length(text_layers, 2L) # one for inside bars, one for group N labels
    # The second text layer (index 2) should have bold fontface for the bar stats
    expect_equal(text_layers[[2]]$aes_params$fontface, "bold")
})

test_that("inside_bar_text_bold = FALSE applies plain fontface", {
    d <- make_cat_df()
    res <- plot_2_categorical_vars(
        d,
        "cyl",
        "gear",
        inside_bar_stats = "pct",
        inside_bar_text_bold = FALSE
    )
    text_layers <- Filter(
        function(l) class(l$geom)[1] == "GeomText",
        res$ggplot$layers
    )
    expect_length(text_layers, 2L)
    # The second text layer (index 2) should have plain fontface for the bar stats
    expect_equal(text_layers[[2]]$aes_params$fontface, "plain")
})

# ---------------------------------------------------------------------------
# yvar_colors with named vectors
# ---------------------------------------------------------------------------

test_that("yvar_colors accepts named character vector", {
    d <- make_cat_df()
    expect_no_error(
        plot_2_categorical_vars(
            d,
            "cyl",
            "gear",
            yvar_colors = c("3" = "lightgrey", "4" = "grey", "5" = "black")
        )
    )
})

test_that("yvar_colors accepts unnamed vector (positional)", {
    d <- make_cat_df()
    expect_no_error(
        plot_2_categorical_vars(
            d,
            "cyl",
            "gear",
            yvar_colors = c("lightgrey", "grey", "black")
        )
    )
})
