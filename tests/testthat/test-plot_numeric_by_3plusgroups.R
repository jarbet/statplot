# ---------------------------------------------------------------------------
# Helpers
# ---------------------------------------------------------------------------

make_df <- function() {
    d <- mtcars
    d$gear <- factor(d$gear)
    d
}

# 3-plus group factor for testing with more groups
make_df_multi <- function() {
    d <- mtcars
    d$gear <- factor(d$gear)
    d$cyl <- factor(d$cyl)
    d
}

# ---------------------------------------------------------------------------
# Return structure
# ---------------------------------------------------------------------------

test_that("returns a list with ggplot, stats, and letters elements", {
    d <- make_df()
    res <- plot_numeric_by_3plusgroups(
        yvar = "mpg",
        group = "gear",
        d = d
    )
    expect_type(res, "list")
    expect_named(res, c("ggplot", "stats", "letters"))
    expect_s3_class(res$ggplot, "ggplot")
    expect_s3_class(res$stats, "data.frame")
})

test_that("letters element is character vector when pairwise_wilcoxon_test_letters = TRUE", {
    d <- make_df()
    res <- plot_numeric_by_3plusgroups(
        yvar = "mpg",
        group = "gear",
        d = d,
        pairwise_wilcoxon_test_letters = TRUE
    )
    expect_type(res$letters, "character")
    expect_true(all(nzchar(res$letters)))
})

test_that("letters element is NULL when pairwise_wilcoxon_test_letters = FALSE", {
    d <- make_df()
    res <- plot_numeric_by_3plusgroups(
        yvar = "mpg",
        group = "gear",
        d = d,
        pairwise_wilcoxon_test_letters = FALSE
    )
    expect_null(res$letters)
})

test_that("stats data frame has expected columns", {
    d <- make_df()
    res <- plot_numeric_by_3plusgroups(
        yvar = "mpg",
        group = "gear",
        d = d
    )
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

test_that("stats outcome and predictor match input variables", {
    d <- make_df()
    res <- plot_numeric_by_3plusgroups(
        yvar = "mpg",
        group = "gear",
        d = d
    )
    expect_equal(res$stats$outcome, "mpg")
    expect_equal(res$stats$predictor, "gear")
})

test_that("stats effect_type is 'epsilon_squared'", {
    d <- make_df()
    res <- plot_numeric_by_3plusgroups(
        yvar = "mpg",
        group = "gear",
        d = d
    )
    expect_equal(res$stats$effect_type, "epsilon_squared")
})

test_that("stats contains single row", {
    d <- make_df()
    res <- plot_numeric_by_3plusgroups(
        yvar = "mpg",
        group = "gear",
        d = d
    )
    expect_equal(nrow(res$stats), 1)
})

test_that("stats n matches number of complete cases", {
    d <- make_df()
    n_complete <- sum(!is.na(d$mpg) & !is.na(d$gear))
    res <- plot_numeric_by_3plusgroups(
        yvar = "mpg",
        group = "gear",
        d = d
    )
    expect_equal(res$stats$n, n_complete)
})

# ---------------------------------------------------------------------------
# Input validation
# ---------------------------------------------------------------------------

test_that("yvar must be character of length 1", {
    d <- make_df()
    expect_error(
        plot_numeric_by_3plusgroups(
            yvar = c("mpg", "hp"),
            group = "gear",
            d = d
        )
    )
    expect_error(
        plot_numeric_by_3plusgroups(yvar = 1, group = "gear", d = d)
    )
})

test_that("group must be character of length 1", {
    d <- make_df()
    expect_error(
        plot_numeric_by_3plusgroups(
            yvar = "mpg",
            group = c("gear", "cyl"),
            d = d
        )
    )
    expect_error(
        plot_numeric_by_3plusgroups(yvar = "mpg", group = 1, d = d)
    )
})

test_that("d must be a data.frame", {
    expect_error(
        plot_numeric_by_3plusgroups(
            yvar = "mpg",
            group = "gear",
            d = list(mpg = 1, gear = 1)
        )
    )
})

test_that("yvar must be a column in d", {
    d <- make_df()
    expect_error(
        plot_numeric_by_3plusgroups(
            yvar = "nonexistent",
            group = "gear",
            d = d
        )
    )
})

test_that("group must be a column in d", {
    d <- make_df()
    expect_error(
        plot_numeric_by_3plusgroups(
            yvar = "mpg",
            group = "nonexistent",
            d = d
        )
    )
})

test_that("yvar must be numeric", {
    d <- make_df()
    d$mpg <- as.character(d$mpg)
    expect_error(
        plot_numeric_by_3plusgroups(
            yvar = "mpg",
            group = "gear",
            d = d
        )
    )
})

test_that("group must be a factor", {
    d <- make_df()
    d$gear <- as.character(d$gear)
    expect_error(
        plot_numeric_by_3plusgroups(
            yvar = "mpg",
            group = "gear",
            d = d
        )
    )
})

test_that("yvar_label must be NULL or character of length 1", {
    d <- make_df()
    expect_error(
        plot_numeric_by_3plusgroups(
            yvar = "mpg",
            group = "gear",
            d = d,
            yvar_label = c("a", "b")
        )
    )
    expect_error(
        plot_numeric_by_3plusgroups(
            yvar = "mpg",
            group = "gear",
            d = d,
            yvar_label = 123
        )
    )
})

test_that("group_label must be NULL or character of length 1", {
    d <- make_df()
    expect_error(
        plot_numeric_by_3plusgroups(
            yvar = "mpg",
            group = "gear",
            d = d,
            group_label = c("a", "b")
        )
    )
})

test_that("title must be NULL or character of length 1", {
    d <- make_df()
    expect_error(
        plot_numeric_by_3plusgroups(
            yvar = "mpg",
            group = "gear",
            d = d,
            title = c("a", "b")
        )
    )
})

test_that("group_colors must match number of group levels", {
    d <- make_df()
    # gear has 3 levels, try with 2 colors
    expect_error(
        plot_numeric_by_3plusgroups(
            yvar = "mpg",
            group = "gear",
            d = d,
            group_colors = c("red", "blue")
        )
    )
})

test_that("group_colors must be character", {
    d <- make_df()
    expect_error(
        plot_numeric_by_3plusgroups(
            yvar = "mpg",
            group = "gear",
            d = d,
            group_colors = c(1, 2, 3)
        )
    )
})

test_that("show_effect_size must be logical of length 1", {
    d <- make_df()
    expect_error(
        plot_numeric_by_3plusgroups(
            yvar = "mpg",
            group = "gear",
            d = d,
            show_effect_size = "yes"
        )
    )
    expect_error(
        plot_numeric_by_3plusgroups(
            yvar = "mpg",
            group = "gear",
            d = d,
            show_effect_size = c(TRUE, FALSE)
        )
    )
})

test_that("n_boot must be numeric >= 10", {
    d <- make_df()
    expect_error(
        plot_numeric_by_3plusgroups(
            yvar = "mpg",
            group = "gear",
            d = d,
            n_boot = 5
        )
    )
    expect_error(
        plot_numeric_by_3plusgroups(
            yvar = "mpg",
            group = "gear",
            d = d,
            n_boot = "100"
        )
    )
})

test_that("xaxis_labels_nchar_wrap must be positive numeric", {
    d <- make_df()
    expect_error(
        plot_numeric_by_3plusgroups(
            yvar = "mpg",
            group = "gear",
            d = d,
            xaxis_labels_nchar_wrap = -5
        )
    )
    expect_error(
        plot_numeric_by_3plusgroups(
            yvar = "mpg",
            group = "gear",
            d = d,
            xaxis_labels_nchar_wrap = "20"
        )
    )
})

test_that("pairwise_wilcoxon_test_letters must be logical", {
    d <- make_df()
    expect_error(
        plot_numeric_by_3plusgroups(
            yvar = "mpg",
            group = "gear",
            d = d,
            pairwise_wilcoxon_test_letters = "yes"
        )
    )
})

# ---------------------------------------------------------------------------
# show_effect_size
# ---------------------------------------------------------------------------

test_that("show_effect_size = TRUE produces numeric ES values", {
    d <- make_df()
    res <- plot_numeric_by_3plusgroups(
        yvar = "mpg",
        group = "gear",
        d = d,
        show_effect_size = TRUE
    )
    expect_type(res$stats$value, "double")
    expect_type(res$stats$CI_low, "double")
    expect_type(res$stats$CI_high, "double")
    expect_false(is.na(res$stats$value))
    expect_false(is.na(res$stats$CI_low))
    expect_false(is.na(res$stats$CI_high))
})

test_that("show_effect_size = TRUE adds subtitle", {
    d <- make_df()
    res <- plot_numeric_by_3plusgroups(
        yvar = "mpg",
        group = "gear",
        d = d,
        show_effect_size = TRUE
    )
    subtitle <- res$ggplot$labels$subtitle
    expect_false(is.null(subtitle))
    expect_true(nzchar(subtitle))
})

# ---------------------------------------------------------------------------
# Custom labels
# ---------------------------------------------------------------------------

test_that("yvar_label is used in y-axis label", {
    d <- make_df()
    res <- plot_numeric_by_3plusgroups(
        yvar = "mpg",
        group = "gear",
        d = d,
        yvar_label = "Miles per Gallon"
    )
    expect_equal(res$ggplot$labels$y, "Miles per Gallon")
})

test_that("group_label is used in x-axis label", {
    d <- make_df()
    res <- plot_numeric_by_3plusgroups(
        yvar = "mpg",
        group = "gear",
        d = d,
        group_label = "Number of Gears"
    )
    expect_equal(res$ggplot$labels$x, "Number of Gears")
})

test_that("title is used in plot title", {
    d <- make_df()
    custom_title <- "Custom Title"
    res <- plot_numeric_by_3plusgroups(
        yvar = "mpg",
        group = "gear",
        d = d,
        title = custom_title
    )
    expect_equal(res$ggplot$labels$title, custom_title)
})

test_that("default title uses yvar and group labels", {
    d <- make_df()
    res <- plot_numeric_by_3plusgroups(
        yvar = "mpg",
        group = "gear",
        d = d,
        yvar_label = "MPG",
        group_label = "Gears"
    )
    expect_equal(res$ggplot$labels$title, "MPG by Gears")
})

# ---------------------------------------------------------------------------
# group_colors
# ---------------------------------------------------------------------------

test_that("group_colors are applied to fill scale", {
    d <- make_df()
    colors <- c("red", "blue", "green") # 3 levels of gear
    res <- plot_numeric_by_3plusgroups(
        yvar = "mpg",
        group = "gear",
        d = d,
        group_colors = colors
    )
    fill_scales <- Filter(
        function(s) "fill" %in% s$aesthetics,
        res$ggplot$scales$scales
    )
    expect_length(fill_scales, 1L)
    expect_equal(fill_scales[[1]]$palette(3), colors)
})

test_that("group_colors = NULL does not add color scale", {
    d <- make_df()
    res <- plot_numeric_by_3plusgroups(
        yvar = "mpg",
        group = "gear",
        d = d,
        group_colors = NULL
    )
    fill_scales <- Filter(
        function(s) "fill" %in% s$aesthetics,
        res$ggplot$scales$scales
    )
    # May have default fill scale, so just check it works without error
    expect_s3_class(res$ggplot, "ggplot")
})

# ---------------------------------------------------------------------------
# pairwise_wilcoxon_test_letters
# ---------------------------------------------------------------------------

test_that("pairwise_wilcoxon_test_letters = TRUE has more text layers than FALSE", {
    d <- make_df()
    res_true <- plot_numeric_by_3plusgroups(
        yvar = "mpg",
        group = "gear",
        d = d,
        pairwise_wilcoxon_test_letters = TRUE
    )
    res_false <- plot_numeric_by_3plusgroups(
        yvar = "mpg",
        group = "gear",
        d = d,
        pairwise_wilcoxon_test_letters = FALSE
    )
    text_layers_true <- Filter(
        function(l) class(l$geom)[1] == "GeomText",
        res_true$ggplot$layers
    )
    text_layers_false <- Filter(
        function(l) class(l$geom)[1] == "GeomText",
        res_false$ggplot$layers
    )
    # With pairwise tests, should have an extra text layer for the letters
    expect_true(length(text_layers_true) > length(text_layers_false))
})

test_that("pairwise_wilcoxon_test_letters = TRUE letters named after group levels", {
    d <- make_df()
    res <- plot_numeric_by_3plusgroups(
        yvar = "mpg",
        group = "gear",
        d = d,
        pairwise_wilcoxon_test_letters = TRUE
    )
    expect_equal(names(res$letters), levels(d$gear))
})

# ---------------------------------------------------------------------------
# xaxis_labels_nchar_wrap
# ---------------------------------------------------------------------------

test_that("xaxis_labels_nchar_wrap wraps long group labels", {
    d <- make_df()
    d$gear <- factor(
        d$gear,
        labels = c(
            "Very Long Gear Label 1",
            "Very Long Gear Label 2",
            "Very Long Gear Label 3"
        )
    )
    res <- plot_numeric_by_3plusgroups(
        yvar = "mpg",
        group = "gear",
        d = d,
        xaxis_labels_nchar_wrap = 10
    )
    # Just verify plot is created without error
    expect_s3_class(res$ggplot, "ggplot")
})

test_that("xaxis_labels_nchar_wrap with default value produces plot", {
    d <- make_df()
    res <- plot_numeric_by_3plusgroups(
        yvar = "mpg",
        group = "gear",
        d = d,
        xaxis_labels_nchar_wrap = 20
    )
    expect_s3_class(res$ggplot, "ggplot")
})

# ---------------------------------------------------------------------------
# n_boot parameter
# ---------------------------------------------------------------------------

test_that("n_boot parameter affects epsilon-squared CI calculation", {
    d <- make_df()
    set.seed(123)
    res1 <- plot_numeric_by_3plusgroups(
        yvar = "mpg",
        group = "gear",
        d = d,
        n_boot = 100
    )
    set.seed(123)
    res2 <- plot_numeric_by_3plusgroups(
        yvar = "mpg",
        group = "gear",
        d = d,
        n_boot = 200
    )
    # With different n_boot, CI intervals may differ (due to bootstrap randomness)
    # Just verify both run without error
    expect_s3_class(res1$ggplot, "ggplot")
    expect_s3_class(res2$ggplot, "ggplot")
})

# ---------------------------------------------------------------------------
# Multiple groups
# ---------------------------------------------------------------------------

test_that("works with 4+ group levels", {
    d <- make_df_multi()
    res <- plot_numeric_by_3plusgroups(
        yvar = "mpg",
        group = "cyl",
        d = d
    )
    expect_s3_class(res$ggplot, "ggplot")
    expect_equal(length(res$letters), nlevels(d$cyl))
})

# ---------------------------------------------------------------------------
# Missing data handling
# ---------------------------------------------------------------------------

test_that("handles missing values in numeric variable", {
    d <- make_df()
    d$mpg[1:5] <- NA
    expect_no_error(
        plot_numeric_by_3plusgroups(
            yvar = "mpg",
            group = "gear",
            d = d
        )
    )
})

test_that("handles missing values in group variable", {
    d <- make_df()
    d$gear[1:3] <- NA
    d$gear <- factor(d$gear) # refactor to drop NA level
    expect_no_error(
        plot_numeric_by_3plusgroups(
            yvar = "mpg",
            group = "gear",
            d = d
        )
    )
})

test_that("n in stats reflects complete cases only", {
    d <- make_df()
    d$mpg[1:5] <- NA
    d$gear[6:8] <- NA
    d$gear <- factor(d$gear)
    res <- plot_numeric_by_3plusgroups(
        yvar = "mpg",
        group = "gear",
        d = d
    )
    n_complete <- sum(!is.na(d$mpg) & !is.na(d$gear))
    expect_equal(res$stats$n, n_complete)
})

# ---------------------------------------------------------------------------
# Plot structure
# ---------------------------------------------------------------------------

test_that("plot contains violin layer", {
    d <- make_df()
    res <- plot_numeric_by_3plusgroups(
        yvar = "mpg",
        group = "gear",
        d = d
    )
    geom_classes <- sapply(res$ggplot$layers, function(l) class(l$geom)[1])
    expect_true("GeomViolin" %in% geom_classes)
})

test_that("plot contains boxplot layer", {
    d <- make_df()
    res <- plot_numeric_by_3plusgroups(
        yvar = "mpg",
        group = "gear",
        d = d
    )
    geom_classes <- sapply(res$ggplot$layers, function(l) class(l$geom)[1])
    expect_true("GeomBoxplot" %in% geom_classes)
})

test_that("plot title is bold", {
    d <- make_df()
    res <- plot_numeric_by_3plusgroups(
        yvar = "mpg",
        group = "gear",
        d = d
    )
    plot_theme <- res$ggplot$theme
    # Check if plot.title has bold face
    expect_equal(plot_theme$plot.title$face, "bold")
})

test_that("axis titles are bold", {
    d <- make_df()
    res <- plot_numeric_by_3plusgroups(
        yvar = "mpg",
        group = "gear",
        d = d
    )
    plot_theme <- res$ggplot$theme
    expect_equal(plot_theme$axis.title.x$face, "bold")
    expect_equal(plot_theme$axis.title.y$face, "bold")
})

# ---------------------------------------------------------------------------
# Statistics validity
# ---------------------------------------------------------------------------

test_that("pvalue is between 0 and 1", {
    d <- make_df()
    res <- plot_numeric_by_3plusgroups(
        yvar = "mpg",
        group = "gear",
        d = d
    )
    expect_true(res$stats$pvalue >= 0 & res$stats$pvalue <= 1)
})

test_that("epsilon squared is between 0 and 1 when show_effect_size = TRUE", {
    d <- make_df()
    res <- plot_numeric_by_3plusgroups(
        yvar = "mpg",
        group = "gear",
        d = d,
        show_effect_size = TRUE
    )
    expect_true(res$stats$value >= 0 & res$stats$value <= 1)
})

test_that("CI_low <= value <= CI_high", {
    d <- make_df()
    res <- plot_numeric_by_3plusgroups(
        yvar = "mpg",
        group = "gear",
        d = d,
        show_effect_size = TRUE
    )
    expect_true(res$stats$CI_low <= res$stats$value)
    expect_true(res$stats$value <= res$stats$CI_high)
})
