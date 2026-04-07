library(testthat)

# ── Shared fixture ─────────────────────────────────────────────────────────────
make_df <- function() {
    data.frame(
        group = rep(c("Group A", "Group B"), each = 2),
        condition = rep(c("Control", "Exercise"), 2),
        mean = c(10.2, 14.8, 12.5, 13.1),
        se = c(0.9, 1.0, 1.1, 1.0),
        p_value = c(0.004, 0.004, 0.18, 0.18)
    )
}

# ── Returns ggplot ─────────────────────────────────────────────────────────────
test_that("returns a ggplot with default arguments", {
    p <- plot_barplot_by_group(make_df(), y_label = "Score")
    expect_s3_class(p, "ggplot")
})

test_that("returns a ggplot with error_direction = 'up'", {
    p <- plot_barplot_by_group(make_df(), error_direction = "up")
    expect_s3_class(p, "ggplot")
})

test_that("returns a ggplot with facet = TRUE", {
    p <- plot_barplot_by_group(make_df(), facet = TRUE)
    expect_s3_class(p, "ggplot")
})

test_that("returns a ggplot with facet = TRUE and strip_position = 'bottom'", {
    p <- plot_barplot_by_group(
        make_df(),
        facet = TRUE,
        strip_position = "bottom"
    )
    expect_s3_class(p, "ggplot")
})

test_that("returns a ggplot when p_col = NULL (no brackets)", {
    p <- plot_barplot_by_group(make_df(), p_col = NULL)
    expect_s3_class(p, "ggplot")
})

# ── Input validation ───────────────────────────────────────────────────────────
test_that("errors when required column is missing", {
    df <- make_df()
    expect_error(
        plot_barplot_by_group(df, mean_col = "nonexistent"),
        "Column\\(s\\) not found"
    )
})

test_that("errors when condition_col has more than 2 levels", {
    df <- make_df()
    df$condition <- c("A", "B", "C", "D")
    expect_error(
        plot_barplot_by_group(df),
        "exactly 2 levels"
    )
})

test_that("errors when condition_col has fewer than 2 levels", {
    df <- make_df()
    df$condition <- "Control"
    expect_error(
        plot_barplot_by_group(df),
        "exactly 2 levels"
    )
})

test_that("errors when bar_colors is not length 2", {
    expect_error(
        plot_barplot_by_group(
            make_df(),
            bar_colors = c("black", "grey", "red")
        )
    )
})

test_that("errors when error_direction is invalid", {
    expect_error(
        plot_barplot_by_group(make_df(), error_direction = "down")
    )
})

test_that("errors when strip_position is invalid", {
    expect_error(
        plot_barplot_by_group(
            make_df(),
            facet = TRUE,
            strip_position = "diagonal"
        )
    )
})

test_that("errors when df is not a data.frame", {
    expect_error(plot_barplot_by_group(list(a = 1)))
})

# ── Error bar layers ───────────────────────────────────────────────────────────
test_that("error_direction = 'both' uses geom_errorbar", {
    p <- plot_barplot_by_group(make_df(), error_direction = "both")
    layer_classes <- vapply(p$layers, \(l) class(l$geom)[1], character(1))
    expect_true("GeomErrorbar" %in% layer_classes)
})

test_that("error_direction = 'up' uses geom_linerange and geom_errorbar", {
    p <- plot_barplot_by_group(make_df(), error_direction = "up")
    layer_classes <- vapply(p$layers, \(l) class(l$geom)[1], character(1))
    expect_true("GeomLinerange" %in% layer_classes)
    expect_true("GeomErrorbar" %in% layer_classes)
})

test_that("error_direction = 'both' does not use geom_linerange", {
    p <- plot_barplot_by_group(make_df(), error_direction = "both")
    layer_classes <- vapply(p$layers, \(l) class(l$geom)[1], character(1))
    expect_false("GeomLinerange" %in% layer_classes)
})

# ── Significance brackets ──────────────────────────────────────────────────────
test_that("significance brackets appear when p < p_cutoff", {
    p <- plot_barplot_by_group(make_df(), p_cutoff = 0.05)
    layer_classes <- vapply(p$layers, \(l) class(l$geom)[1], character(1))
    expect_true("GeomSegment" %in% layer_classes)
    expect_true("GeomText" %in% layer_classes)
})

test_that("no brackets when all p-values exceed p_cutoff", {
    df <- make_df()
    df$p_value <- 0.9
    p <- plot_barplot_by_group(df, p_cutoff = 0.05)
    layer_classes <- vapply(p$layers, \(l) class(l$geom)[1], character(1))
    expect_false("GeomSegment" %in% layer_classes)
    expect_false("GeomText" %in% layer_classes)
})

test_that("p_col = NULL suppresses brackets entirely", {
    p <- plot_barplot_by_group(make_df(), p_col = NULL)
    layer_classes <- vapply(p$layers, \(l) class(l$geom)[1], character(1))
    expect_false("GeomSegment" %in% layer_classes)
    expect_false("GeomText" %in% layer_classes)
})

test_that("custom label_col is used in bracket text", {
    df <- make_df()
    df$label <- c("OR=1.5, p=0.004", "OR=1.5, p=0.004", NA, NA)
    p <- plot_barplot_by_group(df, label_col = "label", p_cutoff = 0.05)
    layer_classes <- vapply(p$layers, \(l) class(l$geom)[1], character(1))
    text_layer <- p$layers[[which(layer_classes == "GeomText")]]
    expect_true("lbl" %in% names(text_layer$data))
    expect_true(any(grepl("OR=1.5", text_layer$data$lbl, fixed = TRUE)))
})

# ── Faceting ───────────────────────────────────────────────────────────────────
test_that("facet = FALSE uses FacetWrap", {
    p <- plot_barplot_by_group(make_df(), facet = FALSE)
    expect_s3_class(p$facet, "FacetWrap")
})

test_that("facet = TRUE uses FacetWrap", {
    p <- plot_barplot_by_group(make_df(), facet = TRUE)
    expect_s3_class(p$facet, "FacetWrap")
})

test_that("facet = FALSE places strip at bottom", {
    p <- plot_barplot_by_group(make_df(), facet = FALSE)
    params <- p$facet$params
    expect_equal(params$strip.position, "bottom")
})

test_that("facet = TRUE respects strip_position argument", {
    for (pos in c("top", "bottom", "left", "right")) {
        p <- plot_barplot_by_group(
            make_df(),
            facet = TRUE,
            strip_position = pos
        )
        expect_equal(p$facet$params$strip.position, pos)
    }
})

# ── Condition ordering ─────────────────────────────────────────────────────────
test_that("condition_order sets factor level order", {
    p <- plot_barplot_by_group(
        make_df(),
        condition_order = c("Exercise", "Control")
    )
    lvls <- levels(p$data$condition)
    expect_equal(lvls, c("Exercise", "Control"))
})

test_that("custom group_col and condition_col names are respected", {
    df <- make_df()
    names(df)[names(df) == "group"] <- "grp"
    names(df)[names(df) == "condition"] <- "cond"
    p <- plot_barplot_by_group(df, group_col = "grp", condition_col = "cond")
    expect_s3_class(p, "ggplot")
})

# ── Custom column names ────────────────────────────────────────────────────────
test_that("custom mean_col and error_col are respected", {
    df <- make_df()
    names(df)[names(df) == "mean"] <- "estimate"
    names(df)[names(df) == "se"] <- "std_err"
    p <- plot_barplot_by_group(
        df,
        mean_col = "estimate",
        error_col = "std_err"
    )
    expect_s3_class(p, "ggplot")
})
