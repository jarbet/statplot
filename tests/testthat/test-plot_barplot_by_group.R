library(testthat)

# ── Shared fixtures ────────────────────────────────────────────────────────────
# 2-row df: one row per condition (single-panel, no faceting)
make_df <- function() {
    data.frame(
        condition = c("Control", "Exercise"),
        mean = c(10.2, 14.8),
        se = c(0.9, 1.0),
        p_value = c(0.004, 0.004)
    )
}

# 4-row df: two groups x two conditions (for faceted tests)
make_df_facet <- function() {
    data.frame(
        group = rep(c("Group A", "Group B"), each = 2),
        condition = rep(c("Control", "Exercise"), 2),
        mean = c(10.2, 14.8, 12.5, 13.1),
        se = c(0.9, 1.0, 1.1, 1.0),
        p_value = c(0.004, 0.004, 0.18, 0.18)
    )
}

# Helper: call with all required args using 2-row df
barplot <- function(df = make_df(), ...) {
    plot_barplot_by_group(
        df,
        condition_col = "condition",
        mean_col = "mean",
        error_col = "se",
        p_col = "p_value",
        ...
    )
}

# ── Returns ggplot ─────────────────────────────────────────────────────────────
test_that("returns a ggplot with default arguments", {
    p <- barplot(y_label = "Score")
    expect_s3_class(p, "ggplot")
})

test_that("returns a ggplot with error_direction = 'up'", {
    p <- barplot(error_direction = "up")
    expect_s3_class(p, "ggplot")
})

test_that("returns a ggplot when p_col = NULL (no brackets)", {
    p <- plot_barplot_by_group(
        make_df(),
        condition_col = "condition",
        mean_col = "mean",
        error_col = "se",
        p_col = NULL
    )
    expect_s3_class(p, "ggplot")
})

# ── Input validation ───────────────────────────────────────────────────────────
test_that("errors when required column is missing", {
    expect_error(
        plot_barplot_by_group(
            make_df(),
            condition_col = "condition",
            mean_col = "nonexistent",
            error_col = "se",
            p_col = "p_value"
        ),
        "Column\\(s\\) not found"
    )
})

test_that("errors when condition_col has more than 2 levels", {
    df <- data.frame(
        condition = c("A", "B", "C"),
        mean = c(1, 2, 3),
        se = c(0.1, 0.1, 0.1),
        p_value = c(0.01, 0.01, 0.01)
    )
    expect_error(
        plot_barplot_by_group(
            df,
            condition_col = "condition",
            mean_col = "mean",
            error_col = "se",
            p_col = "p_value"
        ),
        "exactly 2 levels"
    )
})

test_that("errors when condition_col has fewer than 2 levels", {
    df <- data.frame(
        condition = "Control",
        mean = 10,
        se = 0.9,
        p_value = 0.004
    )
    expect_error(
        plot_barplot_by_group(
            df,
            condition_col = "condition",
            mean_col = "mean",
            error_col = "se",
            p_col = "p_value"
        ),
        "exactly 2 levels"
    )
})

test_that("errors when bar_colors is not length 2", {
    expect_error(
        barplot(bar_colors = c("black", "grey", "red"))
    )
})

test_that("errors when error_direction is invalid", {
    expect_error(
        barplot(error_direction = "down")
    )
})

test_that("errors when label_col is not found in df", {
    expect_error(
        barplot(label_col = "nonexistent"),
        "Column.*nonexistent.*not found"
    )
})

test_that("errors when df is not a data.frame", {
    expect_error(
        plot_barplot_by_group(
            list(a = 1),
            condition_col = "condition",
            mean_col = "mean",
            error_col = "se",
            p_col = "p_value"
        )
    )
})

test_that("errors when facet_cols column not found in df", {
    expect_error(
        barplot(facet_cols = "nonexistent"),
        "facet_cols.*not found"
    )
})

test_that("errors when condition appears more than once without facet_cols", {
    expect_error(
        plot_barplot_by_group(
            make_df_facet(),
            condition_col = "condition",
            mean_col = "mean",
            error_col = "se",
            p_col = "p_value"
        ),
        "exactly once per facet group"
    )
})

# ── Error bar layers ───────────────────────────────────────────────────────────
test_that("error_direction = 'both' uses geom_errorbar", {
    p <- barplot(error_direction = "both")
    layer_classes <- vapply(p$layers, \(l) class(l$geom)[1], character(1))
    expect_true("GeomErrorbar" %in% layer_classes)
})

test_that("error_direction = 'up' uses geom_linerange and geom_errorbar", {
    p <- barplot(error_direction = "up")
    layer_classes <- vapply(p$layers, \(l) class(l$geom)[1], character(1))
    expect_true("GeomLinerange" %in% layer_classes)
    expect_true("GeomErrorbar" %in% layer_classes)
})

test_that("error_direction = 'both' does not use geom_linerange", {
    p <- barplot(error_direction = "both")
    layer_classes <- vapply(p$layers, \(l) class(l$geom)[1], character(1))
    expect_false("GeomLinerange" %in% layer_classes)
})

# ── Significance brackets ──────────────────────────────────────────────────────
test_that("significance brackets appear when p < p_cutoff", {
    p <- barplot(p_cutoff = 0.05)
    layer_classes <- vapply(p$layers, \(l) class(l$geom)[1], character(1))
    expect_true("GeomSegment" %in% layer_classes)
    expect_true("GeomText" %in% layer_classes)
})

test_that("no brackets when all p-values exceed p_cutoff", {
    df <- make_df()
    df$p_value <- 0.9
    p <- barplot(df, p_cutoff = 0.05)
    layer_classes <- vapply(p$layers, \(l) class(l$geom)[1], character(1))
    expect_false("GeomSegment" %in% layer_classes)
    expect_false("GeomText" %in% layer_classes)
})

test_that("p_col = NULL suppresses brackets entirely", {
    p <- plot_barplot_by_group(
        make_df(),
        condition_col = "condition",
        mean_col = "mean",
        error_col = "se",
        p_col = NULL
    )
    layer_classes <- vapply(p$layers, \(l) class(l$geom)[1], character(1))
    expect_false("GeomSegment" %in% layer_classes)
    expect_false("GeomText" %in% layer_classes)
})

test_that("custom label_col is used in bracket text", {
    df <- make_df()
    df$label <- c("OR=1.5, p=0.004", NA)
    p <- barplot(df, label_col = "label", p_cutoff = 0.05)
    layer_classes <- vapply(p$layers, \(l) class(l$geom)[1], character(1))
    text_layer <- p$layers[[which(layer_classes == "GeomText")]]
    expect_true("lbl" %in% names(text_layer$data))
    expect_true(any(grepl("OR=1.5", text_layer$data$lbl, fixed = TRUE)))
})

# ── Condition ordering ─────────────────────────────────────────────────────────
test_that("condition_order sets factor level order", {
    p <- barplot(condition_order = c("Exercise", "Control"))
    lvls <- levels(p$data$condition)
    expect_equal(lvls, c("Exercise", "Control"))
})

# ── Custom column names ────────────────────────────────────────────────────────
test_that("custom condition_col name is respected", {
    df <- make_df()
    names(df)[names(df) == "condition"] <- "cond"
    p <- plot_barplot_by_group(
        df,
        condition_col = "cond",
        mean_col = "mean",
        error_col = "se",
        p_col = "p_value"
    )
    expect_s3_class(p, "ggplot")
})

test_that("custom mean_col and error_col are respected", {
    df <- make_df()
    names(df)[names(df) == "mean"] <- "estimate"
    names(df)[names(df) == "se"] <- "std_err"
    p <- plot_barplot_by_group(
        df,
        condition_col = "condition",
        mean_col = "estimate",
        error_col = "std_err",
        p_col = "p_value"
    )
    expect_s3_class(p, "ggplot")
})

# ── Non-syntactic column names ─────────────────────────────────────────────────
test_that("non-syntactic condition_col names with spaces are handled", {
    df <- make_df()
    names(df)[names(df) == "condition"] <- "study condition"
    p <- plot_barplot_by_group(
        df,
        condition_col = "study condition",
        mean_col = "mean",
        error_col = "se",
        p_col = "p_value"
    )
    expect_s3_class(p, "ggplot")
})

test_that("non-syntactic condition_col names with hyphens are handled", {
    df <- make_df()
    names(df)[names(df) == "condition"] <- "study-condition"
    p <- plot_barplot_by_group(
        df,
        condition_col = "study-condition",
        mean_col = "mean",
        error_col = "se",
        p_col = "p_value"
    )
    expect_s3_class(p, "ggplot")
})

# ── Faceting ───────────────────────────────────────────────────────────────────
test_that("facet_cols retains grouping columns in bracket segment data", {
    p <- plot_barplot_by_group(
        make_df_facet(),
        condition_col = "condition",
        mean_col = "mean",
        error_col = "se",
        p_col = "p_value",
        facet_cols = "group"
    )
    expect_s3_class(p, "ggplot")
    layer_classes <- vapply(p$layers, \(l) class(l$geom)[1], character(1))
    seg_layer <- p$layers[[which(layer_classes == "GeomSegment")]]
    expect_true("group" %in% names(seg_layer$data))
})

test_that("multiple facet_cols are retained in bracket data", {
    df_multi <- data.frame(
        study     = rep(c("Study A", "Study B"), each = 4),
        sex       = rep(c("M", "M", "F", "F"), 2),
        condition = rep(c("Exercise", "Control"), 4),
        mean      = c(10.2, 14.8, 12.5, 13.1, 11.0, 15.2, 13.0, 13.8),
        se        = c(0.9, 1.0, 1.1, 1.0, 0.8, 1.1, 1.0, 0.9),
        p_value   = c(0.004, 0.004, 0.18, 0.18, 0.01, 0.01, 0.25, 0.25)
    )
    p <- plot_barplot_by_group(
        df_multi,
        condition_col = "condition",
        mean_col = "mean",
        error_col = "se",
        p_col = "p_value",
        facet_cols = c("study", "sex")
    )
    expect_s3_class(p, "ggplot")
    layer_classes <- vapply(p$layers, \(l) class(l$geom)[1], character(1))
    seg_layer <- p$layers[[which(layer_classes == "GeomSegment")]]
    expect_true(all(c("study", "sex") %in% names(seg_layer$data)))
})

