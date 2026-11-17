make_df <- function(n = 120) {
    set.seed(42)
    data.frame(
        grp = sample(c("A", "B", "C"), n, TRUE),
        age = c(rnorm(n - 2, 50, 10), 150, NA),
        sex = sample(c("F", "M"), n, TRUE),
        diabetes = rbinom(n, 1, 0.3)
    )
}

test_that("ungrouped table returns gt with svg graphics", {
    tab <- table_visual(make_df()[, -1])
    expect_s3_class(tab, "gt_tbl")
    html <- as.character(gt::as_raw_html(tab, inline_css = FALSE))
    expect_match(html, "<svg")
    expect_match(html, "<circle") # outlier at 150
    expect_false(grepl("&#9632;", html)) # no swatch when ungrouped
})

test_that("grouped table has overall + group columns and colored swatches", {
    tab <- table_visual(make_df(), name_groupvar = "grp")
    expect_true(all(c("stat_0", "stat_1", "stat_2", "stat_3") %in% names(tab[["_data"]])))
    html <- as.character(gt::as_raw_html(tab, inline_css = FALSE))
    expect_match(html, "#0072B2")
    expect_match(html, "Group")
})

test_that("overall = FALSE drops overall column", {
    tab <- table_visual(make_df(), name_groupvar = "grp", overall = FALSE)
    html <- as.character(gt::as_raw_html(tab, inline_css = FALSE))
    expect_false(grepl("Overall", html))
})

test_that("continuous_plot options all render", {
    for (type in c("violin_box", "boxplot", "density")) {
        tab <- table_visual(make_df()[, -1], continuous_plot = type)
        html <- as.character(gt::as_raw_html(tab, inline_css = FALSE))
        expect_match(html, "<svg")
    }
    expect_error(table_visual(make_df(), continuous_plot = "nope"))
})

test_that("custom colors are validated and applied", {
    tab <- table_visual(
        make_df(), "grp",
        colors = c(A = "#111111", B = "#222222", C = "#333333")
    )
    html <- as.character(gt::as_raw_html(tab, inline_css = FALSE))
    expect_match(html, "#222222")
    expect_error(table_visual(make_df(), "grp", colors = c(A = "red")), "missing group")
    expect_error(table_visual(make_df(), "grp", colors = "red"), "at least one color")
})

test_that("bar widths reflect column percentages", {
    d <- data.frame(x = c(rep("a", 3), "b"))
    html <- as.character(gt::as_raw_html(table_visual(d, bar_width = 100), inline_css = FALSE))
    expect_match(html, 'width="75.0"')
    expect_match(html, 'width="25.0"')
})

test_that("binary bars use the level gtsummary displays", {
    bar_html <- function(d) {
        as.character(gt::as_raw_html(table_visual(d, bar_width = 100), inline_css = FALSE))
    }
    # 0/1 numeric: shown level is 1
    html <- bar_html(data.frame(y = c(1, 1, 1, 0)))
    expect_match(html, 'width="75.0"')
    expect_false(grepl('width="25.0"', html))
    # No/Yes factor: shown level is "Yes"
    html <- bar_html(data.frame(y = factor(c("No", "Yes", "Yes", "Yes"))))
    expect_match(html, 'width="75.0"')
    expect_false(grepl('width="25.0"', html))
    # logical: shown level is TRUE
    html <- bar_html(data.frame(y = c(TRUE, FALSE, FALSE, FALSE)))
    expect_match(html, 'width="25.0"')
    expect_false(grepl('width="75.0"', html))
})

test_that("gtsummary p25/p75 use type-2 quantiles, as the box plot assumes", {
    x <- c(1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 100)
    q_type2 <- stats::quantile(x, c(0.25, 0.75), names = FALSE, type = 2)
    q_type7 <- stats::quantile(x, c(0.25, 0.75), names = FALSE, type = 7)
    skip_if(isTRUE(all.equal(q_type2, q_type7)))
    tbl <- gtsummary::tbl_summary(
        data.frame(x = x),
        statistic = list(x ~ "{p25}, {p75}")
    )
    printed <- tbl$cards$tbl_summary
    p25 <- unlist(printed$stat[printed$stat_name == "p25"])
    p75 <- unlist(printed$stat[printed$stat_name == "p75"])
    expect_equal(c(p25, p75), q_type2)
})

test_that("constant and all-NA numeric variables do not error", {
    d <- data.frame(k = rep(5, 10), z = rep(NA_real_, 10), s = letters[1:10])
    expect_s3_class(table_visual(d), "gt_tbl")
})

test_that("invalid inputs error", {
    expect_error(table_visual("x"))
    expect_error(table_visual(make_df(), name_groupvar = "nope"))
})

test_that("combined layout draws one distribution column with a lane per group", {
    tab <- table_visual(make_df(), name_groupvar = "grp", plot_layout = "combined")
    expect_true("distribution" %in% names(tab[["_data"]]))
    html <- as.character(gt::as_raw_html(tab, inline_css = FALSE))
    expect_match(html, "Distribution")
    # continuous: one lane (<g>) per column (overall + 3 groups) in a single svg
    age_svg <- regmatches(html, regexpr("<svg[^>]*><title>Distribution of age</title>.*?</svg>", html, perl = TRUE))
    expect_length(age_svg, 1)
    expect_equal(lengths(regmatches(age_svg, gregexpr("<g>", age_svg))), 4)
    # categorical: per-level bar sets, each with 4 bars labelled by group
    expect_match(html, "<title>Overall: [0-9.]+%</title>")
    expect_match(html, "<title>A: [0-9.]+%</title>")
    # per-column graphics are not drawn
    expect_false(grepl("Distribution of age \\(A\\)", html))
})

test_that("combined bars reflect column percentages per group", {
    d <- data.frame(g = c("A", "A", "B", "B"), x = c("u", "u", "u", "v"))
    tab <- table_visual(d, "g", overall = FALSE, plot_layout = "combined", plot_width = 100)
    html <- as.character(gt::as_raw_html(tab, inline_css = FALSE))
    expect_match(html, "<title>A: 100.0%</title>")
    expect_match(html, "<title>B: 50.0%</title>")
    expect_match(html, 'width="50.0"')
})

test_that("plot_layout is ignored without a group and validated", {
    tab <- table_visual(make_df()[, -1], plot_layout = "combined")
    expect_false("distribution" %in% names(tab[["_data"]]))
    expect_error(table_visual(make_df(), "grp", plot_layout = "nope"))
})

test_that("combined layout works for every continuous_plot type", {
    for (type in c("violin_box", "boxplot", "density")) {
        tab <- table_visual(make_df(), "grp", continuous_plot = type, plot_layout = "combined")
        html <- as.character(gt::as_raw_html(tab, inline_css = FALSE))
        expect_match(html, "Distribution of age")
    }
})

test_that("show_outliers toggles outlier dots in both layouts", {
    for (layout in c("columns", "combined")) {
        on <- as.character(gt::as_raw_html(
            table_visual(make_df(), "grp", plot_layout = layout), inline_css = FALSE))
        off <- as.character(gt::as_raw_html(
            table_visual(make_df(), "grp", plot_layout = layout, show_outliers = FALSE),
            inline_css = FALSE))
        expect_match(on, "<circle")
        expect_false(grepl("<circle", off))
        expect_match(off, "<rect") # boxes still drawn
    }
    expect_error(table_visual(make_df(), show_outliers = NA))
})

test_that("combined continuous plot spans the variable's statistic rows", {
    tab <- table_visual(make_df(), "grp", plot_layout = "combined")
    tb <- tab[["_data"]]
    age_rows <- which(tb$variable == "age")
    html <- as.character(gt::as_raw_html(tab, inline_css = TRUE))
    # label row holds the overflowing plot; the 3 statistic rows hold spacers
    expect_match(html, "position:absolute")
    n_age_stats <- sum(tb$variable == "age" & tb$row_type == "level")
    expect_equal(lengths(regmatches(html, gregexpr('<div style="height:[0-9]+px"></div>', html))), n_age_stats)
    expect_match(html, "overflow: visible")
    # all cell styles survive together (a later tab_style() string would
    # otherwise replace an earlier one on the same cell)
    expect_match(html, 'headers="distribution"[^>]*padding-top: 0;[^>]*overflow: visible')
    expect_match(html, 'headers="stat_1"[^>]*white-space: nowrap')
})

test_that("axis tick labels are shown by default and can be turned off", {
    for (layout in c("columns", "combined")) {
        on <- as.character(gt::as_raw_html(
            table_visual(make_df(), "grp", plot_layout = layout), inline_css = FALSE))
        off <- as.character(gt::as_raw_html(
            table_visual(make_df(), "grp", plot_layout = layout, show_axis = FALSE),
            inline_css = FALSE))
        expect_false(grepl("<text", off))
        expect_match(on, "<text[^>]*>[0-9,.-]+</text>")
    }
    expect_error(table_visual(make_df(), show_axis = NA))
})

test_that("axis tick labels stay in range and don't overlap", {
    for (xlim in list(c(23, 2024.6), c(0.001, 0.0042), c(-5, 5), c(1e5, 9.9e6))) {
        svg <- svg_axis(xlim, width = 140, y0 = 0, height = 12)
        labs <- regmatches(svg, gregexpr("(?<=>)[^<]+(?=</text>)", svg, perl = TRUE))[[1]]
        mult <- c(k = 1e3, M = 1e6, B = 1e9)[sub("^[^kMB]*", "", labs)]
        mult[is.na(mult)] <- 1
        vals <- as.numeric(gsub("[,kMB]", "", labs)) * mult
        expect_gte(length(vals), 2)
        expect_true(all(vals >= xlim[1] & vals <= xlim[2]))
    }
    expect_match(svg_axis(c(5, 5), 140, 0, 12), ">5</text>") # constant variable
    expect_identical(svg_axis(c(0, 1), 140, 0, 0), "")
})

test_that("axis_breaks sets ticks per variable; others stay automatic", {
    tab <- table_visual(
        make_df(), "grp", plot_layout = "columns",
        axis_breaks = list(age = c(0, 75, 150))
    )
    html <- as.character(gt::as_raw_html(tab, inline_css = FALSE))
    age_svg <- regmatches(html, regexpr("<title>Distribution of age \\(A\\)</title>.*?</svg>", html, perl = TRUE))
    labs <- regmatches(age_svg, gregexpr("(?<=>)[^<]+(?=</text>)", age_svg, perl = TRUE))[[1]]
    expect_equal(labs, c("0", "75", "150"))
})

test_that("axis_breaks names become labels and out-of-range breaks widen the scale", {
    svg <- svg_axis(c(0, 100), 140, 0, 12, breaks = c(low = 0, high = 100))
    expect_match(svg, ">low</text>")
    expect_match(svg, ">high</text>")
    # break at 0 below the data range is still drawn inside the graphic
    d <- data.frame(x = 50:70)
    html <- as.character(gt::as_raw_html(
        table_visual(d, show_axis = TRUE, axis_breaks = list(x = c(0, 50, 100))),
        inline_css = FALSE))
    expect_match(html, ">0</text>")
    expect_match(html, ">100</text>")
})

test_that("axis_breaks is validated", {
    expect_error(table_visual(make_df(), show_axis = TRUE, axis_breaks = c(1, 2)), "named list")
    expect_error(table_visual(make_df(), show_axis = TRUE, axis_breaks = list(nope = 1)), "not found")
    expect_error(table_visual(make_df(), show_axis = TRUE, axis_breaks = list(age = "a")), "finite numeric")
    expect_warning(
        table_visual(make_df(), show_axis = FALSE, axis_breaks = list(age = c(20, 40))),
        "ignored"
    )
})

make_miss_df <- function() {
    d <- make_df()
    d$sex[1:12] <- NA
    d$grp[c(5, 50, 100)] <- NA
    d
}

test_that("N missing row shows n (%) with hatched bars in both layouts", {
    for (layout in c("columns", "combined")) {
        tab <- table_visual(make_miss_df(), "grp", plot_layout = layout)
        tb <- tab[["_data"]]
        miss <- tb[tb$variable == "sex" & tb$row_type == "missing", ]
        expect_match(miss$stat_0, "^[0-9]+ \\([0-9.]+%\\)$")
        html <- as.character(gt::as_raw_html(tab, inline_css = FALSE))
        expect_match(html, 'fill="url\\(#tv-hatch-')
        expect_match(html, "<pattern id=\"tv-hatch-")
    }
})

test_that("% missing bar is out of all observations in the column", {
    d <- data.frame(x = c("a", "a", NA, NA))
    html <- as.character(gt::as_raw_html(table_visual(d, bar_width = 100), inline_css = FALSE))
    # 2 of 4 missing -> 50; category "a" is 100% of non-missing
    expect_match(html, 'width="50.0" height="10" fill="url\\(#tv-hatch-')
    expect_match(html, 'width="100.0" height="10" fill="#7F7F7F"')
})

test_that("combined continuous plots do not span the N missing row", {
    d <- make_df()
    d$age[1:10] <- NA
    tab <- table_visual(d, "grp", plot_layout = "combined")
    tb <- tab[["_data"]]
    html <- as.character(gt::as_raw_html(tab, inline_css = FALSE))
    miss_row <- which(tb$variable == "age" & tb$row_type == "missing")
    expect_length(miss_row, 1)
    # spacers only for the 3 statistic rows of age (and none for its missing row)
    n_age_stats <- sum(tb$variable == "age" & tb$row_type == "level")
    expect_equal(lengths(regmatches(html, gregexpr('<div style="height:[0-9]+px"></div>', html))), n_age_stats)
})

test_that("missing group rows are excluded with a note, or shown as a column", {
    d <- make_miss_df()
    ex <- table_visual(d, "grp")
    html <- as.character(gt::as_raw_html(ex, inline_css = FALSE))
    expect_match(html, "3 observations with missing grp excluded.")
    expect_match(html, "N = 117")
    sh <- table_visual(d, "grp", missing_group = "show")
    html <- as.character(gt::as_raw_html(sh, inline_css = FALSE))
    expect_false(grepl("excluded", html))
    expect_match(html, "Missing")
    expect_match(html, "N = 120")
    expect_true("stat_4" %in% names(sh[["_data"]]))
    # named colors may include the Missing group
    sh2 <- table_visual(d, "grp", missing_group = "show",
                        colors = c(A = "#111111", B = "#222222", C = "#333333", Missing = "#444444"))
    expect_match(as.character(gt::as_raw_html(sh2, inline_css = FALSE)), "#444444")
})

test_that("denominator notes appear only when relevant", {
    html <- as.character(gt::as_raw_html(table_visual(make_df()[, -1]), inline_css = FALSE))
    expect_false(grepl("Category percentages exclude", html)) # no missing categories
    expect_match(html, "% missing is out of all observations") # age has an NA
    html <- as.character(gt::as_raw_html(table_visual(make_miss_df(), "grp"), inline_css = FALSE))
    expect_match(html, "Category percentages exclude missing values.")
})

test_that("header_align centers column labels by default, left optional", {
    label_align <- function(tab, col) {
        st <- tab[["_styles"]]
        st <- st[st$locname == "columns_columns" & st$colname == col, ]
        aligns <- unlist(lapply(st$styles, function(x) x$cell_text$align))
        utils::tail(aligns, 1)
    }
    expect_equal(label_align(table_visual(make_df(), "grp"), "stat_1"), "center")
    expect_equal(label_align(table_visual(make_df(), "grp", plot_layout = "combined"), "distribution"), "center")
    expect_equal(label_align(table_visual(make_df(), "grp", header_align = "left"), "stat_1"), "left")
    expect_error(table_visual(make_df(), header_align = "right"))
})
