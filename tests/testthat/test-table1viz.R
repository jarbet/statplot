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
    tab <- table1viz(make_df()[, -1])
    expect_s3_class(tab, "gt_tbl")
    html <- as.character(gt::as_raw_html(tab, inline_css = FALSE))
    expect_match(html, "<svg")
    expect_match(html, "<circle") # outlier at 150
    expect_false(grepl("&#9632;", html)) # no swatch when ungrouped
})

test_that("grouped table has overall + group columns and colored swatches", {
    tab <- table1viz(make_df(), name_groupvar = "grp")
    expect_true(all(c("stat_0", "stat_1", "stat_2", "stat_3") %in% names(tab[["_data"]])))
    html <- as.character(gt::as_raw_html(tab, inline_css = FALSE))
    expect_match(html, "#0072B2")
    expect_match(html, "Group")
})

test_that("overall = FALSE drops overall column", {
    tab <- table1viz(make_df(), name_groupvar = "grp", overall = FALSE)
    html <- as.character(gt::as_raw_html(tab, inline_css = FALSE))
    expect_false(grepl("Overall", html))
})

test_that("continuous_plot options all render", {
    for (type in c("violin_box", "boxplot", "density")) {
        tab <- table1viz(make_df()[, -1], continuous_plot = type)
        html <- as.character(gt::as_raw_html(tab, inline_css = FALSE))
        expect_match(html, "<svg")
    }
    expect_error(table1viz(make_df(), continuous_plot = "nope"))
})

test_that("custom colors are validated and applied", {
    tab <- table1viz(
        make_df(), "grp",
        colors = c(A = "#111111", B = "#222222", C = "#333333")
    )
    html <- as.character(gt::as_raw_html(tab, inline_css = FALSE))
    expect_match(html, "#222222")
    expect_error(table1viz(make_df(), "grp", colors = c(A = "red")), "missing group")
    expect_error(table1viz(make_df(), "grp", colors = "red"), "at least one color")
})

test_that("bar widths reflect column percentages", {
    d <- data.frame(x = c(rep("a", 3), "b"))
    html <- as.character(gt::as_raw_html(table1viz(d, bar_width = 100), inline_css = FALSE))
    expect_match(html, 'width="75.0"')
    expect_match(html, 'width="25.0"')
})

test_that("binary bars use the level gtsummary displays", {
    bar_html <- function(d) {
        as.character(gt::as_raw_html(table1viz(d, bar_width = 100), inline_css = FALSE))
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
    expect_s3_class(table1viz(d), "gt_tbl")
})

test_that("invalid inputs error", {
    expect_error(table1viz("x"))
    expect_error(table1viz(make_df(), name_groupvar = "nope"))
})

test_that("combined layout draws one distribution column with a lane per group", {
    tab <- table1viz(make_df(), name_groupvar = "grp", plot_layout = "combined")
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
    tab <- table1viz(d, "g", overall = FALSE, plot_layout = "combined", plot_width = 100)
    html <- as.character(gt::as_raw_html(tab, inline_css = FALSE))
    expect_match(html, "<title>A: 100.0%</title>")
    expect_match(html, "<title>B: 50.0%</title>")
    expect_match(html, 'width="50.0"')
})

test_that("plot_layout is ignored without a group and validated", {
    tab <- table1viz(make_df()[, -1], plot_layout = "combined")
    expect_false("distribution" %in% names(tab[["_data"]]))
    expect_error(table1viz(make_df(), "grp", plot_layout = "nope"))
})

test_that("combined layout works for every continuous_plot type", {
    for (type in c("violin_box", "boxplot", "density")) {
        tab <- table1viz(make_df(), "grp", continuous_plot = type, plot_layout = "combined")
        html <- as.character(gt::as_raw_html(tab, inline_css = FALSE))
        expect_match(html, "Distribution of age")
    }
})

test_that("show_outliers toggles outlier dots in both layouts", {
    for (layout in c("columns", "combined")) {
        on <- as.character(gt::as_raw_html(
            table1viz(make_df(), "grp", plot_layout = layout), inline_css = FALSE))
        off <- as.character(gt::as_raw_html(
            table1viz(make_df(), "grp", plot_layout = layout, show_outliers = FALSE),
            inline_css = FALSE))
        expect_match(on, "<circle")
        expect_false(grepl("<circle", off))
        expect_match(off, "<rect") # boxes still drawn
    }
    expect_error(table1viz(make_df(), show_outliers = NA))
})

test_that("combined continuous plot spans the variable's statistic rows", {
    tab <- table1viz(make_df(), "grp", plot_layout = "combined")
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
            table1viz(make_df(), "grp", plot_layout = layout), inline_css = FALSE))
        off <- as.character(gt::as_raw_html(
            table1viz(make_df(), "grp", plot_layout = layout, show_axis = FALSE),
            inline_css = FALSE))
        expect_false(grepl("<text", off))
        expect_match(on, "<text[^>]*>[0-9,.-]+</text>")
    }
    expect_error(table1viz(make_df(), show_axis = NA))
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
    tab <- table1viz(
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
        table1viz(d, show_axis = TRUE, axis_breaks = list(x = c(0, 50, 100))),
        inline_css = FALSE))
    expect_match(html, ">0</text>")
    expect_match(html, ">100</text>")
})

test_that("axis_breaks is validated", {
    expect_error(table1viz(make_df(), show_axis = TRUE, axis_breaks = c(1, 2)), "named list")
    expect_error(table1viz(make_df(), show_axis = TRUE, axis_breaks = list(nope = 1)), "not found")
    expect_error(table1viz(make_df(), show_axis = TRUE, axis_breaks = list(age = "a")), "finite numeric")
    expect_warning(
        table1viz(make_df(), show_axis = FALSE, axis_breaks = list(age = c(20, 40))),
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
        tab <- table1viz(make_miss_df(), "grp", plot_layout = layout)
        tb <- tab[["_data"]]
        miss <- tb[tb$variable == "sex" & tb$row_type == "missing", ]
        expect_match(miss$stat_0, "^[0-9]+ / [0-9,]+ \\([0-9.]+%\\)$")
        html <- as.character(gt::as_raw_html(tab, inline_css = FALSE))
        expect_match(html, 'fill="url\\(#tv-hatch-')
        expect_match(html, "<pattern id=\"tv-hatch-")
    }
})

test_that("% missing bar is out of all observations in the column", {
    d <- data.frame(x = c("a", "a", NA, NA))
    html <- as.character(gt::as_raw_html(table1viz(d, bar_width = 100), inline_css = FALSE))
    # 2 of 4 missing -> 50; category "a" is 100% of non-missing
    expect_match(html, 'width="50.0" height="10" fill="url\\(#tv-hatch-')
    expect_match(html, 'width="100.0" height="10" fill="#7F7F7F"')
})

test_that("combined continuous plots do not span the N missing row", {
    d <- make_df()
    d$age[1:10] <- NA
    tab <- table1viz(d, "grp", plot_layout = "combined")
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
    ex <- table1viz(d, "grp")
    html <- as.character(gt::as_raw_html(ex, inline_css = FALSE))
    expect_match(html, "3 observations with missing grp excluded.")
    expect_match(html, "N = 117")
    sh <- table1viz(d, "grp", missing_group = "show")
    html <- as.character(gt::as_raw_html(sh, inline_css = FALSE))
    expect_false(grepl("excluded", html))
    expect_match(html, "Missing")
    expect_match(html, "N = 120")
    expect_true("stat_4" %in% names(sh[["_data"]]))
    # named colors may include the Missing group
    sh2 <- table1viz(d, "grp", missing_group = "show",
                        colors = c(A = "#111111", B = "#222222", C = "#333333", Missing = "#444444"))
    expect_match(as.character(gt::as_raw_html(sh2, inline_css = FALSE)), "#444444")
})

test_that("denominator notes appear only when relevant", {
    html <- as.character(gt::as_raw_html(table1viz(make_df()[, -1]), inline_css = FALSE))
    expect_false(grepl("Category percentages exclude", html)) # no missing categories
    expect_match(html, "% missing is out of all observations") # age has an NA
    expect_match(html, "Hatched \\(striped\\) bars show % missing.")
    # all notes on one line
    expect_length(table1viz(make_miss_df(), "grp")[["_source_notes"]], 1)
    html <- as.character(gt::as_raw_html(table1viz(make_miss_df(), "grp"), inline_css = FALSE))
    expect_match(html, "Category percentages are n / N, where N excludes missing values.")
    html <- as.character(gt::as_raw_html(
        table1viz(make_miss_df(), "grp", show_denominator = "no"),
        inline_css = FALSE
    ))
    expect_match(html, "Category percentages exclude missing values.")
})

test_that("show_denominator shows n / N (%) for categorical variables", {
    d <- data.frame(
        g = c("a", "a", "a", "b", "b", "b"),
        x = c("u", NA, "v", "u", "v", "v"),
        z = c("p", "q", "p", "q", "p", "p")
    )
    for (layout in c("combined", "columns")) {
        tb <- table1viz(d, "g", plot_layout = layout, output = "gt")[["_data"]]
        lev <- tb[tb$variable == "x" & tb$row_type == "level", ]
        # x has an NA in group a: denominators are non-missing counts per column
        expect_match(lev$stat_0, "^\\d+ / 5 \\(")
        expect_match(lev$stat_1, "^\\d+ / 2 \\(")
        expect_match(lev$stat_2, "^\\d+ / 3 \\(")
        # z has no missing values: plain n (%)
        z <- tb$stat_1[tb$variable == "z" & tb$row_type == "level"]
        expect_false(any(grepl("/", z)))
    }
    tb <- table1viz(d, "g", show_denominator = "always", output = "gt")[["_data"]]
    expect_match(tb$stat_1[tb$variable == "z" & tb$row_type == "level"], " / 3 \\(")
    # N missing row: n / N (%) out of all observations in the column
    miss <- tb[tb$variable == "x" & tb$row_type == "missing", ]
    expect_equal(miss$stat_1, "1 / 3 (33%)")
    expect_equal(miss$stat_2, "0 / 3 (0%)")
    tb <- table1viz(d, "g", show_denominator = "no", output = "gt")[["_data"]]
    expect_false(any(grepl("/", tb$stat_1[tb$variable == "x" & tb$row_type == "level"])))
    expect_equal(tb$stat_1[tb$variable == "x" & tb$row_type == "missing"], "1 (33%)")
    # user digits keep applying to n and p (not shifted onto the new N)
    for (dg in list(
        list(gtsummary::all_categorical() ~ c(0, 2)),
        list(x ~ c(0, 2)),
        list(gtsummary::all_categorical() ~ list(p = 2))
    )) {
        tb <- table1viz(d, "g", digits = dg, output = "gt")[["_data"]]
        expect_equal(
            tb$stat_1[tb$variable == "x" & tb$row_type == "level"],
            c("1 / 2 (50.00%)", "1 / 2 (50.00%)")
        )
    }
    # custom categorical statistics are left as given
    tb <- table1viz(
        d, "g",
        statistic = list(gtsummary::all_categorical() ~ "{n}"),
        output = "gt"
    )[["_data"]]
    expect_false(any(grepl("/", tb$stat_1[tb$variable == "x" & tb$row_type == "level"])))
})

test_that("header_align left-aligns column labels by default, center optional", {
    label_align <- function(tab, col) {
        st <- tab[["_styles"]]
        st <- st[st$locname == "columns_columns" & st$colname == col, ]
        aligns <- unlist(lapply(st$styles, function(x) x$cell_text$align))
        utils::tail(aligns, 1)
    }
    expect_equal(label_align(table1viz(make_df(), "grp"), "stat_1"), "left")
    expect_equal(label_align(table1viz(make_df(), "grp", plot_layout = "combined"), "distribution"), "left")
    expect_equal(label_align(table1viz(make_df(), "grp", header_align = "center"), "stat_1"), "center")
    expect_error(table1viz(make_df(), header_align = "right"))
})

test_that("violins are trimmed to each group's own range; densities are not", {
    xlim <- c(0, 100)
    x <- c(40, 45, 50, 52, 55, 60)
    v <- statplot:::compute_density(x, xlim, trim = TRUE)
    expect_equal(range(v$x), c(40, 60))
    d <- statplot:::compute_density(x, xlim)
    expect_equal(range(d$x), xlim)

    # End to end: axis wider than the data, plot_width chosen so that the
    # x-coordinate is value + 2 (2px padding on each side of a 100px range)
    set.seed(1)
    df <- data.frame(
        grp = rep(c("A", "B"), each = 30),
        y = c(runif(30, 40, 60), runif(30, 20, 80))
    )
    tab <- table1viz(
        df, "grp",
        overall = FALSE,
        plot_width = 104,
        axis_breaks = list(y = c(0, 100))
    )
    html <- as.character(gt::as_raw_html(tab, inline_css = FALSE))
    paths <- regmatches(
        html,
        gregexpr('<path d="M[^"]*" fill="[^"]*" fill-opacity="0.25"', html)
    )[[1]]
    expect_length(paths, 2)
    path_x <- lapply(paths, function(p) {
        pts <- regmatches(p, gregexpr("[0-9.]+,[0-9.]+", p))[[1]]
        as.numeric(sub(",.*", "", pts))
    })
    for (i in 1:2) {
        g <- df$y[df$grp == c("A", "B")[i]]
        expect_lt(max(abs(range(path_x[[i]]) - (range(g) + 2))), 0.11) # SVG rounds to 0.1
    }
})

# ---- Word / flextable output ----

word_xml <- function(ft) {
    withr::local_options(knitr.in.progress = TRUE)
    knitr::opts_knit$set(rmarkdown.pandoc.to = "docx")
    withr::defer(knitr::opts_knit$set(rmarkdown.pandoc.to = NULL))
    as.character(knitr::knit_print(ft))
}

test_that("output = 'auto' gives gt outside Word; 'flextable' is validated", {
    skip_if_not_installed("flextable")
    skip_if_not_installed("xml2")
    expect_s3_class(table1viz(make_df()), "gt_tbl")
    expect_s3_class(table1viz(make_df(), output = "gt"), "gt_tbl")
    ft <- table1viz(make_df(), "grp", output = "flextable")
    expect_s3_class(ft, c("table1viz_flextable", "flextable"))
    expect_error(table1viz(make_df(), output = "word"))
})

test_that("flextable keeps a graphic per cell and merges spanning plots", {
    skip_if_not_installed("flextable")
    skip_if_not_installed("xml2")
    df <- make_df()
    comb <- table1viz(df, "grp", output = "flextable")
    expect_true("distribution" %in% comb$col_keys)
    # one graphic per continuous variable and per categorical/binary row
    tb <- comb$body$dataset
    n_cont <- sum(vapply(df[-1], is.numeric, logical(1)) &
        !vapply(df[-1], function(x) all(x %in% 0:1), logical(1)))
    expect_gte(length(comb$statplot_graphics), n_cont)
    expect_true(all(grepl("^<svg", comb$statplot_graphics)))
    # the age plot spans its label + statistic rows in one merged cell
    age_rows <- which(tb$label == "age") + 0:2 # label + 2 statistic rows
    spans <- comb$body$spans$columns[age_rows, match("distribution", comb$col_keys)]
    expect_equal(spans, c(length(age_rows), rep(0, length(age_rows) - 1)))

    cols <- table1viz(df, "grp", output = "flextable", plot_layout = "columns")
    expect_false("distribution" %in% cols$col_keys)
    expect_gt(length(cols$statplot_graphics), length(comb$statplot_graphics))
})

test_that("knitting to Word swaps every placeholder for a native drawing", {
    skip_if_not_installed("flextable")
    skip_if_not_installed("xml2")
    for (lay in c("combined", "columns")) {
        ft <- table1viz(make_df(), "grp", output = "flextable", plot_layout = lay)
        out <- word_xml(ft)
        expect_false(grepl("STATPLOTGFX", out))
        expect_equal(
            lengths(regmatches(out, gregexpr("<wp:inline ", out))),
            length(ft$statplot_graphics)
        )
    }
    # bar cells keep their n (%) text after the bar
    ft <- table1viz(make_df(), "grp", output = "flextable", plot_layout = "columns")
    expect_match(word_xml(ft), "</w:drawing></w:r>.*[0-9]+ \\([0-9.]+%\\)")
})

test_that("printing to HTML draws graphics as SVG", {
    skip_if_not_installed("flextable")
    skip_if_not_installed("xml2")
    ft <- table1viz(make_df(), "grp", output = "flextable")
    html <- fill_graphics(
        as.character(flextable::htmltools_value(plain_flextable(ft))),
        ft$statplot_graphics,
        "html"
    )
    expect_false(grepl("STATPLOTGFX", html))
    expect_equal(lengths(regmatches(html, gregexpr("<svg ", html))), length(ft$statplot_graphics))
})

test_that("svg_to_wml converts each SVG element to a Word shape", {
    skip_if_not_installed("xml2")
    wrap <- function(x) {
        xml2::read_xml(paste0(
            '<root xmlns:w="http://schemas.openxmlformats.org/wordprocessingml/2006/main" ',
            'xmlns:wp="http://schemas.openxmlformats.org/drawingml/2006/wordprocessingDrawing">',
            x, '</root>'
        ))
    }
    ns <- c(
        a = "http://schemas.openxmlformats.org/drawingml/2006/main",
        wps = "http://schemas.microsoft.com/office/word/2010/wordprocessingShape",
        wp = "http://schemas.openxmlformats.org/drawingml/2006/wordprocessingDrawing",
        w = "http://schemas.openxmlformats.org/wordprocessingml/2006/main"
    )
    # hatched bars: one background + one patterned bar per lane
    bars <- wrap(svg_to_wml(svg_bars_combined(
        c(0.5, 0.25), colors = c("#0072B2", "red"), lane_names = c("A", "B"),
        width = 100, title = "x", hatched = TRUE
    )))
    expect_length(xml2::xml_find_all(bars, "//wps:wsp", ns), 4)
    patt <- xml2::xml_find_all(bars, "//a:pattFill/a:fgClr/a:srgbClr", ns)
    expect_equal(xml2::xml_attr(patt, "val"), c("0072B2", "FF0000"))
    # 100px wide in EMU, bar of 50% = 50px
    expect_equal(xml2::xml_attr(xml2::xml_find_first(bars, "//wp:extent", ns), "cx"), "952500")
    expect_true("476250" %in% xml2::xml_attr(xml2::xml_find_all(bars, "//a:ext", ns), "cx"))

    # violin (path), box (rect), median/whiskers (lines), outlier (circle), axis text
    set.seed(1)
    x <- c(rnorm(50), 6) # 6 is an outlier
    xlim <- range(x)
    svg <- svg_continuous(
        compute_density(x, xlim, trim = TRUE), x, xlim, ymax = 0.5, color = "#0072B2",
        width = 140, height = 30, title = "t", show_outliers = TRUE, axis_height = 12
    )
    w <- wrap(svg_to_wml(svg))
    expect_length(xml2::xml_find_all(w, "//a:custGeom/a:pathLst/a:path[a:close]", ns), 1)
    expect_length(
        xml2::xml_find_all(w, "//a:prstGeom[@prst='ellipse']", ns),
        lengths(regmatches(svg, gregexpr("<circle", svg)))
    )
    labs <- xml2::xml_text(xml2::xml_find_all(w, "//wps:txbx//w:t", ns))
    expect_equal(labs, regmatches(svg, gregexpr("(?<=>)[^<>]+(?=</text>)", svg, perl = TRUE))[[1]])
    # drawing ids are unique across graphics
    ids <- c(
        xml2::xml_attr(xml2::xml_find_all(w, "//wp:docPr | //wps:cNvPr", ns), "id"),
        xml2::xml_attr(xml2::xml_find_all(bars, "//wp:docPr | //wps:cNvPr", ns), "id")
    )
    expect_false(anyDuplicated(ids) > 0)
})

test_that("Group spanner is centered; column labels follow header_align", {
    skip_if_not_installed("flextable")
    skip_if_not_installed("xml2")
    html <- as.character(gt::as_raw_html(table1viz(make_df(), "grp"), inline_css = FALSE))
    expect_match(html, '<th class="gt_center[^"]*gt_column_spanner_outer')
    ft <- table1viz(make_df(), "grp", output = "flextable")
    al <- ft$header$styles$pars$text.align$data
    j <- match("stat_1", ft$col_keys)
    expect_equal(unname(al[1, j]), "center")
    expect_equal(unname(al[2, j]), "left")
    ft <- table1viz(make_df(), "grp", output = "flextable", header_align = "center")
    expect_equal(unname(ft$header$styles$pars$text.align$data[2, j]), "center")
})

test_that("overall_divider draws a line right of the Overall column", {
    stat0_cells <- function(tab) {
        h <- as.character(gt::as_raw_html(tab, inline_css = TRUE))
        regmatches(h, gregexpr('<t[dh][^>]*(headers|id)="stat_0"[^>]*>', h))[[1]]
    }
    for (lay in c("combined", "columns")) {
        on <- stat0_cells(table1viz(make_df(), "grp", overall_divider = TRUE, plot_layout = lay))
        expect_true(all(grepl("border-right-width: 2px; border-right-style: solid; border-right-color: #D3D3D3", fixed = TRUE, on)))
    }
    expect_true(all(grepl("border-right-width: 2px; border-right-style: solid; border-right-color: #D3D3D3", fixed = TRUE, stat0_cells(table1viz(make_df(), "grp"))))) # default
    expect_false(any(grepl("border-right-width: 2px; border-right-style: solid; border-right-color: #D3D3D3", fixed = TRUE, stat0_cells(table1viz(make_df(), "grp", overall_divider = FALSE)))))
    # ignored without group columns
    expect_s3_class(table1viz(make_df(), overall_divider = TRUE), "gt_tbl")
    expect_s3_class(table1viz(make_df(), "grp", overall = FALSE, overall_divider = TRUE), "gt_tbl")
    expect_error(table1viz(make_df(), "grp", overall_divider = NA))

    skip_if_not_installed("flextable")
    skip_if_not_installed("xml2")
    ft <- table1viz(make_df(), "grp", overall_divider = TRUE, output = "flextable")
    j <- match("stat_0", ft$col_keys)
    expect_true(all(ft$body$styles$cells$border.color.right$data[, j] == "#D3D3D3"))
    expect_true(all(ft$header$styles$cells$border.color.right$data[, j] == "#D3D3D3"))
    ft <- table1viz(make_df(), "grp", overall_divider = FALSE, output = "flextable")
    expect_false(any(ft$body$styles$cells$border.color.right$data[, j] == "#D3D3D3"))
})

test_that("the caller's gtsummary theme is restored", {
    gtsummary::reset_gtsummary_theme()
    withr::defer(gtsummary::reset_gtsummary_theme())
    # no theme before -> none after
    table1viz(make_df(), "grp")
    expect_length(gtsummary::get_gtsummary_theme(), 0)
    expect_s3_class(table1viz(make_df(), "grp", output = "flextable"), "flextable")
    expect_length(gtsummary::get_gtsummary_theme(), 0)
    # a user's own theme survives the call
    suppressMessages(gtsummary::theme_gtsummary_language("fr"))
    before <- gtsummary::get_gtsummary_theme()
    table1viz(make_df(), "grp")
    expect_identical(gtsummary::get_gtsummary_theme(), before)
    # but the table itself still uses the package theme (English, compact)
    html <- as.character(gt::as_raw_html(table1viz(make_df(), "grp"), inline_css = FALSE))
    expect_match(html, "N missing")
})

test_that("the caller's gtsummary theme does not change the table's numbers", {
    gtsummary::reset_gtsummary_theme()
    withr::defer(gtsummary::reset_gtsummary_theme())
    stats <- function() table1viz(make_df(), "grp")[["_data"]][, c("label", "stat_0", "stat_1")]
    base <- stats()
    gtsummary::set_gtsummary_theme(list(
        "tbl_summary-fn:percent_fun" = function(x) gtsummary::style_percent(x, digits = 2)
    ))
    expect_identical(stats(), base)
    # and the caller's theme is still in place afterwards
    expect_true("tbl_summary-fn:percent_fun" %in% names(gtsummary::get_gtsummary_theme()))
})

test_that("bar_axis adds one 0-100% axis per variable, under its last bar", {
    d <- make_df()
    d$sex[1:5] <- NA # sex gets an N missing row
    count_axes <- function(html, lab) {
        lengths(regmatches(html, gregexpr(sprintf(">%s</text>", lab), html)))
    }
    for (lay in c("combined", "columns")) {
        off <- as.character(gt::as_raw_html(
            table1viz(d, "grp", plot_layout = lay), inline_css = FALSE))
        expect_equal(count_axes(off, "50%"), 0)
        tab <- table1viz(d, "grp", plot_layout = lay, bar_axis = TRUE)
        html <- as.character(gt::as_raw_html(tab, inline_css = FALSE))
        # variables with bars: sex (axis on its N missing row), diabetes,
        # and age's N missing row; one axis per variable (per column if "columns")
        n_cols <- if (lay == "combined") 1 else 4
        expect_equal(count_axes(html, "0%"), 3 * n_cols)
        expect_equal(count_axes(html, "100%"), 3 * n_cols)
    }
    # the axis sits on sex's N missing row, not on its last level ("M")
    skip_if_not_installed("flextable")
    skip_if_not_installed("xml2")
    g <- table1viz(d, "grp", bar_axis = TRUE, output = "flextable")$statplot_graphics
    has_axis <- grepl(">50%</text>", g)
    expect_false(any(has_axis[grepl("<title>M</title>", g)]))
    expect_true(all(has_axis[grepl("<title>Missing</title>", g)]))
    expect_true(all(has_axis[grepl("<title>diabetes</title>", g)]))
})

test_that("bar_axis_breaks sets ticks and labels and is validated", {
    tab <- table1viz(make_df(), "grp", bar_axis = TRUE, bar_axis_breaks = c(0, 25, 100))
    html <- as.character(gt::as_raw_html(tab, inline_css = FALSE))
    expect_match(html, ">25%</text>")
    expect_false(grepl(">50%</text>", html))
    tab <- table1viz(make_df(), "grp", bar_axis = TRUE, bar_axis_breaks = c(none = 0, half = 50))
    html <- as.character(gt::as_raw_html(tab, inline_css = FALSE))
    expect_match(html, ">half</text>")
    expect_error(table1viz(make_df(), "grp", bar_axis = TRUE, bar_axis_breaks = c(0, 150)))
    expect_error(table1viz(make_df(), "grp", bar_axis = NA))
})

test_that("bar axis is drawn in Word output", {
    skip_if_not_installed("flextable")
    skip_if_not_installed("xml2")
    ft <- table1viz(make_df(), "grp", bar_axis = TRUE, output = "flextable")
    svgs <- ft$statplot_graphics
    expect_true(any(grepl(">50%</text>", svgs)))
    wml <- svg_to_wml(svgs[grepl(">50%</text>", svgs)][[1]])
    expect_match(wml, "50%</w:t>")
})

test_that("narrow bar axes keep every tick but hide overlapping labels", {
    ax <- svg_axis(c(0, 100), 60, y0 = 0, height = 12, pad = 0,
        breaks = c("0%" = 0, "50%" = 50, "100%" = 100), drop_overlapping = TRUE)
    expect_equal(lengths(regmatches(ax, gregexpr("<line", ax))), 4) # baseline + 3 ticks
    expect_match(ax, ">0%</text>")
    expect_match(ax, ">100%</text>")
    expect_false(grepl(">50%</text>", ax))
    wide <- svg_axis(c(0, 100), 140, y0 = 0, height = 12, pad = 0,
        breaks = c("0%" = 0, "50%" = 50, "100%" = 100), drop_overlapping = TRUE)
    expect_match(wide, ">50%</text>")
})

test_that("variable_divider draws a thicker line above each variable but the first", {
    d <- make_df()
    for (lay in c("combined", "columns")) {
        tab <- table1viz(d, "grp", plot_layout = lay)
        n_cols <- ncol(tab[["_data"]]) - sum(tab[["_boxhead"]]$type == "hidden")
        html <- as.character(gt::as_raw_html(tab, inline_css = TRUE))
        # 3 variables -> lines above the 2nd and 3rd, across every column
        n_lines <- lengths(regmatches(html, gregexpr("border-top-width: 2px; border-top-style: solid; border-top-color: #D3D3D3", html)))
        expect_equal(n_lines, 2 * n_cols)
        expect_match(html, "border-top-width: 2px")
    }
    off <- as.character(gt::as_raw_html(
        table1viz(d, "grp", variable_divider = FALSE, overall_divider = FALSE),
        inline_css = TRUE
    ))
    expect_false(grepl("border-top-width: 2px; border-top-style: solid; border-top-color: #D3D3D3", off, fixed = TRUE))
    expect_error(table1viz(d, "grp", variable_divider = NA))
})

test_that("variable_divider draws lines in Word output", {
    skip_if_not_installed("flextable")
    skip_if_not_installed("xml2")
    d <- make_df()
    ft <- table1viz(d, "grp", output = "flextable")
    starts <- which(ft$body$dataset$row_type == "label")
    if (is.null(ft$body$dataset$row_type)) {
        starts <- which(table1viz(d, "grp", variable_divider = FALSE)[["_data"]]$row_type == "label")
    }
    bottom <- ft$body$styles$cells$border.color.bottom$data[, 1]
    expect_true(all(bottom[starts[-1] - 1] == "#D3D3D3"))
    off <- table1viz(d, "grp", variable_divider = FALSE, output = "flextable")
    expect_false(any(off$body$styles$cells$border.color.bottom$data[, 1] == "#D3D3D3"))
})

test_that("continuous variables show Mean (SD) and min; max by default; median (Q1, Q3) can be added", {
    # >= 10 distinct values so gtsummary treats x as continuous
    d <- data.frame(grp = rep(c("A", "B"), 6), x = c(-5, 1:10, 2500.5))
    tab <- table1viz(d, "grp", digits = list(gtsummary::all_continuous() ~ 1))[["_data"]]
    expect_equal(tab$label[tab$row_type == "level"], c("Mean (SD)", "Min; Max"))
    # ";" keeps min and max apart from thousands commas
    expect_equal(tab$stat_0[tab$label == "Min; Max"], "-5.0; 2,500.5")
    with_median <- table1viz(
        d,
        "grp",
        statistic = list(
            gtsummary::all_continuous() ~ c("{mean} ({sd})", "{median} ({p25}, {p75})", "{min}; {max}"),
            gtsummary::all_categorical() ~ "{n} ({p}%)"
        )
    )[["_data"]]$label
    # gtsummary's label is kept: the IQR is one number, not (Q1, Q3)
    expect_true("Median (Q1, Q3)" %in% with_median)
    expect_false("Median (IQR)" %in% with_median)
})

test_that("default digits adapt to each variable's scale", {
    set.seed(1)
    d <- data.frame(small = rlnorm(100, -4, 1))
    tab <- table1viz(d)[["_data"]]
    stat <- tab$stat_0[tab$label == "Mean (SD)"]
    expect_false(grepl("^0\\.0 \\(0\\.0\\)$", stat))
    expect_true(grepl("0\\.0[1-9]", stat))
})

test_that("date/time columns give a clear error", {
    d <- data.frame(
        x = rnorm(20),
        dt = as.Date("2020-01-01") + 1:20,
        ts = as.POSIXct("2020-01-01", tz = "UTC") + 1:20
    )
    expect_error(table1viz(d), "Date/time columns are not supported: `dt`, `ts`")
})

test_that("lines within a variable are hidden by default; row_lines = TRUE shows them", {
    d <- make_df()
    top_hidden <- function(tab) {
        h <- as.character(gt::as_raw_html(tab, inline_css = TRUE))
        # one stat_0 cell per body row, in row order
        cells <- regmatches(h, gregexpr('<td[^>]*headers="stat_0"[^>]*>', h))[[1]]
        grepl("border-top-style: hidden", cells)
    }
    for (lay in c("combined", "columns")) {
        tab <- table1viz(d, "grp", plot_layout = lay)
        label_row <- tab[["_data"]]$row_type == "label"
        hidden <- top_hidden(tab)
        expect_true(all(hidden[!label_row]))
        expect_false(any(hidden[label_row]))
        expect_false(any(top_hidden(table1viz(d, "grp", plot_layout = lay, row_lines = TRUE))))
    }
    expect_error(table1viz(d, "grp", row_lines = NA))
})

test_that("the line under the column labels is darker; the Group spanner line stays light", {
    tab <- table1viz(make_df(), "grp")
    h <- as.character(gt::as_raw_html(tab, inline_css = TRUE))
    labels <- regmatches(h, gregexpr('<th[^>]*id="(label|stat_[0-9]+|distribution)"[^>]*>', h))[[1]]
    expect_length(labels, ncol(tab[["_data"]]) - sum(tab[["_boxhead"]]$type == "hidden"))
    expect_true(all(grepl("border-bottom-width: 2px; border-bottom-style: solid; border-bottom-color: #9E9E9E", labels, fixed = TRUE)))
    spanner <- regmatches(h, regexpr('<div class="gt_column_spanner"[^>]*>', h))
    expect_match(spanner, "border-bottom-color: #D3D3D3", fixed = TRUE)
})
