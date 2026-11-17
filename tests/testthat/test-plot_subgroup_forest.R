# Two groups: A has Sex and Smoking; B has only Smoking and a single Sex level
# (e.g. a study that enrolled only women, whose sex row is its overall estimate)
fake_forest_data <- function() {
    sg_rows <- function(group, subgroup, levels, est, p = 0.04) {
        data.frame(
            group = group,
            subgroup = subgroup,
            level = levels,
            n = c(600, 400),
            events = c(250, 150),
            est = est,
            low = est - 0.1,
            high = est + 0.1,
            p = c(NA, p),
            ref = c(TRUE, FALSE)
        )
    }
    overall <- data.frame(
        group = c("A", "B"),
        subgroup = "Overall",
        level = "",
        n = 1000,
        events = 400,
        est = 0.8,
        low = 0.7,
        high = 0.9,
        p = NA_real_,
        ref = FALSE
    )
    rbind(
        overall,
        sg_rows("A", "Sex", c("Male", "Female"), c(0.8, 0.9), p = 0.0497),
        transform(overall[2, ], subgroup = "Sex", level = "Female", ref = TRUE),
        sg_rows("A", "Smoking", c("Never", "Ever"), c(0.8, 0.7), p = 0.3),
        sg_rows("B", "Smoking", c("Never", "Ever"), c(0.75, 0.85), p = 0.0004)
    )
}

# subgroup_forest_layout() with every optional column
full_layout <- function(data = fake_forest_data(), ...) {
    subgroup_forest_layout(
        data,
        effect_size = "est",
        ci_low = "low",
        ci_high = "high",
        group_col = "group",
        n_col = "n",
        events_col = "events",
        pvalue_col = "p",
        ref_col = "ref",
        ...
    )
}

test_that("subgroup_forest_layout gives every group a row in every level", {
    lay <- full_layout()
    d <- lay$rows
    expect_true(lay$has_group && lay$has_counts && lay$has_pvalue)
    # Overall, Sex x2, Smoking x2 levels, times 2 groups, in order of appearance
    expect_equal(
        paste(d$subgroup, d$level),
        rep(
            c(
                "Overall ",
                "Sex Male",
                "Sex Female",
                "Smoking Never",
                "Smoking Ever"
            ),
            each = 2
        )
    )
    expect_equal(as.character(d$.group), rep(c("A", "B"), 5))
    # B has no male estimate: an empty row with dashes, not a made-up result
    b_male <- d[d$.group == "B" & d$level == "Male", ]
    expect_true(is.na(b_male$.est))
    expect_equal(b_male$events_label, "—")
    expect_equal(b_male$n_label, "")
    expect_equal(b_male$est_label, "—")
    expect_equal(b_male$p_label, "")
})

test_that("subgroup_forest_layout orders by factor levels when given factors", {
    data <- fake_forest_data()
    data$subgroup <- factor(
        data$subgroup,
        levels = c("Overall", "Smoking", "Sex")
    )
    data$level <- factor(
        data$level,
        levels = c("", "Ever", "Never", "Female", "Male")
    )
    data$group <- factor(data$group, levels = c("B", "A"))
    d <- full_layout(data)$rows
    expect_equal(
        unique(paste(d$subgroup, d$level)),
        c(
            "Overall ",
            "Smoking Ever",
            "Smoking Never",
            "Sex Female",
            "Sex Male"
        )
    )
    expect_equal(as.character(d$.group[1:2]), c("B", "A"))
})

test_that("subgroup_forest_layout places rows top to bottom with headings and gaps", {
    lay <- full_layout(gap_level = 0.2, gap_subgroup = 0.4)
    # Overall rows at 1-2; gap_subgroup, Sex heading at 3.4, level gap, Male
    # rows; level gap, Female rows; gap_subgroup, Smoking heading at 9.2, ...
    expect_equal(
        lay$rows$y,
        -c(1, 2, 4.6, 5.6, 6.8, 7.8, 10.4, 11.4, 12.6, 13.6)
    )
    expect_equal(lay$headings$label, c("Sex", "Smoking"))
    expect_equal(lay$headings$y, -c(3.4, 9.2))
    # one band per level, shading alternating within each subgroup
    expect_equal(lay$bands$shade, c(TRUE, TRUE, FALSE, TRUE, FALSE))
    expect_equal(lay$bands$ymax[2], -4.6 + 0.6)
})

test_that("subgroup_forest_layout formats the text columns", {
    d <- full_layout()$rows
    overall <- d[d$subgroup == "Overall" & d$.group == "A", ]
    expect_equal(overall$level_label, "Overall")
    expect_true(overall$level_bold)
    expect_equal(overall$events_label, "400")
    expect_equal(overall$n_label, "/ 1,000")
    expect_equal(overall$est_label, "0.80 (0.70–0.90)")
    expect_equal(overall$p_label, "")
    # the level label is on the first group row of each level only
    expect_equal(d$level_label[d$level == "Male"], c("Male", ""))
    expect_false(any(d$level_bold[d$subgroup != "Overall"]))
    # estimate_digits
    d3 <- full_layout(estimate_digits = 3)$rows
    expect_equal(d3$est_label[1], "0.800 (0.700–0.900)")
})

test_that("subgroup_forest_layout shows per-group interaction p-values", {
    d <- full_layout()$rows
    p_of <- function(group, level) d$p_label[d$.group == group & d$level == level]
    sig_of <- function(group, level) d$p_sig[d$.group == group & d$level == level]
    expect_equal(p_of("A", "Male"), "Ref.")
    expect_equal(p_of("A", "Female"), "0.050")
    # bold from the raw p (0.0497), not the rounded label
    expect_true(sig_of("A", "Female"))
    expect_equal(p_of("A", "Ever"), "0.30")
    expect_false(sig_of("A", "Ever"))
    expect_equal(p_of("B", "Ever"), "<0.001")
    expect_true(sig_of("B", "Ever"))
    # B has one sex level (no interaction to test): no reference label or p
    expect_equal(p_of("B", "Female"), "")
    expect_false(sig_of("B", "Female"))
    # custom reference label and significance threshold
    d2 <- full_layout(ref_label = "Reference", pvalue_threshold = 0.01)$rows
    expect_equal(d2$p_label[d2$.group == "A" & d2$level == "Male"], "Reference")
    expect_false(d2$p_sig[d2$.group == "A" & d2$level == "Female"])
})

test_that("subgroup_forest_layout handles a single group and optional columns", {
    data <- fake_forest_data()
    data <- data[data$group == "A", c("subgroup", "level", "est", "low", "high")]
    lay <- subgroup_forest_layout(data, "est", "low", "high")
    expect_false(lay$has_group || lay$has_counts || lay$has_pvalue)
    # one row per level, every row labelled
    expect_equal(nrow(lay$rows), 5)
    expect_equal(
        lay$rows$level_label,
        c("Overall", "Male", "Female", "Never", "Ever")
    )
    expect_equal(lay$rows$y, -c(1, 3.6, 4.8, 7.4, 8.6))
})

test_that("subgroup_forest_layout shows N alone without events_col", {
    data <- fake_forest_data()
    d <- subgroup_forest_layout(
        data,
        "est",
        "low",
        "high",
        group_col = "group",
        n_col = "n"
    )$rows
    expect_equal(d$events_label[1], "1,000")
    expect_equal(d$n_label[1], "")
    expect_equal(d$events_label[d$.group == "B" & d$level == "Male"], "—")
})

test_that("subgroup_forest_layout validates its input", {
    data <- fake_forest_data()
    expect_error(
        subgroup_forest_layout(data, "est", "low", "nope"),
        "missing column\\(s\\): nope"
    )
    expect_error(
        subgroup_forest_layout(data, "est", "low", "group"),
        "must be numeric: group"
    )
    expect_error(
        subgroup_forest_layout(data, "est", "low", "high"),
        "more than one row"
    )
    expect_error(
        subgroup_forest_layout(
            data,
            "est",
            "low",
            "high",
            group_col = "group",
            ref_col = "n"
        ),
        "ref_col column must be logical"
    )
    expect_error(
        subgroup_forest_layout(
            data,
            "est",
            "low",
            "high",
            group_col = "group",
            events_col = "events"
        ),
        "events_col needs n_col"
    )

    na_subgroup <- data
    na_subgroup$subgroup[2] <- NA
    expect_error(
        full_layout(na_subgroup),
        "subgroup column must not contain NA"
    )
    na_group <- data
    na_group$group[2] <- NA
    expect_error(full_layout(na_group), "group_col column must not contain NA")
    na_ci <- data
    na_ci$low[2] <- NA
    expect_error(full_layout(na_ci), "need non-NA ci_low and ci_high")
    # a missing estimate may have missing CIs
    no_est <- data
    no_est[2, c("est", "low", "high")] <- NA
    expect_no_error(full_layout(no_est))
})

test_that("subgroup_forest_size fits the columns that are drawn", {
    data <- fake_forest_data()
    args <- list(
        data,
        effect_size = "est",
        ci_low = "low",
        ci_high = "high",
        group_col = "group",
        n_col = "n",
        events_col = "events",
        pvalue_col = "p",
        ref_col = "ref"
    )
    size <- do.call(
        subgroup_forest_size,
        c(args, row_height = 0.1, axis_height = 0.5)
    )
    spec <- do.call(subgroup_forest_spec, args)
    expect_equal(size[["width"]], sum(spec$widths))
    expect_equal(size[["height"]], diff(spec$y$y_lim) * 0.1 + 0.5)
    # text columns left to right
    expect_true(all(diff(spec$x[c("subgroup", "group", "counts")]) > 0))
    expect_gt(spec$x[["pvalue"]], spec$x[["estimate"]])
    # dropping the optional columns makes it narrower; a wider forest panel or
    # larger text makes it wider; arguments only for the plot are ignored
    single <- data[data$group == "A", ]
    expect_lt(
        subgroup_forest_size(single, "est", "low", "high")[["width"]],
        size[["width"]]
    )
    expect_equal(
        do.call(subgroup_forest_size, c(args, forest_width = 3))[["width"]] -
            do.call(subgroup_forest_size, c(args, forest_width = 2))[["width"]],
        1
    )
    expect_gt(
        do.call(subgroup_forest_size, c(args, text_size = 10))[["width"]],
        do.call(subgroup_forest_size, args)[["width"]]
    )
    expect_equal(
        do.call(
            subgroup_forest_size,
            c(args, group_colors = "red", log_scale = TRUE)
        ),
        do.call(subgroup_forest_size, args)
    )
    expect_error(
        do.call(subgroup_forest_size, c(args, list(labels = c(foo = "x")))),
        "Unknown labels: foo"
    )
})

test_that("text_width_in measures text and leaves the graphics device alone", {
    dev <- grDevices::dev.cur()
    expect_equal(text_width_in(c(NA, ""), 7), 0)
    expect_gt(text_width_in("abcdef", 7), text_width_in("abc", 7))
    expect_gt(text_width_in("abc", 7, bold = TRUE), text_width_in("abc", 7))
    # PDF families are measured on the PDF device too
    expect_gt(
        text_width_in("abcdef", 7, family = "mono"),
        text_width_in("iiiiii", 7, family = "mono") - 1e-9
    )
    expect_equal(grDevices::dev.cur(), dev)
})

test_that("subgroup_forest_scale names values by group", {
    expect_equal(
        subgroup_forest_scale(NULL, c("A", "B"), c("red", "blue", "green"), "x"),
        c(A = "red", B = "blue")
    )
    expect_equal(
        subgroup_forest_scale(c(B = 2, A = 1, C = 3), c("A", "B"), 1:2, "x"),
        c(A = 1, B = 2)
    )
    expect_error(
        subgroup_forest_scale("red", c("A", "B"), 1:2, "cols"),
        "cols needs a value"
    )
    expect_error(
        subgroup_forest_scale(c(A = 1), c("A", "B"), 1:2, "cols"),
        "no value for: B"
    )
})

test_that("plot_subgroup_forest draws one or several groups", {
    data <- fake_forest_data()
    p <- plot_subgroup_forest(
        data,
        effect_size = "est",
        ci_low = "low",
        ci_high = "high",
        group_col = "group",
        n_col = "n",
        events_col = "events",
        pvalue_col = "p",
        ref_col = "ref",
        group_colors = c(A = "blue", B = "green")
    )
    expect_s3_class(p, "patchwork")
    expect_length(p$patches$plots, 2)
    # the forest panel draws only the estimated rows (no point for B male)
    expect_equal(nrow(p$patches$plots[[2]]$data), sum(!is.na(data$est)))
    path <- tempfile(fileext = ".png")
    on.exit(unlink(path))
    expect_no_error(ggplot2::ggsave(path, p, width = 7, height = 3))

    single <- data[data$group == "A", ]
    p1 <- plot_subgroup_forest(single, "est", "low", "high")
    expect_no_error(ggplot2::ggsave(path, p1, width = 5, height = 2))
    # default axis spans every CI and the null
    x_scale <- p1$patches$plots[[2]]$scales$get_scales("x")
    expect_true(all(c(0.6, 1) %in% x_scale$breaks))
})

test_that("plot_subgroup_forest supports a log scale and other null values", {
    data <- fake_forest_data()
    single <- data[data$group == "A", ]
    p_log <- plot_subgroup_forest(
        single,
        "est",
        "low",
        "high",
        log_scale = TRUE,
        x_limits = c(0.5, 2),
        x_breaks = c(0.5, 1, 2)
    )
    x_scale <- p_log$patches$plots[[2]]$scales$get_scales("x")
    expect_match(x_scale$trans$name, "^log")
    path <- tempfile(fileext = ".png")
    on.exit(unlink(path))
    expect_no_error(ggplot2::ggsave(path, p_log, width = 5, height = 2))
    expect_error(
        plot_subgroup_forest(
            single,
            "est",
            "low",
            "high",
            log_scale = TRUE,
            null_value = 0
        ),
        "positive null_value"
    )
    zero_ci <- single
    zero_ci$low[1] <- 0
    expect_error(
        plot_subgroup_forest(zero_ci, "est", "low", "high", log_scale = TRUE),
        "positive effect_size, ci_low and ci_high"
    )

    # differences: null at 0, default axis spans it
    diffs <- transform(single, est = est - 1, low = low - 1, high = high - 1)
    p0 <- plot_subgroup_forest(diffs, "est", "low", "high", null_value = 0)
    x_scale <- p0$patches$plots[[2]]$scales$get_scales("x")
    expect_true(0 %in% x_scale$breaks)
})

test_that("plot_subgroup_forest warns about and cuts CIs beyond the axis", {
    data <- fake_forest_data()
    expect_warning(
        p <- plot_subgroup_forest(
            data,
            "est",
            "low",
            "high",
            group_col = "group",
            x_limits = c(0.75, 1.25)
        ),
        "beyond the x-axis limits"
    )
    # nothing in the forest panel is drawn beyond the limits
    built <- ggplot2::ggplot_build(p$patches$plots[[2]])
    xs <- unlist(lapply(built$data, function(l) c(l$x, l$xend)))
    xs <- xs[is.finite(xs)]
    expect_true(all(xs >= 0.75 - 1e-8 & xs <= 1.25 + 1e-8))
})

test_that("subgroup_forest_ci_parts draws cut CI ends as arrows, not caps", {
    d <- data.frame(
        y = 1:5,
        .group = factor(rep("A", 5)),
        .est = c(1, 2, 0.4, 5, 0.1),
        .low = c(0.8, 1.5, 0.2, 4.5, 0.05),
        .high = c(1.2, 6, 0.9, 6, 0.2)
    )
    # 1: inside; 2: past the upper limit; 3: past the lower limit;
    # 4, 5: wholly above / below the limits
    parts <- subgroup_forest_ci_parts(d, c(0.3, 4), identity, identity)
    expect_equal(parts$lines$y, 1)
    # caps only on the ends within the limits
    expect_equal(sort(parts$caps$x), sort(c(0.8, 1.2, 1.5, 0.9)))
    arrows <- parts$arrows[order(parts$arrows$y), ]
    expect_equal(arrows$y, 2:5)
    # each arrow ends at the limit it runs past
    expect_equal(arrows$xend, c(4, 0.3, 4, 0.3))
    expect_equal(arrows$x[1:2], c(1.5, 0.9))
    # estimates beyond the limits are dropped, not moved onto the edge
    expect_equal(parts$points$y, 1:3)
})

test_that("subgroup_forest_ci_parts arrows both ends of a CI past both limits", {
    d <- data.frame(
        y = 1,
        .group = factor("A"),
        .est = 1,
        .low = 0.1,
        .high = 10
    )
    parts <- subgroup_forest_ci_parts(d, c(0.5, 2), log, exp)
    expect_equal(nrow(parts$lines), 0)
    expect_equal(nrow(parts$caps), 0)
    expect_equal(sort(parts$arrows$xend), c(0.5, 2))
    # arrows start at the middle of the (log) axis
    expect_equal(parts$arrows$x, rep(1, 2))
    expect_equal(parts$points$y, 1)
})

test_that("subgroup_forest_layout keeps counts when the estimate is missing", {
    data <- fake_forest_data()
    # e.g. no events in a level, so no hazard ratio
    data[data$group == "A" & data$level == "Male", c("est", "low", "high")] <- NA
    data$events[data$group == "A" & data$level == "Male"] <- 0
    d <- full_layout(data)$rows
    a_male <- d[d$.group == "A" & d$level == "Male", ]
    expect_equal(a_male$events_label, "0")
    expect_equal(a_male$n_label, "/ 600")
    expect_equal(a_male$est_label, "—")
})

test_that("plot_subgroup_forest has neutral default labels", {
    single <- fake_forest_data()
    single <- single[single$group == "A", ]
    p <- plot_subgroup_forest(single, "est", "low", "high")
    forest <- p$patches$plots[[2]]
    expect_equal(forest$labels$x, "Estimate (95% CI)")
})

test_that("plot_subgroup_forest omits the null line outside x_limits", {
    single <- fake_forest_data()
    single <- single[single$group == "A", ]
    has_segment <- function(p) {
        any(vapply(
            p$patches$plots[[2]]$layers,
            # the null line (CIs are segments too, but colored by group)
            function(l) {
                inherits(l$geom, "GeomSegment") &&
                    identical(l$aes_params$colour, "gray30")
            },
            TRUE
        ))
    }
    expect_true(has_segment(plot_subgroup_forest(single, "est", "low", "high")))
    p <- suppressWarnings(
        plot_subgroup_forest(single, "est", "low", "high", x_limits = c(0.5, 0.95))
    )
    expect_false(has_segment(p))
})

test_that("plot_subgroup_forest recycles the default colors past 8 groups", {
    data <- fake_forest_data()
    data <- data[data$group == "A", ]
    many <- do.call(
        rbind,
        lapply(letters[1:9], function(g) transform(data, group = g))
    )
    expect_no_error(
        p <- plot_subgroup_forest(many, "est", "low", "high", group_col = "group")
    )
    expect_s3_class(p, "patchwork")
})

test_that("subgroup_forest_layout writes negative CIs with 'to'", {
    data <- fake_forest_data()
    data[c("est", "low", "high")] <- data[c("est", "low", "high")] - 1
    d <- full_layout(data)$rows
    expect_equal(d$est_label[1], "-0.20 (-0.30 to -0.10)")
})

test_that("subgroup_forest_size scales row and axis heights with text_size", {
    data <- fake_forest_data()
    data <- data[data$group == "A", ]
    size_at <- function(ts) {
        subgroup_forest_size(data, "est", "low", "high", text_size = ts)
    }
    expect_equal(size_at(14)[["height"]], 2 * size_at(7)[["height"]])
})

test_that("plot_subgroup_forest validates and uses font_family", {
    data <- fake_forest_data()
    data <- data[data$group == "A", ]
    expect_error(
        plot_subgroup_forest(data, "est", "low", "high", font_family = NA),
        "font_family must be a single string"
    )
    p <- plot_subgroup_forest(data, "est", "low", "high", font_family = "serif")
    text_layer <- p$patches$plots[[1]]$layers[[4]]
    expect_equal(text_layer$aes_params$family, "serif")
})

test_that("text_width_in measures non-PDF families with systemfonts", {
    skip_if_not_installed("systemfonts", "1.2.4")
    family <- systemfonts::system_fonts()$family[1]
    skip_if(family %in% names(grDevices::pdfFonts()))
    one_line <- text_width_in("abcdef", 8, family = family)
    expect_gt(one_line, 0)
    # widest line of a multi-line string, not the sum of its lines
    expect_equal(text_width_in("abc\nabcdef", 8, family = family), one_line)
    expect_warning(
        text_width_in("abc", 8, family = "No Such Font Family xyz"),
        "not found"
    )
})
