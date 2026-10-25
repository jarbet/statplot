#' Table-style subgroup forest plot
#'
#' One stacked forest plot with aligned text columns, like a journal table:
#' Subgroup | Group | Events / N | forest | Estimate (95% CI) | P for
#' interaction. Each subgroup variable gets a bold heading row, then each of its
#' levels a block of rows, one per group (e.g. cohort or study), on alternating
#' gray/white background bands. Without `group_col` there is no Group column and
#' each level is one row. The Events / N and P for interaction columns are drawn
#' only when their columns are supplied.
#'
#' Every group gets a row in every level (in group order), so the groups line
#' up from level to level; a group with no estimate for a level keeps an empty
#' row shown with a dash. The interaction p-value is per group: `ref_label` on
#' the reference level, blank where the group has only one level of the subgroup
#' (no interaction to test), bold where p < `pvalue_threshold`.
#'
#' The plot is built as three ggplots (left text, forest, right text) on the same
#' y scale, joined with patchwork. Column widths are measured from the text, in
#' inches, so draw the plot at the size [subgroup_forest_size()] returns: at a
#' narrower width the text columns overlap, at a wider one they spread apart.
#'
#' @param data A data.frame or tibble with one row per subgroup level (per group).
#' @param effect_size String name of the estimate column (e.g. a hazard ratio).
#' @param ci_low String name of the lower confidence interval column.
#' @param ci_high String name of the upper confidence interval column.
#' @param subgroup String name of the subgroup variable column (e.g. values
#'   `"Age"`, `"Sex"`). Default `"subgroup"`.
#' @param level String name of the subgroup level column (e.g. values `"<65"`,
#'   `"65+"`). A level of `""` or `NA` marks a subgroup without levels, such as
#'   an overall estimate: it gets no heading, and its subgroup name is its (bold)
#'   row label. Default `"level"`. Subgroups and their levels are drawn in the
#'   order of their factor levels or, for character columns, the order they
#'   first appear in `data`.
#' @param group_col Optional string name of a column identifying the group (e.g.
#'   cohort), drawn in its factor-level order or order of appearance. `NULL`
#'   (default) for a single group: no Group column, one row per level.
#' @param n_col,events_col Optional string names of the columns with the number
#'   of participants and of events. With `n_col` there is an N column, and with
#'   both an Events / N column. Default `NULL` (no column).
#' @param pvalue_col Optional string name of the interaction p-value column, for
#'   the P for interaction column. Default `NULL` (no column).
#' @param ref_col Optional string name of a logical column, `TRUE` on each
#'   subgroup's reference level, shown as `ref_label` in the P for interaction
#'   column. Default `NULL` (no reference labels).
#' @param group_colors,group_shapes Point and text color, and point shape (a
#'   filled shape, 21-25), of each group: named by group, or unnamed in group
#'   order. Default: Okabe-Ito colors and shapes `c(21, 24, 22, 25, 23)` in group
#'   order; black circles for a single group.
#' @param null_value Numeric. Position of the vertical "no effect" line. Default
#'   `1` (ratios); use `0` for differences.
#' @param log_scale Logical. Whether to draw the estimate axis on a log scale
#'   (common for ratios). Default `FALSE`.
#' @param x_limits,x_breaks,x_labels Estimate axis limits, breaks and break
#'   labels. Default: limits spanning every CI and `null_value` plus 5% on each
#'   side, breaks from [pretty()] (or, with `log_scale` across at least a
#'   tenfold range, [scales::breaks_log()]), labels from the breaks. A CI beyond the limits is cut at the limit, with a
#'   warning.
#' @param estimate_digits Integer. Decimal places of the estimate and its CI in
#'   the text column. Default `2`.
#' @param pvalue_threshold Numeric. Interaction p-values below this are bold.
#'   Default `0.05`.
#' @param ref_label Character. Label of the reference levels in the P for
#'   interaction column. Default `"Ref."`.
#' @param labels Named character vector replacing any of the default column
#'   headers and axis title: `subgroup` (`"Subgroup"`), `group` (`"Group"`),
#'   `counts` (`"Events / N"`, or `"N"` without `events_col`), `forest`
#'   (`"Hazard Ratio"`), `estimate` (`"HR (95% CI)"`), `pvalue`
#'   (`"P for\nInteraction"`), `x_axis` (`"Hazard Ratio (95% CI)"`).
#' @param text_size Numeric. Text size, in points. Default `7`.
#' @param forest_width Numeric. Width of the forest panel, in inches. Default
#'   `1.8`.
#' @param gap_level Numeric. Vertical space between the levels of a subgroup, in
#'   row heights. Default `0.2`.
#' @param gap_subgroup Numeric. Extra vertical space before each subgroup
#'   heading, in row heights. Default `0.4`.
#' @param band_fill,heading_fill Fill colors of the shaded level bands and of the
#'   subgroup heading rows. Defaults `"gray95"` and `"gray84"`.
#'
#' @return A `patchwork` object.
#' @seealso [subgroup_forest_size()] for the figure size to draw it at, and
#'   [plot_confidence_intervals()] for a plain dot-and-whisker plot.
#' @export
#'
#' @examples
#' df <- data.frame(
#'     study = rep(c("Study A", "Study B"), each = 5),
#'     subgroup = rep(c("Overall", "Age", "Age", "Sex", "Sex"), 2),
#'     level = rep(c("", "<65", "65+", "Male", "Female"), 2),
#'     n = c(1000, 600, 400, 450, 550, 800, 500, 300, 380, 420),
#'     events = c(300, 120, 180, 150, 150, 240, 90, 150, 120, 120),
#'     hr = c(0.80, 0.75, 0.85, 0.78, 0.82, 0.90, 0.85, 0.95, 0.88, 0.92),
#'     lcl = c(0.70, 0.60, 0.72, 0.65, 0.68, 0.76, 0.66, 0.78, 0.71, 0.75),
#'     ucl = c(0.92, 0.94, 1.01, 0.94, 0.99, 1.07, 1.10, 1.16, 1.09, 1.13),
#'     p_int = c(NA, NA, 0.30, NA, 0.70, NA, NA, 0.40, NA, 0.80),
#'     is_ref = rep(c(FALSE, TRUE, FALSE, TRUE, FALSE), 2)
#' )
#'
#' # several cohorts, with the P for interaction column
#' p <- plot_subgroup_forest(
#'     df,
#'     effect_size = "hr",
#'     ci_low = "lcl",
#'     ci_high = "ucl",
#'     group_col = "study",
#'     n_col = "n",
#'     events_col = "events",
#'     pvalue_col = "p_int",
#'     ref_col = "is_ref",
#'     labels = c(group = "Study")
#' )
#' p
#' size <- subgroup_forest_size(
#'     df,
#'     effect_size = "hr",
#'     ci_low = "lcl",
#'     ci_high = "ucl",
#'     group_col = "study",
#'     n_col = "n",
#'     events_col = "events",
#'     pvalue_col = "p_int",
#'     ref_col = "is_ref",
#'     labels = c(group = "Study")
#' )
#' # draw it at that size, e.g.
#' # ggplot2::ggsave("forest.png", p, width = size[["width"]],
#' #     height = size[["height"]])
#'
#' # a single cohort, with the P for interaction column
#' study_a <- df[df$study == "Study A", ]
#' plot_subgroup_forest(
#'     study_a,
#'     effect_size = "hr",
#'     ci_low = "lcl",
#'     ci_high = "ucl",
#'     n_col = "n",
#'     events_col = "events",
#'     pvalue_col = "p_int",
#'     ref_col = "is_ref"
#' )
#'
#' # a single cohort without the P for interaction column: odds ratios on a
#' # log scale, no optional columns
#' plot_subgroup_forest(
#'     study_a,
#'     effect_size = "hr",
#'     ci_low = "lcl",
#'     ci_high = "ucl",
#'     log_scale = TRUE,
#'     labels = c(
#'         forest = "Odds Ratio",
#'         estimate = "OR (95% CI)",
#'         x_axis = "Odds Ratio (95% CI)"
#'     )
#' )
plot_subgroup_forest <- function(
    data,
    effect_size,
    ci_low,
    ci_high,
    subgroup = "subgroup",
    level = "level",
    group_col = NULL,
    n_col = NULL,
    events_col = NULL,
    pvalue_col = NULL,
    ref_col = NULL,
    group_colors = NULL,
    group_shapes = NULL,
    null_value = 1,
    log_scale = FALSE,
    x_limits = NULL,
    x_breaks = NULL,
    x_labels = NULL,
    estimate_digits = 2,
    pvalue_threshold = 0.05,
    ref_label = "Ref.",
    labels = NULL,
    text_size = 7,
    forest_width = 1.8,
    gap_level = 0.2,
    gap_subgroup = 0.4,
    band_fill = "gray95",
    heading_fill = "gray84"
) {
    stopifnot(
        "null_value must be a single number" = is.numeric(null_value) &&
            length(null_value) == 1 &&
            !is.na(null_value),
        "log_scale must be TRUE or FALSE" = is.logical(log_scale) &&
            length(log_scale) == 1 &&
            !is.na(log_scale),
        "x_limits must be NULL or 2 increasing numbers" = is.null(x_limits) ||
            (is.numeric(x_limits) &&
                length(x_limits) == 2 &&
                !anyNA(x_limits) &&
                x_limits[1] < x_limits[2]),
        "x_breaks must be NULL or numeric" = is.null(x_breaks) ||
            is.numeric(x_breaks),
        "x_labels must be NULL or the same length as x_breaks" = is.null(
            x_labels
        ) ||
            (!is.null(x_breaks) && length(x_labels) == length(x_breaks)),
        "band_fill must be a single string" = is.character(band_fill) &&
            length(band_fill) == 1,
        "heading_fill must be a single string" = is.character(heading_fill) &&
            length(heading_fill) == 1
    )
    if (log_scale) {
        stopifnot(
            "log_scale needs a positive null_value" = null_value > 0,
            "log_scale needs positive x_limits" = is.null(x_limits) ||
                all(x_limits > 0)
        )
    }
    spec <- subgroup_forest_spec(
        data,
        effect_size = effect_size,
        ci_low = ci_low,
        ci_high = ci_high,
        subgroup = subgroup,
        level = level,
        group_col = group_col,
        n_col = n_col,
        events_col = events_col,
        pvalue_col = pvalue_col,
        ref_col = ref_col,
        estimate_digits = estimate_digits,
        pvalue_threshold = pvalue_threshold,
        ref_label = ref_label,
        labels = labels,
        text_size = text_size,
        forest_width = forest_width,
        gap_level = gap_level,
        gap_subgroup = gap_subgroup
    )
    layout <- spec$layout
    d <- layout$rows
    bands <- layout$bands
    headings <- layout$headings
    lab <- spec$labels
    x_col <- spec$x
    widths <- spec$widths
    y <- spec$y

    groups <- levels(d$.group)
    group_colors <- subgroup_forest_scale(
        group_colors,
        groups,
        default = if (layout$has_group) {
            c(
                "#0072B2",
                "#D55E00",
                "#009E73",
                "#CC79A7",
                "#E69F00",
                "#56B4E9",
                "#F0E442",
                "#000000"
            )
        } else {
            "black"
        },
        name = "group_colors"
    )
    group_shapes <- subgroup_forest_scale(
        group_shapes,
        groups,
        default = rep_len(c(21, 24, 22, 25, 23), length(groups)),
        name = "group_shapes"
    )
    d$group_text_color <- ifelse(
        is.na(d$.est),
        "gray55",
        group_colors[as.character(d$.group)]
    )

    # estimate axis: by default spans every CI and the null, plus 5% on each side
    # (on the axis scale) so neither sits on the edge
    to_axis <- if (log_scale) log else identity
    from_axis <- if (log_scale) exp else identity
    if (is.null(x_limits)) {
        lims <- to_axis(range(c(d$.low, d$.high, null_value), na.rm = TRUE))
        x_limits <- from_axis(lims + c(-1, 1) * 0.05 * diff(lims))
    }
    if (is.null(x_breaks)) {
        # log breaks (e.g. 0.1, 1, 10) only across at least a tenfold range;
        # narrower, they are uneven, and evenly spaced ones read better
        x_breaks <- if (log_scale && diff(log10(x_limits)) >= 1) {
            scales::breaks_log(n = 5)(x_limits)
        } else {
            pretty(x_limits, n = 4)
        }
    }
    keep <- x_breaks >= x_limits[1] & x_breaks <= x_limits[2]
    if (is.null(x_labels)) {
        x_labels <- if (log_scale) {
            format(x_breaks, trim = TRUE, drop0trailing = TRUE)
        } else {
            sprintf(paste0("%.", estimate_digits, "f"), x_breaks)
        }
    }
    x_breaks <- x_breaks[keep]
    x_labels <- x_labels[keep]

    d_est <- d[!is.na(d$.est), ]
    if (any(d_est$.low < x_limits[1] | d_est$.high > x_limits[2])) {
        warning(
            "Some CIs extend beyond the x-axis limits ",
            x_limits[1],
            "-",
            x_limits[2],
            "; they are cut at the limits."
        )
    }
    d_est$.low <- pmax(d_est$.low, x_limits[1])
    d_est$.high <- pmin(d_est$.high, x_limits[2])
    d_est$.est <- pmin(pmax(d_est$.est, x_limits[1]), x_limits[2])

    # shared y scale, background bands, heading rows and rules: identical in all
    # three panels, so the rows and bands continue across the column gaps
    base_layers <- function(x_lim) {
        list(
            ggplot2::geom_rect(
                data = bands[bands$shade, ],
                mapping = ggplot2::aes(ymin = ymin, ymax = ymax),
                xmin = -Inf,
                xmax = Inf,
                fill = band_fill,
                inherit.aes = FALSE
            ),
            ggplot2::geom_rect(
                data = headings,
                mapping = ggplot2::aes(ymin = y - 0.5, ymax = y + 0.5),
                xmin = -Inf,
                xmax = Inf,
                fill = heading_fill,
                inherit.aes = FALSE
            ),
            ggplot2::geom_hline(yintercept = y$y_rules, linewidth = 0.4),
            ggplot2::coord_cartesian(
                xlim = x_lim,
                ylim = y$y_lim,
                expand = FALSE,
                clip = "off"
            )
        )
    }
    text <- function(...) {
        ggplot2::geom_text(..., size = text_size, size.unit = "pt")
    }
    header <- function(x, label, hjust = 0) {
        ggplot2::annotate(
            "text",
            x = x,
            y = y$y_header,
            label = label,
            hjust = hjust,
            fontface = "bold",
            size = text_size,
            size.unit = "pt",
            lineheight = 0.9
        )
    }
    # explicit white backgrounds, so the unshaded levels are white whatever the
    # device's background (theme_void() has none)
    white <- ggplot2::element_rect(fill = "white", colour = NA)
    text_theme <- ggplot2::theme_void() +
        ggplot2::theme(
            plot.margin = ggplot2::margin(5, 0, 5, 0),
            plot.background = white
        )

    left <- ggplot2::ggplot(d, ggplot2::aes(y = y)) +
        base_layers(c(0, widths[["left"]])) +
        text(
            data = headings,
            mapping = ggplot2::aes(x = x_col[["subgroup"]], label = label),
            hjust = 0,
            fontface = "bold"
        ) +
        text(
            mapping = ggplot2::aes(
                x = ifelse(
                    level_bold,
                    x_col[["subgroup"]],
                    x_col[["level"]]
                ),
                label = level_label,
                fontface = ifelse(level_bold, "bold", "plain")
            ),
            hjust = 0
        ) +
        header(x_col[["subgroup"]], lab[["subgroup"]]) +
        text_theme
    if (layout$has_group) {
        left <- left +
            text(
                mapping = ggplot2::aes(
                    x = x_col[["group"]],
                    label = .group,
                    color = I(group_text_color)
                ),
                hjust = 0
            ) +
            header(x_col[["group"]], lab[["group"]])
    }
    if (layout$has_counts) {
        # events right-aligned and "/ N" left-aligned on either side of a fixed
        # point, so the column lines up on the slash
        left <- left +
            text(
                mapping = ggplot2::aes(
                    x = x_col[["counts"]] - 0.015,
                    label = events_label
                ),
                hjust = 1
            ) +
            text(
                mapping = ggplot2::aes(
                    x = x_col[["counts"]] + 0.015,
                    label = n_label
                ),
                hjust = 0
            ) +
            header(x_col[["counts"]], lab[["counts"]], hjust = 0.5)
    }

    forest_header_x <- from_axis(mean(to_axis(x_limits)))
    forest <- ggplot2::ggplot(d_est, ggplot2::aes(y = y)) +
        base_layers(x_limits) +
        # null line from the bottom of the rows to the header rule, not through
        # the header
        ggplot2::annotate(
            "segment",
            x = null_value,
            xend = null_value,
            y = -Inf,
            yend = y$y_top + 0.2,
            linewidth = 0.4,
            color = "gray30"
        ) +
        ggplot2::geom_errorbar(
            mapping = ggplot2::aes(xmin = .low, xmax = .high, color = .group),
            orientation = "y",
            width = 0.45,
            linewidth = 0.45
        ) +
        ggplot2::geom_point(
            mapping = ggplot2::aes(
                x = .est,
                color = .group,
                fill = .group,
                shape = .group
            ),
            size = 1.6,
            stroke = 0.4
        ) +
        ggplot2::scale_color_manual(values = group_colors, guide = "none") +
        ggplot2::scale_fill_manual(values = group_colors, guide = "none") +
        ggplot2::scale_shape_manual(values = group_shapes, guide = "none") +
        ggplot2::scale_x_continuous(
            breaks = x_breaks,
            labels = x_labels,
            transform = if (log_scale) "log" else "identity"
        ) +
        header(forest_header_x, lab[["forest"]], hjust = 0.5) +
        ggplot2::labs(x = lab[["x_axis"]], y = NULL) +
        ggplot2::theme_classic(base_size = text_size) +
        ggplot2::theme(
            axis.line.y = ggplot2::element_blank(),
            axis.ticks.y = ggplot2::element_blank(),
            axis.text.y = ggplot2::element_blank(),
            axis.text.x = ggplot2::element_text(
                size = text_size,
                color = "black"
            ),
            axis.title.x = ggplot2::element_text(size = text_size),
            plot.margin = ggplot2::margin(5, 0, 5, 0),
            plot.background = white,
            panel.background = white
        )

    right <- ggplot2::ggplot(d, ggplot2::aes(y = y)) +
        base_layers(c(0, widths[["right"]])) +
        text(
            mapping = ggplot2::aes(x = x_col[["estimate"]], label = est_label),
            hjust = 0
        ) +
        header(x_col[["estimate"]], lab[["estimate"]]) +
        text_theme
    if (layout$has_pvalue) {
        right <- right +
            text(
                mapping = ggplot2::aes(
                    x = x_col[["pvalue"]],
                    label = p_label,
                    fontface = ifelse(p_sig, "bold", "plain")
                ),
                hjust = 0.5
            ) +
            header(x_col[["pvalue"]], lab[["pvalue"]], hjust = 0.5)
    }

    patchwork::wrap_plots(
        left,
        forest,
        right,
        nrow = 1,
        widths = unname(widths)
    )
}

#' Figure size for a table-style subgroup forest plot
#'
#' The width and height, in inches, to draw [plot_subgroup_forest()] at: the
#' width fits its text columns and forest panel, and the height gives each row
#' `row_height`. In Quarto, compute it in an earlier chunk and pass it as
#' `fig-width: !expr size[["width"]]` and `fig-height: !expr size[["height"]]`
#' (chunk options are evaluated before the chunk's own code runs).
#'
#' @param data,effect_size,ci_low,ci_high As for [plot_subgroup_forest()].
#' @param ... Other arguments passed to [plot_subgroup_forest()]. Those that
#'   change the layout (`subgroup`, `level`, `group_col`, `n_col`, `events_col`,
#'   `pvalue_col`, `ref_col`, `estimate_digits`, `ref_label`, `labels`,
#'   `text_size`, `forest_width`, `gap_level`, `gap_subgroup`) must match; the
#'   others are ignored, so the same arguments can be passed to both functions.
#' @param row_height Numeric. Height of one row, in inches. Default `0.115`.
#' @param axis_height Numeric. Height of the estimate axis and the plot margins,
#'   in inches. Default `0.45`.
#'
#' @return Named numeric vector: `width` and `height`, in inches.
#' @export
#'
#' @examples
#' df <- data.frame(
#'     subgroup = c("Overall", "Age", "Age"),
#'     level = c("", "<65", "65+"),
#'     hr = c(0.80, 0.75, 0.85),
#'     lcl = c(0.70, 0.60, 0.72),
#'     ucl = c(0.92, 0.94, 1.01)
#' )
#' size <- subgroup_forest_size(df, "hr", "lcl", "ucl")
#' p <- plot_subgroup_forest(df, "hr", "lcl", "ucl")
#' # ggplot2::ggsave("forest.png", p, width = size[["width"]],
#' #     height = size[["height"]])
subgroup_forest_size <- function(
    data,
    effect_size,
    ci_low,
    ci_high,
    ...,
    row_height = 0.115,
    axis_height = 0.45
) {
    stopifnot(
        "row_height must be a single positive number" = is.numeric(
            row_height
        ) &&
            length(row_height) == 1 &&
            row_height > 0,
        "axis_height must be a single non-negative number" = is.numeric(
            axis_height
        ) &&
            length(axis_height) == 1 &&
            axis_height >= 0
    )
    spec <- subgroup_forest_spec(
        data,
        effect_size = effect_size,
        ci_low = ci_low,
        ci_high = ci_high,
        ...
    )
    c(
        width = sum(spec$widths),
        height = diff(spec$y$y_lim) * row_height + axis_height
    )
}

#' Rows, column positions and panel widths of a subgroup forest plot
#'
#' Shared by [plot_subgroup_forest()] and [subgroup_forest_size()], so the size
#' always matches the plot.
#'
#' @param data,effect_size,ci_low,ci_high,subgroup,level,group_col,n_col,events_col,pvalue_col,ref_col,estimate_digits,pvalue_threshold,ref_label,labels,text_size,forest_width,gap_level,gap_subgroup
#'   As for [plot_subgroup_forest()].
#' @param ... Ignored (other arguments of [plot_subgroup_forest()]).
#' @return A list: `layout` (from `subgroup_forest_layout()`), `labels` (column
#'   headers), `x` (x position of each text column within its panel, in inches),
#'   `widths` (`left`, `forest` and `right` panel widths, in inches) and `y`
#'   (from `subgroup_forest_y()`).
#' @noRd
subgroup_forest_spec <- function(
    data,
    effect_size,
    ci_low,
    ci_high,
    subgroup = "subgroup",
    level = "level",
    group_col = NULL,
    n_col = NULL,
    events_col = NULL,
    pvalue_col = NULL,
    ref_col = NULL,
    estimate_digits = 2,
    pvalue_threshold = 0.05,
    ref_label = "Ref.",
    labels = NULL,
    text_size = 7,
    forest_width = 1.8,
    gap_level = 0.2,
    gap_subgroup = 0.4,
    ...
) {
    stopifnot(
        "text_size must be a single positive number" = is.numeric(text_size) &&
            length(text_size) == 1 &&
            text_size > 0,
        "forest_width must be a single positive number" = is.numeric(
            forest_width
        ) &&
            length(forest_width) == 1 &&
            forest_width > 0,
        "labels must be NULL or a named character vector" = is.null(labels) ||
            (is.character(labels) && !is.null(names(labels)))
    )
    default_labels <- c(
        subgroup = "Subgroup",
        group = "Group",
        counts = if (is.null(events_col)) "N" else "Events / N",
        forest = "Hazard Ratio",
        estimate = "HR (95% CI)",
        pvalue = "P for\nInteraction",
        x_axis = "Hazard Ratio (95% CI)"
    )
    unknown <- setdiff(names(labels), names(default_labels))
    if (length(unknown) > 0) {
        stop("Unknown labels: ", paste(unknown, collapse = ", "))
    }
    lab <- default_labels
    lab[names(labels)] <- labels

    layout <- subgroup_forest_layout(
        data,
        effect_size = effect_size,
        ci_low = ci_low,
        ci_high = ci_high,
        subgroup = subgroup,
        level = level,
        group_col = group_col,
        n_col = n_col,
        events_col = events_col,
        pvalue_col = pvalue_col,
        ref_col = ref_col,
        estimate_digits = estimate_digits,
        pvalue_threshold = pvalue_threshold,
        ref_label = ref_label,
        gap_level = gap_level,
        gap_subgroup = gap_subgroup
    )
    d <- layout$rows
    tw <- function(x, bold = FALSE) text_width_in(x, text_size, bold)

    # text columns, left to right, each `gap` inches after the widest text of
    # the column before it
    gap <- 0.15
    indent <- 0.13
    x <- c(subgroup = 0.05, level = 0.05 + indent)
    end <- max(
        x[["subgroup"]] +
            tw(
                c(
                    lab[["subgroup"]],
                    layout$headings$label,
                    d$level_label[d$level_bold]
                ),
                bold = TRUE
            ),
        x[["level"]] + tw(d$level_label[!d$level_bold])
    )
    if (layout$has_group) {
        x[["group"]] <- end + gap
        end <- x[["group"]] +
            max(tw(levels(d$.group)), tw(lab[["group"]], bold = TRUE))
    }
    if (layout$has_counts) {
        # x is the slash, with the header centered on it
        header_half <- tw(lab[["counts"]], bold = TRUE) / 2
        x[["counts"]] <- end + gap + max(tw(d$events_label), header_half)
        end <- x[["counts"]] + max(tw(d$n_label), header_half)
    }
    left_width <- end + gap / 2

    x[["estimate"]] <- gap
    end <- x[["estimate"]] +
        max(tw(d$est_label), tw(lab[["estimate"]], bold = TRUE))
    if (layout$has_pvalue) {
        p_width <- max(
            tw(d$p_label, bold = TRUE),
            tw(lab[["pvalue"]], bold = TRUE)
        )
        x[["pvalue"]] <- end + gap + p_width / 2
        end <- end + gap + p_width
    }

    list(
        layout = layout,
        labels = lab,
        x = x,
        widths = c(
            left = left_width,
            forest = forest_width,
            right = end + 0.05
        ),
        y = subgroup_forest_y(layout)
    )
}

#' Rows of a table-style subgroup forest plot
#'
#' Lays out the estimates as the rows of [plot_subgroup_forest()]: a heading per
#' subgroup variable, then for each of its levels one row per group, with y
#' positions and the formatted text of each column.
#'
#' @param data,effect_size,ci_low,ci_high,subgroup,level,group_col,n_col,events_col,pvalue_col,ref_col,estimate_digits,pvalue_threshold,ref_label,gap_level,gap_subgroup
#'   As for [plot_subgroup_forest()].
#' @return A list: `rows` (one row per group x level, with the group in
#'   `.group`, the estimate and CI in `.est`, `.low`, `.high`, y position `y`
#'   and the display text columns `level_label`, `level_bold`, `events_label`,
#'   `n_label`, `est_label`, `p_label`, `p_sig`), `bands` (one row per level:
#'   `ymin`, `ymax`, `shade`, the alternating background band), `headings`
#'   (`label`, `y`), and `has_group`, `has_counts`, `has_pvalue` (which optional
#'   columns to draw).
#' @noRd
subgroup_forest_layout <- function(
    data,
    effect_size,
    ci_low,
    ci_high,
    subgroup = "subgroup",
    level = "level",
    group_col = NULL,
    n_col = NULL,
    events_col = NULL,
    pvalue_col = NULL,
    ref_col = NULL,
    estimate_digits = 2,
    pvalue_threshold = 0.05,
    ref_label = "Ref.",
    gap_level = 0.2,
    gap_subgroup = 0.4
) {
    is_name <- function(x) is.character(x) && length(x) == 1 && !is.na(x)
    is_opt_name <- function(x) is.null(x) || is_name(x)
    stopifnot(
        "data must be a data.frame" = is.data.frame(data),
        "data must have at least one row" = nrow(data) > 0,
        "effect_size must be a single column name" = is_name(effect_size),
        "ci_low must be a single column name" = is_name(ci_low),
        "ci_high must be a single column name" = is_name(ci_high),
        "subgroup must be a single column name" = is_name(subgroup),
        "level must be a single column name" = is_name(level),
        "group_col must be NULL or a single column name" = is_opt_name(
            group_col
        ),
        "n_col must be NULL or a single column name" = is_opt_name(n_col),
        "events_col must be NULL or a single column name" = is_opt_name(
            events_col
        ),
        "events_col needs n_col" = is.null(events_col) || !is.null(n_col),
        "pvalue_col must be NULL or a single column name" = is_opt_name(
            pvalue_col
        ),
        "ref_col must be NULL or a single column name" = is_opt_name(ref_col),
        "estimate_digits must be a single non-negative whole number" = is.numeric(
            estimate_digits
        ) &&
            length(estimate_digits) == 1 &&
            estimate_digits >= 0 &&
            estimate_digits == round(estimate_digits),
        "pvalue_threshold must be a single number" = is.numeric(
            pvalue_threshold
        ) &&
            length(pvalue_threshold) == 1,
        "ref_label must be a single string" = is_name(ref_label),
        "gap_level must be a single non-negative number" = is.numeric(
            gap_level
        ) &&
            length(gap_level) == 1 &&
            gap_level >= 0,
        "gap_subgroup must be a single non-negative number" = is.numeric(
            gap_subgroup
        ) &&
            length(gap_subgroup) == 1 &&
            gap_subgroup >= 0
    )
    cols <- c(
        effect_size,
        ci_low,
        ci_high,
        subgroup,
        level,
        group_col,
        n_col,
        events_col,
        pvalue_col,
        ref_col
    )
    missing_cols <- setdiff(cols, names(data))
    if (length(missing_cols) > 0) {
        stop(
            "data is missing column(s): ",
            paste(missing_cols, collapse = ", ")
        )
    }
    numeric_cols <- c(effect_size, ci_low, ci_high, n_col, events_col, pvalue_col)
    not_numeric <- numeric_cols[!vapply(data[numeric_cols], is.numeric, TRUE)]
    if (length(not_numeric) > 0) {
        stop(
            "column(s) must be numeric: ",
            paste(not_numeric, collapse = ", ")
        )
    }
    if (!is.null(ref_col) && !is.logical(data[[ref_col]])) {
        stop("ref_col column must be logical")
    }
    has_group <- !is.null(group_col)
    has_counts <- !is.null(n_col)
    has_pvalue <- !is.null(pvalue_col)

    raw <- as.data.frame(data)
    group <- if (has_group) raw[[group_col]] else rep("All", nrow(raw))
    sg_raw <- raw[[subgroup]]
    lv_raw <- raw[[level]]
    d <- data.frame(
        .group = if (is.factor(group)) {
            droplevels(group)
        } else {
            factor(group, levels = unique(group))
        },
        subgroup = as.character(sg_raw),
        level = as.character(lv_raw),
        .est = raw[[effect_size]],
        .low = raw[[ci_low]],
        .high = raw[[ci_high]],
        .n = if (has_counts) raw[[n_col]] else NA_real_,
        .events = if (!is.null(events_col)) raw[[events_col]] else NA_real_,
        .p = if (has_pvalue) raw[[pvalue_col]] else NA_real_,
        .ref = if (!is.null(ref_col)) raw[[ref_col]] else FALSE
    )
    d$level[is.na(d$level)] <- ""

    # display order: factor levels, or order of first appearance
    sg_order <- if (is.factor(sg_raw)) {
        levels(droplevels(sg_raw))
    } else {
        unique(d$subgroup)
    }
    key <- paste(d$subgroup, d$level, sep = "\r")
    lv_rank <- if (is.factor(lv_raw)) {
        match(d$level, levels(lv_raw), nomatch = 0)
    } else {
        match(key, unique(key))
    }
    row_info <- unique(data.frame(
        subgroup = d$subgroup,
        level = d$level,
        .key = key
    )[order(match(d$subgroup, sg_order), lv_rank), ])
    d$.key <- factor(key, levels = row_info$.key)
    row_info$.key <- factor(row_info$.key, levels = row_info$.key)
    if (anyDuplicated(d[c(".group", ".key")])) {
        stop("data has more than one row per group, subgroup and level")
    }

    # one row per group x level; a group without an estimate gets NA values
    grid <- expand.grid(
        .group = levels(d$.group),
        .key = levels(d$.key),
        stringsAsFactors = FALSE
    )
    grid$.group <- factor(grid$.group, levels = levels(d$.group))
    grid$.key <- factor(grid$.key, levels = levels(d$.key))
    d <- dplyr::left_join(
        grid,
        dplyr::select(d, -"subgroup", -"level"),
        by = c(".group", ".key")
    ) |>
        dplyr::left_join(row_info, by = ".key")
    d <- d[order(d$.key, d$.group), ]

    # y positions, top to bottom (negative so that later rows are lower down);
    # `cursor` is the height used so far
    has_heading <- tapply(row_info$level != "", row_info$subgroup, any)
    d$y <- NA_real_
    headings <- data.frame(label = character(), y = numeric())
    cursor <- 0
    for (i in seq_len(nrow(row_info))) {
        sg <- row_info$subgroup[i]
        new_subgroup <- i == 1 || sg != row_info$subgroup[i - 1]
        if (new_subgroup && i > 1) {
            cursor <- cursor + gap_subgroup
        }
        if (new_subgroup && has_heading[[sg]]) {
            cursor <- cursor + 1
            headings <- rbind(headings, data.frame(label = sg, y = -cursor))
        }
        if (i > 1 || has_heading[[sg]]) {
            cursor <- cursor + gap_level
        }
        idx <- which(d$.key == row_info$.key[i])
        d$y[idx] <- -(cursor + seq_along(idx))
        cursor <- cursor + length(idx)
    }

    # background band per level, alternating within each subgroup (first level
    # shaded)
    bands <- d |>
        dplyr::group_by(.data$subgroup, .data$.key) |>
        dplyr::summarise(
            ymin = min(.data$y) - 0.5 - gap_level / 2,
            ymax = max(.data$y) + 0.5 + gap_level / 2,
            .groups = "drop"
        ) |>
        dplyr::arrange(.data$.key) |>
        dplyr::group_by(.data$subgroup) |>
        dplyr::mutate(shade = seq_along(.data$.key) %% 2 == 1) |>
        dplyr::ungroup() |>
        as.data.frame()

    missing <- is.na(d$.est)
    first_of_level <- !duplicated(d$.key)
    d$level_label <- ifelse(
        first_of_level,
        ifelse(d$level == "", d$subgroup, d$level),
        ""
    )
    d$level_bold <- d$level == ""
    fmt <- paste0("%.", estimate_digits, "f")
    d$est_label <- ifelse(
        missing,
        "—",
        sprintf(
            paste0(fmt, " (", fmt, "–", fmt, ")"),
            d$.est,
            d$.low,
            d$.high
        )
    )
    count_text <- function(x) format(round(x), big.mark = ",", trim = TRUE)
    if (!has_counts) {
        d$events_label <- ""
        d$n_label <- ""
    } else if (is.null(events_col)) {
        # N only: right-aligned, like the events in Events / N
        d$events_label <- ifelse(missing, "—", count_text(d$.n))
        d$n_label <- ""
    } else {
        d$events_label <- ifelse(missing, "—", count_text(d$.events))
        d$n_label <- ifelse(missing, "", paste0("/ ", count_text(d$.n)))
    }
    is_ref <- !is.na(d$.ref) & d$.ref
    if (has_pvalue) {
        # number of levels each group has an estimate for, within each subgroup
        n_group_levels <- stats::ave(!missing, d$.group, d$subgroup, FUN = sum)
        p_label <- rep("", nrow(d))
        has_p <- !is.na(d$.p)
        p_label[has_p] <- format_pvalue(
            d$.p[has_p],
            p_text = "",
            p_symbol = "",
            truncate_pvalue = TRUE,
            html = FALSE
        )
        p_label[is_ref] <- ref_label
        p_label[n_group_levels < 2] <- ""
        d$p_label <- p_label
        # from the raw p, so e.g. 0.0497 (shown "0.050") is still bold
        d$p_sig <- p_label != "" &
            !is_ref &
            has_p &
            d$.p < pvalue_threshold
    } else {
        d$p_label <- ""
        d$p_sig <- FALSE
    }
    rownames(d) <- NULL
    list(
        rows = d,
        bands = bands,
        headings = headings,
        has_group = has_group,
        has_counts = has_counts,
        has_pvalue = has_pvalue
    )
}

#' Vertical extent of a table-style subgroup forest plot
#'
#' @param layout Output of `subgroup_forest_layout()`.
#' @return A list: `y_top` (top edge of the first row or heading), `y_header`
#'   (the column header row, above it), `y_rules` (the rules above and below the
#'   header) and `y_lim` (y-axis limits of every panel), in row heights.
#' @noRd
subgroup_forest_y <- function(layout) {
    y_top <- max(c(layout$rows$y, layout$headings$y)) + 0.5
    y_header <- y_top + 1.3
    list(
        y_top = y_top,
        y_header = y_header,
        y_rules = c(y_header + 1, y_top + 0.2),
        y_lim = c(min(layout$bands$ymin), y_header + 1)
    )
}

#' Per-group values of a manual scale, named by group
#'
#' @param values `NULL` (use `default`), an unnamed vector in group order, or a
#'   vector named by group.
#' @param groups Group names, in order.
#' @param default Default values, in group order.
#' @param name Argument name, for error messages.
#' @return `values` for `groups`, named by group.
#' @noRd
subgroup_forest_scale <- function(values, groups, default, name) {
    if (is.null(values)) {
        values <- default
    }
    if (is.null(names(values))) {
        if (length(values) < length(groups)) {
            stop(
                name,
                " needs a value for each of the ",
                length(groups),
                " groups"
            )
        }
        values <- stats::setNames(values[seq_along(groups)], groups)
    }
    missing <- setdiff(groups, names(values))
    if (length(missing) > 0) {
        stop(name, " has no value for: ", paste(missing, collapse = ", "))
    }
    values[groups]
}

#' Width of the widest of some strings, in inches
#'
#' Measured with the font metrics of a throwaway PDF device (the default sans
#' font), so it works without an open graphics device and leaves the current one
#' active.
#'
#' @param x Character vector (`NA` and `""` are ignored; may contain newlines).
#' @param size Font size, in points.
#' @param bold Bold font?
#' @return The width of the widest string, in inches (0 if there are none).
#' @noRd
text_width_in <- function(x, size, bold = FALSE) {
    x <- x[!is.na(x) & x != ""]
    if (length(x) == 0) {
        return(0)
    }
    old_dev <- grDevices::dev.cur()
    grDevices::pdf(NULL)
    on.exit({
        grDevices::dev.off()
        if (old_dev > 1) grDevices::dev.set(old_dev)
    })
    gp <- grid::gpar(fontsize = size, fontface = if (bold) "bold" else "plain")
    max(vapply(
        x,
        function(s) {
            grid::convertWidth(
                grid::grobWidth(grid::textGrob(s, gp = gp)),
                "in",
                valueOnly = TRUE
            )
        },
        numeric(1)
    ))
}
