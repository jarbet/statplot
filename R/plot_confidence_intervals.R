#' Dot-and-whisker plot of estimates with confidence intervals
#'
#' Create a horizontal dot-and-whisker plot showing point estimates and
#' confidence intervals for labeled rows. Optionally offset points and use
#' different shapes or colors when a grouping column is supplied. Optionally
#' append a p-value barplot to the right via patchwork.
#'
#' @param data A data.frame or tibble containing the columns referenced by
#'   `effect_size`, `ci_low`, `ci_high`, and `id`.
#' @param group_col Optional string name of a **factor** grouping column. When
#'   provided, points are offset by group for visibility. Factor levels
#'   control the ordering and direction of the dodge offset: the first level
#'   is plotted at the top of each row.
#' @param effect_size String name of the effect size/estimate column.
#' @param ci_low String name of the lower confidence interval column.
#' @param ci_high String name of the upper confidence interval column.
#' @param id String name of the label/row identifier column. Must be a
#'   **factor**; factor levels control the top-to-bottom row ordering (first
#'   level appears at the top).
#' @param dodge_width Numeric. Total vertical spread across groups. Default `0.3`.
#' @param show_separators Logical. Whether to draw horizontal separator lines
#'   between rows when `group_col` is supplied. Default `TRUE`.
#' @param sep_linetype Line type for row separator lines. Default `"solid"`.
#' @param sep_linewidth Line width for row separator lines. Default `0.4`.
#' @param sep_color Color for row separator lines. Default `"black"`.
#' @param vline_xintercept Numeric. Position of the vertical reference line. Default `0`.
#' @param vline_linetype Line type for the vertical reference line. Default `"dashed"`.
#' @param vline_color Color for the vertical reference line. Default `"black"`.
#' @param color_col Optional string name of a column to use for coloring
#'   segments and points. When `NULL` (default), no colors are applied. Common
#'   choices are `group_col` to distinguish groups or `id` to color by
#'   row. Must be a factor or character column.
#' @param color_values Optional named character vector specifying custom colors
#'   for the levels in `color_col`. Names should match the factor levels and
#'   values should be valid R color names or hex codes (e.g.,
#'   `c(g1 = "#FF0000", g2 = "#0000FF")`). If `NULL` (default), ggplot2's
#'   default color scale is used. Ignored if `color_col` is `NULL`.
#' @param shape_col Optional string name of a column to use for point shapes.
#'   When `NULL` (default), no shape encoding is applied. Must be a factor or
#'   character column. Can be used independently or in combination with `color_col`.
#' @param point_shapes Integer vector of point shapes to use when
#'   `shape_col` is specified. Must have at least as many elements as there are
#'   levels in `shape_col`. Defaults to `c(21, 24, 22, 25, 23)` (up to 5
#'   groups). See [graphics::points()] for shape codes.
#' @param pvalue_col Optional string name of a column in `data` containing
#'   p-values (one per row). When supplied, a [plot_pvalue_barplot()] is
#'   appended to the right using patchwork. When `group_col` is specified,
#'   p-values are combined across groups for each `id` level using the method
#'   specified by `combine_pvalue_method`. When `group_col` is `NULL`, individual
#'   p-values are displayed directly. Requires the \pkg{patchwork} package.
#' @param combine_pvalue_method Character; method for combining p-values when
#'   `group_col` is supplied. One of: "fisher" (default), "CMC", "MCM",
#'   "cauchy", "minp_bonferroni". See [combine_pvalues()] for details.
#'   Ignored if `group_col` is `NULL`.
#' @param pvalue_plot_width Relative width of the p-value panel passed to
#'   `patchwork::wrap_plots()` `widths`. Default `0.3`.
#' @param pvalue_plot_margin Numeric vector of length 4 giving the p-value
#'   panel plot margin in points: `c(top, right, bottom, left)`. Default
#'   `c(5.5, 12, 5.5, 0)` which increases right margin so x-axis labels are
#'   not clipped when the panel is appended to the right.
#' @param ... Additional arguments passed to [plot_pvalue_barplot()] when
#'   `pvalue_col` is supplied.
#'
#' @return A `ggplot2` object, or a `patchwork` object when `pvalue_col` is
#'   supplied.
#' @export
#'
#' @examples
#' df <- data.frame(
#'   cell_line = factor(c("A", "B", "C", "A", "B", "C"), levels = c('A', 'B', 'C')),
#'   est       = c(0.2, -0.1, 0.5, 0.35, 0.05, 0.3),
#'   conf.low  = c(0.0, -0.3, 0.2, 0.10, -0.10, 0.1),
#'   conf.high = c(0.4, 0.1, 0.8, 0.60, 0.20, 0.5),
#'   group     = factor(c("g1", "g1", "g1", "g2", "g2", "g2"), levels = c("g1", "g2")),
#'   pvalue    = c(0.01, 0.4, 0.001, 0.02, 0.3, 0.0001)
#' )
#'
#' # Simplest: no grouping, no color
#' plot_confidence_intervals(
#'   df[1:3, ],
#'   effect_size = "est",
#'   ci_low = "conf.low",
#'   ci_high = "conf.high",
#'   id = "cell_line"
#' )
#'
#' # With grouping: separate rows with color by default
#' plot_confidence_intervals(
#'   df,
#'   effect_size = "est",
#'   ci_low = "conf.low",
#'   ci_high = "conf.high",
#'   id = "cell_line",
#'   group_col = "group",
#'   color_col = "group"
#' )
#'
#' # With grouping: different shapes for groups
#' plot_confidence_intervals(
#'   df,
#'   effect_size = "est",
#'   ci_low = "conf.low",
#'   ci_high = "conf.high",
#'   id = "cell_line",
#'   group_col = "group",
#'   shape_col = "group",
#'   show_separators = FALSE
#' )
#'
#' # With grouping: color by group with custom colors
#' plot_confidence_intervals(
#'   df,
#'   effect_size = "est",
#'   ci_low = "conf.low",
#'   ci_high = "conf.high",
#'   id = "cell_line",
#'   group_col = "group",
#'   color_col = "group",
#'   color_values = c(g1 = "#E69F00", g2 = "#56B4E9")
#' )
#'
#' # Color by label, shapes by group
#' plot_confidence_intervals(
#'   df,
#'   effect_size = "est",
#'   ci_low = "conf.low",
#'   ci_high = "conf.high",
#'   id = "cell_line",
#'   group_col = "group",
#'   shape_col = "group",
#'   color_col = "cell_line"
#' )
#'
#' # Grouping with p-value barplot using Fisher's combined p-value
#' plot_confidence_intervals(
#'   df,
#'   effect_size = "est",
#'   ci_low = "conf.low",
#'   ci_high = "conf.high",
#'   id = "cell_line",
#'   group_col = "group",
#'   color_col = "group",
#'   pvalue_col = "pvalue",
#'   combine_pvalue_method = "fisher",
#'   mlog10_transform_pvalue = TRUE
#' )
#'
#' # No grouping, but with p-value barplot on the right
#' plot_confidence_intervals(
#'   df[1:3, ],
#'   effect_size = "est",
#'   ci_low = "conf.low",
#'   ci_high = "conf.high",
#'   id = "cell_line",
#'   pvalue_col = "pvalue",
#'   mlog10_transform_pvalue = TRUE
#' )
plot_confidence_intervals <- function(
    data,
    effect_size,
    ci_low,
    ci_high,
    id,
    group_col = NULL,
    dodge_width = 0.3,
    color_col = NULL,
    color_values = NULL,
    show_separators = TRUE,
    shape_col = NULL,
    sep_linetype = "solid",
    sep_linewidth = 0.4,
    sep_color = "black",
    vline_xintercept = 0,
    vline_linetype = "dashed",
    vline_color = "black",
    point_shapes = c(21, 24, 22, 25, 23),
    pvalue_col = NULL,
    combine_pvalue_method = c(
        "fisher",
        "CMC",
        "MCM",
        "cauchy",
        "minp_bonferroni"
    ),
    pvalue_plot_width = 0.3,
    pvalue_plot_margin = c(5.5, 12, 5.5, 0),
    ...
) {
    combine_pvalue_method <- match.arg(combine_pvalue_method)

    # ---- input validation ----
    stopifnot(
        "data must be a data.frame or tibble" = is.data.frame(data),
        "effect_size must be a single string" = is.character(effect_size) &&
            length(effect_size) == 1,
        "ci_low must be a single string" = is.character(ci_low) &&
            length(ci_low) == 1,
        "ci_high must be a single string" = is.character(ci_high) &&
            length(ci_high) == 1,
        "id must be a single string" = is.character(id) &&
            length(id) == 1,
        "effect_size must be a column in data" = effect_size %in% names(data),
        "ci_low must be a column in data" = ci_low %in% names(data),
        "ci_high must be a column in data" = ci_high %in% names(data),
        "id must be a column in data" = id %in% names(data),
        "id must be a factor" = is.factor(data[[id]]),
        "color_col must be NULL or a single string" = is.null(color_col) ||
            (is.character(color_col) && length(color_col) == 1),
        "color_col must be a column in data" = is.null(color_col) ||
            color_col %in% names(data),
        "color_values must be NULL or a named character vector" = is.null(
            color_values
        ) ||
            (is.character(color_values) && !is.null(names(color_values))),
        "show_separators must be logical" = is.logical(show_separators) &&
            length(show_separators) == 1,
        "shape_col must be NULL or a single string" = is.null(shape_col) ||
            (is.character(shape_col) && length(shape_col) == 1),
        "shape_col must be a column in data" = is.null(shape_col) ||
            shape_col %in% names(data),
        "group_col must be NULL or a single string" = is.null(group_col) ||
            (is.character(group_col) && length(group_col) == 1),
        "group_col must be a column in data" = is.null(group_col) ||
            group_col %in% names(data),
        "group_col must be a factor" = is.null(group_col) ||
            is.factor(data[[group_col]]),
        "group_col must have more than 1 level" = is.null(group_col) ||
            nlevels(data[[group_col]]) > 1,
        "pvalue_col must be NULL or a single string" = is.null(pvalue_col) ||
            (is.character(pvalue_col) && length(pvalue_col) == 1),
        "pvalue_col must be a column in data" = is.null(pvalue_col) ||
            pvalue_col %in% names(data),
        "dodge_width must be a single non-negative number" = is.numeric(
            dodge_width
        ) &&
            length(dodge_width) == 1 &&
            dodge_width >= 0,
        "sep_linewidth must be a single positive number" = is.numeric(
            sep_linewidth
        ) &&
            length(sep_linewidth) == 1 &&
            sep_linewidth > 0,
        "sep_linetype must be a single string" = is.character(sep_linetype) &&
            length(sep_linetype) == 1,
        "sep_color must be a single string" = is.character(sep_color) &&
            length(sep_color) == 1,
        "vline_xintercept must be a single number" = is.numeric(
            vline_xintercept
        ) &&
            length(vline_xintercept) == 1,
        "vline_linetype must be a single string" = is.character(
            vline_linetype
        ) &&
            length(vline_linetype) == 1,
        "vline_color must be a single string" = is.character(vline_color) &&
            length(vline_color) == 1,
        "pvalue_plot_width must be a single positive number" = is.numeric(
            pvalue_plot_width
        ) &&
            length(pvalue_plot_width) == 1 &&
            pvalue_plot_width > 0,
        "pvalue_plot_margin must be a numeric vector of length 4" = is.numeric(
            pvalue_plot_margin
        ) &&
            length(pvalue_plot_margin) == 4 &&
            all(is.finite(pvalue_plot_margin)) &&
            all(pvalue_plot_margin >= 0),
        "point_shapes must be a numeric or integer vector" = is.numeric(
            point_shapes
        ) &&
            length(point_shapes) >= 1
    )

    # Validate point_shapes length against number of shape_col levels
    if (!is.null(shape_col)) {
        n_shapes_check <- nlevels(data[[shape_col]])
        if (length(point_shapes) < n_shapes_check) {
            stop(
                "point_shapes has ",
                length(point_shapes),
                " element(s) but ",
                "shape_col '",
                shape_col,
                "' has ",
                n_shapes_check,
                " levels. ",
                "Provide a point_shapes vector with at least ",
                n_shapes_check,
                " elements."
            )
        }
    }

    d <- data

    # ---- y positions from factor levels ----
    # label levels run top-to-bottom: first level -> highest y value
    units <- levels(d[[id]]) # e.g. c("C", "B", "A") -> C on top
    n_units <- length(units)
    y_num_map <- setNames(rev(seq_len(n_units)), units) # C->3, B->2, A->1... wait
    # first level should be at TOP (highest numeric y), so:
    # units[1] gets n_units, units[n_units] gets 1
    y_num_map <- setNames(seq(n_units, 1), units)
    d$y_num <- y_num_map[as.character(d[[id]])]

    # ---- dodged y positions from group factor levels ----
    if (!is.null(group_col)) {
        groups <- levels(d[[group_col]])
        n_groups <- length(groups)
        # first level -> top offset (positive), last -> bottom (negative)
        offsets <- seq(dodge_width / 2, -dodge_width / 2, length.out = n_groups)
        names(offsets) <- groups
        d$y_pos <- d$y_num + offsets[as.character(d[[group_col]])] # fix: was offsets[d[[group_col]]]
    } else {
        d$y_pos <- d$y_num
    }

    # ---- base plot ----
    p <- ggplot2::ggplot(d, ggplot2::aes(x = .data[[effect_size]])) +
        ggplot2::geom_vline(
            xintercept = vline_xintercept,
            linetype = vline_linetype,
            linewidth = 0.6,
            color = vline_color
        )

    # ---- row separator lines ----
    if (!is.null(group_col) && show_separators && n_units > 1) {
        separators <- seq_len(n_units - 1) + 0.5
        p <- p +
            ggplot2::geom_hline(
                yintercept = separators,
                linetype = sep_linetype,
                linewidth = sep_linewidth,
                color = sep_color
            )
    }

    # ---- segments ----
    if (!is.null(color_col)) {
        p <- p +
            ggplot2::geom_segment(
                ggplot2::aes(
                    x = .data[[ci_low]],
                    xend = .data[[ci_high]],
                    y = y_pos,
                    yend = y_pos,
                    color = .data[[color_col]]
                ),
                linewidth = 0.8,
                show.legend = TRUE
            )
        if (!is.null(color_values)) {
            p <- p + ggplot2::scale_color_manual(values = color_values)
        }
    } else {
        p <- p +
            ggplot2::geom_segment(
                ggplot2::aes(
                    x = .data[[ci_low]],
                    xend = .data[[ci_high]],
                    y = y_pos,
                    yend = y_pos
                ),
                linewidth = 0.8,
                color = "black",
                show.legend = FALSE
            )
    }

    # ---- points ----
    if (!is.null(shape_col)) {
        # Use shapes for shape_col, optionally with colors
        if (!is.null(color_col)) {
            p <- p +
                ggplot2::geom_point(
                    ggplot2::aes(
                        y = y_pos,
                        color = .data[[color_col]],
                        fill = .data[[color_col]],
                        shape = .data[[shape_col]]
                    ),
                    size = 4,
                    stroke = 0.4,
                    show.legend = TRUE
                ) +
                ggplot2::scale_shape_manual(
                    values = point_shapes[seq_len(nlevels(d[[shape_col]]))]
                )
            if (!is.null(color_values)) {
                p <- p +
                    ggplot2::scale_color_manual(values = color_values) +
                    ggplot2::scale_fill_manual(values = color_values)
            }
        } else {
            p <- p +
                ggplot2::geom_point(
                    ggplot2::aes(
                        y = y_pos,
                        shape = .data[[shape_col]]
                    ),
                    size = 4,
                    stroke = 0.4,
                    color = "black",
                    fill = "black",
                    show.legend = TRUE
                ) +
                ggplot2::scale_shape_manual(
                    values = point_shapes[seq_len(nlevels(d[[shape_col]]))]
                )
        }
    } else if (!is.null(color_col)) {
        # Color specified: use it for points
        p <- p +
            ggplot2::geom_point(
                ggplot2::aes(
                    y = y_pos,
                    color = .data[[color_col]],
                    fill = .data[[color_col]]
                ),
                size = 4,
                stroke = 0.4,
                show.legend = TRUE
            )
        if (!is.null(color_values)) {
            p <- p +
                ggplot2::scale_color_manual(values = color_values) +
                ggplot2::scale_fill_manual(values = color_values)
        }
    } else {
        # No shape or color: plain points
        p <- p +
            ggplot2::geom_point(
                ggplot2::aes(y = y_pos),
                size = 4,
                color = "black",
                fill = "black",
                show.legend = FALSE
            )
    }

    # ---- scales & theme ----
    p <- p +
        ggplot2::scale_y_continuous(
            breaks = seq_len(n_units),
            labels = rev(units), # y=1 is bottom, so rev() maps units back correctly
            limits = c(min(d$y_pos) - 0.5, max(d$y_pos) + 0.5),
            expand = c(0, 0)
        ) +
        ggplot2::labs(x = "Estimate", y = NULL) +
        ggplot2::theme_bw() +
        ggplot2::theme(axis.text.y = ggplot2::element_text(face = "bold"))

    # Add shape legend overrides when using shape_col
    if (!is.null(shape_col)) {
        # Only override color to black if no color_col specified
        override_aes <- list(linetype = 0, linewidth = 0)
        if (is.null(color_col)) {
            override_aes$color <- "black"
        }
        p <- p +
            ggplot2::guides(
                shape = ggplot2::guide_legend(override.aes = override_aes)
            )
    }

    # Add color legend overrides when using color_col
    if (!is.null(color_col)) {
        p <- p +
            ggplot2::guides(
                color = ggplot2::guide_legend(
                    override.aes = list(linewidth = 1.2)
                )
            )
    }

    # ---- optional p-value panel ----
    if (!is.null(pvalue_col)) {
        if (!requireNamespace("patchwork", quietly = TRUE)) {
            stop("Package 'patchwork' is required when pvalue_col is supplied.")
        }

        p <- p +
            ggplot2::theme(
                legend.position = "left",
                plot.margin = ggplot2::margin(5.5, 5.5, 5.5, 5.5)
            )

        # Build data for p-value barplot
        # When group_col is specified, compute combined p-values across groups for each id
        # Otherwise, extract one p-value per id unit
        if (!is.null(group_col)) {
            # Compute combined p-values by id across groups
            pv_data <- d |>
                dplyr::group_by(.data[[id]]) |>
                dplyr::summarise(
                    p_combined = combine_pvalues(
                        .data[[pvalue_col]][!is.na(.data[[pvalue_col]])],
                        methods = combine_pvalue_method
                    ),
                    .groups = "drop"
                )
            pv_data[[id]] <- factor(
                pv_data[[id]],
                levels = rev(units)
            )
            pv_col_name <- "p_combined"
        } else {
            # No grouping: check that pvalue_col is constant within each id group
            # and extract one value per id
            non_constant <- vapply(
                units,
                function(u) {
                    vals <- d[[pvalue_col]][as.character(d[[id]]) == u]
                    vals <- vals[!is.na(vals)]
                    length(unique(vals)) > 1
                },
                logical(1)
            )

            if (any(non_constant)) {
                bad <- units[non_constant]
                stop(
                    "pvalue_col must be constant within each ",
                    id,
                    " group. ",
                    "Non-constant values found in: ",
                    paste(bad, collapse = ", ")
                )
            }

            pv_data <- do.call(
                rbind,
                lapply(units, function(u) {
                    rows <- d[as.character(d[[id]]) == u, , drop = FALSE]
                    rows[1, c(id, pvalue_col), drop = FALSE]
                })
            )
            pv_data[[id]] <- factor(
                pv_data[[id]],
                levels = rev(units)
            )
            pv_col_name <- pvalue_col
        }

        pv_plot <- plot_pvalue_barplot(
            data = pv_data,
            x = pv_col_name,
            y = id,
            show_y_labels = FALSE,
            ...
        ) +
            ggplot2::scale_y_discrete(
                limits = rev(units),
                expand = c(0, 0)
            ) +
            ggplot2::coord_cartesian(ylim = c(0.5, n_units + 0.5)) +
            # p-value panel margin (top, right, bottom, left) in points
            ggplot2::theme(
                plot.margin = ggplot2::margin(
                    t = pvalue_plot_margin[1],
                    r = pvalue_plot_margin[2],
                    b = pvalue_plot_margin[3],
                    l = pvalue_plot_margin[4],
                    unit = "pt"
                )
            )

        return(
            patchwork::wrap_plots(
                p,
                pv_plot,
                widths = c(1, pvalue_plot_width)
            ) +
                patchwork::plot_layout(axes = "collect_y")
        )
    }

    p
}
