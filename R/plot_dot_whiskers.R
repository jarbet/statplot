#' Dot-and-whisker plot of estimates with confidence intervals
#'
#' Create a horizontal dot-and-whisker plot showing point estimates and
#' confidence intervals for labeled rows. Optionally offset points and use
#' different shapes or colors when a grouping column is supplied. Optionally
#' append a p-value barplot to the right via patchwork.
#'
#' @param data A data.frame or tibble containing the columns referenced by
#'   `x`, `xmin`, `xmax`, and `label_col`.
#' @param group_col Optional string name of a grouping column. When provided,
#'   points are offset by group for visibility.
#' @param x String name of the estimate column. Default `"est"`.
#' @param xmin String name of the lower-interval column. Default `"conf.low"`.
#' @param xmax String name of the upper-interval column. Default `"conf.high"`.
#' @param label_col String name of the label/row identifier column. Default `"cell_line"`.
#' @param dodge_width Numeric. Total vertical spread across groups. Default `0.3`.
#' @param style String. Controls group encoding when `group_col` is supplied.
#'   `"shape"` uses the same color per `label_col` row and different
#'   point shapes per group. `"color"` uses the same point shape for all groups
#'   but different colors per group, and draws horizontal lines between rows.
#' @param sep_linetype Line type for row separator lines when `style = "color"`. Default `"solid"`.
#' @param sep_linewidth Line width for row separator lines when `style = "color"`. Default `0.4`.
#' @param sep_color Color for row separator lines when `style = "color"`. Default `"black"`.
#' @param vline_xintercept Numeric. Position of the vertical reference line. Default `0`.
#' @param vline_linetype Line type for the vertical reference line. Default `"dashed"`.
#' @param vline_color Color for the vertical reference line. Default `"black"`.
#' @param pvalue_col Optional string name of a column in `data` containing one
#'   p-value per `label_col` row. When supplied, a [plot_pvalue_barplot()] is
#'   appended to the right using patchwork. The p-value data is derived by
#'   taking the first value per `label_col` group, so the column must be
#'   constant within each row. Requires the \pkg{patchwork} package.
#' @param pvalue_plot_width Relative width of the p-value panel passed to
#'   patchwork `widths`. Default `0.3`.
#' @param ... Additional arguments passed to [plot_pvalue_barplot()] when
#'   `pvalue_col` is supplied.
#' @return A ggplot2 object, or a patchwork object when `pvalue_col` is supplied.
#' @export
#' @examples
#' df <- data.frame(
#'   cell_line = c("A", "A", "B", "B", "C", "C"),
#'   est       = c(0.2, 0.35, -0.1, 0.05, 0.5, 0.3),
#'   conf.low  = c(0.0, 0.10, -0.3, -0.10, 0.2, 0.1),
#'   conf.high = c(0.4, 0.60,  0.1,  0.20, 0.8, 0.5),
#'   group     = c("g1", "g2", "g1", "g2", "g1", "g2"),
#'   pvalue    = c(0.01, 0.01, 0.4, 0.4, 0.001, 0.001)
#' )
#' plot_dot_whiskers(df, group_col = "group", style = "color")
#' plot_dot_whiskers(df, group_col = "group", style = "shape")
#' plot_dot_whiskers(
#'   df, group_col = "group", style = "color",
#'   pvalue_col = "pvalue", mlog10_transform_pvalue = TRUE
#' )
plot_dot_whiskers <- function(
    data,
    group_col = NULL,
    x = "est",
    xmin = "conf.low",
    xmax = "conf.high",
    label_col = "cell_line",
    dodge_width = 0.3,
    style = c("color", "shape"),
    sep_linetype = "solid",
    sep_linewidth = 0.4,
    sep_color = "black",
    vline_xintercept = 0,
    vline_linetype = "dashed",
    vline_color = "black",
    pvalue_col = NULL,
    pvalue_plot_width = 0.3,
    ...
) {
    style <- match.arg(style)

    # ---- input validation ----
    stopifnot(
        "data must be a data.frame or tibble" = is.data.frame(data),
        "x must be a single string" = is.character(x) && length(x) == 1,
        "xmin must be a single string" = is.character(xmin) &&
            length(xmin) == 1,
        "xmax must be a single string" = is.character(xmax) &&
            length(xmax) == 1,
        "label_col must be a single string" = is.character(label_col) &&
            length(label_col) == 1,
        "x must be a column in data" = x %in% names(data),
        "xmin must be a column in data" = xmin %in% names(data),
        "xmax must be a column in data" = xmax %in% names(data),
        "label_col must be a column in data" = label_col %in% names(data),
        "group_col must be NULL or a single string" = is.null(group_col) ||
            (is.character(group_col) && length(group_col) == 1),
        "group_col must be a column in data" = is.null(group_col) ||
            group_col %in% names(data),
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
            pvalue_plot_width > 0
    )

    d <- data

    # ---- auto-generate y_num ----
    units <- unique(as.character(d[[label_col]]))
    y_num_map <- setNames(rev(seq_along(units)), units)
    d$y_num <- y_num_map[as.character(d[[label_col]])]

    # ---- auto-generate y_pos (dodged per group) ----
    if (!is.null(group_col)) {
        groups <- sort(unique(as.character(d[[group_col]])))
        n_groups <- length(groups)
        offsets <- seq(dodge_width / 2, -dodge_width / 2, length.out = n_groups)
        names(offsets) <- groups
        d$y_pos <- d$y_num + offsets[as.character(d[[group_col]])]
    } else {
        d$y_pos <- d$y_num
    }

    # ---- base plot ----
    p <- ggplot2::ggplot(d, ggplot2::aes(x = .data[[x]])) +
        ggplot2::geom_vline(
            xintercept = vline_xintercept,
            linetype = vline_linetype,
            linewidth = 0.6,
            color = vline_color
        )

    # ---- row separator lines (style = "color" only) ----
    if (!is.null(group_col) && style == "color" && length(units) > 1) {
        separators <- seq_along(units)[-length(units)] + 0.5
        p <- p +
            ggplot2::geom_hline(
                yintercept = separators,
                linetype = sep_linetype,
                linewidth = sep_linewidth,
                color = sep_color
            )
    }

    # ---- segments ----
    if (!is.null(group_col) && style == "color") {
        p <- p +
            ggplot2::geom_segment(
                ggplot2::aes(
                    x = .data[[xmin]],
                    xend = .data[[xmax]],
                    y = y_pos,
                    yend = y_pos,
                    color = .data[[group_col]]
                ),
                linewidth = 0.8,
                show.legend = TRUE
            )
    } else {
        p <- p +
            ggplot2::geom_segment(
                ggplot2::aes(
                    x = .data[[xmin]],
                    xend = .data[[xmax]],
                    y = y_pos,
                    yend = y_pos,
                    color = .data[[label_col]]
                ),
                linewidth = 0.8,
                show.legend = TRUE # drives the color legend for style = "shape"
            )
    }

    # ---- points ----
    if (is.null(group_col)) {
        p <- p +
            ggplot2::geom_point(
                ggplot2::aes(y = y_pos, fill = .data[[label_col]]),
                size = 4,
                stroke = 0.4,
                color = "black",
                shape = 21,
                show.legend = FALSE # legend already shown via segments
            )
    } else if (style == "shape") {
        p <- p +
            ggplot2::geom_point(
                ggplot2::aes(
                    y = y_pos,
                    fill = .data[[label_col]],
                    shape = .data[[group_col]]
                ),
                size = 4,
                stroke = 0.4,
                color = "black",
                show.legend = c(fill = FALSE, shape = TRUE) # only shape legend from points
            ) +
            ggplot2::scale_shape_manual(
                values = c(21, 24, 22, 25, 23)[seq_along(sort(unique(d[[
                    group_col
                ]])))]
            ) +
            ggplot2::guides(
                color = ggplot2::guide_legend(
                    override.aes = list(linewidth = 1.2)
                ),
                shape = ggplot2::guide_legend(
                    override.aes = list(linetype = 0, linewidth = 0)
                )
            )
    } else {
        p <- p +
            ggplot2::geom_point(
                ggplot2::aes(y = y_pos, fill = .data[[group_col]]),
                size = 4,
                stroke = 0.4,
                color = "black",
                shape = 21,
                show.legend = FALSE
            ) +
            ggplot2::guides(
                color = ggplot2::guide_legend(
                    override.aes = list(linewidth = 1.2)
                )
            )
    }

    # ---- scales & theme ----
    y_breaks <- rev(seq_along(units))
    names(y_breaks) <- units

    p <- p +
        ggplot2::scale_y_continuous(
            breaks = y_breaks,
            labels = names(y_breaks),
            limits = c(min(d$y_pos) - 0.5, max(d$y_pos) + 0.5),
            expand = c(0, 0)
        ) +
        ggplot2::labs(x = "Estimate", y = NULL) +
        ggplot2::theme_bw() +
        ggplot2::theme(axis.text.y = ggplot2::element_text(face = "bold"))

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

        # one row per label_col unit, preserving y-axis order
        pv_data <- do.call(
            rbind,
            lapply(units, function(u) {
                rows <- d[d[[label_col]] == u, , drop = FALSE]
                rows[1, c(label_col, pvalue_col), drop = FALSE]
            })
        )

        # factor with levels matching top-to-bottom y-axis order
        pv_data[[label_col]] <- factor(
            pv_data[[label_col]],
            levels = rev(units)
        )

        pv_plot <- plot_pvalue_barplot(
            data = pv_data,
            x = pvalue_col,
            y = label_col,
            show_y_labels = FALSE,
            ...
        ) +
            ggplot2::scale_y_discrete(
                limits = rev(units),
                expand = c(0, 0)
            ) +
            ggplot2::coord_cartesian(
                ylim = c(0.5, length(units) + 0.5)
            ) +
            ggplot2::theme(
                plot.margin = ggplot2::margin(5.5, 5.5, 5.5, 0)
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
