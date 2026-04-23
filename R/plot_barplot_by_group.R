#' Bar plot comparing a numeric outcome between two conditions
#'
#' Draws a bar chart comparing a numeric outcome between exactly two conditions.
#' Bar height = mean (or any effect size); error bars span +/- 1 error unit
#' (SE, SD, CI half-width, etc.). A significance bracket with optional label is
#' drawn above bars when the corresponding p-value falls below \code{p_cutoff}.
#'
#' When \code{facet_cols} is supplied, the significance brackets are computed
#' separately for each unique combination of those columns and those columns are
#' retained in the annotation layers. This means adding
#' \code{+ ggplot2::facet_wrap()} or \code{+ ggplot2::facet_grid()} after the
#' function call will correctly split both bars and brackets across panels.
#'
#' @param df Data frame in **long format** — one row per condition (and per
#'   faceting-group combination when faceting).
#' @param condition_col Column name for the two conditions to compare.
#'   Default \code{"condition"}.
#' @param mean_col Column name for bar heights (means or effect sizes).
#'   Default \code{"mean"}.
#' @param error_col Column name for error-bar half-widths (SE, SD, CI
#'   half-width, etc.). Default \code{"se"}.
#' @param error_direction Direction of error bars. \code{"both"} draws
#'   \code{mean +/- error}; \code{"up"} draws only the upper whisker
#'   (\code{mean} to \code{mean + error}). Default \code{"up"}.
#' @param p_col Column name for p-values. When faceting, the value should be
#'   the same for both condition rows within each facet group (i.e. repeated).
#'   Set to \code{NULL} to suppress brackets entirely. Default \code{"p_value"}.
#' @param label_col Optional column name supplying custom bracket label text
#'   (e.g. \code{"OR = 1.5 [1.1-2.0], p = 0.012"}). When \code{NULL} labels
#'   are auto-formatted as \code{"p = <value>"} using \code{\link{signif}}.
#' @param condition_order Length-2 character vector setting the left-to-right
#'   display order of the two conditions. Defaults to the existing factor level
#'   order or alphabetical.
#' @param facet_cols Optional character vector of column name(s) to use as
#'   faceting variables (e.g. \code{"study"} or \code{c("study", "sex")}).
#'   When supplied, significance brackets are computed per unique combination of
#'   these columns, and the columns are retained in bracket annotation layers so
#'   that \code{+ facet_wrap()} or \code{+ facet_grid()} work correctly.
#'   Default \code{NULL} (single panel, no grouping for brackets).
#' @param p_cutoff Significance threshold; brackets appear only when
#'   \code{p < p_cutoff}. Default \code{0.05}.
#' @param y_label Y-axis label. Default \code{"Outcome"}.
#' @param bar_colors Length-2 fill colour vector applied to the two conditions
#'   in the order given by \code{condition_order}. Default
#'   \code{c("black", "grey70")}.
#' @param bar_width Width of the bars passed to \code{\link[ggplot2]{geom_col}}.
#'   Default \code{0.4}.
#' @param bar_gap Gap between the two bars, in x-axis units.
#'   Default \code{0.6}.
#' @param bar_padding White space added to the left and right of the bars,
#'   in x-axis units. Passed to \code{ggplot2::expansion(add = bar_padding)}.
#'   Default \code{0.5}.
#' @param text_size Size of bracket label text (ggplot2 \code{size} units).
#'   Default \code{3.5}.
#' @param bracket_offset Fraction of the per-facet y range added as vertical
#'   spacing above bar tops for bracket placement. Using a fraction (rather than
#'   an absolute data-unit distance) ensures consistent visual spacing across
#'   facets even when \code{scales = "free_y"} is used. Default \code{0.05}.
#' @param bracket_gap Fraction of the per-facet y range inserted as white space
#'   between the top of each error bar and the start of the significance bracket
#'   tick. Default \code{0.04}.
#' @param bracket_text_gap Fraction of the per-facet y range used as white space
#'   between the horizontal bracket line and the label text above it. Using a
#'   fraction ensures consistent visual spacing across facets even when
#'   \code{scales = "free_y"} is used. Default \code{0.05}.
#'
#' @return A \code{\link[ggplot2]{ggplot}} object. Add
#'   \code{+ ggplot2::facet_wrap()} or \code{+ ggplot2::facet_grid()} to
#'   create multi-panel layouts; bracket annotations facet automatically.
#' @export
#'
#' @examples
#' library(ggplot2)
#' df <- data.frame(
#'     study     = rep(c("Study A", "Study B"), each = 2),
#'     group     = rep(c("Exercise", "Control"), 2),
#'     mean      = c(10.2, 14.8, 12.5, 13.1),
#'     se        = c(0.9, 1.0, 1.1, 1.0),
#'     p_value   = c(0.004, 0.004, 0.18, 0.18)
#' )
#' df$group <- factor(df$group, levels = c("Exercise", "Control"))
#' ggplot2::theme_set(theme_bw2())
#'
#' # Single bar plot (no faceting)
#' df_a <- df[df$study == "Study A", ]
#' plot_barplot_by_group(
#'     df = df_a,
#'     condition_col = "group",
#'     mean_col = "mean",
#'     error_col = "se",
#'     p_col = "p_value",
#'     y_label = "Performance score"
#' )
#'
#' # Facet by study using facet_wrap
#' plot_barplot_by_group(
#'     df = df,
#'     condition_col = "group",
#'     facet_cols = "study",
#'     mean_col = "mean",
#'     error_col = "se",
#'     p_col = "p_value",
#'     y_label = "Performance score"
#' ) +
#'     ggplot2::facet_wrap(~study)
#'
#' # Set p_cutoff = 1 to always show all p-values
#' plot_barplot_by_group(
#'     df = df,
#'     condition_col = "group",
#'     facet_cols = "study",
#'     mean_col = "mean",
#'     error_col = "se",
#'     p_col = "p_value",
#'     y_label = "Performance score",
#'     p_cutoff = 1
#' ) +
#'     ggplot2::facet_wrap(~study)
#'
#' # facet_grid works too (useful with multiple facet_cols)
#' df_multi <- data.frame(
#'     study     = rep(c("Study A", "Study B"), each = 4),
#'     sex       = rep(c("M", "M", "F", "F"), 2),
#'     group     = rep(c("Exercise", "Control"), 4),
#'     mean      = c(10.2, 14.8, 12.5, 13.1, 11.0, 15.2, 13.0, 13.8),
#'     se        = c(0.9, 1.0, 1.1, 1.0, 0.8, 1.1, 1.0, 0.9),
#'     p_value   = c(0.004, 0.004, 0.18, 0.18, 0.01, 0.01, 0.25, 0.25)
#' )
#' df_multi$group <- factor(df_multi$group, levels = c("Exercise", "Control"))
#' plot_barplot_by_group(
#'     df = df_multi,
#'     condition_col = "group",
#'     facet_cols = c("study", "sex"),
#'     mean_col = "mean",
#'     error_col = "se",
#'     p_col = "p_value",
#'     y_label = "Performance score"
#' ) +
#'     ggplot2::facet_grid(
#'         rows = ggplot2::vars(sex),
#'         cols = ggplot2::vars(study)
#'     )
#'
#' # Custom bracket label from a column
#' df$label <- ifelse(
#'     df$p_value < 0.05,
#'     paste0("d = 1.5 [1.1-2.1]\n", format_pvalue(df$p_value)),
#'     NA
#' )
#' plot_barplot_by_group(
#'     df = df,
#'     condition_col = "group",
#'     facet_cols = "study",
#'     mean_col = "mean",
#'     error_col = "se",
#'     p_col = "p_value",
#'     y_label = "Performance score",
#'     label_col = "label"
#' ) +
#'     ggplot2::facet_wrap(~study) +
#'     ggplot2::coord_cartesian(ylim = c(0, 20))
#'
#' # Using theme_classic2 with strip bars positioned below the plot
#' ggplot2::theme_set(theme_classic2())
#' plot_barplot_by_group(
#'     df = df,
#'     condition_col = "group",
#'     facet_cols = "study",
#'     mean_col = "mean",
#'     error_col = "se",
#'     p_col = "p_value",
#'     y_label = "Performance score"
#' ) +
#'     ggplot2::facet_wrap(~study, strip.position = "bottom") +
#'     ggplot2::theme(
#'         axis.text.x = ggplot2::element_text(margin = ggplot2::margin(b = 5)),
#'         strip.placement = "outside"
#'     )
#'
#' # Facet by outcome with different scales
#' # When outcomes are on different scales (e.g., one is 0-20, another is 0-200),
#' # use facet_wrap(scales = "free_y") to let each panel have its own y-axis range
#' df_outcomes <- data.frame(
#'     outcome   = rep(c("Strength (kg)", "Endurance (min)"), each = 2),
#'     group     = rep(c("Exercise", "Control"), 2),
#'     mean      = c(12.5, 10.2, 45.0, 28.3),
#'     se        = c(1.1, 0.9, 3.5, 2.8),
#'     p_value   = c(0.008, 0.008, 0.012, 0.012)
#' )
#' df_outcomes$group <- factor(df_outcomes$group, levels = c("Exercise", "Control"))
#' plot_barplot_by_group(
#'     df = df_outcomes,
#'     condition_col = "group",
#'     facet_cols = "outcome",
#'     mean_col = "mean",
#'     error_col = "se",
#'     p_col = "p_value",
#'     y_label = "Measurement"
#' ) +
#'     ggplot2::facet_wrap(~outcome, scales = "free_y")
#'
#' @importFrom stats setNames
#' @importFrom rlang %||%
plot_barplot_by_group <- function(
    df,
    condition_col,
    mean_col,
    error_col,
    p_col,
    facet_cols = NULL,
    label_col = NULL,
    error_direction = "up",
    condition_order = NULL,
    p_cutoff = 0.05,
    y_label = "Outcome",
    bar_colors = c("black", "grey70"),
    bar_width = 0.4,
    bar_gap = 0.6,
    bar_padding = 0.5,
    text_size = 3.5,
    bracket_offset = 0.05,
    bracket_gap = 0.04,
    bracket_text_gap = 0.05
) {
    # -- Input validation --
    stopifnot(is.data.frame(df))
    required <- c(condition_col, mean_col, error_col)
    missing_cols <- setdiff(required, names(df))
    if (length(missing_cols)) {
        stop(
            "Column(s) not found in `df`: ",
            paste(missing_cols, collapse = ", ")
        )
    }

    if (!is.null(facet_cols)) {
        missing_facet <- setdiff(facet_cols, names(df))
        if (length(missing_facet)) {
            stop(
                "`facet_cols` column(s) not found in `df`: ",
                paste(missing_facet, collapse = ", ")
            )
        }
    }

    # -- Condition factor ordering (before facet validation) --
    if (!is.null(condition_order)) {
        if (length(condition_order) != 2L) {
            stop(
                "`condition_order` must have exactly 2 elements; found ",
                length(condition_order)
            )
        }
        if (anyDuplicated(condition_order)) {
            stop(
                "`condition_order` contains duplicate values: ",
                paste(
                    condition_order[duplicated(condition_order)],
                    collapse = ", "
                )
            )
        }
        condition_values <- unique(df[[condition_col]])
        missing_cols <- setdiff(condition_order, condition_values)
        if (length(missing_cols)) {
            stop(
                "Values in `condition_order` not found in `",
                condition_col,
                "`: ",
                paste(missing_cols, collapse = ", ")
            )
        }
        df[[condition_col]] <- factor(
            df[[condition_col]],
            levels = condition_order
        )
    } else if (!is.factor(df[[condition_col]])) {
        df[[condition_col]] <- factor(df[[condition_col]])
    }
    cond_levels <- levels(df[[condition_col]])
    if (length(cond_levels) != 2L) {
        stop(
            "`condition_col` must contain exactly 2 levels; found ",
            length(cond_levels)
        )
    }

    # Validate that each facet group contains exactly both conditions
    # (one row per condition, and all conditions must be present)
    group_by_cols_temp <- facet_cols %||% character(0)
    val_df <- df |>
        dplyr::group_by(dplyr::pick(dplyr::all_of(group_by_cols_temp))) |>
        dplyr::summarize(
            n_rows = dplyr::n(),
            n_conditions = dplyr::n_distinct(.data[[condition_col]]),
            .groups = "drop"
        )
    if (!all(val_df$n_rows == 2L & val_df$n_conditions == 2L)) {
        stop(
            "Each condition must appear exactly once per facet group. ",
            "Some groups have multiple rows per condition or missing conditions."
        )
    }

    stopifnot(length(bar_colors) == 2L)
    error_direction <- match.arg(error_direction, c("both", "up"))

    # -- Bar x positions (continuous axis) --
    x_left <- 1
    x_right <- x_left + bar_width + bar_gap
    x_mid <- (x_left + x_right) / 2
    df$.x_pos <- ifelse(df[[condition_col]] == cond_levels[1], x_left, x_right)

    # -- Validate optional columns --
    if (!is.null(p_col) && !(p_col %in% names(df))) {
        stop("Column `", p_col, "` not found in `df`")
    }
    if (!is.null(label_col) && !(label_col %in% names(df))) {
        stop("Column `", label_col, "` not found in `df`")
    }

    # -- Build significance bracket data --
    # group_by_cols determines how brackets are summarised and which columns
    # are retained in seg_df/text_df. When facet_cols is NULL (single panel)
    # no grouping is applied. When facet_cols is supplied, brackets are
    # computed per unique combination of those columns so that
    # facet_wrap() / facet_grid() added by the caller routes brackets
    # to the correct panel.
    group_by_cols <- facet_cols %||% character(0)

    seg_df <- NULL
    text_df <- NULL
    draw_brackets <- !is.null(p_col)

    if (draw_brackets) {
        # Determine label text per row (NA -> suppress bracket)
        if (!is.null(label_col)) {
            df$.lbl <- df[[label_col]]
        } else {
            df$.lbl <- ifelse(
                !is.na(df[[p_col]]) & df[[p_col]] < p_cutoff,
                paste0("p = ", signif(df[[p_col]], 2)),
                NA_character_
            )
        }

        # Summarise to one row per bracket (one per unique combination of
        # grouping cols). All grouping cols are retained via dplyr::across().
        # Per-facet y range is computed so that bracket_offset, bracket_gap,
        # and bracket_text_gap (all fractions) scale correctly with free_y.
        bracket_df <- df |>
            dplyr::mutate(.bar_top = .data[[mean_col]] + .data[[error_col]]) |>
            dplyr::arrange(
                dplyr::pick(dplyr::all_of(group_by_cols)),
                .data[[condition_col]]
            ) |>
            dplyr::group_by(dplyr::pick(dplyr::all_of(group_by_cols))) |>
            dplyr::summarize(
                y_left = .bar_top[.data[[condition_col]] == cond_levels[1]],
                y_right = .bar_top[.data[[condition_col]] == cond_levels[2]],
                .y_max = max(.bar_top),
                .y_min = min(c(.data[[mean_col]], 0)),
                pval = dplyr::first(.data[[p_col]]),
                lbl = dplyr::first(.data$.lbl),
                .groups = "drop"
            ) |>
            dplyr::filter(
                !is.na(pval),
                !is.na(lbl),
                lbl != ""
            ) |>
            dplyr::mutate(
                .y_range = .y_max - .y_min,
                .y_gap = .y_range * bracket_gap,
                y_top = pmax(y_left, y_right) +
                    .y_gap +
                    .y_range * bracket_offset
            )

        if (nrow(bracket_df) > 0L) {
            # Three segments per bracket: horizontal cap, left tick, right tick.
            seg_df <- dplyr::bind_rows(
                dplyr::mutate(
                    bracket_df,
                    x = x_left,
                    xend = x_right,
                    y = y_top,
                    yend = y_top
                ),
                dplyr::mutate(
                    bracket_df,
                    x = x_left,
                    xend = x_left,
                    y = y_left + .y_gap,
                    yend = y_top
                ),
                dplyr::mutate(
                    bracket_df,
                    x = x_right,
                    xend = x_right,
                    y = y_right + .y_gap,
                    yend = y_top
                )
            )
            text_df <- dplyr::mutate(
                bracket_df,
                x = x_mid,
                y = y_top + .y_range * bracket_text_gap
            )
        }
    }

    # -- Base plot --
    p <- ggplot2::ggplot(
        df,
        ggplot2::aes(
            x = .data$.x_pos,
            y = .data[[mean_col]],
            fill = .data[[condition_col]]
        )
    ) +
        ggplot2::geom_col(width = bar_width) +
        ggplot2::scale_fill_manual(
            values = setNames(bar_colors, cond_levels)
        ) +
        ggplot2::scale_x_continuous(
            breaks = c(x_left, x_right),
            labels = cond_levels,
            expand = ggplot2::expansion(add = bar_padding)
        ) +
        ggplot2::labs(x = NULL, y = y_label) +
        ggplot2::theme(legend.position = "none")

    # -- Error bars --
    if (error_direction == "both") {
        p <- p +
            ggplot2::geom_errorbar(
                ggplot2::aes(
                    ymin = .data[[mean_col]] - .data[[error_col]],
                    ymax = .data[[mean_col]] + .data[[error_col]]
                ),
                width = 0.15,
                linewidth = 0.5
            )
    } else {
        # "up": vertical stem from bar top, horizontal cap at top only
        p <- p +
            ggplot2::geom_linerange(
                ggplot2::aes(
                    ymin = .data[[mean_col]],
                    ymax = .data[[mean_col]] + .data[[error_col]]
                ),
                linewidth = 0.5
            ) +
            ggplot2::geom_errorbar(
                ggplot2::aes(
                    ymin = .data[[mean_col]] + .data[[error_col]],
                    ymax = .data[[mean_col]] + .data[[error_col]]
                ),
                width = 0.15,
                linewidth = 0.5
            )
    }

    # -- Overlay significance brackets --
    # seg_df / text_df retain all grouping columns from df, so any
    # facet_wrap() / facet_grid() added by the caller automatically
    # splits the brackets into the correct panels.
    if (!is.null(seg_df)) {
        p <- p +
            ggplot2::geom_segment(
                data = seg_df,
                ggplot2::aes(x = x, xend = xend, y = y, yend = yend),
                inherit.aes = FALSE,
                linewidth = 0.5
            ) +
            ggplot2::geom_text(
                data = text_df,
                ggplot2::aes(x = x, y = y, label = lbl),
                inherit.aes = FALSE,
                size = text_size,
                vjust = 0
            )
    }

    p
}
