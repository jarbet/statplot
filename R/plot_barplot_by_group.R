#' Bar plot comparing a numeric outcome between two conditions
#'
#' Draws a grouped bar chart (one facet per group) comparing a numeric outcome
#' between exactly two conditions. Bar height = mean (or any effect size); error
#' bars span ±1 error unit (SE, SD, CI half-width, etc.). A significance
#' bracket with optional label is drawn above bars when the corresponding
#' p-value falls below \code{p_cutoff}.
#'
#' @param df Data frame in **long format** — one row per group × condition
#'   combination.
#' @param group_col Column name for independent groups shown as facets.
#'   Default \code{"group"}.
#' @param condition_col Column name for the two conditions to compare.
#'   Default \code{"condition"}.
#' @param mean_col Column name for bar heights (means or effect sizes).
#'   Default \code{"mean"}.
#' @param error_col Column name for error-bar half-widths (SE, SD, CI
#'   half-width, etc.). Default \code{"se"}.
#' @param error_direction Direction of error bars. \code{"both"} draws
#'   \code{mean ± error}; \code{"up"} draws only the upper whisker
#'   (\code{mean} to \code{mean + error}). Default \code{"both"}.
#' @param p_col Column name for p-values. The value should be the same for
#'   both rows belonging to a group (i.e. repeated). Set to \code{NULL} to
#'   suppress brackets entirely. Default \code{"p_value"}.
#' @param label_col Optional column name supplying custom bracket label text
#'   (e.g. \code{"OR = 1.5 [1.1-2.0], p = 0.012"}). When \code{NULL} labels
#'   are auto-formatted as \code{"p = <value>"} using \code{\link{signif}}.
#' @param condition_order Length-2 character vector setting the left-to-right
#'   display order of the two conditions. Defaults to the existing factor level
#'   order or alphabetical.
#' @param p_cutoff Significance threshold; brackets appear only when
#'   \code{p < p_cutoff}. Default \code{0.05}.
#' @param y_label Y-axis label. Default \code{"Outcome"}.
#' @param bar_colors Length-2 fill colour vector applied to the two conditions
#'   in the order given by \code{condition_order}. Default
#'   \code{c("black", "grey70")}.
#' @param bar_width Width of the bars passed to \code{\link[ggplot2]{geom_col}}.
#'   Default \code{0.4}.
#' @param bar_gap Gap between the two bars within each facet, in x-axis units.
#'   Default \code{0.6} (matches the original discrete-axis spacing).
#' @param bar_padding White space added to the left and right of the bars
#'   within each facet panel, in x-axis units. Passed to
#'   \code{ggplot2::expansion(add = bar_padding)}. Default \code{0.5}.
#' @param text_size Size of bracket label text (ggplot2 \code{size} units).
#'   Default \code{3.5}.
#' @param bracket_offset Fraction of the y range used as vertical spacing
#'   above bar tops for bracket placement. Default \code{0.08}.
#' @param bracket_gap Fraction of the y range inserted as white space between
#'   the top of each error bar and the start of the significance bracket tick.
#'   Default \code{0.04}.
#' @param bracket_text_gap Fraction of the y range used as white space between
#'   the horizontal bracket line and the label text above it.
#'   Default \code{0.024}.
#' @param facet Whether to use faceting. Default \code{TRUE}. When \code{FALSE},
#'   \code{strip_position} is forced to \code{"bottom"}.
#' @param strip_position Controls where the facet strip label is placed.
#'   One of \code{"top"} (default when \code{facet = TRUE}), \code{"bottom"},
#'   \code{"left"}, or \code{"right"}. Ignored when \code{facet = FALSE}
#'   (always uses \code{"bottom"}).
#'
#' @return A \code{\link[ggplot2]{ggplot}} object.
#' @export
#'
#' @examples
#' library(ggplot2)
#' df <- data.frame(
#'     group     = rep(c("Group A", "Group B"), each = 2),
#'     condition = rep(c("Exercise", "Control"), 2),
#'     mean      = c(10.2, 14.8, 12.5, 13.1),
#'     se        = c(0.9, 1.0, 1.1, 1.0),
#'     p_value   = c(0.004, 0.004, 0.18, 0.18)
#' )
#' df$condition <- factor(df$condition, levels = c("Exercise", "Control"))
#' # Default: auto-formatted p-value label
#' plot_barplot_by_group(df, y_label = "Performance score") + ggplot2::theme_bw()
#'
#' # Custom bracket label from a column
#' df$label <- ifelse(
#'     df$p_value < 0.05,
#'     paste0("d = 1.5 [1.1-2.1]\n", format_pvalue(df$p_value)),
#'     NA
#' )
#' plot_barplot_by_group(df, y_label = "Performance score", label_col = "label") +
#'     ggplot2::coord_cartesian(ylim = c(0, 20)) +
#'     ggplot2::theme_bw()
#' @importFrom stats setNames as.formula
plot_barplot_by_group <- function(
    df,
    group_col = "group",
    condition_col = "condition",
    mean_col = "mean",
    error_col = "se",
    error_direction = "up",
    p_col = "p_value",
    label_col = NULL,
    condition_order = NULL,
    p_cutoff = 0.05,
    y_label = "Outcome",
    bar_colors = c("black", "grey70"),
    bar_width = 0.4,
    bar_gap = 0.6,
    bar_padding = 0.5,
    text_size = 3.5,
    bracket_offset = 0.08,
    bracket_gap = 0.04,
    bracket_text_gap = 0.024,
    facet = TRUE,
    strip_position = "top"
) {
    # ── Input validation ──────────────────────────────────────────────────────
    stopifnot(is.data.frame(df))
    required <- c(group_col, condition_col, mean_col, error_col)
    missing <- setdiff(required, names(df))
    if (length(missing)) {
        stop("Column(s) not found in `df`: ", paste(missing, collapse = ", "))
    }

    stopifnot(is.logical(facet))
    strip_position <- match.arg(
        strip_position,
        c("top", "bottom", "left", "right")
    )
    # When facet = FALSE, override strip_position to "bottom"
    if (!facet) {
        strip_position <- "bottom"
    }

    # ── Condition factor ordering ─────────────────────────────────────────────
    if (!is.null(condition_order)) {
        # Validate condition_order
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
        missing <- setdiff(condition_order, condition_values)
        if (length(missing)) {
            stop(
                "Values in `condition_order` not found in `",
                condition_col,
                "`: ",
                paste(missing, collapse = ", ")
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

    # ── Validate long-format: each group must have exactly 2 rows ─────────────
    group_condition_counts <- df |>
        dplyr::count(.data[[group_col]], .data[[condition_col]]) |>
        dplyr::group_by(.data[[group_col]]) |>
        dplyr::summarize(n_conditions = dplyr::n(), .groups = "drop")

    invalid_groups <- group_condition_counts[
        group_condition_counts$n_conditions != 2L,
    ]
    if (nrow(invalid_groups) > 0L) {
        stop(
            "Long-format requirement violated: each group must have exactly ",
            "one row per condition (2 rows total). Groups with incorrect counts: ",
            paste(invalid_groups[[group_col]], collapse = ", ")
        )
    }

    stopifnot(length(bar_colors) == 2L)
    error_direction <- match.arg(error_direction, c("both", "up"))

    # ── Bar x positions (continuous axis) ─────────────────────────────────────
    x_left <- 1
    x_right <- x_left + bar_width + bar_gap
    x_mid <- (x_left + x_right) / 2
    df$.x_pos <- ifelse(df[[condition_col]] == cond_levels[1], x_left, x_right)

    # ── Y offset used to space brackets above bar tops ────────────────────────
    bar_tops <- df[[mean_col]] + df[[error_col]]
    ymax <- max(bar_tops, na.rm = TRUE)
    ymin <- min(c(df[[mean_col]], 0), na.rm = TRUE)
    y_offset <- (ymax - ymin) * bracket_offset
    y_gap <- (ymax - ymin) * bracket_gap

    # ── Build significance bracket data ───────────────────────────────────────
    seg_df <- NULL
    text_df <- NULL

    # Validate p_col if provided
    if (!is.null(p_col) && !(p_col %in% names(df))) {
        stop("Column `", p_col, "` not found in `df`")
    }

    draw_brackets <- !is.null(p_col)

    if (draw_brackets) {
        # Determine label text for each row (NA → suppress bracket)
        if (!is.null(label_col) && label_col %in% names(df)) {
            df$.lbl <- df[[label_col]]
        } else {
            df$.lbl <- ifelse(
                !is.na(df[[p_col]]) & df[[p_col]] < p_cutoff,
                paste0("p = ", signif(df[[p_col]], 2)),
                NA_character_
            )
        }

        # Summarise to one row per group: bar-top y values for left & right bars
        # Select by condition name (not position) to be robust against row ordering
        bracket_df <- df |>
            dplyr::mutate(.bar_top = .data[[mean_col]] + .data[[error_col]]) |>
            dplyr::arrange(.data[[group_col]], .data[[condition_col]]) |>
            dplyr::group_by(.data[[group_col]]) |>
            dplyr::mutate(
                .cond_is_left = .data[[condition_col]] == cond_levels[1],
                .cond_is_right = .data[[condition_col]] == cond_levels[2]
            ) |>
            dplyr::summarize(
                y_left = .bar_top[.cond_is_left],
                y_right = .bar_top[.cond_is_right],
                pval = dplyr::first(.data[[p_col]]),
                lbl = dplyr::first(.data$.lbl),
                .groups = "drop"
            ) |>
            dplyr::filter(
                !is.na(pval),
                pval < p_cutoff,
                !is.na(lbl),
                lbl != ""
            ) |>
            dplyr::mutate(
                y_top = pmax(y_left, y_right) + y_gap + y_offset * 0.5
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
                    y = y_left + y_gap,
                    yend = y_top
                ),
                dplyr::mutate(
                    bracket_df,
                    x = x_right,
                    xend = x_right,
                    y = y_right + y_gap,
                    yend = y_top
                )
            )
            text_df <- dplyr::mutate(
                bracket_df,
                x = x_mid,
                y = y_top + (ymax - ymin) * bracket_text_gap
            )
        }
    }

    # ── Base plot ─────────────────────────────────────────────────────────────
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
        ggplot2::theme(
            legend.position = "none"
        )

    # ── Faceting ──────────────────────────────────────────────────────────────
    p <- p +
        ggplot2::facet_wrap(
            as.formula(paste("~", group_col)),
            strip.position = strip_position
        ) +
        ggplot2::theme(
            strip.text = ggplot2::element_text(face = "bold"),
            strip.placement = if (strip_position == "bottom") {
                "outside"
            } else {
                "inside"
            }
        )

    # ── Error bars ────────────────────────────────────────────────────────────
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

    # ── Overlay significance brackets ─────────────────────────────────────────
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
