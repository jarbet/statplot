#' Stacked percent bar chart for two categorical variables
#'
#' Create a stacked percent bar chart of an x categorical variable broken down by a
#' second categorical variable. The plot includes within-group percentages, group
#' sample sizes above each bar, and (optionally) a subtitle showing Cramer's V and
#' the test p-value.
#'
#' @param d A data.frame containing the variables.
#' @param xvar Character scalar, name of the categorical x variable (column in d).
#' @param yvar Character scalar, name of the categorical y variable (column in d).
#' @param xvar_label Optional character scalar to use for the x axis label.
#' @param yvar_label Optional character scalar to use for the legend label.
#' @param title Optional character scalar for the plot title.
#' @param title_nchar_wrap Integer scalar. Maximum number of characters per line of title.
#' @param show_effect_size Logical; if TRUE the subtitle will include Cramer's V and p-value.
#' @param yvar_colors Optional character vector of colours to use for the y-variable levels.
#'   Must be length equal to `nlevels(d[[yvar]])`. Provide colour names (e.g. "red") or hex
#'   codes (e.g. "#FF0000"). If NULL (default) ggplot2's default palette is used.
#' @param n_pct_size Numeric scalar. Point size used for the percent labels inside
#'   the stacked bars and for the group N labels above bars. Must be a single
#'   positive numeric value.
#' @param inside_bar_stats Character scalar controlling what statistics are
#'   printed inside the stacked bars. One of `"pct"` (default; shows within-group
#'   percentage), `"n"` (shows count only), `"pct_and_n"` (shows percentage with
#'   count in parentheses, e.g. "12% (34)"), or `"none"` (no labels inside bars).
#' @param pct_digits Integer scalar (default 0). Number of decimal places to show
#'   for the within-group percent labels (e.g. 0 => "12%", 1 => "12.3%"). Must be
#'   a single non-negative numeric value.
#' @param flip Logical scalar (default FALSE). If TRUE the plot is flipped to show
#'   horizontal bars.
#' @param plot_horizontal Logical scalar. Deprecated/alias for `flip`. If TRUE the plot
#'   will be shown with horizontal bars. Prefer using `flip`.
#' @param xaxis_labels_nchar_wrap Integer scalar. Maximum number of characters
#'   per line for x-axis group labels. Longer labels will be wrapped to multiple lines.
#' @param y_max Numeric scalar. Maximum y-axis limit for the plot.
#' @param n_ypos Numeric scalar. y-axis position for the group N labels.
#' @param include_overall_bar Logical scalar (default FALSE). If TRUE, a pooled
#'   "Overall" bar showing the marginal distribution of `yvar` across all
#'   observations is prepended to the left of the per-group bars, separated by a
#'   solid vertical line.
#' @param overall_label Character scalar (default `"Overall"`). Label used for
#'   the pooled bar when `include_overall_bar = TRUE`.
#' @return A list of the ggplot2 object, Cramer's V effect size, p-value.
#'
#' @examples
#' data(mtcars)
#' mtcars$cyl <- factor(mtcars$cyl)
#' mtcars$gear <- factor(mtcars$gear)
#' p <- plot_2_categorical_vars(
#'   d = mtcars,
#'   xvar = "cyl",
#'   yvar = "gear",
#'   xvar_label = 'Cylinders',
#'   yvar_label = 'Gears'
#' )
#' p$ggplot
#'
#' @export
plot_2_categorical_vars <- function(
    d,
    xvar,
    yvar,
    xvar_label = NULL,
    yvar_label = NULL,
    yvar_colors = NULL,
    title = NULL,
    title_nchar_wrap = 30,
    show_effect_size = TRUE,
    n_pct_size = 3.5,
    inside_bar_stats = c('pct', 'n', 'pct_and_n', 'none'),
    pct_digits = 0,
    plot_horizontal = FALSE,
    flip = FALSE,
    xaxis_labels_nchar_wrap = 20,
    y_max = 110,
    n_ypos = y_max - 5,
    include_overall_bar = FALSE,
    overall_label = "Overall"
) {
    ### Input checks
    inside_bar_stats <- match.arg(inside_bar_stats)
    stopifnot(
        length(pct_digits) == 1 & is.numeric(pct_digits) & pct_digits >= 0
    )
    stopifnot(is.data.frame(d))
    stopifnot(all(c(xvar, yvar) %in% names(d)))
    stopifnot(is.factor(d[[xvar]]) & is.factor(d[[yvar]]))

    stopifnot(
        is.null(xvar_label) |
            (length(xvar_label) == 1 & is.character(xvar_label))
    )
    stopifnot(
        is.null(yvar_label) |
            (length(yvar_label) == 1 & is.character(yvar_label))
    )
    stopifnot(
        is.null(title) |
            (length(title) == 1 & is.character(title))
    )
    yvar_nlevels <- nlevels(d[[yvar]])
    stopifnot(
        is.null(yvar_colors) |
            (is.vector(yvar_colors) &
                is.character(yvar_colors) &
                length(yvar_colors) == yvar_nlevels)
    )

    stopifnot(
        length(xaxis_labels_nchar_wrap) == 1 &
            is.numeric(xaxis_labels_nchar_wrap) &
            xaxis_labels_nchar_wrap > 0
    )

    stopifnot(
        length(y_max) == 1 & is.numeric(y_max) & y_max > 0
    )
    stopifnot(
        length(n_ypos) == 1 & is.numeric(n_ypos)
    )
    stopifnot(
        length(include_overall_bar) == 1 & is.logical(include_overall_bar)
    )
    stopifnot(
        length(overall_label) == 1 & is.character(overall_label)
    )
    stopifnot(
        length(title_nchar_wrap) == 1 &
            is.numeric(title_nchar_wrap) &
            title_nchar_wrap > 0
    )

    # --- added: validate yvar_colors when supplied ---
    yvar_nlevels <- nlevels(d[[yvar]])

    if (is.null(xvar_label)) {
        xvar_label <- xvar
    }
    if (is.null(yvar_label)) {
        yvar_label <- yvar
    }
    if (is.null(title)) {
        title <- paste0('Distribution of ', yvar_label, " by ", xvar_label)
    }
    ### Cramer's V effect size
    d_sub <- d[!is.na(d[[xvar]]) & !is.na(d[[yvar]]), ]
    tab <- table(d_sub[[xvar]], d_sub[[yvar]])
    v <- effectsize::cramers_v(
        tab,
        adjust = TRUE,
        alternative = 'two.sided'
    )
    v_text <- sprintf(
        'V = %.2f (%.2f, %.2f), p',
        as.numeric(v['Cramers_v_adjusted']),
        as.numeric(v['CI_low']),
        as.numeric(v['CI_high'])
    )

    ### test pvalue
    ch <- suppressWarnings(chisq.test(tab))
    pval <- ch$p.value
    if (any(ch$expected < 5)) {
        ft <- fisher.test(tab, simulate.p.value = TRUE, B = 100000)
        pval <- ft$p.value
    }

    pval_text <- BoutrosLab.plotting.general::display.statistical.result(
        x = pval,
        statistic.type = v_text,
        symbol = '= '
    )
    ### Make plot

    sprint_pct_digits <- paste0('%1.', pct_digits, 'f')

    plot_data <- d |>
        dplyr::filter(!is.na(.data[[xvar]]), !is.na(.data[[yvar]])) |>
        dplyr::count(.data[[xvar]], .data[[yvar]], name = "n") |>
        dplyr::group_by(.data[[xvar]]) |>
        dplyr::mutate(pct = n / sum(n) * 100) |>
        dplyr::ungroup() |>
        dplyr::mutate(
            bar_label = dplyr::case_when(
                inside_bar_stats == 'pct' ~ paste0(
                    sprintf(sprint_pct_digits, pct),
                    "%"
                ),
                inside_bar_stats == 'n' ~ format(
                    n,
                    big.mark = ",",
                    scientific = FALSE,
                    trim = TRUE
                ),
                inside_bar_stats == 'pct_and_n' ~ paste0(
                    sprintf(sprint_pct_digits, pct),
                    "% (",
                    format(n, big.mark = ",", scientific = FALSE, trim = TRUE),
                    ")"
                ),
                .default = NA_character_
            )
        )

    # --- new: prepend Overall bar if requested ---
    if (include_overall_bar) {
        existing_levels <- levels(plot_data[[xvar]])
        new_levels <- c(overall_label, existing_levels)

        overall_data <- d |>
            dplyr::filter(!is.na(.data[[xvar]]), !is.na(.data[[yvar]])) |>
            dplyr::count(.data[[yvar]], name = "n") |>
            dplyr::mutate(
                pct = n / sum(n) * 100,
                bar_label = dplyr::case_when(
                    inside_bar_stats == 'pct' ~ paste0(
                        sprintf(sprint_pct_digits, pct),
                        "%"
                    ),
                    inside_bar_stats == 'n' ~ format(
                        n,
                        big.mark = ",",
                        scientific = FALSE,
                        trim = TRUE
                    ),
                    inside_bar_stats == 'pct_and_n' ~ paste0(
                        sprintf(sprint_pct_digits, pct),
                        "% (",
                        format(
                            n,
                            big.mark = ",",
                            scientific = FALSE,
                            trim = TRUE
                        ),
                        ")"
                    ),
                    .default = NA_character_
                )
            )
        overall_data[[xvar]] <- factor(overall_label, levels = new_levels)
        plot_data[[xvar]] <- factor(
            as.character(plot_data[[xvar]]),
            levels = new_levels
        )
        plot_data <- dplyr::bind_rows(overall_data, plot_data)
    }

    # --- added: totals per x-group for labels ---
    total_n_labels <- plot_data |>
        dplyr::group_by(x = .data[[xvar]]) |>
        dplyr::summarize(total = sum(n), .groups = "drop") |>
        dplyr::mutate(
            label = paste0(
                "n=",
                format(total, big.mark = ",", scientific = FALSE, trim = TRUE)
            )
        )

    p <- ggplot2::ggplot(
        plot_data,
        ggplot2::aes(x = .data[[xvar]], y = pct, fill = .data[[yvar]])
    ) +
        ggplot2::geom_col() +
        # --- modified: add room above 100% so N labels aren't clipped ---
        ggplot2::scale_y_continuous(
            limits = c(0, y_max),
            breaks = seq(0, 100, by = 25),
            labels = scales::label_number(suffix = "%")
        ) +
        # --- added: place group sample sizes above each stacked bar ---
        ggplot2::geom_text(
            data = total_n_labels,
            ggplot2::aes(x = x, y = n_ypos, label = label),
            inherit.aes = FALSE,
            vjust = -0.2,
            size = n_pct_size
        ) +
        # use provided axis / legend labels
        ggplot2::labs(
            x = xvar_label,
            y = "Percent",
            fill = yvar_label
        ) +
        ggplot2::theme_bw()

    # inside-bar stats labels
    if (inside_bar_stats != 'none') {
        p <- p +
            ggplot2::geom_text(
                ggplot2::aes(label = bar_label),
                position = ggplot2::position_stack(vjust = 0.5),
                size = n_pct_size
            )
    }

    # --- added: apply supplied colors if provided (otherwise use ggplot defaults) ---
    if (!is.null(yvar_colors)) {
        p <- p + ggplot2::scale_fill_manual(values = yvar_colors)
    }

    # --- new: vertical separator between Overall bar and per-group bars ---
    if (include_overall_bar) {
        p <- p +
            ggplot2::geom_vline(
                xintercept = 1.5,
                linetype = "solid",
                color = "gray40",
                linewidth = 0.8
            )
    }
    # } else {
    #     p <- p +
    #         ggplot2::scale_fill_manual(
    #             values = BoutrosLab.plotting.general::default.colours(
    #                 yvar_nlevels
    #             )
    #         )
    # }

    # Add main title and put pval_text_label as the subtitle underneath
    if (!show_effect_size) {
        pval_text <- NULL
    }
    p <- p +
        ggplot2::ggtitle(
            stringr::str_wrap(title, width = title_nchar_wrap),
            subtitle = pval_text
        ) +
        ggplot2::theme(
            plot.title = ggplot2::element_text(face = "bold"),
            axis.title.x = ggplot2::element_text(face = "bold"),
            axis.title.y = ggplot2::element_text(face = "bold")
        )

    # prevent x-axis group labels from overlapping
    p <- p +
        ggplot2::scale_x_discrete(
            labels = scales::label_wrap(xaxis_labels_nchar_wrap)
        )
    # --- new: flip the plot to horizontal bars if requested ---
    if (isTRUE(flip)) {
        p <- p + ggplot2::coord_flip()
    }

    v2 <- data.frame(
        outcome = yvar,
        predictor = xvar,
        value = as.numeric(v$Cramers_v_adjusted),
        CI_low = as.numeric(v$CI_low),
        CI_high = as.numeric(v$CI_high),
        stringsAsFactors = FALSE,
        effect_type = "Cramers_v_adjusted",
        pvalue = pval,
        n = nrow(d_sub)
    )
    res <- list(
        ggplot = p,
        stats = v2
    )

    return(res)
}
