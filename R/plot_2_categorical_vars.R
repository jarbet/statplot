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
#' @param title_nchar_wrap Optional integer scalar. Maximum number of characters per line of title.
#'   If NULL (default), no wrapping is applied.
#' @param show_effect_size Logical; if TRUE the subtitle will include Cramer's V and p-value.
#' @param yvar_colors Optional character vector of colours to use for the y-variable levels.
#'   Can be either:
#'   - An unnamed vector of length equal to `nlevels(d[[yvar]])` (positional mapping)
#'   - A named character vector with names corresponding to `yvar` levels (partial or complete mapping)
#'
#'   Provide colour names (e.g. "red") or hex codes (e.g. "#FF0000").
#'   If using a named vector, unmapped levels will be filled with ggplot2's default palette.
#'   If NULL (default), ggplot2's default palette is used for all levels.
#' @param yvar_text_colors Optional named character vector of colors for the text inside bars.
#'   Names should correspond to the levels of `yvar`. If a level is not included in the vector,
#'   the text color defaults to black. If NULL (default), all text is black.
#' @param n_pct_size Numeric scalar. Point size used for the percent labels inside
#'   the stacked bars and for the group N labels above bars. Must be a single
#'   positive numeric value.
#' @param inside_bar_stats Character scalar controlling what statistics are
#'   printed inside the stacked bars. One of `"pct"` (default; shows within-group
#'   percentage), `"n"` (shows count only), `"pct_and_n"` (shows percentage with
#'   count in parentheses, e.g. "12% (34)"), or `"none"` (no labels inside bars).
#' @param inside_bar_text_bold Logical scalar (default FALSE). If TRUE, the text
#'   inside the bars will be displayed in bold.
#' @param pct_digits Integer scalar (default 0). Number of decimal places to show
#'   for the within-group percent labels (e.g. 0 => "12%", 1 => "12.3%"). Must be
#'   a single non-negative numeric value.
#' @param flip Logical scalar (default FALSE). If TRUE the plot is flipped to show
#'   horizontal bars.
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
#' ggplot2::theme_set(theme_bw2())
#' data(mtcars)
#' mtcars$cyl <- factor(mtcars$cyl)
#' mtcars$gear <- factor(mtcars$gear)
#'
#' # Default usage
#' p <- plot_2_categorical_vars(
#'   d = mtcars,
#'   xvar = "cyl",
#'   yvar = "gear",
#'   xvar_label = "Cylinders",
#'   yvar_label = "Gears"
#' )
#' p$ggplot
#'
#' # Show both percent and count inside bars
#' p_pct_n <- plot_2_categorical_vars(
#'   d = mtcars,
#'   xvar = "cyl",
#'   yvar = "gear",
#'   inside_bar_stats = "pct_and_n",
#'   pct_digits = 1
#' )
#' p_pct_n$ggplot
#'
#' # Include a pooled 'Overall' bar on the left
#' p_overall <- plot_2_categorical_vars(
#'   d = mtcars,
#'   xvar = "cyl",
#'   yvar = "gear",
#'   include_overall_bar = TRUE
#' )
#' p_overall$ggplot
#'
#' # Horizontal bars using `flip = TRUE`
#' p_horiz <- plot_2_categorical_vars(
#'   d = mtcars,
#'   xvar = "cyl",
#'   yvar = "gear",
#'   flip = TRUE,
#'   inside_bar_stats = "pct_and_n"
#' )
#' p_horiz$ggplot
#'
#' # Customize text colors by yvar level, and use bold text inside bars
#' p_text_colors <- plot_2_categorical_vars(
#'   d = mtcars,
#'   xvar = "cyl",
#'   yvar = "gear",
#'   yvar_colors = c("3" = "lightgrey", "4" = "darkgrey", "5" = "black"),
#'   yvar_text_colors = c("3" = "black", "4" = "black", "5" = "white"),
#'   inside_bar_stats = "pct_and_n",
#'   inside_bar_text_bold = TRUE
#' )
#' p_text_colors$ggplot
#'
#' ######### Combine stacked barchart with a horizontal covariate bar
#' # The covariate heatmap is designed for group-level annotations where each
#' # x-axis group has exactly one value per covariate (e.g. treatment arms with
#' # fixed properties). Here we use simulated group-level covariates.

#'
#' # Create the main stacked barchart
#' p <- plot_2_categorical_vars(
#'   d = mtcars,
#'   xvar = "cyl",
#'   yvar = "gear",
#'   xvar_label = "Cylinders",
#'   yvar_label = "Gears",
#'   inside_bar_stats = "pct_and_n"
#' )
#'
#' # Simulated group-level covariates: one row per cylinder group, in the same
#' # order as the barplot x-axis (i.e. factor level order).
#' cov_data <- data.frame(
#'   cyl       = factor(c("4",     "6",        "8")),
#'   fuel_type = factor(c("Electric", "Gasoline", "Gasoline")),
#'   origin    = factor(c("A", "B", "C"))
#' )
#'
#' # Verify x-axis labels match before hiding the barplot x-axis.
#' # patchwork's axes = "collect_x" cannot reach into a nested patchwork,
#' # so we suppress the duplicate axis manually.
#' barplot_xlabels <- levels(mtcars[["cyl"]])
#' covbar_xlabels  <- as.character(cov_data[["cyl"]])
#' stopifnot(
#'   "x-axis labels of barplot and covariate bar must be identical" =
#'     identical(barplot_xlabels, covbar_xlabels)
#' )
#' p$ggplot <- p$ggplot +
#'   ggplot2::theme(
#'     axis.title.x = ggplot2::element_blank(),
#'     axis.text.x  = ggplot2::element_blank(),
#'     axis.ticks.x = ggplot2::element_blank()
#'   )
#'
#' # Create horizontal covariate bar.
#' # Use collect_guides = FALSE so the outer wrap_plots() can collect all
#' # legends together; if collect_guides = TRUE (the default) the inner
#' # patchwork absorbs the guides before the outer composition sees them.
#' cov_bar <- plot_covariate_heatmap(
#'   dataset = cov_data,
#'   color_map = list(
#'     fuel_type = c("Electric" = "#619CBA", "Gasoline" = "#F39C12"),
#'     origin    = c("A" = "#9B59B6", "B" = "#2ECC71", "C" = "#E67E22")
#'   ),
#'   row_id_var = "cyl",
#'   show_column_names = TRUE,
#'   show_row_names = TRUE,
#'   horizontal = TRUE,
#'   collect_guides = FALSE,
#'   x_title = "Cylinders"
#' ) &
#'   ggplot2::scale_x_discrete(expand = ggplot2::expansion(add = 0.6)) &
#'   ggplot2::theme(panel.border = ggplot2::element_blank())
#'
#' # Combine plots vertically with collected legends
#' patchwork::wrap_plots(
#'   p$ggplot +
#'     ggplot2::scale_y_continuous(
#'       limits = c(0, 110),
#'       breaks = seq(0, 100, by = 25),
#'       labels = scales::label_number(suffix = "%"),
#'       expand = ggplot2::expansion(add = c(0, 0))
#'     ),
#'   cov_bar,
#'   ncol = 1,
#'   heights = c(0.85, 0.15),
#'   guides = "collect"
#' ) & ggplot2::theme(legend.position = "right")
#'
#' @export
plot_2_categorical_vars <- function(
    d,
    xvar,
    yvar,
    xvar_label = NULL,
    yvar_label = NULL,
    yvar_colors = NULL,
    yvar_text_colors = NULL,
    title = NULL,
    title_nchar_wrap = NULL,
    show_effect_size = TRUE,
    n_pct_size = 3.5,
    inside_bar_stats = c('pct', 'n', 'pct_and_n', 'none'),
    inside_bar_text_bold = FALSE,
    pct_digits = 0,
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
                (length(yvar_colors) == yvar_nlevels |
                    !is.null(names(yvar_colors))))
    )
    if (!is.null(yvar_text_colors)) {
        stopifnot(
            is.vector(yvar_text_colors) &
                is.character(yvar_text_colors)
        )
        if (
            is.null(names(yvar_text_colors)) ||
                any(!nzchar(names(yvar_text_colors))) ||
                any(is.na(names(yvar_text_colors)))
        ) {
            stop(
                "`yvar_text_colors` must be a *named* character vector with ",
                "non-empty, non-NA names corresponding to `yvar` levels."
            )
        }
    }
    stopifnot(
        length(inside_bar_text_bold) == 1 & is.logical(inside_bar_text_bold)
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
        is.null(title_nchar_wrap) |
            (length(title_nchar_wrap) == 1 &
                is.numeric(title_nchar_wrap) &
                title_nchar_wrap > 0)
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
        statplot::theme_bw2()

    # inside-bar stats labels
    if (inside_bar_stats != 'none') {
        if (is.null(yvar_text_colors)) {
            # Default: all text is black, no color aesthetic or scale
            p <- p +
                ggplot2::geom_text(
                    ggplot2::aes(label = bar_label),
                    color = 'black',
                    position = ggplot2::position_stack(vjust = 0.5),
                    size = n_pct_size,
                    fontface = ifelse(inside_bar_text_bold, 'bold', 'plain'),
                    show.legend = FALSE
                )
        } else {
            # Custom text colors: map color aesthetic to yvar and use manual scale
            yvar_levels <- levels(plot_data[[yvar]])
            # Use provided colors where available, default to black otherwise
            text_color_map <- ifelse(
                yvar_levels %in% names(yvar_text_colors),
                yvar_text_colors[yvar_levels],
                'black'
            )
            names(text_color_map) <- yvar_levels

            p <- p +
                ggplot2::geom_text(
                    ggplot2::aes(label = bar_label, color = .data[[yvar]]),
                    position = ggplot2::position_stack(vjust = 0.5),
                    size = n_pct_size,
                    fontface = ifelse(inside_bar_text_bold, 'bold', 'plain'),
                    show.legend = FALSE
                ) +
                ggplot2::scale_color_manual(values = text_color_map)
        }
    }

    # --- added: apply supplied colors if provided (otherwise use ggplot defaults) ---
    if (!is.null(yvar_colors)) {
        # If yvar_colors is a named vector, fill in unmapped levels with defaults
        if (!is.null(names(yvar_colors))) {
            yvar_levels <- levels(plot_data[[yvar]])
            unmapped_levels <- setdiff(yvar_levels, names(yvar_colors))

            if (length(unmapped_levels) > 0) {
                # Use ggplot2's default scale to get colors for unmapped levels
                n_unmapped <- length(unmapped_levels)
                default_colors <- scales::hue_pal()(n_unmapped)

                # Create a complete color mapping
                complete_colors <- c(yvar_colors)
                for (i in seq_along(unmapped_levels)) {
                    complete_colors[unmapped_levels[i]] <- default_colors[i]
                }
                yvar_colors <- complete_colors
            }
        }
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
    # Apply title wrapping only if title_nchar_wrap is specified
    title_label <- if (is.null(title_nchar_wrap)) {
        title
    } else {
        stringr::str_wrap(title, width = title_nchar_wrap)
    }
    p <- p +
        ggplot2::ggtitle(
            title_label,
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
