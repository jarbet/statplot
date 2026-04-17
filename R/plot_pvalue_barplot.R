#' Plot p-value barplot
#'
#' Create a horizontal barplot of p-values (optionally -log10 transformed) with an
#' optional significance vertical line and optional fill mapping.
#'
#' @param data A data.frame or tibble containing the variables.
#' @param x Character, name of the column with raw p-values. \code{NA} values are
#'   allowed; rows with \code{NA} are retained on the y-axis but drawn without a bar.
#' @param y Character, name of the column for y-axis categories (factor or character).
#' @param fill Character or NULL, column name to use for fill; if NULL draw solid black bars.
#'   Ignored when also_show_qvalue = TRUE.
#' @param alpha Numeric significance threshold for the vertical line (default 0.05). When
#'   mlog10_transform_pvalue = TRUE the vertical line is drawn at -log10(alpha).
#' @param width Numeric bar width (passed to geom_col).
#' @param xlim Numeric vector of length 2 giving x-axis limits; computed if NULL.
#' @param xbreaks Numeric vector of x-axis breaks; computed if NULL.
#' @param xlab Character, x-axis label.
#' @param vline Logical, whether to draw a vertical line at alpha (or -log10(alpha)).
#' @param vline_linetype Character, linetype for the vertical line.
#' @param vline_color Character, color for the vertical line.
#' @param vline_legend Logical, whether to include a legend entry for the vertical
#'   significance line (default `TRUE`). When `TRUE` a legend key showing the
#'   `alpha` value is added; when `FALSE` the line is drawn without a legend.
#' @param show_y_labels Logical, whether to show y-axis labels (default FALSE).
#' @param mlog10_transform_pvalue Logical; when TRUE compute -log10(p) for plotting/order and
#'   format x-axis tick labels as p-values (10^-x).
#' @param exponentiate_labels Logical; when TRUE and mlog10_transform_pvalue = TRUE, format x-axis
#'   labels as expressions with superscripts (e.g., 1, 10^-1, 10^-2, ...) instead of decimal notation.
#' @param also_show_qvalue Logical; when TRUE compute FDR-adjusted q-values (Benjamini-Hochberg)
#'   (or use custom_qvalues if supplied) and draw two overlapping bars per row: both p and q
#'   are shown. Note on drawing order:
#'   - If mlog10_transform_pvalue = TRUE the p-value bar is drawn first (behind) and the q-value
#'     bar is drawn on top.
#'   - If mlog10_transform_pvalue = FALSE the q-value bar is drawn first (behind) and the p-value
#'     bar is drawn on top.
#'   When TRUE, the 'fill' argument is ignored and fixed colors are used for p/q bars.
#' @param custom_qvalues Character or NULL; column name in `data` containing user-supplied
#'   q-values. When supplied and `also_show_qvalue = TRUE`, these values are used instead of
#'   computing FDR-adjusted q-values. \code{NA} values are allowed and result in no
#'   q-value bar for that row.
#' @param color_qvalue Character, color for q-value bars when also_show_qvalue = TRUE.
#' @param color_pvalue Character, color for p-value bars when also_show_qvalue = TRUE.
#' @param legend_title Character, title for the legend when also_show_qvalue = TRUE.
#'   Default is 'Bar type'.
#' @param fill_colors Named character vector or NULL. When provided along with `fill`,
#'   specifies custom colors for the fill mapping. Names should match unique values in
#'   the fill column, e.g., c("GroupA" = "red", "GroupB" = "blue", "GroupC" = "green").
#' @return A `ggplot2` plot object.
#' @examples
#' ggplot2::theme_set(theme_bw2())
#' set.seed(123)
#' n <- 6
#' example_df <- tibble::tibble(
#'   cell_line = paste0("Cell", sprintf("%02d", 1:n)),
#'   pvalue = 10^(-runif(n, 0.2, 3.5)),
#'   group = rep(c("GroupA", "GroupB", "GroupC"), length.out = n)
#' )
#' example_df$cell_line <- factor(
#'   example_df$cell_line,
#'   levels = rev(example_df$cell_line)
#' )
#'
#' # Example 1: Default behavior - overlay p-values and q-values (FDR-adjusted)
#' plot_pvalue_barplot(
#'   data = example_df,
#'   x = "pvalue",
#'   y = "cell_line",
#'   mlog10_transform_pvalue = TRUE,
#'   also_show_qvalue = TRUE,
#'   show_y_labels = TRUE,
#'   color_pvalue = "black",
#'   color_qvalue = "lightgrey"
#' )
#'
#' # Example 2: Basic -log10 transformed p-value barplot with significance line
#' plot_pvalue_barplot(
#'   data = example_df,
#'   x = "pvalue",
#'   y = "cell_line",
#'   mlog10_transform_pvalue = TRUE,
#'   show_y_labels = TRUE,
#'   vline = TRUE,
#'   alpha = 0.05,
#'   also_show_qvalue = FALSE
#' )
#'
#' # Example 3: -log10 transformed p-values with exponential notation labels (1, 10^-1, 10^-2, ...)
#' plot_pvalue_barplot(
#'   data = example_df,
#'   x = "pvalue",
#'   y = "cell_line",
#'   mlog10_transform_pvalue = TRUE,
#'   exponentiate_labels = TRUE,
#'   xlim = c(0, 4),
#'   xbreaks = 0:4,
#'   show_y_labels = TRUE,
#'   vline = TRUE,
#'   also_show_qvalue = FALSE
#' )
#'
#' # Example 4: Raw p-value scale (no transformation) without significance line
#' plot_pvalue_barplot(
#'   data = example_df,
#'   x = "pvalue",
#'   y = "cell_line",
#'   mlog10_transform_pvalue = FALSE,
#'   show_y_labels = TRUE,
#'   vline = FALSE,
#'   also_show_qvalue = FALSE
#' )
#'
#' # Example 5: Colored bars by group using fill mapping
#' plot_pvalue_barplot(
#'   data = example_df,
#'   x = "pvalue",
#'   y = "cell_line",
#'   fill = "group",
#'   fill_colors = c("GroupA" = "#FF6B6B", "GroupB" = "#4ECDC4", "GroupC" = "#FFE66D"),
#'   mlog10_transform_pvalue = TRUE,
#'   show_y_labels = TRUE,
#'   vline = TRUE,
#'   vline_legend = TRUE,
#'   also_show_qvalue = FALSE
#' )
#'
#' # Example 6: Custom significance threshold and vline styling
#' plot_pvalue_barplot(
#'   data = example_df,
#'   x = "pvalue",
#'   y = "cell_line",
#'   mlog10_transform_pvalue = TRUE,
#'   alpha = 0.01,
#'   vline = TRUE,
#'   vline_color = "blue",
#'   vline_linetype = "solid",
#'   vline_legend = TRUE,
#'   show_y_labels = TRUE,
#'   also_show_qvalue = FALSE
#' )
#'
#' # Example 7: Custom q-values
#' example_df$custom_qvalue <- c(0.001, 0.005, 0.05, 0.1, 0.2, 0.5)
#' plot_pvalue_barplot(
#'   data = example_df,
#'   x = "pvalue",
#'   y = "cell_line",
#'   mlog10_transform_pvalue = TRUE,
#'   also_show_qvalue = TRUE,
#'   custom_qvalues = "custom_qvalue",
#'   show_y_labels = TRUE
#' )
#' @importFrom rlang sym
#' @importFrom ggplot2 ggplot aes geom_col scale_x_continuous scale_y_discrete scale_fill_manual labs geom_vline theme element_text element_rect theme_bw margin
#' @importFrom scales squish
#' @importFrom stats p.adjust
#' @export
plot_pvalue_barplot <- function(
    data,
    x,
    y,
    fill = NULL, # column name to use for fill; if NULL draw solid black bars
    alpha = 0.05, # significance threshold -> vertical line at -log10(alpha)
    width = 0.6,
    xlim = NULL, # computed depending on transformation
    xbreaks = NULL,
    xlab = 'p-value',
    vline = TRUE,
    vline_linetype = "dashed",
    vline_color = "red",
    vline_legend = TRUE,
    show_y_labels = FALSE, # whether to show y-axis labels (default FALSE)
    mlog10_transform_pvalue = TRUE, # when TRUE compute -log10(p) for plotting/order
    exponentiate_labels = FALSE, # when TRUE and mlog10_transform_pvalue = TRUE, use expression labels with superscripts
    also_show_qvalue = TRUE, # when TRUE compute q-values (FDR) and draw p-values (grey, behind) plus q-values (black, on top) per row
    custom_qvalues = NULL, # column name in `data` containing user-supplied q-values
    color_qvalue = 'grey',
    color_pvalue = 'black',
    legend_title = 'Bar type',
    fill_colors = NULL # named character vector for custom fill colors
) {
    # ---- simple argument checks ----
    stopifnot(is.data.frame(data))
    stopifnot(is.character(x), length(x) == 1)
    stopifnot(is.character(y), length(y) == 1)
    stopifnot(x %in% names(data), y %in% names(data))
    stopifnot(is.null(fill) || (is.character(fill) && length(fill) == 1))
    stopifnot(is.null(fill) || fill %in% names(data))
    stopifnot(is.numeric(alpha), length(alpha) == 1, alpha > 0, alpha < 1)
    stopifnot(is.numeric(width), length(width) == 1, width > 0)
    if (!is.null(xlim)) {
        stopifnot(is.numeric(xlim), length(xlim) == 2, xlim[1] < xlim[2])
    }
    if (!is.null(xbreaks)) {
        stopifnot(is.numeric(xbreaks))
    }
    stopifnot(is.character(xlab), length(xlab) == 1)
    stopifnot(is.logical(vline), length(vline) == 1)
    stopifnot(is.logical(vline_legend), length(vline_legend) == 1)
    stopifnot(is.character(vline_linetype), length(vline_linetype) == 1)
    stopifnot(is.character(vline_color), length(vline_color) == 1)
    stopifnot(is.logical(show_y_labels), length(show_y_labels) == 1)
    stopifnot(
        is.logical(mlog10_transform_pvalue),
        length(mlog10_transform_pvalue) == 1
    )
    stopifnot(is.logical(exponentiate_labels), length(exponentiate_labels) == 1)
    if (exponentiate_labels && !mlog10_transform_pvalue) {
        warning(
            "Argument 'exponentiate_labels = TRUE' has no effect when mlog10_transform_pvalue = FALSE."
        )
    }
    stopifnot(is.logical(also_show_qvalue), length(also_show_qvalue) == 1)
    stopifnot(
        is.null(custom_qvalues) ||
            (is.character(custom_qvalues) && length(custom_qvalues) == 1)
    )
    if (!is.null(custom_qvalues)) {
        stopifnot(custom_qvalues %in% names(data))
    }
    stopifnot(is.numeric(data[[x]]))
    .x_non_na <- data[[x]][!is.na(data[[x]])]
    stopifnot(all(is.finite(.x_non_na)))
    stopifnot(all(.x_non_na >= 0 & .x_non_na <= 1))
    if (mlog10_transform_pvalue) {
        stopifnot(all(.x_non_na > 0))
    }
    stopifnot(is.character(color_qvalue), length(color_qvalue) == 1)
    stopifnot(is.character(color_pvalue), length(color_pvalue) == 1)
    stopifnot(is.character(legend_title), length(legend_title) == 1)
    stopifnot(is.logical(also_show_qvalue), length(also_show_qvalue) == 1)
    stopifnot(
        is.null(fill_colors) ||
            (is.character(fill_colors) && !is.null(names(fill_colors)))
    )
    if (!is.null(fill_colors) && is.null(fill)) {
        warning("Argument 'fill_colors' is ignored when fill = NULL.")
    }
    if (!is.null(fill_colors) && also_show_qvalue) {
        warning(
            "Argument 'fill_colors' is ignored when also_show_qvalue = TRUE."
        )
    }
    if (!is.null(custom_qvalues)) {
        stopifnot(is.numeric(data[[custom_qvalues]]))
        .cq_non_na <- data[[custom_qvalues]][!is.na(data[[custom_qvalues]])]
        stopifnot(all(is.finite(.cq_non_na)))
        stopifnot(all(.cq_non_na >= 0 & .cq_non_na <= 1))
        if (mlog10_transform_pvalue) {
            stopifnot(all(.cq_non_na > 0))
        }
    }
    # ---- end checks ----

    # Dependencies: ggplot2, rlang, stats
    fill_null <- is.null(fill)

    # If also_show_qvalue requested compute q-values (FDR)
    if (also_show_qvalue) {
        if (!fill_null) {
            warning(
                "Argument 'fill' is ignored when also_show_qvalue = TRUE; p/q bars use fixed colors."
            )
        }
        if (!is.null(custom_qvalues)) {
            data[[".qvalue_raw"]] <- data[[custom_qvalues]]
        } else {
            # exclude NAs from BH correction so non-NA rows are not affected
            qv <- rep(NA_real_, nrow(data))
            non_na <- !is.na(data[[x]])
            qv[non_na] <- stats::p.adjust(data[[x]][non_na], method = "fdr")
            data[[".qvalue_raw"]] <- qv
        }
        stopifnot(is.numeric(data[[".qvalue_raw"]]))
        .qr_non_na <- data[[".qvalue_raw"]][!is.na(data[[".qvalue_raw"]])]
        stopifnot(all(is.finite(.qr_non_na)))
        stopifnot(all(.qr_non_na >= 0 & .qr_non_na <= 1))

        # create plotting columns for p and q (transformed if requested)
        if (mlog10_transform_pvalue) {
            data[[".plot_p"]] <- -log10(data[[x]])
            data[[".plot_q"]] <- -log10(data[[".qvalue_raw"]])
            # ensure transformed q is finite where not NA
            stopifnot(all(is.finite(data[[".plot_q"]][
                !is.na(data[[".plot_q"]])
            ])))
        } else {
            data[[".plot_p"]] <- data[[x]]
            data[[".plot_q"]] <- data[[".qvalue_raw"]]
        }
        # use these names downstream
        plot_x_p_name <- ".plot_p"
        plot_x_q_name <- ".plot_q"
    } else {
        # create plotting x column when requested (keeps original data untouched otherwise)
        plot_x_name <- x
        if (mlog10_transform_pvalue) {
            plot_x_name <- ".plot_mlog10p"
            data[[plot_x_name]] <- -log10(data[[x]])
        }
    }

    # set sensible defaults for xlim and xbreaks depending on whether we use -log10 scale
    if (is.null(xlim)) {
        if (mlog10_transform_pvalue) {
            xlim <- c(0, 3)
        } else {
            xlim <- c(0, 1)
        }
    }
    if (is.null(xbreaks)) {
        if (mlog10_transform_pvalue) {
            # Default p-value breaks: 1, 0.1, 0.01, 0.001 -> -log10 scale
            xbreaks <- -log10(c(1, 0.1, 0.01, 0.001))
        } else {
            xbreaks <- pretty(xlim, n = 5)
        }
    }

    # prepare quosures for tidy-eval in ggplot2::aes()
    if (also_show_qvalue) {
        p_sym <- rlang::sym(plot_x_p_name)
        q_sym <- rlang::sym(plot_x_q_name)
    } else {
        x_sym <- rlang::sym(plot_x_name)
    }
    y_sym <- rlang::sym(y)
    if (!fill_null) {
        fill_sym <- rlang::sym(fill)
    }

    # x-axis label function: if plotting -log10(p) show p on ticks (10^-x); otherwise show raw values
    x_label_fun <- function(b) {
        if (mlog10_transform_pvalue) {
            if (exponentiate_labels) {
                # Create expression labels with superscripts: 1, 10^-1, 10^-2, etc.
                # b is -log10(p), so exponents are -b
                exponents <- -b
                max_idx <- which.max(b)
                labels <- lapply(seq_along(exponents), function(i) {
                    exp_val <- exponents[i]
                    if (i == max_idx) {
                        bquote("<" * 10^.(exp_val))
                    } else {
                        bquote(10^.(exp_val))
                    }
                })
                return(as.expression(labels))
            } else {
                pvals <- 10^(-b)
                labs <- trimws(formatC(pvals, digits = 2, format = "g"))
                # Add < sign to the smallest p-value threshold (largest -log10 value)
                smallest_idx <- which.max(b)
                if (length(smallest_idx) > 0) {
                    labs[smallest_idx] <- paste0("<", labs[smallest_idx])
                }
                labs
            }
        } else {
            b
        }
    }

    # build base plot: handle the special also_show_qvalue case which draws two overlapping bars per row
    if (also_show_qvalue) {
        p <- ggplot2::ggplot(
            data,
            ggplot2::aes(
                y = !!y_sym
            )
        )

        if (mlog10_transform_pvalue) {
            # -log10(q) <= -log10(p), so draw p first (behind), q on top
            p <- p +
                ggplot2::geom_col(
                    ggplot2::aes(x = !!p_sym, fill = "p-value"),
                    width = width,
                    position = "identity",
                    alpha = 1
                ) +
                ggplot2::geom_col(
                    ggplot2::aes(x = !!q_sym, fill = "q-value"),
                    width = width,
                    position = "identity",
                    alpha = 1
                )
        } else {
            # q >= p (FDR adjustment), so draw q first (behind), p on top
            p <- p +
                ggplot2::geom_col(
                    ggplot2::aes(x = !!q_sym, fill = "q-value"),
                    width = width,
                    position = "identity",
                    alpha = 1
                ) +
                ggplot2::geom_col(
                    ggplot2::aes(x = !!p_sym, fill = "p-value"),
                    width = width,
                    position = "identity",
                    alpha = 1
                )
        }

        p <- p +
            ggplot2::scale_fill_manual(
                values = c("q-value" = color_qvalue, "p-value" = color_pvalue),
                breaks = c("q-value", "p-value"),
                labels = c("q-value", "p-value"),
                name = legend_title,
                guide = ggplot2::guide_legend(override.aes = list(linetype = 0))
            )
    } else {
        # previous behaviour: either fill mapping or solid black bars
        if (fill_null) {
            p <- ggplot2::ggplot(
                data,
                ggplot2::aes(
                    x = !!x_sym,
                    y = !!y_sym
                )
            ) +
                ggplot2::geom_col(width = width, fill = "black")
        } else {
            p <- ggplot2::ggplot(
                data,
                ggplot2::aes(
                    x = !!x_sym,
                    y = !!y_sym,
                    fill = !!fill_sym
                )
            ) +
                ggplot2::geom_col(width = width)

            if (!is.null(fill_colors)) {
                p <- p +
                    ggplot2::scale_fill_manual(values = fill_colors)
            }

            p <- p +
                ggplot2::guides(
                    fill = ggplot2::guide_legend(
                        override.aes = list(linetype = 0)
                    )
                )
        }
    }

    if (also_show_qvalue) {
        # show combined p/q axis label when showing both p- and q-value bars
        p <- p +
            ggplot2::labs(
                x = "p/q-value",
                y = NULL
            ) +
            ggplot2::theme(
                legend.position = "right",
                legend.direction = "vertical",
                legend.background = ggplot2::element_rect(
                    fill = NA,
                    color = NA
                ),
                legend.box.background = ggplot2::element_rect(
                    color = NA,
                    fill = NA
                )
            )
    } else {
        p <- p +
            ggplot2::labs(
                x = xlab,
                y = NULL
            )
    }

    if (vline) {
        # choose x intercept depending on whether we plot -log10(p) or raw p
        x_vline <- if (mlog10_transform_pvalue) -log10(alpha) else alpha
        if (vline_legend) {
            vline_label <- as.character(signif(alpha, 3))
            vline_df <- data.frame(x = x_vline, vline_label = vline_label)
            p <- p +
                ggplot2::geom_vline(
                    data = vline_df,
                    ggplot2::aes(xintercept = x, color = vline_label),
                    linetype = vline_linetype,
                    show.legend = TRUE
                ) +
                ggplot2::scale_color_manual(
                    values = setNames(vline_color, vline_label),
                    name = NULL,
                    guide = ggplot2::guide_legend(
                        override.aes = list(linetype = vline_linetype)
                    )
                )
        } else {
            p <- p +
                ggplot2::geom_vline(
                    xintercept = x_vline,
                    linetype = vline_linetype,
                    color = vline_color
                )
        }

        # Ensure the colour scale stores the explicit named values vector so tests
        # that inspect `scale$values` find the expected named entry.
        if (vline && vline_legend) {
            sc <- p$scales$get_scales("colour")
            if (
                !is.null(sc) && (is.null(sc$values) || length(sc$values) == 0)
            ) {
                vline_label <- as.character(signif(alpha, 3))
                sc$values <- setNames(vline_color, vline_label)
            }
        }
    }

    # hide y-axis labels by setting their font size to 0 when requested
    if (!show_y_labels) {
        p <- p + ggplot2::theme(axis.text.y = ggplot2::element_text(size = 0))
    } else {
        p <- p +
            ggplot2::theme(axis.text.y = ggplot2::element_text(face = "bold"))
    }

    # ensure discrete y uses an explicit ordering (works for factor or character)
    y_levels <- if (is.factor(data[[y]])) {
        levels(data[[y]])
    } else {
        unique(data[[y]])
    }
    p <- p + ggplot2::scale_y_discrete(limits = y_levels, expand = c(0, 0))

    p <- p +
        ggplot2::scale_x_continuous(
            expand = c(0, 0),
            limits = xlim,
            breaks = xbreaks,
            labels = x_label_fun,
            oob = scales::squish
        ) + # add extra right padding so the barplot has more white space on the right
        ggplot2::theme(
            plot.margin = ggplot2::margin(
                t = 5.5,
                r = 30,
                b = 5.5,
                l = 5.5,
                unit = "pt"
            )
        )

    p
}
