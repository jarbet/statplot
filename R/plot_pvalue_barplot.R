#' Plot p-value barplot
#'
#' Create a horizontal barplot of p-values (optionally -log10 transformed) with an
#' optional significance vertical line and optional fill mapping.
#'
#' @param data A data.frame or tibble containing the variables.
#' @param x Character, name of the column with raw p-values (default \"pvalue\").
#' @param y Character, name of the column for y-axis categories (default \"cell_line\").
#' @param fill Character or NULL, column name to use for fill; if NULL draw solid black bars.
#' @param alpha Numeric significance threshold for the vertical line (default 0.05).
#' @param width Numeric bar width.
#' @param xlim Numeric vector of length 2 giving x-axis limits; computed if NULL.
#' @param xbreaks Numeric vector of x-axis breaks; computed if NULL.
#' @param xlab Character, x-axis label.
#' @param vline Logical, whether to draw a vertical line at alpha (or -log10(alpha)).
#' @param vline_linetype Character, linetype for the vertical line.
#' @param vline_color Character, color for the vertical line.
#' @param show_y_labels Logical, whether to show y-axis labels (default FALSE).
#' @param mlog10_transform_pvalue Logical; when TRUE compute -log10(p) for plotting/order.
#' @return A ggplot2 object (invisible when returned from functions).
#' @examples
#' set.seed(123)
#' n <- 4
#' example_df <- tibble::tibble(
#'   cell_line = paste0("Cell", sprintf("%02d", 1:n)),
#'   pvalue = 10^(-runif(n, 0.2, 2.8)),
#'   group = rep(c("A", "B"), length.out = n)
#' )
#' example_df$cell_line <- factor(
#'   example_df$cell_line,
#'   levels = rev(example_df$cell_line)
#' )
#' plot_pvalue_barplot(
#'   data = example_df,
#'   x = "pvalue",
#'   y = "cell_line",
#'   fill = NULL,
#'   mlog10_transform_pvalue = TRUE,
#'   show_y_labels = TRUE
#' )
#' @importFrom rlang sym
#' @importFrom ggplot2 ggplot aes geom_col scale_x_continuous labs geom_vline theme element_text
#' @export
plot_pvalue_barplot <- function(
    data,
    x = "pvalue", # now expects raw p-values by default
    y = "cell_line",
    fill = NULL, # column name to use for fill; if NULL draw solid black bars
    alpha = 0.05, # significance threshold -> vertical line at -log10(alpha)
    width = 0.6,
    xlim = NULL, # computed depending on transformation
    xbreaks = NULL,
    xlab = 'p-value',
    vline = TRUE,
    vline_linetype = "dashed",
    vline_color = "red",
    show_y_labels = FALSE, # whether to show y-axis labels (default FALSE)
    mlog10_transform_pvalue = FALSE # when TRUE compute -log10(p) for plotting/order
) {
    # Dependencies: ggplot2, rlang
    fill_null <- is.null(fill)

    # create plotting x column when requested (keeps original data untouched otherwise)
    plot_x_name <- x
    if (mlog10_transform_pvalue) {
        plot_x_name <- ".plot_mlog10p"
        data[[plot_x_name]] <- -log10(data[[x]])
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
            xbreaks <- seq(xlim[1], xlim[2], by = 1)
        } else {
            xbreaks <- pretty(xlim, n = 5)
        }
    }

    # prepare quosures for tidy-eval in ggplot2::aes()
    x_sym <- rlang::sym(plot_x_name)
    y_sym <- rlang::sym(y)
    if (!fill_null) {
        fill_sym <- rlang::sym(fill)
    }

    # x-axis label function: if plotting -log10(p) show p on ticks (10^-x); otherwise show raw values
    x_label_fun <- function(b) {
        if (mlog10_transform_pvalue) {
            pvals <- 10^(-b)
            labs <- formatC(pvals, digits = 2, format = "g")
            maxb <- max(xbreaks)
            is_max <- abs(b - maxb) < .Machine$double.eps^0.5
            if (any(is_max)) {
                labs[is_max] <- paste0("\u2264", labs[is_max])
            }
            labs
        } else {
            b
        }
    }

    # build base plot: if fill is NULL draw solid black bars; otherwise map fill aesthetic
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
    }

    p <- p +
        ggplot2::scale_x_continuous(
            expand = c(0, 0),
            limits = xlim,
            breaks = xbreaks,
            labels = x_label_fun
        ) +
        ggplot2::labs(
            x = xlab,
            y = NULL
        )

    if (vline) {
        # choose x intercept depending on whether we plot -log10(p) or raw p
        x_vline <- if (mlog10_transform_pvalue) -log10(alpha) else alpha
        p <- p +
            ggplot2::geom_vline(
                xintercept = x_vline,
                linetype = vline_linetype,
                color = vline_color
            )
    }

    # hide y-axis labels by setting their font size to 0 when requested
    if (!show_y_labels) {
        p <- p + ggplot2::theme(axis.text.y = ggplot2::element_text(size = 0))
    }

    p
}

# Example: demo dataset and usage (only runs interactively)
set.seed(123)
n <- 4
example_df <- tibble::tibble(
    cell_line = paste0("Cell", sprintf("%02d", 1:n)),
    # generate raw p-values and show how to transform for plotting
    pvalue = 10^(-runif(n, 0.2, 2.8)),
    group = rep(c("A", "B"), length.out = n)
)
example_df$cell_line <- factor(
    example_df$cell_line,
    levels = rev(example_df$cell_line)
) # preserve order in plot

plot_pvalue_barplot(
    data = example_df,
    x = "pvalue",
    y = "cell_line",
    fill = NULL,
    mlog10_transform_pvalue = TRUE, # compute -log10(p) internally
    show_y_labels = TRUE
)
