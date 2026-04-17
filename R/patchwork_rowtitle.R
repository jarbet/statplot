#' Create a Row Title for Patchwork
#'
#' Creates a plot with a centered text label designed to be used as a row
#' title in a multi-row patchwork composition. The plot has a minimal theme
#' with a customizable background and bold text.
#'
#' @param label Character string for the row title label.
#' @param x Numeric x position of text (0 to 1). Default: 0.5 (center).
#' @param y Numeric y position of text (0 to 1). Default: 0.5 (center).
#' @param size Numeric text size. Default: 5.5.
#' @param hjust Numeric horizontal justification (0 to 1). Default: 0.5 (center).
#' @param vjust Numeric vertical justification (0 to 1). Default: 0.5 (center).
#' @param fill Character color for plot background. Default: "grey92".
#' @param color Character color for plot border. Default: "black".
#' @param linewidth Numeric width of plot border. Default: 1.5.
#' @param margin_top Numeric top margin in points. Default: 6.
#' @param margin_right Numeric right margin in points. Default: 0.
#' @param margin_bottom Numeric bottom margin in points. Default: 0.
#' @param margin_left Numeric left margin in points. Default: 0.
#'
#' @return A ggplot object suitable for use in a patchwork composition.
#'
#' @examples
#' # Create sample plots
#' p1 <- ggplot2::ggplot(mtcars, ggplot2::aes(wt, mpg)) +
#'   ggplot2::geom_point() +
#'   ggplot2::labs(title = "Plot 1")
#' p2 <- ggplot2::ggplot(mtcars, ggplot2::aes(wt, hp)) +
#'   ggplot2::geom_point() +
#'   ggplot2::labs(title = "Plot 2")
#' p3 <- ggplot2::ggplot(mtcars, ggplot2::aes(factor(cyl), mpg)) +
#'   ggplot2::geom_boxplot() +
#'   ggplot2::labs(title = "Plot 3")
#' p4 <- ggplot2::ggplot(mtcars, ggplot2::aes(factor(cyl), hp)) +
#'   ggplot2::geom_boxplot() +
#'   ggplot2::labs(title = "Plot 4")
#'
#' # Use case 1: Row title as an entire row (spanning all columns)
#' row_title_1 <- patchwork_rowtitle("Group A")
#' row_title_2 <- patchwork_rowtitle("Group B")
#' patchwork1 <- p1 + p2
#' patchwork2 <- p3 + p4
#'
#' row_title_1 +
#'   patchwork1 +
#'   row_title_2 +
#'   patchwork2 +
#'   patchwork::plot_layout(ncol = 1, heights = c(0.2, 1, 0.2, 1))
#'
#' # Use case 2: Row title as a box to the left (narrow column)
#' row_title_left_1 <- patchwork_rowtitle("Group\nA")
#' row_title_left_2 <- patchwork_rowtitle("Group\nB")
#'
#' row_title_left_1 + patchwork1 +
#'   row_title_left_2 + patchwork2 +
#'   patchwork::plot_layout(ncol = 2, widths = c(0.2, 1))
#'
#' @export
patchwork_rowtitle <- function(
    label,
    x = 0.5,
    y = 0.5,
    size = 5.5,
    hjust = 0.5,
    vjust = 0.5,
    fill = "grey92",
    color = "black",
    linewidth = 1.5,
    margin_top = 6,
    margin_right = 0,
    margin_bottom = 0,
    margin_left = 0
) {
    ggplot2::ggplot() +
        ggplot2::annotate(
            "text",
            x = x,
            y = y,
            label = label,
            size = size,
            fontface = "bold",
            hjust = hjust,
            vjust = vjust
        ) +
        ggplot2::scale_x_continuous(limits = c(0, 1), expand = c(0, 0)) +
        ggplot2::scale_y_continuous(limits = c(0, 1), expand = c(0, 0)) +
        ggplot2::theme_void() +
        ggplot2::theme(
            plot.background = ggplot2::element_rect(
                fill = fill,
                color = color,
                linewidth = linewidth
            ),
            plot.margin = ggplot2::margin(
                margin_top,
                margin_right,
                margin_bottom,
                margin_left
            )
        )
}
