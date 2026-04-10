#' Bold-styled Black and White Theme
#'
#' A modified ggplot2 black and white theme with bold text elements.
#'
#' @return A ggplot2 theme object
#' @export
#'
#' @examples
#' ggplot(mtcars, aes(x = wt, y = mpg)) +
#'   geom_point() +
#'   theme_bw2()
theme_bw2 <- function() {
    ggplot2::theme_bw() +
        ggplot2::theme(
            plot.title = ggplot2::element_text(face = "bold"),
            axis.title.x = ggplot2::element_text(face = "bold"),
            axis.title.y = ggplot2::element_text(face = "bold"),
            legend.title = ggplot2::element_text(face = "bold"),
            strip.text = ggplot2::element_text(face = "bold"),
            strip.text.y = ggplot2::element_text(face = "bold")
        )
}

#' Bold-styled Classic Theme
#'
#' A modified ggplot2 classic theme with bold text elements and centered title.
#'
#' @return A ggplot2 theme object
#' @export
#'
#' @examples
#' ggplot(mtcars, aes(x = wt, y = mpg)) +
#'   geom_point() +
#'   theme_classic2()
theme_classic2 <- function() {
    ggplot2::theme_classic() +
        ggplot2::theme(
            plot.title = ggplot2::element_text(face = "bold", hjust = 0.5),
            axis.title.x = ggplot2::element_text(face = "bold"),
            axis.title.y = ggplot2::element_text(face = "bold"),
            legend.title = ggplot2::element_text(face = "bold"),
            strip.text = ggplot2::element_text(face = "bold"),
            strip.text.y = ggplot2::element_text(face = "bold")
        )
}
