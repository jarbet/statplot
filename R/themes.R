#' Base theme with common bold styling elements
#'
#' Internal utility function that creates the common ggplot2 theme
#' modifications used by theme_bw2 and theme_classic2.
#'
#' @return A ggplot2 theme object with bold text elements
#'
#' @keywords internal
theme_base <- function() {
    ggplot2::theme(
        plot.title = ggplot2::element_text(face = "bold"),
        plot.tag = ggplot2::element_text(face = "bold"),
        axis.title.x = ggplot2::element_text(face = "bold"),
        axis.title.y = ggplot2::element_text(face = "bold"),
        legend.title = ggplot2::element_text(face = "bold"),
        strip.text = ggplot2::element_text(face = "bold"),
        strip.text.y = ggplot2::element_text(face = "bold")
    )
}

#' Bold-styled Black and White Theme
#'
#' A modified ggplot2 black and white theme with bold text elements.
#'
#' @return A ggplot2 theme object
#' @export
#'
#' @examples
#' library(ggplot2)
#' ggplot(mtcars, aes(x = wt, y = mpg)) +
#'   geom_point() +
#'   labs(tag = "A") +
#'   theme_bw2()
theme_bw2 <- function() {
    ggplot2::theme_bw() +
        theme_base()
}

#' Bold-styled Classic Theme
#'
#' A modified ggplot2 classic theme with bold text elements.
#'
#' @return A ggplot2 theme object
#' @export
#'
#' @examples
#' library(ggplot2)
#' ggplot(mtcars, aes(x = wt, y = mpg)) +
#'   geom_point() +
#'   labs(tag = "A") +
#'   theme_classic2()
theme_classic2 <- function() {
    ggplot2::theme_classic() +
        theme_base()
}
