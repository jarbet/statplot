#' Render a named list as a Quarto tabset
#'
#' Print each element of a list with a level-2 heading (##) suitable for Quarto
#' tabsets. If `title` is NULL the list names are used. Typical use is printing
#' ggplot objects or other printable plot objects so each becomes a separate tab.
#'
#' @param list_obj A list of printable R objects (e.g. ggplot objects).
#' @param title Optional character vector of titles to use for each element;
#'   if NULL names(list_obj) are used.
#' @return The input list, returned invisibly.
#' @examples
#' \dontrun{
#' library(ggplot2)
#' plots <- list(
#'   One = ggplot(mtcars, aes(mpg, wt)) + geom_point(),
#'   Two = ggplot(mtcars, aes(hp, qsec)) + geom_point()
#' )
#' quarto_tabset_list(plots)
#' }
#' @export
quarto_html_tabset_list <- function(list_obj, title = NULL) {
    if (is.null(title)) {
        title <- names(list_obj)
    }
    for (i in seq_along(list_obj)) {
        p <- list_obj[[i]]
        if (is.null(p)) {
            next
        }
        nm <- title[i]
        cat("\n", sprintf("## %s", nm), "\n\n", sep = "")
        print(p)
        cat("\n")
    }
    invisible(list_obj)
}
