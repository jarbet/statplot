#' Format p-values for display
#'
#' Vectorised helper to format numeric p-values into readable strings.
#'
#' @param x Numeric vector of p-values
#' @param p_symbol Character; symbol to prefix p-values with (default 'p')
#' @param include_p_symbol Logical; whether to include the p_symbol prefix (default TRUE)
#' @return Character vector of formatted p-values, optionally prefixed with \code{p_symbol} when \code{include_p_symbol} is TRUE.
#' @examples
#' format_pvalue(0.0005)
#' format_pvalue(c(0.0005, 0.005, 0.02, 0.236))
#' @export
format_pvalue <- function(x, p_symbol = "p", include_p_symbol = TRUE) {
    stopifnot(
        is.numeric(x),
        all(x >= 0 & x <= 1)
    )
    stopifnot(
        is.character(p_symbol),
        length(p_symbol) == 1
    )
    stopifnot(
        is.logical(include_p_symbol),
        length(include_p_symbol) == 1
    )
    p <- ifelse(
        test = x < 0.001,
        yes = "<0.001",
        no = ifelse(
            test = x < 0.1,
            yes = format(round(x, 3), nsmall = 3),
            no = format(round(x, 2), nsmall = 2)
        )
    )
    if (include_p_symbol) {
        p <- ifelse(
            x < 0.001,
            yes = paste0(p_symbol, "<0.001"),
            no = paste0(p_symbol, "=", p)
        )
    }

    return(p)
}
