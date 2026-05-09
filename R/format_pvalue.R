#' Format p-values for display in text, ggplot2 plots, or tables.
#'
#' Vectorised helper to format numeric p-values into readable strings.
#'
#' @param x Numeric vector of p-values
#' @param p_text Character; text prefix for p-values (default "p"). Use "" to omit.
#' @param p_symbol Character; symbol/operator to display including spacing (default "= "). When truncate_pvalue=TRUE and p<0.001, "<" is used instead.
#' @param truncate_pvalue Logical; whether to truncate pvalues <0.001 to "p<0.001". If FALSE, scientific notation is used (default FALSE).
#' @param html Logical; whether to format scientific notation as HTML for small p-values (default TRUE).
#' @examples
#' # Default behavior: HTML formatting for small pvalues using scientific notation
#' # This can more easily be added to ggplot2 plots compared to expression()/plotmath formatting
#' format_pvalue(0.0005, html = TRUE)
#' format_pvalue(c(0.0005, 0.005, 0.02, 0.236))
#'
#' # Truncate small p-values<0.001 to "p<0.001"
#' format_pvalue(0.0005, truncate_pvalue = TRUE)
#' format_pvalue(c(0.0005, 0.005, 0.02, 0.236), truncate_pvalue = TRUE)
#'
#' # Custom p_text and p_symbol
#' format_pvalue(0.0005, p_text = "q")
#' format_pvalue(c(0.0005, 0.005), p_text = "p", p_symbol = ": ")
#'
#' # Just the p-value without prefix
#' format_pvalue(0.0005, p_text = "", p_symbol = "")
#' @export
format_pvalue <- function(
    x,
    p_text = "p",
    p_symbol = "= ",
    truncate_pvalue = FALSE,
    html = TRUE
) {
    stopifnot(
        is.numeric(x),
        all(x >= 0 & x <= 1, na.rm = TRUE) # allow NAs through
    )
    stopifnot(is.logical(truncate_pvalue), length(truncate_pvalue) == 1)
    stopifnot(is.logical(html), length(html) == 1)
    stopifnot(
        is.character(p_text),
        length(p_text) == 1
    )
    stopifnot(
        is.character(p_symbol),
        length(p_symbol) == 1
    )

    # Format the numeric values
    fmt_numeric <- ifelse(
        test = x < 0.001,
        yes = if (truncate_pvalue) "0.001" else "scientific",
        no = ifelse(
            test = x < 0.1,
            yes = format(round(x, 3), nsmall = 3),
            no = format(round(x, 2), nsmall = 2)
        )
    )

    # Handle scientific notation for values < 0.001 when not truncating
    if (!truncate_pvalue) {
        fmt_small <- function(v, use_html = FALSE) {
            sci <- formatC(v, format = "e", digits = 2)
            if (isTRUE(use_html)) {
                # e.g. "5.00 × 10<sup>-4</sup>"
                parts <- strsplit(sci, "e", fixed = TRUE)[[1]]
                coef <- format(round(as.numeric(parts[1]), 2), nsmall = 2)
                exp <- as.integer(parts[2])
                paste0(coef, " \u00d7 10<sup>", exp, "</sup>")
            } else {
                sci
            }
        }
        small_idx <- x < 0.001
        fmt_numeric[small_idx] <- vapply(
            x[small_idx],
            fmt_small,
            character(1),
            use_html = html
        )
    }

    # Determine the appropriate symbol for each value
    if (truncate_pvalue) {
        # When truncating: use "<" for values < 0.001, p_symbol for others
        symbol <- ifelse(x < 0.001, "<", p_symbol)
    } else {
        # When not truncating: always use p_symbol
        symbol <- p_symbol
    }

    # Combine p_text and symbol and value
    # If both p_text and symbol are empty, return just the value
    if (p_text == "" && symbol == "") {
        return(fmt_numeric)
    }

    paste0(p_text, symbol, fmt_numeric)
}
