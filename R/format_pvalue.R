#' Format p-values for display in text, ggplot2 plots, or tables.
#'
#' Vectorised helper to format numeric p-values into readable strings.
#'
#' @param x Numeric vector of p-values
#' @param p_text Character; text prefix for p-values (default "p"). Use "" to omit.
#' @param p_symbol Character; symbol/operator to display including spacing (default "= "). When truncate_pvalue=TRUE and p<0.001, "<" is used instead.
#' @param truncate_pvalue Logical; whether to truncate pvalues <0.001 to "p<0.001". If FALSE, scientific notation is used (default FALSE).
#' @param html Logical; whether to format scientific notation as HTML for small p-values (default TRUE). HTML output is suitable for markdown documents, Quarto, or with `ggtext::geom_richtext()`, but not with standard `ggplot2::geom_text()`. Ignored when `format = "plotmath"`.
#' @param format Character; output format for small p-values. One of "text" (default; plain scientific notation), or "plotmath" (suitable for ggplot2 with parse=TRUE). When "plotmath", returns a quoted string with plotmath notation (e.g., `"p =" ~ 5.0 %*% 10^{-5}`).
#'
#' @return Character vector of formatted p-values. For p >= 0.001, returns values formatted to 2-3 decimal places. For p < 0.001 with truncate_pvalue = TRUE, returns "p<0.001". For p < 0.001 with truncate_pvalue = FALSE, returns scientific notation in the format specified by `format`.
#'
#' @examples
#' # Default behavior: plain text scientific notation
#' format_pvalue(0.0005)
#' format_pvalue(c(0.0005, 0.005, 0.02, 0.236))
#'
#' # HTML formatting for markdown/Quarto or ggtext::geom_richtext()
#' format_pvalue(0.0005, html = TRUE)
#'
#' # Truncate small p-values<0.001 to "p<0.001"
#' format_pvalue(0.0005, truncate_pvalue = TRUE)
#' format_pvalue(c(0.0005, 0.005, 0.02, 0.236), truncate_pvalue = TRUE)
#'
#' # Plotmath notation for use with geom_text(parse = TRUE)
#' format_pvalue(0.0005, format = "plotmath")
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
    html = TRUE,
    format = c("text", "plotmath")
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
    format <- match.arg(format)
    stopifnot(format %in% c("text", "plotmath"))

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
        fmt_small <- function(v, fmt = "text", use_html = FALSE) {
            sci <- formatC(v, format = "e", digits = 1)
            if (fmt == "plotmath") {
                # Return plotmath expression using p_text and p_symbol
                parts <- strsplit(sci, "e", fixed = TRUE)[[1]]
                coef <- format(round(as.numeric(parts[1]), 1), nsmall = 1)
                exp_val <- as.integer(parts[2])

                # Build the plotmath prefix from p_text and p_symbol
                prefix <- paste0(p_text, p_symbol)
                # Trim trailing whitespace to avoid double-spacing with tilde operator
                prefix_trimmed <- trimws(prefix, which = "right")

                if (prefix_trimmed == "") {
                    # No prefix, just the scientific notation
                    sprintf('%s %%*%% 10^{%d}', coef, exp_val)
                } else {
                    # Include prefix with tilde for spacing
                    sprintf(
                        '"%s" ~ %s %%*%% 10^{%d}',
                        prefix_trimmed,
                        coef,
                        exp_val
                    )
                }
            } else if (isTRUE(use_html)) {
                # e.g. "5.0 × 10<sup>-4</sup>"
                parts <- strsplit(sci, "e", fixed = TRUE)[[1]]
                coef <- format(round(as.numeric(parts[1]), 1), nsmall = 1)
                exp <- as.integer(parts[2])
                paste0(coef, " \u00d7 10<sup>", exp, "</sup>")
            } else {
                sci
            }
        }
        small_idx <- !is.na(x) & x < 0.001
        fmt_numeric[small_idx] <- vapply(
            x[small_idx],
            fmt_small,
            character(1),
            fmt = format,
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
    # Special handling for plotmath with small p-values
    if (format == "plotmath") {
        small_idx <- !is.na(x) & x < 0.001 & !truncate_pvalue
        result <- fmt_numeric
        # For non-small values, assemble normally then quote for parse
        non_small_idx <- !small_idx
        if (any(non_small_idx)) {
            if (truncate_pvalue) {
                symbol_ns <- ifelse(x[non_small_idx] < 0.001, "<", p_symbol)
            } else {
                symbol_ns <- p_symbol
            }
            result[non_small_idx] <- paste0(
                p_text,
                symbol_ns,
                fmt_numeric[non_small_idx]
            )
        }
        # small values already have plotmath format from fmt_small
        # but we need to ensure they're quoted properly
        if (any(small_idx)) {
            # Already quoted from fmt_small as "p =" ~ coef %*% 10^{exp}
            result[small_idx] <- result[small_idx]
        }
        # For non-small values, wrap in quotes for parse=TRUE
        result[non_small_idx] <- paste0('"', result[non_small_idx], '"')
        return(result)
    }

    # Combine p_text and symbol and value (text or HTML format)
    # If both p_text and symbol are empty, return just the value
    # (only when truncate_pvalue is FALSE, when symbol is scalar)
    if (p_text == "" && p_symbol == "" && !truncate_pvalue) {
        return(fmt_numeric)
    }

    paste0(p_text, symbol, fmt_numeric)
}
