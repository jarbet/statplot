#' Render a collaborator comment for Quarto output
#'
#' Format reviewer or collaborator notes so they render as colored text in HTML,
#' as colored LaTeX in PDF, and as a Word custom style in Docx.
#'
#' When knitting to Word, the referenced Word template must define a paragraph style
#' matching the `style` argument. The default `style = "Comment"` requires a Word
#' style named "Comment" in the target document or template.
#'
#' @param text Character scalar containing the comment text.
#' @param color Character scalar for the HTML/PDF color name.
#' @param style Character scalar for the Word custom style name.
#'
#' @export
#' @importFrom htmltools HTML htmlEscape
#' @importFrom knitr asis_output is_html_output is_latex_output
#' @examples
#' if (knitr::is_html_output()) {
#'   quarto_note("Please review this section")
#' }
quarto_note <- function(text, color = "Green", style = "Comment") {
    if (!is.character(text) || length(text) != 1) {
        stop("`text` must be a length-one character string", call. = FALSE)
    }
    if (!is.character(color) || length(color) != 1) {
        stop("`color` must be a length-one character string", call. = FALSE)
    }
    # Validate color to prevent CSS/LaTeX injection
    if (
        !grepl(
            "^(#[0-9a-fA-F]{3}([0-9a-fA-F]{3})?([0-9a-fA-F]{2})?|[a-zA-Z][a-zA-Z0-9-]*)$",
            color
        )
    ) {
        stop(
            "`color` must be a valid CSS color (hex or named color), received: ",
            dQuote(color),
            call. = FALSE
        )
    }
    if (!is.character(style) || length(style) != 1) {
        stop("`style` must be a length-one character string", call. = FALSE)
    }
    # Validate style to prevent quote injection and invalid attributes
    if (!grepl("^[a-zA-Z0-9 -]+$", style)) {
        stop(
            "`style` must contain only alphanumeric characters, spaces, and hyphens, received: ",
            dQuote(style),
            call. = FALSE
        )
    }

    if (knitr::is_html_output()) {
        knitr::asis_output(htmltools::HTML(sprintf(
            '<span style="color:%s; font-weight: bold;">%s</span>',
            color,
            htmltools::htmlEscape(text)
        )))
    } else if (knitr::is_latex_output()) {
        knitr::asis_output(sprintf(
            "\\textbf{\\textcolor{%s}{%s}}",
            color,
            knitr:::escape_latex(text)
        ))
    } else {
        # Use inline bracketed span syntax for Word output
        # This works correctly inline and applies the custom-style attribute
        knitr::asis_output(sprintf(
            '[%s]{custom-style="%s"}',
            text,
            style
        ))
    }
}
