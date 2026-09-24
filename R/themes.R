#' Base theme with common bold styling elements
#'
#' Internal utility function that creates the common ggplot2 theme
#' modifications used by theme_bw2 and theme_classic2.
#'
#' @param markdown Logical; if `TRUE` (default), the plot title, subtitle,
#'   caption, axis titles, and legend title use [ggtext::element_markdown()]
#'   so they can contain markdown/HTML. Axis tick labels and legend text are
#'   left as plain [ggplot2::element_text()], since those are often
#'   populated from data values rather than authored by the user.
#'
#' @return A ggplot2 theme object with bold text elements
#'
#' @keywords internal
theme_base <- function(markdown = TRUE) {
    if (!is.logical(markdown) || length(markdown) != 1 || is.na(markdown)) {
        stop("`markdown` must be TRUE or FALSE.", call. = FALSE)
    }
    element_fn <- if (markdown) {
        ggtext::element_markdown
    } else {
        ggplot2::element_text
    }
    ggplot2::theme(
        plot.title = element_fn(face = "bold"),
        plot.subtitle = element_fn(),
        plot.caption = element_fn(),
        plot.tag = ggplot2::element_text(face = "bold"),
        axis.title.x = element_fn(face = "bold"),
        axis.title.y = element_fn(face = "bold"),
        legend.title = element_fn(face = "bold"),
        strip.text = ggplot2::element_text(face = "bold"),
        strip.text.y = ggplot2::element_text(face = "bold")
    )
}

#' Force a plot's legend title to plain text
#'
#' Internal helper for functions (e.g. `plot_dotmap`, `plot_pathways`) that
#' allow a legend title to be supplied as a plotmath `expression()`.
#' [ggtext::element_markdown()] cannot render an `expression()` (it would be
#' coerced to a literal, deparsed character string, e.g.
#' `"bold(-log[\"10\"] ~ \"pvalue\")"`, rather than parsed as plotmath); and a
#' later `theme(legend.title = ggplot2::element_text(...))` cannot simply be
#' added on top of a plot whose (possibly global, via `theme_set()`) legend
#' title element is `element_markdown()`, since ggplot2 >= 4.0 errors when
#' merging theme elements of different classes for the same slot. This
#' resolves the plot's current theme (its own local overrides plus the
#' current global default from [ggplot2::theme_get()]) into a single,
#' complete theme with a plain-text, bold legend title, using
#' [ggplot2::%+replace%] to swap the element in directly rather than merging
#' it, which sidesteps the class-merge restriction entirely.
#'
#' @param p A ggplot object.
#' @return `p` with its theme resolved to a complete theme using a plain
#'   [ggplot2::element_text()] `legend.title`.
#' @keywords internal
#' @importFrom ggplot2 %+replace%
#' @importFrom rlang %||%
plain_legend_title_theme <- function(p) {
    resolved <- ggplot2::theme_get() %+replace%
        (p$theme %||% ggplot2::theme())
    p$theme <- resolved %+replace%
        ggplot2::theme(legend.title = ggplot2::element_text(face = "bold"))
    p
}

#' Bold-styled Black and White Theme
#'
#' A modified ggplot2 black and white theme with bold text elements. By
#' default, the plot title, subtitle, caption, axis titles, and legend title
#' support markdown/HTML via [ggtext::element_markdown()].
#'
#' @param markdown Logical; if `TRUE` (default), the plot title, subtitle,
#'   caption, axis titles, and legend title use [ggtext::element_markdown()],
#'   so labels can contain markdown/HTML such as `"*italic*"`,
#'   `"**bold**"`, or `"log<sub>2</sub> FC"`. Use `"<br>"` for line breaks in
#'   these labels instead of `"\n"`, which markdown/HTML rendering ignores.
#'   Axis tick labels and legend text are left as plain
#'   [ggplot2::element_text()] (they can still be switched to
#'   [ggtext::element_markdown()] manually, e.g.
#'   `theme(axis.text.x = ggtext::element_markdown(angle = 45))`). Note that
#'   when `TRUE`, further overriding one of the markdown elements (plot
#'   title/subtitle/caption, axis titles, or legend title) with
#'   [ggplot2::element_text()] errors in ggplot2 >= 4.0 ("Only elements of
#'   the same class can be merged"); use [ggtext::element_markdown()] for
#'   that override instead, or set `markdown = FALSE` for plain
#'   [ggplot2::element_text()] elements throughout.
#'
#' @return A ggplot2 theme object
#' @export
#'
#' @examples
#' library(ggplot2)
#' ggplot(mtcars, aes(x = wt, y = mpg)) +
#'   geom_point() +
#'   labs(tag = "A", x = "Weight (10<sup>3</sup> lbs)", y = "*Miles* per gallon") +
#'   theme_bw2()
theme_bw2 <- function(markdown = TRUE) {
    ggplot2::theme_bw() +
        theme_base(markdown = markdown)
}

#' Bold-styled Classic Theme
#'
#' A modified ggplot2 classic theme with bold text elements. By default, the
#' plot title, subtitle, caption, axis titles, and legend title support
#' markdown/HTML via [ggtext::element_markdown()].
#'
#' @param markdown Logical; if `TRUE` (default), the plot title, subtitle,
#'   caption, axis titles, and legend title use [ggtext::element_markdown()],
#'   so labels can contain markdown/HTML such as `"*italic*"`,
#'   `"**bold**"`, or `"log<sub>2</sub> FC"`. Use `"<br>"` for line breaks in
#'   these labels instead of `"\n"`, which markdown/HTML rendering ignores.
#'   Axis tick labels and legend text are left as plain
#'   [ggplot2::element_text()] (they can still be switched to
#'   [ggtext::element_markdown()] manually, e.g.
#'   `theme(axis.text.x = ggtext::element_markdown(angle = 45))`). Note that
#'   when `TRUE`, further overriding one of the markdown elements (plot
#'   title/subtitle/caption, axis titles, or legend title) with
#'   [ggplot2::element_text()] errors in ggplot2 >= 4.0 ("Only elements of
#'   the same class can be merged"); use [ggtext::element_markdown()] for
#'   that override instead, or set `markdown = FALSE` for plain
#'   [ggplot2::element_text()] elements throughout.
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
theme_classic2 <- function(markdown = TRUE) {
    ggplot2::theme_classic() +
        theme_base(markdown = markdown)
}
