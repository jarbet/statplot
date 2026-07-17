#' Plot Distribution of a Categorical Variable as a Stacked Bar
#'
#' Create a single 100% stacked bar displaying the distribution of a
#' categorical variable.
#'
#' @param data A data.frame.
#' @param var A categorical variable in `data`.
#' @param text_inside_bars Character specifying labels displayed within bar
#'   segments. One of `"count_and_percent"`, `"count"`, `"percent"`, or
#'   `"none"`.
#' @param fill_palette Optional named vector of fill colors passed to
#'   `ggplot2::scale_fill_manual()`.
#' @param bar_width Width of the stacked bar.
#' @param border_color Color of borders separating bar segments.
#' @param text_size Size of text labels displayed within bar segments.
#'
#' @return A ggplot object.
#'
#' @examples
#' set.seed(123)
#'
#' d <- data.frame(
#'     smoking = factor(
#'         sample(
#'             c("Never", "Former", "Current"),
#'             size = 1000,
#'             replace = TRUE,
#'             prob = c(0.55, 0.30, 0.15)
#'         ),
#'         levels = c("Never", "Former", "Current")
#'     )
#' )
#'
#' plot_1_categorical_var(
#'     data = d,
#'     var = smoking,
#'     text_inside_bars = "count_and_percent"
#' )
#'
#' @export
plot_1_categorical_var <- function(
    data,
    var,
    text_inside_bars = c(
        "count_and_percent",
        "none",
        "count",
        "percent"
    ),
    fill_palette = NULL,
    bar_width = 0.8,
    border_color = "white",
    text_size = 4
) {
    text_inside_bars <- match.arg(text_inside_bars)

    var_name <- deparse(substitute(var))

    d_plot <- data |>
        dplyr::count({{ var }}, name = "n") |>
        dplyr::mutate(
            pct = n / sum(n),
            label = dplyr::case_when(
                text_inside_bars == "count" ~ sprintf(
                    "%s",
                    scales::comma(n)
                ),
                text_inside_bars == "percent" ~ scales::percent(
                    pct,
                    accuracy = 0.1
                ),
                text_inside_bars == "count_and_percent" ~ sprintf(
                    "%s (%.1f%%)",
                    scales::comma(n),
                    pct * 100
                ),
                TRUE ~ NA_character_
            )
        )

    p <- ggplot2::ggplot(
        d_plot,
        ggplot2::aes(
            x = "",
            y = pct,
            fill = {{ var }}
        )
    ) +
        ggplot2::geom_col(
            width = bar_width,
            color = border_color
        ) +
        ggplot2::scale_y_continuous(
            labels = scales::percent_format(),
            limits = c(0, 1),
            expand = c(0, 0)
        ) +
        ggplot2::labs(
            x = NULL,
            y = "Percent",
            fill = var_name
        ) +
        ggplot2::theme(
            axis.text.x = ggplot2::element_blank(),
            axis.ticks.x = ggplot2::element_blank()
        )

    if (!is.null(fill_palette)) {
        p <- p +
            ggplot2::scale_fill_manual(
                values = fill_palette
            )
    }

    if (text_inside_bars != "none") {
        p <- p +
            ggplot2::geom_text(
                ggplot2::aes(label = label),
                position = ggplot2::position_stack(
                    vjust = 0.5
                ),
                size = text_size
            )
    }

    p
}
