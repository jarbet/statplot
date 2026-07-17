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
#' @param include_cat_labels Logical. If TRUE, display the category name
#'   above the count/percent label inside each bar segment. Category names
#'   are shown in bold.
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
    text_size = 4,
    include_cat_labels = TRUE
) {
    text_inside_bars <- match.arg(text_inside_bars)

    var_name <- deparse(substitute(var))

    d_plot <- data |>
        dplyr::count({{ var }}, name = "n") |>
        dplyr::mutate(
            pct = n / sum(n),
            value_label = dplyr::case_when(
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
                TRUE ~ ""
            ),
            cat_label = as.character({{ var }}),
            label = dplyr::case_when(
                include_cat_labels &
                    text_inside_bars != "none" ~
                    sprintf(
                        "<b>%s</b><br>%s",
                        cat_label,
                        value_label
                    ),
                include_cat_labels &
                    text_inside_bars == "none" ~
                    sprintf(
                        "<b>%s</b>",
                        cat_label
                    ),
                TRUE ~ value_label
            )
        ) |>
        dplyr::arrange(dplyr::desc({{ var }})) |>
        dplyr::mutate(
            y_pos = cumsum(pct) - pct / 2
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
            axis.ticks.x = ggplot2::element_blank(),
            legend.position = if (include_cat_labels) "none" else "right"
        )

    if (!is.null(fill_palette)) {
        p <- p +
            ggplot2::scale_fill_manual(
                values = fill_palette
            )
    }

    if (text_inside_bars != "none" || include_cat_labels) {
        p <- p +
            ggtext::geom_richtext(
                data = d_plot,
                ggplot2::aes(
                    x = "",
                    y = y_pos,
                    label = label
                ),
                inherit.aes = FALSE,
                size = text_size,
                fill = NA,
                label.color = NA
            )
    }

    p
}
