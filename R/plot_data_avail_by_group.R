#' Plot data availability by group
#'
#' Create a tile plot showing availability (0/1) of data types across groups.
#'
#' @param data A data frame or tibble containing the columns specified by `data_type`, `group`, and `available`.
#' @param data_type Name of the column with data types. Default: "data_type".
#' @param group Name of the column with group labels. Default: "group".
#' @param available Name of the column indicating availability (values 0 or 1). Default: "available".
#' @param legend_position Legend position for the plot. Default: "right".
#' @param tile_line_color Tile border color (gridlines between tiles). Default: "lightgrey".
#' @param xlabel X axis label. Default: NULL (when supplied, shown in bold).
#' @param ylabel Y axis label. Default: NULL (when supplied, shown in bold).
#' @param xlabel_top_margin Distance (in pts) between the top x-axis title and the axis labels when the x-axis is placed at the top. Default: 8.
#' @return A ggplot object (tile plot).
#' @export
#' @importFrom rlang sym
#' @importFrom ggplot2 ggplot aes geom_tile scale_fill_manual scale_x_discrete scale_y_discrete labs theme_minimal theme element_text element_blank element_rect margin
#' @examples
#' ggplot2::theme_set(theme_bw2())
#' avail <- data.frame(
#'   data_type = rep(c("SNV", "mRNA", "Proteomics", "CNA", "Methylation"), each = 3),
#'   group = rep(c("GroupA", "GroupB", "GroupC"), times = 5),
#'   available = c(1, 1, 0, 1, 0, 1, 0, 1, 1, 1, 1, 1, 0, 1, 0)
#' )
#' p <- plot_data_avail_by_group(avail, xlabel = "Group", ylabel = "Data Type", xlabel_top_margin = 10)
#' p
plot_data_avail_by_group <- function(
    data,
    data_type = "data_type",
    group = "group",
    available = "available",
    legend_position = "right",
    tile_line_color = "lightgrey",
    xlabel = NULL,
    ylabel = NULL,
    xlabel_top_margin = 8
) {
    stopifnot(
        is.character(data_type),
        length(data_type) == 1,
        is.character(group),
        length(group) == 1,
        is.character(available),
        length(available) == 1
    )
    data <- data.frame(data, check.names = FALSE)
    missing_cols <- setdiff(c(data_type, group, available), names(data))
    if (length(missing_cols) > 0) {
        stop(
            "Column(s) not found in `data`: ",
            paste(missing_cols, collapse = ", ")
        )
    }
    # NA values in `available` are allowed; they map to NA fill and appear as
    # a third tile colour distinct from 0/1. Validation ignores them via na.omit().
    stopifnot(all(unique(stats::na.omit(data[, available])) %in% c(0, 1)))
    fill_values <- c(`0` = "white", `1` = "black")
    fill_labels <- c(`0` = "No", `1` = "Yes")
    dt <- data

    p <- ggplot2::ggplot(
        dt,
        ggplot2::aes(
            x = !!rlang::sym(group),
            y = !!rlang::sym(data_type),
            fill = factor(!!rlang::sym(available))
        )
    ) +
        ggplot2::geom_tile(
            color = tile_line_color
        ) +
        ggplot2::scale_x_discrete(position = "top", expand = c(0, 0)) +
        ggplot2::scale_y_discrete(expand = c(0, 0)) +
        ggplot2::scale_fill_manual(
            values = fill_values,
            breaks = c("0", "1"),
            labels = fill_labels,
            name = "Available"
        ) +
        ggplot2::theme_minimal() +
        ggplot2::theme(
            panel.grid = ggplot2::element_blank(),
            panel.border = ggplot2::element_rect(color = "black", fill = NA),
            legend.position = legend_position
        )

    # handle axis labels: default NULL -> no label; non-NULL -> add and make bold
    if (!is.null(xlabel)) {
        p <- p +
            ggplot2::labs(x = xlabel) +
            ggplot2::theme(
                # when axis is placed at top, set axis.title.x.top and add bottom margin
                axis.title.x.top = ggplot2::element_text(
                    face = "bold",
                    margin = ggplot2::margin(b = xlabel_top_margin)
                ),
                # keep fallback for bottom-positioned x axis
                axis.title.x = ggplot2::element_text(
                    face = "bold",
                    margin = ggplot2::margin(t = 6)
                )
            )
    } else {
        p <- p + ggplot2::labs(x = NULL)
    }

    if (!is.null(ylabel)) {
        p <- p +
            ggplot2::labs(y = ylabel) +
            ggplot2::theme(axis.title.y = ggplot2::element_text(face = "bold"))
    } else {
        p <- p + ggplot2::labs(y = NULL)
    }

    p
}
