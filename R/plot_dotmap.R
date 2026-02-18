#' Create a dotmap showing effect size (dot size & color) and p-value (tile fill)
#'
#' A combined tile + point "dotmap" that visualizes an effect (size and
#' direction) and a p-value (tile fill). The function returns a ggplot object.
#'
#' @param data data.frame or tibble containing the plotting variables
#' @param x Character; column name for x axis
#' @param y Character; column name for y axis
#' @param effect Character; numeric column name used for dot size and direction
#' @param p Character; numeric column name used for tile fill (p-value)
#' @param dot_size_vals Numeric vector of reference effect values used for the size legend (signed to indicate direction)
#' @param dot_size_labels Character vector of labels for the size legend
#' @param dot_range Numeric(2) range of point sizes (min, max)
#' @param palette Named character vector with elements "positive" and "negative" specifying dot colors
#' @param xlab_angle Numeric angle to rotate x-axis labels (degrees)
#' @param mlog10_transform_pvalue Logical; when TRUE the fill uses -log10(p) instead of raw p
#' @param fill_limits Numeric(2) or NULL; limits for the fill scale
#' @param legend_pvalue_title Character or NULL; override title for p-value/color legend
#' @param legend_dotsize_title Character; title for the dot-size legend
#'
#' @return A ggplot2::ggplot object
#'
#' @examples
#' set.seed(42)
#' genes <- paste0("gene", 1:6)
#' df <- expand.grid(col = c("A", "B", "C"), row = genes, stringsAsFactors = FALSE)
#' df$effect <- rnorm(nrow(df), mean = 0, sd = 1.2)         # realistic effect sizes
#' df$mlog10_p <- runif(nrow(df), min = 0, max = 3)         # -log10(p) between 0 and 3
#' df$p <- 10^(-df$mlog10_p)
#' df$row <- factor(df$row, levels = rev(genes))
#' plot_dotmap(df, x = "col", y = "row", effect = "effect", p = "p",
#'             mlog10_transform_pvalue = TRUE)
#'
#' @export
#' @importFrom ggplot2 ggplot geom_tile geom_point scale_fill_gradient scale_color_manual scale_size_continuous labs theme element_text element_blank guide_legend
plot_dotmap <- function(
    data,
    x,
    y,
    effect,
    p,
    dot_size_vals = c(-2, -1, -0.5, -0.25, 0.25, 0.5, 1, 2),
    dot_size_labels = as.character(dot_size_vals),
    dot_range = c(5, 30),
    palette = c("positive" = "darkorange1", "negative" = "dodgerblue2"),
    xlab_angle = 0, # set to 45 to rotate x-axis labels
    mlog10_transform_pvalue = FALSE,
    fill_limits = NULL,
    legend_pvalue_title = NULL, # new: title for the p-value/color legend
    legend_dotsize_title = "Effect size" # new: label for the dot-size legend
) {
    # basic input checks
    stopifnot(
        is.data.frame(data),
        is.character(x),
        length(x) == 1,
        is.character(y),
        length(y) == 1,
        is.character(effect),
        length(effect) == 1,
        is.character(p),
        length(p) == 1,
        x %in% names(data),
        y %in% names(data),
        effect %in% names(data),
        p %in% names(data),
        is.numeric(dot_size_vals),
        length(dot_size_vals) >= 1,
        is.character(dot_size_labels),
        length(dot_size_labels) == length(dot_size_vals),
        is.numeric(dot_range),
        length(dot_range) == 2,
        is.character(palette),
        all(c("positive", "negative") %in% names(palette)),
        is.numeric(xlab_angle),
        length(xlab_angle) == 1,
        is.logical(mlog10_transform_pvalue),
        length(mlog10_transform_pvalue) == 1,
        (is.null(fill_limits) ||
            (is.numeric(fill_limits) && length(fill_limits) == 2)),
        (is.null(legend_pvalue_title) ||
            (is.character(legend_pvalue_title) &&
                length(legend_pvalue_title) == 1)),
        is.character(legend_dotsize_title),
        length(legend_dotsize_title) == 1
    )

    data <- tibble::as_tibble(data) |>
        dplyr::mutate(
            size_val = abs(.data[[effect]]),
            dir = dplyr::case_when(
                .data[[effect]] > 0 ~ "positive",
                .data[[effect]] < 0 ~ "negative",
                TRUE ~ NA_character_
            ),
            fill_val = if (mlog10_transform_pvalue) {
                -log10(.data[[p]])
            } else {
                .data[[p]]
            }
        )

    if (!is.null(fill_limits)) {
        if (!is.numeric(fill_limits) || length(fill_limits) != 2) {
            stop(
                "`fill_limits` must be NULL or a numeric vector of length 2 (c(min, max))."
            )
        }
    }

    if (is.null(fill_limits)) {
        fill_limits <- if (mlog10_transform_pvalue) {
            c(0, 3)
        } else {
            c(0, 1)
        }
    }

    legend_df <- tibble::tibble(
        label = factor(
            as.character(dot_size_labels),
            levels = as.character(dot_size_labels)
        ),
        size_val = abs(dot_size_vals),
        dir = ifelse(dot_size_vals > 0, "positive", "negative"),
        x = Inf,
        y = Inf
    )

    # determine fill legend title: either user-provided or automatic
    fill_label_auto <- if (mlog10_transform_pvalue) "-log10(pvalue)" else p
    fill_label <- if (!is.null(legend_pvalue_title)) {
        legend_pvalue_title
    } else {
        fill_label_auto
    }

    # choose fill gradient direction:
    # - when using -log10(p): larger values -> darker (low = "white", high = "black")
    # - when using raw p: smaller values -> darker (low = "black", high = "white")
    if (mlog10_transform_pvalue) {
        fill_low <- "white"
        fill_high <- "black"
    } else {
        fill_low <- "black"
        fill_high <- "white"
    }

    # label function: prefix top tick with "≥" when using -log10(p)
    fill_labels_fn <- function(x) {
        if (length(x) == 0) {
            return(character(0))
        }
        max_b <- max(x, na.rm = TRUE)
        sapply(
            x,
            function(v) {
                lbl <- as.character(v)
                if (
                    isTRUE(mlog10_transform_pvalue) &&
                        is.finite(v) &&
                        abs(v - max_b) < (.Machine$double.eps^0.5)
                ) {
                    paste0("\u2265 ", lbl)
                } else {
                    lbl
                }
            },
            USE.NAMES = FALSE
        )
    }

    p_obj <- ggplot2::ggplot(
        data,
        ggplot2::aes(x = .data[[x]], y = .data[[y]])
    ) +
        ggplot2::geom_tile(
            ggplot2::aes(fill = .data[["fill_val"]]),
            color = 'darkgrey'
        ) +
        ggplot2::scale_fill_gradient(
            low = fill_low,
            high = fill_high,
            na.value = "grey95",
            name = fill_label,
            limits = fill_limits,
            labels = fill_labels_fn
        ) +
        ggplot2::scale_x_discrete(expand = c(0, 0), position = "top") +
        ggplot2::scale_y_discrete(expand = c(0, 0)) +
        ggnewscale::new_scale_fill() +
        # use shape 21 so the interior (fill) is the direction color and the border (colour) is white
        ggplot2::geom_point(
            ggplot2::aes(size = size_val, fill = dir),
            shape = 21,
            colour = "white",
            stroke = 0.4, # thin white border
            na.rm = TRUE
        ) +
        ggplot2::scale_fill_manual(
            # separate fill scale for points (uses ggnewscale)
            values = palette,
            name = "Direction",
            guide = "none",
            na.translate = FALSE
        ) +
        ggplot2::scale_size_continuous(
            range = dot_range,
            name = legend_dotsize_title,
            breaks = abs(dot_size_vals),
            labels = dot_size_labels,
            guide = ggplot2::guide_legend(
                override.aes = list(
                    fill = ifelse(
                        dot_size_vals > 0,
                        palette["positive"],
                        palette["negative"]
                    ),
                    colour = "white",
                    shape = 21
                )
            )
        ) +
        ggplot2::labs(x = NULL, y = NULL) +
        ggplot2::theme(
            axis.text.x = ggplot2::element_text(
                angle = xlab_angle,
                vjust = 0,
                margin = ggplot2::margin(t = -6, b = 0)
            ),
            panel.grid = ggplot2::element_blank(),
            panel.background = ggplot2::element_blank(),
            plot.background = ggplot2::element_blank(),
            axis.text.y = ggplot2::element_text(face = "bold")
        )

    p_obj <- p_obj +
        ggplot2::geom_point(
            data = legend_df,
            mapping = ggplot2::aes(
                x = x,
                y = size_val,
                size = size_val,
                fill = dir
            ),
            inherit.aes = FALSE,
            show.legend = TRUE,
            alpha = 0,
            shape = 21,
            colour = "white",
            stroke = 0.35
        ) +
        ggplot2::scale_size_continuous(
            range = dot_range,
            name = legend_dotsize_title,
            breaks = legend_df$size_val,
            labels = legend_df$label,
            guide = ggplot2::guide_legend(
                override.aes = list(
                    fill = ifelse(
                        dot_size_vals > 0,
                        palette["positive"],
                        palette["negative"]
                    ),
                    colour = "white",
                    shape = 21
                )
            )
        )

    p_obj
}
