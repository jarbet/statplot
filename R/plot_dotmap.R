#' Create a dotmap showing effect size (dot size & color) and p-value (tile fill)
#'
#' A combined tile + point "dotmap" that visualizes an effect (size and
#' direction) and a p-value (tile fill). The function returns a ggplot object
#' or a patchwork composition when a combined p-value barplot is requested.
#'
#' @param data data.frame or tibble containing the plotting variables
#' @param x Character; name of variable in \code{data} to use for x-axis/columns
#' @param y Character; name of variable in \code{data} to use for y-axis/rows
#' @param effect Character; column name of numeric variable in \code{data} to use for dot size and color (direction)
#' @param p Character; column name of numeric variable in \code{data} to use for tile fill (p-value)
#' @param dot_size_vals Numeric vector of reference effect values used for the size legend (signed to indicate direction)
#' @param dot_size_labels Character vector of labels for the size legend; must have same length as \code{dot_size_vals}
#' @param dot_range Numeric(2) range of point sizes (min, max)
#' @param palette Named character vector with elements "positive" and "negative" specifying dot fill colours for positive/negative effects
#' @param xlab_angle Numeric angle to rotate x-axis labels (degrees)
#' @param mlog10_transform_pvalue Logical; when TRUE the fill uses -log10(p) instead of raw p
#' @param fill_limits Numeric(2) or NULL; limits for the fill scale (c(min, max)). If NULL a sensible default is used (c(0,3) for -log10(p) or c(0,1) for raw p)
#' @param legend_pvalue_title Character or expression or NULL; override title for the p-value (tile fill) legend. If NULL an automatic title is used.
#' @param legend_dotsize_title Character or expression; title for the dot-size legend
#' @param add_combined_pvalue_barplot Logical; when TRUE adds a combined p-value barplot to the right of the dotmap (requires \pkg{patchwork})
#' @param combine_pvalue_method Character; method for combining p-values in the barplot. One of: "CMC", "fisher", "MCM", "cauchy", "minp_bonferroni". Defaults to "CMC".
#' @param ... Additional arguments passed on to \code{plot_pvalue_barplot()} when \code{add_combined_pvalue_barplot = TRUE}
#' @param patchwork_widths Numeric(2); widths passed to \pkg{patchwork}::\code{wrap_plots()} when adding the combined p-value barplot (default c(3, 1))
#'
#' @return A \code{ggplot2}::\code{ggplot} object when \code{add_combined_pvalue_barplot = FALSE},
#'   or a \pkg{patchwork} composition object (from \pkg{patchwork}) when
#'   \code{add_combined_pvalue_barplot = TRUE}.
#'
#' @details
#' The tile fill encodes p-values (optionally transformed as -log10(p)), while
#' the overplotted points encode effect size (size) and direction (fill color).
#' NA values for \code{effect} are marked with an "×" symbol. When a combined
#' p-value barplot is requested the function groups by \code{y} and computes the
#' combined p-value using \code{combine_pvalues()}; the combined panel is aligned
#' vertically with the main dotmap.
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
#' # Add Fisher's combination pvalue barplot on the right which combines p-values across columns for each row category
#' plot_dotmap(
#'   df,
#'   x = "col",
#'   y = "row",
#'   effect = "effect",
#'   p = "p",
#'   mlog10_transform_pvalue = TRUE,
#'   add_combined_pvalue_barplot = TRUE,
#'   combine_pvalue_method = "CMC"
#'   )
#'
#'
#' @seealso plot_pvalue_barplot, combine_pvalues
#' @export
#' @importFrom ggplot2 ggplot geom_tile geom_point scale_fill_gradient scale_fill_manual scale_size_continuous scale_x_discrete scale_y_discrete coord_cartesian labs theme element_text element_blank guide_legend expansion margin scale_shape_manual
#' @importFrom ggnewscale new_scale_fill
#' @importFrom scales squish
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
    mlog10_transform_pvalue = TRUE,
    fill_limits = NULL,
    legend_pvalue_title = NULL, # new: title for the p-value/color legend
    legend_dotsize_title = expression(bold("Effect size")), # new: label for the dot-size legend
    add_combined_pvalue_barplot = FALSE, # NEW: add combined p-value barplot to the right
    combine_pvalue_method = c(
        "CMC",
        "fisher",
        "MCM",
        "cauchy",
        "minp_bonferroni"
    ),
    ...,
    patchwork_widths = c(3, 1) # NEW: widths for patchwork layout when combined plot is requested
) {
    combine_pvalue_method <- match.arg(combine_pvalue_method)
    # simple input checks (one-line checks per argument)
    stopifnot(is.data.frame(data))
    stopifnot(is.character(x), length(x) == 1, x %in% names(data))
    stopifnot(is.character(y), length(y) == 1, y %in% names(data))
    stopifnot(
        is.character(effect),
        length(effect) == 1,
        effect %in% names(data)
    )
    stopifnot(is.character(p), length(p) == 1, p %in% names(data))
    stopifnot(is.numeric(dot_size_vals))
    stopifnot(
        is.character(dot_size_labels),
        length(dot_size_labels) == length(dot_size_vals)
    )
    stopifnot(is.numeric(dot_range), length(dot_range) == 2)
    stopifnot(is.character(palette))
    stopifnot(
        !is.null(names(palette)),
        all(c("positive", "negative") %in% names(palette))
    )
    stopifnot(is.numeric(xlab_angle), length(xlab_angle) == 1)
    stopifnot(
        is.logical(mlog10_transform_pvalue),
        length(mlog10_transform_pvalue) == 1
    )
    stopifnot(
        is.null(fill_limits) ||
            (is.numeric(fill_limits) && length(fill_limits) == 2)
    )
    stopifnot(
        is.null(legend_pvalue_title) ||
            (is.character(legend_pvalue_title) &&
                length(legend_pvalue_title) == 1) ||
            (is.expression(legend_pvalue_title) &&
                length(legend_pvalue_title) == 1)
    )
    stopifnot(
        (is.character(legend_dotsize_title) &&
            length(legend_dotsize_title) == 1) ||
            (is.expression(legend_dotsize_title) &&
                length(legend_dotsize_title) == 1)
    )
    stopifnot(
        is.logical(add_combined_pvalue_barplot),
        length(add_combined_pvalue_barplot) == 1
    )
    stopifnot(is.numeric(patchwork_widths), length(patchwork_widths) == 2)

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
    fill_label_auto <- if (mlog10_transform_pvalue) {
        expression(bold(-log['10'] ~ 'pvalue'))
    } else {
        p
    }
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
            oob = scales::squish,
            labels = fill_labels_fn
        ) +
        ggplot2::scale_x_discrete(expand = c(0, 0), position = "top") +
        ggplot2::scale_y_discrete(
            expand = c(0, 0),
            limits = if (is.factor(data[[y]])) {
                levels(data[[y]])
            } else {
                unique(data[[y]])
            }
        ) +
        ggnewscale::new_scale_fill() +
        # NA effect marker: draw an × symbol where effect is NA
        ggplot2::geom_point(
            data = dplyr::filter(data, is.na(.data[[effect]])),
            ggplot2::aes(x = .data[[x]], y = .data[[y]], shape = "Missing"),
            size = 5,
            colour = "grey40",
            stroke = 1,
            inherit.aes = FALSE
        ) +
        ggplot2::scale_shape_manual(
            values = c("Missing" = 4),
            name = NULL,
            guide = ggplot2::guide_legend(
                override.aes = list(colour = "grey40", stroke = 1)
            )
        ) +
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
                vjust = 0.5,
                hjust = 0.5, # center-align (also when rotated), for top axis
                margin = ggplot2::margin(t = -6, b = 0)
            )
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
    y_levels <- if (is.factor(data[[y]])) {
        levels(data[[y]])
    } else {
        unique(data[[y]])
    }
    # If requested, compute combined p-values per y and attach a right-side barplot using patchwork
    if (isTRUE(add_combined_pvalue_barplot)) {
        if (!requireNamespace("patchwork", quietly = TRUE)) {
            stop(
                "`add_combined_pvalue_barplot = TRUE` requires the 'patchwork' package. Please install it."
            )
        }

        combined_df <- data |>
            dplyr::group_by(.data[[y]]) |>
            dplyr::summarise(
                p_combined = {
                    pv <- .data[[p]]
                    pv <- pv[!is.na(pv)]
                    if (length(pv) == 0) {
                        NA_real_
                    } else {
                        combine_pvalues(pv)[combine_pvalue_method]
                    }
                },
                .groups = "drop"
            )

        # preserve factor levels / order to align plots
        combined_df[[y]] <- factor(combined_df[[y]], levels = y_levels)

        # ensure main plot uses the exact same discrete y limits / no expansion
        # build right-side combined p-value barplot but hide its y labels so only the left plot shows labels
        p_comb <- plot_pvalue_barplot(
            data = combined_df,
            x = "p_combined",
            y = y,
            fill = NULL,
            mlog10_transform_pvalue = mlog10_transform_pvalue,
            show_y_labels = FALSE, # <- hide labels on the right plot
            ...
        )

        n_levels <- length(y_levels)

        # lock both panels to the same vertical extent
        p_obj <- p_obj +
            ggplot2::scale_y_discrete(limits = y_levels, expand = c(0, 0)) +
            ggplot2::coord_cartesian(ylim = c(0.5, n_levels + 0.5)) +
            ggplot2::theme(legend.position = "left") # move legend to left when combined

        p_comb <- p_comb +
            ggplot2::scale_y_discrete(limits = y_levels, expand = c(0, 0)) +
            ggplot2::coord_cartesian(ylim = c(0.5, n_levels + 0.5)) +
            ggplot2::theme(
                axis.text.y = ggplot2::element_blank(),
                axis.ticks.y = ggplot2::element_blank()
            ) +
            # add extra right padding so the barplot has more white space on the right
            ggplot2::theme(
                plot.margin = ggplot2::margin(
                    t = 5.5,
                    r = 30,
                    b = 5.5,
                    l = 5.5,
                    unit = "pt"
                )
            )

        combined <- patchwork::wrap_plots(
            p_obj,
            p_comb,
            ncol = 2,
            widths = patchwork_widths
        )
        return(combined)
    } else {
        p_obj <- p_obj +
            ggplot2::scale_y_discrete(expand = ggplot2::expansion(add = 0)) +
            ggplot2::coord_cartesian(
                clip = "on",
                ylim = c(0.5, length(y_levels) + 0.5)
            ) +
            ggplot2::theme(
                panel.grid.major = ggplot2::element_blank(),
                panel.grid.minor = ggplot2::element_blank(),
                plot.margin = ggplot2::margin(
                    t = 5.5,
                    r = 5.5,
                    b = 5,
                    l = 5.5,
                    unit = "pt"
                )
            )
    }

    p_obj
}
