#' Plot a covariate heatmap
#'
#' Displays one vertical strip per covariate using
#' \code{\link[ggplot2]{geom_tile}}, with all strips combined via
#' \code{\link[patchwork]{wrap_plots}}. Because the output is a standard
#' \pkg{ggplot2}/\pkg{patchwork} object it aligns naturally with other ggplots
#' (e.g. when combined with \code{|} or \code{/} from \pkg{patchwork}).
#' Only categorical (named-vector colors) covariates are supported.
#'
#' @param dataset A data frame. Must contain all columns named in
#'   \code{color_map} and, if supplied, \code{row_id_var}.
#' @param color_map A named list specifying which covariates to plot and their
#'   color mappings. Each name must be a column in \code{dataset}. Each element
#'   must be a **named character vector** mapping factor levels to hex colors.
#'   Any levels present in the data but absent from the vector receive
#'   auto-generated colors.
#' @param row_id_var Character. Column in \code{dataset} used as row labels
#'   (y-axis). Default \code{NULL} uses \code{1:nrow(dataset)}.
#' @param show_row_names Logical. Whether to display sample labels.
#' @param show_column_names Logical. Whether to display covariate name labels
#'   (column labels). When \code{horizontal = FALSE}: labels appear as plot
#'   titles (\code{column_labels_side = "top"}) or captions
#'   (\code{column_labels_side = "bottom"}). When \code{horizontal = TRUE}:
#'   the covariate name appears on the y-axis; \code{column_labels_side}
#'   controls left/right placement. Ignored when \code{show_column_names = FALSE}.
#'   Default \code{TRUE}.
#' @param row_names_side \code{"left"} or \code{"right"}. When
#' @param row_names_side \code{"left"} or \code{"right"}. When
#'   \code{horizontal = FALSE}, row names are shown on the first strip
#'   (\code{"left"}) or the last strip (\code{"right"}). Ignored when
#'   \code{horizontal = TRUE}; in that case, when \code{show_row_names = TRUE},
#'   labels appear on the bottom (last) strip. Default \code{"left"}.
#' @param plot_spacing Numeric (mm). Gap between adjacent covariate strips.
#'   Default \code{0.5}.
#' @param legend_side Character. Position of the legends. One of
#'   \code{"left"}, \code{"right"}, \code{"top"}, or \code{"bottom"}.
#'   Default \code{"left"}.
#' @param legend_title NULL, a single character string, or a named character
#'   vector. When NULL (default) the legend title is the covariate name
#'   (or combined covariate names when \code{merge_legends = TRUE}). If a
#'   single string is supplied it is used for all legends. If a named vector is
#'   supplied, entries matching covariate names override titles for those
#'   covariates.
#' @param column_labels_side Character. Where to show the covariate name
#'   labels. When \code{horizontal = FALSE}: \code{"top"} or \code{"bottom"}
#'   (default). When \code{horizontal = TRUE}: \code{"left"} (default) places
#'   the label on the left y-axis; \code{"right"} places it on the right
#'   y-axis by setting \code{position = "right"} on the continuous y scale.
#' @param horizontal Logical. When \code{TRUE} the layout is transposed:
#'   samples appear on the x-axis and strips are stacked vertically so the
#'   plot can be combined below a main ggplot. Default \code{FALSE}.
#' @param merge_legends Logical. When \code{TRUE}, strips sharing the exact
#'   same color mapping show a legend only on the first occurrence; that
#'   legend's title joins the covariate names with \code{"\\n"}. Default
#'   \code{FALSE}.
#' @param return_details Logical. If \code{TRUE}, returns a named list with
#'   elements \code{ht} (the \code{patchwork} object) and \code{final_colors}
#'   (the resolved color map). Default \code{FALSE}.
#'
#' @return Invisibly returns a \code{\link[patchwork]{wrap_plots}}
#'   (\code{patchwork}) object, or a named list with elements \code{ht} and
#'   \code{final_colors} when \code{return_details = TRUE}.
#'
#' @examples
#' data(ex_data_heatmap)
#'
#' # Build a one-row-per-sample metadata frame
#' sample_meta <- ex_data_heatmap |>
#'   dplyr::select(sample, group, condition, sample_type) |>
#'   dplyr::distinct()
#'
#' # Multiple categorical covariates
#' plot_covariate_heatmap(
#'   dataset = sample_meta,
#'   color_map = list(
#'     group       = c(G1 = "#1b9e77", G2 = "#d95f02"),
#'     condition   = c(healthy = "#b3de69", EAE = "#fccde5"),
#'     sample_type = c(input = "#8dd3c7", IP = "#80b1d3")
#'   ),
#'   row_id_var = "sample"
#' )
#'
#' # Single vertical covariate bar - useful for placing to left/right of the main ggplot
#' plot_covariate_heatmap(
#'   dataset    = sample_meta,
#'   color_map  = list(group = c(G1 = "#1b9e77", G2 = "#d95f02")),
#'   row_id_var = "sample"
#' )
#'
#' # Single horizontal covariate bar — useful for placing below a main ggplot
#' plot_covariate_heatmap(
#'   dataset    = sample_meta,
#'   color_map  = list(group = c(G1 = "#1b9e77", G2 = "#d95f02")),
#'   row_id_var = "sample",
#'   horizontal = TRUE
#' )
#'
#'
#' # merge_legends: two strips sharing the same color mapping → one legend
#' # whose title combines both covariate names.
#' grp_colors <- c(G1 = "#1b9e77", G2 = "#d95f02")
#' sample_meta2 <- dplyr::mutate(sample_meta, group_rep = group)
#' plot_covariate_heatmap(
#'   dataset = sample_meta2,
#'   color_map = list(
#'     group     = grp_colors,
#'     group_rep = grp_colors
#'   ),
#'   row_id_var    = "sample",
#'   merge_legends = TRUE
#' )
#' @export
plot_covariate_heatmap <- function(
    dataset,
    color_map,
    row_id_var = NULL,
    show_row_names = TRUE,
    show_column_names = TRUE,
    row_names_side = "left",
    plot_spacing = 0.5,
    legend_side = "left",
    legend_title = NULL,
    column_labels_side = "bottom",
    horizontal = FALSE,
    merge_legends = FALSE,
    return_details = FALSE
) {
    # ---- input checks -------------------------------------------------------
    stopifnot(is.data.frame(dataset))
    stopifnot(is.list(color_map), length(color_map) >= 1L)

    if (!is.null(legend_title)) {
        stopifnot(is.character(legend_title), length(legend_title) >= 1L)
    }

    cov_names <- names(color_map)
    if (is.null(cov_names) || any(!nzchar(cov_names))) {
        stop("`color_map` must be a *named* list.")
    }
    miss <- setdiff(cov_names, names(dataset))
    if (length(miss)) {
        stop(
            "Column(s) in `color_map` not found in `dataset`: ",
            paste(miss, collapse = ", ")
        )
    }
    if (!is.null(row_id_var)) {
        stopifnot(is.character(row_id_var), length(row_id_var) == 1L)
        if (!row_id_var %in% names(dataset)) {
            stop("`row_id_var` '", row_id_var, "' not found in `dataset`.")
        }
    }
    # ---- validate color_map entries: categorical only -----------------------
    for (nm in cov_names) {
        entry <- color_map[[nm]]
        if (is.function(entry)) {
            stop(
                "`color_map$",
                nm,
                "`: continuous (colorRamp2) covariates are ",
                "not supported. Supply a named character vector instead."
            )
        }
        if (!is.character(entry)) {
            stop(
                "`color_map$",
                nm,
                "` must be a named character vector."
            )
        }
        if (is.null(names(entry)) || any(!nzchar(names(entry)))) {
            stop("`color_map$", nm, "` must be a *named* character vector.")
        }
    }

    # ---- row labels ---------------------------------------------------------
    row_labels <- if (!is.null(row_id_var)) {
        as.character(dataset[[row_id_var]])
    } else {
        as.character(seq_len(nrow(dataset)))
    }
    if (anyDuplicated(row_labels) > 0L) {
        stop(
            "`row_id_var` '",
            row_id_var,
            "' contains duplicate values. ",
            "Each row must have a unique label."
        )
    }

    # ---- auto-generate missing categorical colors ---------------------------
    brewer_quals <- c(
        "Set1",
        "Set2",
        "Set3",
        "Dark2",
        "Accent",
        "Paired",
        "Pastel1",
        "Pastel2"
    )
    auto_colors_for <- function(levels, var_index) {
        pal <- brewer_quals[(var_index - 1L) %% length(brewer_quals) + 1L]
        max_n <- if (pal %in% rownames(RColorBrewer::brewer.pal.info)) {
            RColorBrewer::brewer.pal.info[pal, "maxcolors"]
        } else {
            0L
        }
        if (length(levels) <= max_n && length(levels) > 0L) {
            cols <- RColorBrewer::brewer.pal(
                max(3L, length(levels)),
                pal
            )[seq_along(levels)]
        } else if (length(levels) > 0L) {
            cols <- grDevices::hcl.colors(length(levels), "Tableau10")
        } else {
            cols <- character(0L)
        }
        stats::setNames(cols, levels)
    }

    final_colors <- vector("list", length(cov_names))
    names(final_colors) <- cov_names
    for (i in seq_along(cov_names)) {
        nm <- cov_names[i]
        data_lvls <- unique(as.character(dataset[[nm]]))
        data_lvls <- data_lvls[!is.na(data_lvls)] # exclude NA before color resolution
        user_map <- color_map[[nm]]
        all_lvls <- union(data_lvls, names(user_map))
        missing_lvls <- setdiff(all_lvls, names(user_map))
        if (length(missing_lvls)) {
            user_map <- c(user_map, auto_colors_for(missing_lvls, i))
        }
        final_colors[[nm]] <- user_map
    }

    # ---- merge-legend deduplication -----------------------------------------
    # When merge_legends = TRUE, duplicate-color strips suppress their legend;
    # the first occurrence gets a combined title.
    color_key <- function(col) {
        list(type = "named_vector", sorted = col[order(names(col))])
    }

    legend_info <- if (isTRUE(merge_legends)) {
        show <- stats::setNames(rep(TRUE, length(cov_names)), cov_names)
        titles <- stats::setNames(cov_names, cov_names)
        seen_keys <- list()
        seen_reps <- character()
        group_rep <- stats::setNames(character(length(cov_names)), cov_names)

        for (v in cov_names) {
            key <- color_key(final_colors[[v]])
            match_idx <- which(vapply(
                seen_keys,
                function(s) identical(s, key),
                logical(1)
            ))
            if (length(match_idx)) {
                show[[v]] <- FALSE
                group_rep[[v]] <- seen_reps[[match_idx]]
            } else {
                seen_keys <- c(seen_keys, list(key))
                seen_reps <- c(seen_reps, v)
                group_rep[[v]] <- v
            }
        }
        for (rep_v in unique(group_rep)) {
            members <- cov_names[group_rep == rep_v]
            if (length(members) > 1L) {
                titles[[rep_v]] <- paste(members, collapse = "\n")
            }
        }
        list(show = show, titles = titles)
    } else {
        NULL
    }

    n_covs <- length(cov_names)

    # ---- build one ggplot per covariate strip -------------------------------
    plots <- vector("list", n_covs)
    for (i in seq_along(cov_names)) {
        nm <- cov_names[i]

        df_i <- data.frame(
            y = factor(row_labels, levels = row_labels),
            fill_val = factor(
                as.character(dataset[[nm]]),
                levels = names(final_colors[[nm]])
            ),
            stringsAsFactors = FALSE
        )

        show_leg <- if (!is.null(legend_info)) legend_info$show[[nm]] else TRUE
        if (!is.null(legend_title)) {
            if (length(legend_title) == 1L) {
                leg_title <- as.character(legend_title)
            } else if (
                !is.null(names(legend_title)) && nm %in% names(legend_title)
            ) {
                leg_title <- as.character(legend_title[[nm]])
            } else {
                leg_title <- if (!is.null(legend_info)) {
                    legend_info$titles[[nm]]
                } else {
                    nm
                }
            }
        } else {
            leg_title <- if (!is.null(legend_info)) {
                legend_info$titles[[nm]]
            } else {
                nm
            }
        }

        half_gap_mm <- plot_spacing / 2

        if (isTRUE(horizontal)) {
            # horizontal strip: samples on x-axis, strips stack vertically
            show_rn <- show_row_names && (i == n_covs)

            p <- ggplot2::ggplot(
                df_i,
                ggplot2::aes(x = y, y = 0.5, fill = fill_val)
            ) +
                ggplot2::geom_tile(color = "white", linewidth = 0.5) +
                ggplot2::scale_fill_manual(
                    values = final_colors[[nm]],
                    name = leg_title,
                    drop = FALSE
                ) +
                ggplot2::scale_x_discrete() +
                ggplot2::scale_y_continuous(
                    position = if (column_labels_side == "right") {
                        "right"
                    } else {
                        "left"
                    },
                    limits = c(0, 1),
                    expand = ggplot2::expansion(0)
                ) +
                ggplot2::labs(
                    x = NULL,
                    y = if (isTRUE(show_column_names)) nm else NULL
                ) +
                ggplot2::theme(
                    axis.title.y = ggplot2::element_text(
                        angle = 0,
                        vjust = 0.5,
                        hjust = if (column_labels_side == "right") 0 else 1,
                        size = 10,
                        margin = if (column_labels_side == "right") {
                            ggplot2::margin(l = 4)
                        } else {
                            ggplot2::margin(r = 4)
                        }
                    ),
                    plot.margin = ggplot2::margin(
                        half_gap_mm,
                        0,
                        half_gap_mm,
                        0,
                        unit = "mm"
                    ),
                    axis.text.y = ggplot2::element_blank(),
                    axis.ticks.y = ggplot2::element_blank(),
                    panel.grid = ggplot2::element_blank()
                )

            if (!show_rn) {
                p <- p +
                    ggplot2::theme(
                        axis.text.x = ggplot2::element_blank(),
                        axis.ticks.x = ggplot2::element_blank()
                    )
            }
        } else {
            # vertical strip: samples on y-axis, strips sit side by side
            show_rn <- show_row_names &&
                ((row_names_side == "left" && i == 1L) ||
                    (row_names_side == "right" && i == n_covs))

            p <- ggplot2::ggplot(
                df_i,
                ggplot2::aes(x = 0.5, y = y, fill = fill_val)
            ) +
                ggplot2::geom_tile(color = "white", linewidth = 0.5) +
                ggplot2::scale_fill_manual(
                    values = final_colors[[nm]],
                    name = leg_title,
                    drop = FALSE
                ) +
                ggplot2::scale_x_continuous(
                    limits = c(0, 1),
                    expand = ggplot2::expansion(0)
                ) +
                ggplot2::scale_y_discrete(limits = rev) +
                ggplot2::labs(
                    x = NULL,
                    y = NULL,
                    title = if (
                        isTRUE(show_column_names) && column_labels_side == "top"
                    ) {
                        nm
                    } else {
                        NULL
                    },
                    caption = if (
                        isTRUE(show_column_names) &&
                            column_labels_side == "bottom"
                    ) {
                        nm
                    } else {
                        NULL
                    }
                ) +
                ggplot2::theme(
                    plot.title = ggplot2::element_text(
                        hjust = 0.5,
                        size = 10,
                        margin = ggplot2::margin(b = 2)
                    ),
                    plot.caption = ggplot2::element_text(
                        hjust = 0.5,
                        size = 10,
                        margin = ggplot2::margin(t = 2)
                    ),
                    plot.margin = ggplot2::margin(
                        0,
                        half_gap_mm,
                        0,
                        half_gap_mm,
                        unit = "mm"
                    ),
                    axis.text.x = ggplot2::element_blank(),
                    axis.ticks.x = ggplot2::element_blank(),
                    panel.grid = ggplot2::element_blank()
                )

            if (!show_rn) {
                p <- p +
                    ggplot2::theme(
                        axis.text.y = ggplot2::element_blank(),
                        axis.ticks.y = ggplot2::element_blank()
                    )
            }
        }

        if (!show_leg) {
            p <- p + ggplot2::guides(fill = "none")
        }

        plots[[i]] <- p
    }

    # ---- combine strips with patchwork --------------------------------------
    base <- if (isTRUE(horizontal)) {
        patchwork::wrap_plots(plots, ncol = 1L)
    } else {
        patchwork::wrap_plots(plots, nrow = 1L)
    }
    combined <- base +
        patchwork::plot_layout(guides = "collect") &
        ggplot2::theme(legend.position = legend_side)

    if (isTRUE(return_details)) {
        return(invisible(list(ht = combined, final_colors = final_colors)))
    } else {
        return(invisible(combined))
    }
}
