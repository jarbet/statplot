#' Plot a covariate heatmap
#'
#' Displays one vertical strip per covariate, where each strip is an
#' independent \code{\link[ComplexHeatmap]{Heatmap}} combined into a
#' \code{\link[ComplexHeatmap]{HeatmapList}}. Both categorical (named-vector
#' colors) and continuous (\code{\link[circlize]{colorRamp2}} function) variables
#' are supported.
#'
#' @param dataset A data frame. Must contain all columns named in
#'   \code{color_map} and, if supplied, \code{row_id_var} and
#'   \code{row_split_var}.
#' @param color_map A named list specifying which covariates to plot and their
#'   color mappings. Each name must be a column in \code{dataset}. Each element
#'   is either:
#'   \itemize{
#'     \item A **named character vector** mapping factor levels to hex colors
#'       (for categorical/character/factor columns). Any levels present in the
#'       data but absent from the vector receive auto-generated colors.
#'     \item A **colorRamp2 function** (for continuous/numeric columns), e.g.
#'       one created by \code{\link[circlize]{colorRamp2}}.
#'   }
#' @param row_id_var Character. Column in \code{dataset} used as row labels
#'   (matrix rownames). Default \code{NULL} uses \code{1:nrow(dataset)}.
#' @param row_split_var Character. Column in \code{dataset} used to split rows
#'   into groups. Must have \eqn{\geq 2} distinct levels. Default \code{NULL}.
#' @param cluster_rows Logical. Whether to cluster rows. When \code{TRUE},
#'   clustering is performed on the first covariate strip and the resulting
#'   row order is shared across all strips. Default \code{FALSE}.
#' @param show_row_names Logical. Default \code{TRUE}.
#' @param row_names_side \code{"left"} or \code{"right"}. Row names are shown
#'   on the first strip when \code{"left"} and the last strip when
#'   \code{"right"}. Default \code{"left"}.
#' @param ht_gap A \code{\link[grid]{unit}} object controlling the gap between
#'   adjacent covariate strips. Passed as \code{ht_gap} to
#'   \code{\link[ComplexHeatmap]{draw}}. Default
#'   \code{grid::unit(1, "mm")}. Set to \code{grid::unit(0, "mm")} to remove
#'   all gaps.
#' @param row_gap A \code{\link[grid]{unit}} object controlling the gap between
#'   row-split groups. Passed as \code{row_gap} to each
#'   \code{\link[ComplexHeatmap]{Heatmap}} call. Default
#'   \code{grid::unit(2, "mm")}. Set to \code{grid::unit(0, "mm")} to remove
#'   row gaps entirely.
#' @param legend_side Character. Position of the legends. One of
#'   \code{"left"}, \code{"right"}, \code{"top"}, or \code{"bottom"}.
#'   Passed as both \code{heatmap_legend_side} and
#'   \code{annotation_legend_side} to \code{\link[ComplexHeatmap]{draw}}.
#'   Default \code{"left"}.
#' @param merge_legends Logical. When \code{TRUE}: (1) strips sharing the exact
#'   same color mapping are collapsed into one legend whose title joins the
#'   covariate names with \code{"\\n"}; (2) the value is forwarded as
#'   \code{merge_legends} to \code{\link[ComplexHeatmap]{draw}}. Default
#'   \code{FALSE}.
#' @param return_details Logical. If \code{TRUE}, returns a named list with
#'   elements \code{ht} (the drawn \code{HeatmapList}) and \code{final_colors}
#'   (the resolved color map). Default \code{FALSE}.
#' @param ... Additional arguments passed to every
#'   \code{\link[ComplexHeatmap]{Heatmap}} call (e.g. \code{column_names_rot},
#'   \code{width}, \code{column_title_gp}).
#'
#' @return Invisibly returns the drawn \code{\link[ComplexHeatmap]{HeatmapList}}
#'   object, or a named list with elements \code{ht} and \code{final_colors}
#'   when \code{return_details = TRUE}.
#'
#' @examples
#' data(ex_data_heatmap)
#'
#' # Build a one-row-per-sample metadata frame
#' sample_meta <- ex_data_heatmap |>
#'   dplyr::select(sample, group, condition, sample_type, qc_score) |>
#'   dplyr::distinct()
#'
#' rng_qc <- range(sample_meta$qc_score, na.rm = TRUE)
#' col_fun_qc <- circlize::colorRamp2(
#'   c(rng_qc[1], mean(rng_qc), rng_qc[2]),
#'   c("#ffffcc", "#41b6c4", "#0c2c84")
#' )
#'
#' # Multiple covariates (categorical + continuous)
#' plot_covariate_heatmap(
#'   dataset = sample_meta,
#'   color_map = list(
#'     group       = c(G1 = "#1b9e77", G2 = "#d95f02"),
#'     condition   = c(healthy = "#b3de69", EAE = "#fccde5"),
#'     sample_type = c(input = "#8dd3c7", IP = "#80b1d3"),
#'     qc_score    = col_fun_qc
#'   ),
#'   row_id_var    = "sample",
#'   row_split_var = NULL,
#'   cluster_rows  = FALSE
#' )
#'
#' # Single categorical covariate bar
#' plot_covariate_heatmap(
#'   dataset    = sample_meta,
#'   color_map  = list(group = c(G1 = "#1b9e77", G2 = "#d95f02")),
#'   row_id_var = "sample"
#' )
#'
#' # Single continuous covariate bar
#' plot_covariate_heatmap(
#'   dataset    = sample_meta,
#'   color_map  = list(qc_score = col_fun_qc),
#'   row_id_var = "sample"
#' )
#' @export
plot_covariate_heatmap <- function(
    dataset,
    color_map,
    row_id_var = NULL,
    row_split_var = NULL,
    cluster_rows = FALSE,
    show_row_names = TRUE,
    row_names_side = "left",
    ht_gap = grid::unit(0.2, "mm"),
    row_gap = grid::unit(0.2, "mm"),
    legend_side = "left",
    merge_legends = FALSE,
    return_details = FALSE,
    ...
) {
    # ---- input checks -------------------------------------------------------
    stopifnot(is.data.frame(dataset))
    stopifnot(is.list(color_map), length(color_map) >= 1L)

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
    if (!is.null(row_split_var)) {
        stopifnot(is.character(row_split_var), length(row_split_var) == 1L)
        if (!row_split_var %in% names(dataset)) {
            stop(
                "`row_split_var` '",
                row_split_var,
                "' not found in `dataset`."
            )
        }
    }
    if (isTRUE(cluster_rows) && nrow(dataset) < 2L) {
        stop("`cluster_rows = TRUE` requires at least 2 rows.")
    }

    # ---- validate color_map entries -----------------------------------------
    for (nm in cov_names) {
        entry <- color_map[[nm]]
        if (!is.function(entry)) {
            if (!is.character(entry)) {
                stop(
                    "`color_map$",
                    nm,
                    "` must be a named character vector or a colorRamp2 function."
                )
            }
            if (is.null(names(entry)) || any(!nzchar(names(entry)))) {
                stop("`color_map$", nm, "` must be a *named* character vector.")
            }
        }
    }

    # ---- row labels & split -------------------------------------------------
    row_labels <- if (!is.null(row_id_var)) {
        as.character(dataset[[row_id_var]])
    } else {
        as.character(seq_len(nrow(dataset)))
    }

    row_split <- if (!is.null(row_split_var)) {
        sv <- factor(dataset[[row_split_var]])
        if (nlevels(sv) < 2L) {
            stop(
                "`row_split_var` must have at least 2 levels; found ",
                nlevels(sv),
                "."
            )
        }
        sv
    } else {
        NULL
    }

    # ---- helper: is this covariate continuous? -------------------------------
    is_continuous <- function(nm) is.function(color_map[[nm]])

    # ---- fill final_colors: auto-generate missing categorical colors --------
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
        if (is_continuous(nm)) {
            final_colors[[nm]] <- color_map[[nm]]
        } else {
            data_lvls <- unique(as.character(dataset[[nm]]))
            user_map <- color_map[[nm]]
            all_lvls <- union(data_lvls, names(user_map))
            missing_lvls <- setdiff(all_lvls, names(user_map))
            if (length(missing_lvls)) {
                user_map <- c(user_map, auto_colors_for(missing_lvls, i))
            }
            final_colors[[nm]] <- user_map
        }
    }

    # ---- merge-legend deduplication (mirrors plot_heatmap logic) ------------
    color_key <- function(col) {
        if (is.function(col)) {
            env <- environment(col)
            env_names <- ls(env)
            if (all(c("breaks", "colors") %in% env_names)) {
                list(
                    type = "colorRamp2",
                    breaks = env$breaks,
                    colors = env$colors
                )
            } else {
                list(type = "unknown_fn", fn = col)
            }
        } else if (is.character(col) && !is.null(names(col))) {
            list(type = "named_vector", sorted = col[order(names(col))])
        } else {
            list(type = "other", val = col)
        }
    }

    dedup_legend <- function(covs, colors) {
        show <- stats::setNames(rep(TRUE, length(covs)), covs)
        params <- stats::setNames(vector("list", length(covs)), covs)
        group_rep <- character(length(covs))
        seen_keys <- list()
        seen_reps <- character()
        for (v in covs) {
            key <- color_key(colors[[v]])
            match_idx <- which(vapply(
                seen_keys,
                function(s) identical(s, key),
                logical(1)
            ))
            if (length(match_idx)) {
                group_rep[[which(covs == v)]] <- seen_reps[[match_idx]]
                show[[v]] <- FALSE
            } else {
                seen_keys <- c(seen_keys, list(key))
                seen_reps <- c(seen_reps, v)
                group_rep[[which(covs == v)]] <- v
            }
        }
        for (rep_v in unique(group_rep)) {
            members <- covs[group_rep == rep_v]
            if (length(members) > 1L) {
                params[[rep_v]] <- list(title = paste(members, collapse = "\n"))
            }
        }
        list(
            show_legend = show,
            legend_params = Filter(Negate(is.null), params)
        )
    }

    legend_info <- if (isTRUE(merge_legends)) {
        dedup_legend(cov_names, final_colors)
    } else {
        NULL
    }

    n_covs <- length(cov_names)

    # ---- build HeatmapList: one Heatmap per covariate -----------------------
    ht_list <- NULL
    for (i in seq_along(cov_names)) {
        nm <- cov_names[i]
        vals <- dataset[[nm]]

        mat1 <- matrix(
            if (is_continuous(nm)) as.numeric(vals) else as.character(vals),
            ncol = 1L,
            dimnames = list(row_labels, nm)
        )

        # Only the first strip clusters; the rest follow its row order
        cluster_this <- if (i == 1L) cluster_rows else FALSE

        # Show row names only on the appropriate edge strip
        show_rn <- show_row_names &&
            ((row_names_side == "left" && i == 1L) ||
                (row_names_side == "right" && i == n_covs))

        show_leg <- if (!is.null(legend_info)) {
            legend_info$show_legend[[nm]]
        } else {
            TRUE
        }
        leg_param <- if (
            !is.null(legend_info) && !is.null(legend_info$legend_params[[nm]])
        ) {
            legend_info$legend_params[[nm]]
        } else {
            list(title = nm)
        }

        ht_i <- ComplexHeatmap::Heatmap(
            mat1,
            name = nm,
            col = final_colors[[nm]],
            cluster_rows = cluster_this,
            cluster_columns = FALSE,
            show_row_names = show_rn,
            row_names_side = row_names_side,
            row_split = row_split,
            row_gap = row_gap,
            show_heatmap_legend = show_leg,
            heatmap_legend_param = leg_param,
            column_names_rot = 0,
            rect_gp = grid::gpar(col = "white", lwd = 0.5),
            ...
        )

        ht_list <- if (is.null(ht_list)) ht_i else ht_list + ht_i
    }

    ht_drawn <- ComplexHeatmap::draw(
        ht_list,
        heatmap_legend_side = legend_side,
        annotation_legend_side = legend_side,
        merge_legends = merge_legends,
        ht_gap = ht_gap
    )

    if (isTRUE(return_details)) {
        return(invisible(list(ht = ht_drawn, final_colors = final_colors)))
    } else {
        return(invisible(ht_drawn))
    }
}
