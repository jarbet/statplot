#' Plot a heatmap from tidy data
#'
#' Converts a tidy (long-format) data frame into a matrix and renders a
#' \code{\link[ComplexHeatmap]{Heatmap}} with optional row/column annotations,
#' splits, and row scaling.
#'
#' @param df A data frame in long (tidy) format.
#' @param row_var <[`data-masking`][rlang::args_data_masking]> Column in \code{df}
#'   used as row identifiers.
#' @param col_var <[`data-masking`][rlang::args_data_masking]> Column in \code{df}
#'   used as column identifiers.
#' @param value_var <[`data-masking`][rlang::args_data_masking]> Numeric column
#'   in \code{df} used as heatmap fill values.
#' @param row_covariates Character vector of column names in \code{df} to use as
#'   row annotations. Default \code{NULL}.
#' @param col_covariates Character vector of column names in \code{df} to use as
#'   column annotations. Default \code{NULL}.
#' @param row_split_var Character. Column name in \code{df} used to split rows.
#'   Must have \eqn{\geq 2} levels. Default \code{NULL}.
#' @param col_split_var Character. Column name in \code{df} used to split
#'   columns. Must have \eqn{\geq 2} levels. Default \code{NULL}.
#' @param heatmap_colors A color mapping passed to
#'   \code{\link[ComplexHeatmap]{Heatmap}}'s \code{col} argument. Default
#'   \code{NULL}, in which case a numeric color mapping is automatically
#'   generated (e.g. via \code{\link[circlize]{colorRamp2}}) rather than using
#'   ComplexHeatmap's built-in default.
#' @param anno_colors Named list specifying colors for annotation covariates.
#'   For discrete (categorical) covariates, each element should be a named
#'   character vector mapping annotation levels to hex colors; any levels not
#'   supplied are auto-colored. For continuous (numeric) covariates, each
#'   element may instead be a function that maps numeric values to colors
#'   (e.g., a function created by \code{circlize::colorRamp2}), which will be
#'   applied to the numeric annotation values to generate colors. Default
#'   \code{NULL}.
#' @param scale_rows Logical. Whether to z-score scale rows. Default \code{TRUE}.
#' @param cluster_rows Logical. Whether to cluster rows. Default \code{TRUE}.
#' @param cluster_columns Logical. Whether to cluster columns. Default \code{TRUE}.
#' @param show_row_names Logical. Default \code{TRUE}.
#' @param row_names_side \code{"left"} or \code{"right"}. Default \code{"left"}.
#' @param show_column_names Logical. Default \code{TRUE}.
#' @param return_details Logical. If \code{TRUE}, returns a list with the drawn
#'   heatmap object, final annotation colors, and levels. Default \code{FALSE}.
#' @param heatmap_legend_title Character. Title for the heatmap legend
#' @param ... Additional arguments passed to
#'   \code{\link[ComplexHeatmap]{Heatmap}}.
#'
#' @return Invisibly returns the drawn \code{\link[ComplexHeatmap]{HeatmapList}}
#'   object, or a named list with elements \code{ht}, \code{final_colors}, and
#'   \code{levels} when \code{return_details = TRUE}.
#'
#' @examples
#' data(ex_data_heatmap)
#'
#' ht_cols_small <- circlize::colorRamp2(
#'     c(min(1:16), mean(1:16), max(1:16)),
#'     c("#145afc", "white", "#ee4445")
#' )
#'
#' ann_cols_small <- list(
#'     group = c(G1 = "#1b9e77", G2 = "#d95f02"),
#'     direction = c(up = "#e41a1c", down = "#4daf4a"),
#'     is_immune_gene = c(yes = "#fb8072", no = "#d9d9d9"),
#'     sample_type = c(input = "#8dd3c7", IP = "#80b1d3"),
#'     condition = c(healthy = "#b3de69", EAE = "#fccde5")
#' )
#'
#' # default colors
#' plot_heatmap(
#'     df = ex_data_heatmap,
#'     row_var = external_gene_name,
#'     col_var = sample,
#'     value_var = expression,
#'     row_covariates = c("is_immune_gene", "direction"),
#'     col_covariates = c("sample_type", "condition", "group"),
#'     row_split_var = "direction", # up vs down (2 slices)
#'     col_split_var = "group", # G1 vs G2 (2 slices)
#'     scale_rows = FALSE,
#'     cluster_rows = TRUE,
#'     cluster_columns = TRUE,
#'     return_details = TRUE,
#'     row_names_side = "left"
#' )
#'
#' plot_heatmap(
#'     df = ex_data_heatmap,
#'     row_var = external_gene_name,
#'     col_var = sample,
#'     value_var = expression,
#'     row_covariates = c("is_immune_gene", "direction"),
#'     col_covariates = c("sample_type", "condition", "group"),
#'     row_split_var = "direction",
#'     col_split_var = "group",
#'     scale_rows = FALSE,
#'     cluster_rows = FALSE,
#'     cluster_columns = FALSE,
#'     heatmap_colors = ht_cols_small,
#'     anno_colors = ann_cols_small,
#'     return_details = TRUE
#' )
#' ### Continuous row and column covariates via colorRamp2 functions
#' rng_log2fc <- range(ex_data_heatmap$log2fc, na.rm = TRUE)
#' col_fun_log2fc <- circlize::colorRamp2(
#'     c(rng_log2fc[1], 0, rng_log2fc[2]),
#'     c("#762a83", "white", "#e66101")
#' )
#'
#' rng_qc <- range(ex_data_heatmap$qc_score, na.rm = TRUE)
#' col_fun_qc <- circlize::colorRamp2(
#'     c(rng_qc[1], mean(rng_qc), rng_qc[2]),
#'     c("#ffffcc", "#41b6c4", "#0c2c84")
#' )
#'
#' expr_rng <- range(ex_data_heatmap$expression, na.rm = TRUE)
#' expr_cols <- circlize::colorRamp2(
#'     c(expr_rng[1], mean(expr_rng), expr_rng[2]),
#'     c("#313695", "#f7f7f7", "#a50026")
#' )
#'
#' plot_heatmap(
#'     df = ex_data_heatmap,
#'     row_var = external_gene_name,
#'     col_var = sample,
#'     value_var = expression,
#'     row_covariates = c("log2fc", "is_immune_gene", "direction"),
#'     col_covariates = c("qc_score", "condition", "sample_type"),
#'     col_split_var = "group",
#'     row_split_var = "direction",
#'     heatmap_colors = expr_cols,
#'     anno_colors = list(
#'         log2fc         = col_fun_log2fc,
#'         qc_score       = col_fun_qc,
#'         is_immune_gene = c(yes = "#e7298a", no = "#a6761d"),
#'         direction      = c(up = "#1f78b4", down = "#e31a1c"),
#'         condition      = c(healthy = "#1b9e77", EAE = "#d95f02"),
#'         sample_type    = c(input = "#7570b3", IP = "#66a61e")
#'     ),
#'     scale_rows = FALSE,
#'     cluster_rows = TRUE,
#'     cluster_columns = TRUE,
#'     show_row_names = TRUE,
#'     row_names_side = "left",
#'     heatmap_legend_title = "Expression"
#' )
#' @export
plot_heatmap <- function(
    df,
    row_var,
    col_var,
    value_var,
    row_covariates = NULL,
    col_covariates = NULL,
    row_split_var = NULL,
    col_split_var = NULL,
    heatmap_colors = NULL,
    anno_colors = NULL,
    scale_rows = TRUE,
    cluster_rows = TRUE,
    cluster_columns = TRUE,
    show_row_names = TRUE,
    row_names_side = "left",
    show_column_names = TRUE,
    return_details = FALSE,
    heatmap_legend_title = 'Value',
    ...
) {
    # --- mapping names
    row_key <- rlang::as_name(rlang::ensym(row_var))
    col_key <- rlang::as_name(rlang::ensym(col_var))
    val_key <- rlang::as_name(rlang::ensym(value_var))

    # --- BASIC INPUT CHECKS ----------------------------------------------------
    stopifnot(is.data.frame(df))
    stopifnot(all(c(row_key, col_key, val_key) %in% names(df)))

    if (!is.numeric(df[[val_key]])) {
        stop(
            "`value_var` must be numeric; got class: ",
            paste(class(df[[val_key]]), collapse = "/")
        )
    }

    if (all(is.na(df[[val_key]]))) {
        stop("All values in `value_var` are NA.")
    }

    # --- matrix (rows x cols)
    mat <- df |>
        dplyr::select(dplyr::all_of(c(row_key, col_key, val_key))) |>
        dplyr::distinct() |>
        tidyr::pivot_wider(
            names_from = !!rlang::sym(col_key),
            values_from = !!rlang::sym(val_key)
        ) |>
        tibble::column_to_rownames(row_key) |>
        as.matrix()

    if (nrow(mat) == 0L || ncol(mat) == 0L) {
        stop(
            "After pivoting, the matrix has zero rows or columns. Check your keys and data."
        )
    }

    # Optional row scaling
    if (scale_rows) {
        mat <- t(scale(t(mat)))
        mat[is.na(mat)] <- 0
    }

    # Recompute heatmap color scale AFTER scaling so breakpoints match the matrix
    if (is.null(heatmap_colors)) {
        rng <- range(mat, na.rm = TRUE)
        mid <- mean(rng)
        heatmap_colors <- circlize::colorRamp2(
            c(rng[1], mid, rng[2]),
            c("#313695", "#f7f7f7", "#a50026")
        )
    }

    if (isTRUE(cluster_rows) && nrow(mat) < 2) {
        stop("`cluster_rows=TRUE` requires at least 2 rows.")
    }
    if (isTRUE(cluster_columns) && ncol(mat) < 2) {
        stop("`cluster_columns=TRUE` requires at least 2 columns.")
    }

    # --- aligned covariate frames
    col_needed <- unique(c(col_key, col_covariates, col_split_var))
    row_needed <- unique(c(row_key, row_covariates, row_split_var))

    miss_c <- setdiff(col_needed, names(df))
    miss_r <- setdiff(row_needed, names(df))
    if (length(miss_c)) {
        stop(
            "Missing column(s) on column side: ",
            paste(miss_c, collapse = ", ")
        )
    }
    if (length(miss_r)) {
        stop("Missing column(s) on row side: ", paste(miss_r, collapse = ", "))
    }

    # Build column annotation data frame - one row per unique col_var value
    col_split_needed <- if (
        !is.null(col_split_var) && !col_split_var %in% col_covariates
    ) {
        col_split_var
    } else {
        NULL
    }
    row_split_needed <- if (
        !is.null(row_split_var) && !row_split_var %in% row_covariates
    ) {
        row_split_var
    } else {
        NULL
    }

    col_df_cols <- unique(c(col_covariates, col_split_needed))
    row_df_cols <- unique(c(row_covariates, row_split_needed))

    if (length(col_df_cols)) {
        col_df <- df |>
            dplyr::select(dplyr::all_of(c(col_key, col_df_cols))) |>
            dplyr::distinct(
                dplyr::across(dplyr::all_of(col_key)),
                .keep_all = TRUE
            ) |>
            tibble::column_to_rownames(col_key)
        col_df <- col_df[colnames(mat), , drop = FALSE]
    } else {
        col_df <- NULL
    }

    if (length(row_df_cols)) {
        row_df <- df |>
            dplyr::select(dplyr::all_of(c(row_key, row_df_cols))) |>
            dplyr::distinct(
                dplyr::across(dplyr::all_of(row_key)),
                .keep_all = TRUE
            ) |>
            tibble::column_to_rownames(row_key)
        row_df <- row_df[rownames(mat), , drop = FALSE]
    } else {
        row_df <- NULL
    }

    # ---------------------------
    # ALIGNMENT CHECKS (hard stops)
    # ---------------------------
    if (!is.null(row_df)) {
        stopifnot(identical(rownames(row_df), rownames(mat)))
    }
    if (!is.null(col_df)) {
        stopifnot(identical(rownames(col_df), colnames(mat)))
    }

    chk_atomic <- function(x) is.atomic(x) && !is.list(x)
    if (length(row_covariates)) {
        for (v in row_covariates) {
            stopifnot(!any(is.na(row_df[[v]])))
            stopifnot(length(row_df[[v]]) == nrow(mat))
            if (!chk_atomic(row_df[[v]])) {
                stop("Row covariate `", v, "` is not an atomic vector.")
            }
        }
    }
    if (length(col_covariates)) {
        for (v in col_covariates) {
            stopifnot(!any(is.na(col_df[[v]])))
            stopifnot(length(col_df[[v]]) == ncol(mat))
            if (!chk_atomic(col_df[[v]])) {
                stop("Column covariate `", v, "` is not an atomic vector.")
            }
        }
    }

    # Splits
    row_split <- if (!is.null(row_split_var)) {
        factor(row_df[[row_split_var]])
    } else {
        NULL
    }
    col_split <- if (!is.null(col_split_var)) {
        factor(col_df[[col_split_var]])
    } else {
        NULL
    }

    if (!is.null(row_split)) {
        stopifnot(length(row_split) == nrow(mat))
        stopifnot(!any(is.na(row_split)))
        if (nlevels(row_split) < 2) {
            stop(
                "`row_split_var` must have at least 2 distinct levels; found ",
                nlevels(row_split),
                "."
            )
        }
    }
    if (!is.null(col_split)) {
        stopifnot(length(col_split) == ncol(mat))
        stopifnot(!any(is.na(col_split)))
        if (nlevels(col_split) < 2) {
            stop(
                "`col_split_var` must have at least 2 distinct levels; found ",
                nlevels(col_split),
                "."
            )
        }
    }

    # ---------- defaults per variable
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
    distinct_default_for_levels <- function(levels, var_index) {
        pal_name <- brewer_quals[(var_index - 1) %% length(brewer_quals) + 1]
        max_n <- if (pal_name %in% rownames(RColorBrewer::brewer.pal.info)) {
            RColorBrewer::brewer.pal.info[pal_name, "maxcolors"]
        } else {
            0
        }
        if (length(levels) <= max_n && length(levels) > 0) {
            cols <- RColorBrewer::brewer.pal(
                max(3, length(levels)),
                pal_name
            )[seq_along(levels)]
        } else if (length(levels) > 0) {
            cols <- grDevices::hcl.colors(length(levels), "Tableau10")
        } else {
            cols <- character(0)
        }
        stats::setNames(cols, levels)
    }

    if (!is.null(anno_colors)) {
        stopifnot(is.list(anno_colors))
        for (nm in names(anno_colors)) {
            vec <- anno_colors[[nm]]
            # Allow functions (colorRamp2) for continuous covariates
            if (is.function(vec)) {
                next
            }
            if (length(vec)) {
                if (is.null(names(vec)) || any(!nzchar(names(vec)))) {
                    stop(
                        "`anno_colors$",
                        nm,
                        "` must be a *named* character vector of hex colors ",
                        "or a colorRamp2 function for continuous covariates."
                    )
                }
            }
        }
    }

    # Helper: is this covariate continuous?
    is_continuous_cov <- function(v) {
        usr <- if (!is.null(anno_colors)) anno_colors[[v]] else NULL
        if (is.function(usr)) {
            return(TRUE)
        }
        # Also treat numeric covariates with no user color spec as continuous
        val <- c(row_df[[v]], col_df[[v]])
        val <- val[!is.na(val)]
        is.numeric(val) && is.null(usr)
    }

    # ---------- levels per variable (categorical only)
    var_names <- unique(c(
        if (is.null(row_covariates)) character(0) else row_covariates,
        if (is.null(col_covariates)) character(0) else col_covariates
    ))

    levels_list <- lapply(var_names, function(v) {
        if (is_continuous_cov(v)) {
            return(NULL)
        }
        sort(unique(as.character(c(row_df[[v]], col_df[[v]]))))
    })
    names(levels_list) <- var_names

    # ---------- finalize color maps
    final_colors <- vector("list", length(var_names))
    names(final_colors) <- var_names
    for (i in seq_along(var_names)) {
        v <- var_names[i]
        usr <- if (!is.null(anno_colors)) anno_colors[[v]] else NULL

        if (is.function(usr)) {
            # Continuous: pass the colorRamp2 function through directly
            final_colors[[v]] <- usr
        } else {
            lv <- levels_list[[v]]
            if (is.null(usr)) {
                final_colors[[v]] <- distinct_default_for_levels(
                    lv,
                    var_index = i
                )
            } else {
                usr <- usr[intersect(names(usr), lv)]
                missing <- setdiff(lv, names(usr))
                if (length(missing)) {
                    add <- distinct_default_for_levels(missing, var_index = i)
                    usr <- c(usr, add)
                }
                final_colors[[v]] <- usr[lv]
            }
        }
    }

    # ---------- annotations
    # For continuous covariates, HeatmapAnnotation expects a numeric vector
    # passed via `df=` arg, NOT via individual named args -- this is fine.
    # But we must ensure continuous columns are NOT coerced to factor/character.

    ha_col <- if (length(col_covariates)) {
        anno_df <- col_df[, col_covariates, drop = FALSE]
        # Ensure continuous covariates remain numeric (not factor)
        for (v in col_covariates) {
            if (is_continuous_cov(v) && !is.numeric(anno_df[[v]])) {
                anno_df[[v]] <- as.numeric(as.character(anno_df[[v]]))
            }
        }
        ComplexHeatmap::HeatmapAnnotation(
            df = anno_df,
            col = final_colors[col_covariates],
            which = "column"
        )
    } else {
        NULL
    }

    ha_row <- if (length(row_covariates)) {
        anno_df <- row_df[, row_covariates, drop = FALSE]
        # Ensure continuous covariates remain numeric (not factor)
        for (v in row_covariates) {
            if (is_continuous_cov(v) && !is.numeric(anno_df[[v]])) {
                anno_df[[v]] <- as.numeric(as.character(anno_df[[v]]))
            }
        }
        ComplexHeatmap::rowAnnotation(
            df = anno_df,
            col = final_colors[row_covariates]
        )
    } else {
        NULL
    }

    # ---------- heatmap
    ht <- ComplexHeatmap::Heatmap(
        mat,
        name = heatmap_legend_title,
        col = heatmap_colors,
        row_split = row_split,
        column_split = col_split,
        cluster_rows = cluster_rows,
        cluster_columns = cluster_columns,
        show_row_names = show_row_names,
        row_names_side = row_names_side,
        show_column_names = show_column_names,
        top_annotation = ha_col,
        ...
    )
    if (!is.null(ha_row)) {
        ht <- ht + ha_row
    }

    ht_drawn <- ComplexHeatmap::draw(
        ht,
        heatmap_legend_side = "right",
        annotation_legend_side = "right"
    )

    if (isTRUE(return_details)) {
        return(invisible(list(
            ht = ht_drawn,
            final_colors = final_colors,
            levels = levels_list
        )))
    } else {
        return(invisible(ht_drawn))
    }
}
