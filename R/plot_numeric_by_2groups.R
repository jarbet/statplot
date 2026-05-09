#' Compare a numeric variable between two groups with violins/boxplots and Wilcoxon test
#'
#' Compare a numeric variable between two groups with violins/boxplots and Wilcoxon test.
#' Returns the ggplot object and a tidy Wilcoxon result.
#' Optionally you can facet by additional variables.
#'
#' When \code{facet_cols} is supplied, a separate Wilcoxon test is run within
#' each unique combination of those columns and per-panel p-value and sample
#' size annotations are prepared automatically. The annotation layers retain
#' the faceting columns so that adding \code{+ ggplot2::facet_wrap()} or
#' \code{+ ggplot2::facet_grid()} after the function call correctly splits both
#' violins and annotations across panels.
#'
#' @param yvar character(1) Name of the numeric outcome column in `d`.
#' @param group character(1) Name of the factor column in `d` (must have 2 levels).
#' @param d data.frame Data containing `yvar` and `group`.
#' @param colors character vector Length-2 vector of fill colours (default c('white', 'white')); if NULL the default ggplot2 fill scale is used.  When supplied, must be a character vector with length equal to the number of groups (two after filtering); if it has names, they must exactly match the factor levels.
#' @param digits numeric(1) Number of decimal places to use in effect size results.
#' @param alpha numeric(1) Fill transparency for violin and boxplot geoms,
#'   passed to the \code{alpha} argument of
#'   [ggplot2::geom_violin()] and [ggplot2::geom_boxplot()]. Values range from
#'   0 (fully transparent) to 1 (fully opaque). Default \code{0.7}.
#' @param effect_size character(1) Type of effect size to compute. Either
#'   \code{"median_difference"} (default; Hodges-Lehmann estimate of location
#'   shift) or \code{"c_index"} (concordance probability using
#'   \code{asht::wmwTest}).
#' @param facet_cols character vector of column names to facet by.
#'   Default \code{NULL} (single panel).
#' @param facet_pvalue character(1) When \code{facet_cols} is supplied, which
#'   p-value information to display in annotations. One of "pvalue",
#'   "qvalue", or "both" (default). Q-values are always computed when \code{facet_cols}
#'   is supplied using FDR correction.
#' @param text_effectsize_vjust numeric(1) Vertical justification for the
#'   effect size annotation text (used when \code{facet_cols} is supplied).
#'   Default \code{1.5}.
#' @param text_n_vjust numeric(1) Vertical justification for the sample size
#'   annotation text (used when \code{facet_cols} is supplied). Default
#'   \code{-0.4}.
#' @param text_effectsize_prefix character(1) Prefix text for the effect size
#'   annotation. Default \code{"Median diff: "}.
#' @return A list with elements:
#' \describe{
#'   \item{ggplot}{A ggplot2 object (violin + boxplot). Add
#'     \code{+ ggplot2::facet_wrap()} or \code{+ ggplot2::facet_grid()} to
#'     create multi-panel layouts; annotations facet automatically.}
#'   \item{wilcox}{A tibble with the Wilcoxon test results (from
#'     \code{broom::tidy}). When \code{facet_cols} is supplied the tibble
#'     includes the faceting column(s) identifying which facet each row belongs
#'     to.}
#' }
#' @examples
#' ggplot2::theme_set(theme_bw2())
#' mtcars$am <- factor(mtcars$am)
#'
#' # Basic example
#' res <- plot_numeric_by_2groups("mpg", "am", mtcars)
#' res$ggplot
#' res$wilcox
#'
#' # Show C-index effect size instead of median difference
#' plot_numeric_by_2groups(
#'   yvar = "mpg",
#'   group = "am",
#'   d = mtcars,
#'   effect_size = "c_index"
#' )
#'
#' # Faceted example: compare a "score" between exercisers and non-exercisers,
#' # faceted by group (A/B/C/D)
#' set.seed(42)
#' n <- 200
#' df <- data.frame(
#'   score    = c(rnorm(n / 2, mean = 5), rnorm(n / 2, mean = 6)),
#'   exercise = factor(rep(c("No", "Yes"), each = n / 2)),
#'   group    = factor(sample(LETTERS[1:4], n, replace = TRUE))
#' )
#' res_facet <- plot_numeric_by_2groups(
#'   yvar       = "score",
#'   group      = "exercise",
#'   d          = df,
#'   facet_cols = "group",
#'   colors     = c("No" = "grey80", "Yes" = "steelblue"),
#'   alpha      = 0.6,
#'   effect_size = "c_index"
#' )
#' res_facet$ggplot + ggplot2::facet_wrap(~group, ncol = 2)
#' res_facet$wilcox
#' @importFrom stats wilcox.test reformulate
#' @importFrom broom tidy
#' @export
plot_numeric_by_2groups <- function(
    yvar,
    group,
    d,
    colors = c('white', 'white'),
    digits = ifelse(effect_size == "median_difference", 1, 2),
    alpha = 0.7,
    effect_size = c("median_difference"),
    facet_cols = NULL,
    facet_pvalue = "both",
    text_effectsize_vjust = 1.5,
    text_n_vjust = -0.4,
    text_effectsize_prefix = ifelse(
        effect_size == "median_difference",
        "Median diff: ",
        "C-index: "
    )
) {
    stopifnot(
        effect_size %in%
            c("median_difference", "c_index") &
            length(effect_size) == 1
    )
    stopifnot(length(yvar) == 1, length(group) == 1, is.data.frame(d))
    stopifnot(all(c(yvar, group) %in% names(d)))
    stopifnot(is.factor(d[[group]]) & length(levels(d[[group]])) == 2)
    stopifnot(is.numeric(digits), length(digits) == 1, digits >= 0)

    stopifnot(
        facet_pvalue %in%
            c("pvalue", "qvalue", "both") &
            length(facet_pvalue) == 1
    )
    stopifnot(
        facet_pvalue %in%
            c("pvalue", "qvalue", "both") &
            length(facet_pvalue) == 1
    )
    if (!is.null(facet_cols)) {
        missing_facet <- setdiff(facet_cols, names(d))
        if (length(missing_facet)) {
            stop(
                "`facet_cols` column(s) not found in `d`: ",
                paste(missing_facet, collapse = ", ")
            )
        }
    }

    keep_cols <- unique(c(yvar, group, facet_cols))
    na_mask <- !is.na(d[[yvar]]) & !is.na(d[[group]])
    if (!is.null(facet_cols)) {
        for (fc in facet_cols) {
            na_mask <- na_mask & !is.na(d[[fc]])
        }
    }
    d_sub <- d[na_mask, keep_cols, drop = FALSE]

    # drop any leftover factor levels for the grouping variable so that later
    # checks and coloring logic reflect only the observed groups.  This also
    # ensures an early, friendly error if one of the two original levels
    # disappears after filtering (see tests).
    d_sub[[group]] <- droplevels(d_sub[[group]])

    # require that both levels are present with at least one observation each
    counts <- table(d_sub[[group]])
    if (length(counts) != 2L || any(counts == 0L)) {
        stop(
            "must have observations in both levels of '",
            group,
            "' after removing missing values"
        )
    }

    group_levels <- levels(d_sub[[group]])

    # -------------------------------------------------------------------------
    # Wilcoxon test(s)
    # -------------------------------------------------------------------------
    f <- stats::reformulate(
        sprintf("`%s`", group),
        response = sprintf("`%s`", yvar)
    )

    if (is.null(facet_cols)) {
        # --- single test, original behaviour ---
        if (effect_size == "median_difference") {
            w <- stats::wilcox.test(f, data = d_sub, conf.int = TRUE)
            wilcox_res <- broom::tidy(w, conf.int = TRUE)
            wilcox_res$outcome <- yvar
            est <- as.numeric(wilcox_res$estimate)
            lo <- as.numeric(wilcox_res$conf.low)
            hi <- as.numeric(wilcox_res$conf.high)
            pval <- w$p.value
        } else {
            # effect_size == "c_index"
            w <- asht::wmwTest(f, data = d_sub)
            est <- as.numeric(w$estimate)
            ci <- as.numeric(w$conf.int)
            lo <- ci[1]
            hi <- ci[2]
            pval <- w$p.value
            # Create a broom-style tibble for consistency
            wilcox_res <- data.frame(
                estimate = est,
                statistic = NA_real_,
                p.value = pval,
                conf.low = lo,
                conf.high = hi,
                method = w$method,
                alternative = w$alternative,
                outcome = yvar,
                stringsAsFactors = FALSE
            )
        }

        # Build annotation data for inside the plot (no subtitle)
        yr <- range(d_sub[[yvar]], na.rm = TRUE)

        est_str <- sprintf(
            paste0(
                "%.",
                as.integer(digits),
                "f (%.",
                as.integer(digits),
                "f, %.",
                as.integer(digits),
                "f)"
            ),
            est,
            lo,
            hi
        )

        diff_expr <- sprintf('"%s%s"', text_effectsize_prefix, est_str)
        if (pval < 0.001) {
            sci <- formatC(pval, format = "e", digits = 2)
            parts <- strsplit(sci, "e", fixed = TRUE)[[1]]
            coef <- trimws(parts[1])
            exp_val <- as.integer(parts[2])
            p_expr <- sprintf(
                '"p =" ~ %s %%*%% 10^{%d}',
                coef,
                exp_val
            )
        } else {
            pval_str <- trimws(format_pvalue(
                pval
            ))
            p_expr <- sprintf('"%s"', pval_str)
        }

        pval_anno <- data.frame(
            .x = mean(seq_along(group_levels)),
            .lbl = sprintf("atop(%s, %s)", diff_expr, p_expr),
            .y = yr[2] + diff(yr) * 0.15,
            stringsAsFactors = FALSE
        )

        # n= labels near bottom
        counts_per_group <- vapply(
            group_levels,
            function(l) sum(d_sub[[group]] == l),
            integer(1)
        )

        n_anno <- do.call(
            rbind,
            lapply(seq_along(group_levels), function(j) {
                data.frame(
                    .x = group_levels[j],
                    .lbl = paste0("n=", counts_per_group[j]),
                    .y = -Inf,
                    stringsAsFactors = FALSE
                )
            })
        )

        subtitle <- NULL
    } else {
        # --- one test per unique combination of facet_cols ---
        facet_groups <- unique(d_sub[, facet_cols, drop = FALSE])
        wilcox_list <- lapply(seq_len(nrow(facet_groups)), function(i) {
            mask <- rep(TRUE, nrow(d_sub))
            for (fc in facet_cols) {
                mask <- mask & (d_sub[[fc]] == facet_groups[[fc]][i])
            }
            d_lvl <- d_sub[mask, , drop = FALSE]
            d_lvl[[group]] <- droplevels(d_lvl[[group]])
            if (length(levels(d_lvl[[group]])) < 2L) {
                return(NULL)
            }

            if (effect_size == "median_difference") {
                w <- tryCatch(
                    stats::wilcox.test(f, data = d_lvl, conf.int = TRUE),
                    error = function(e) NULL
                )
                if (is.null(w)) {
                    return(NULL)
                }
                res <- broom::tidy(w, conf.int = TRUE)
                res$outcome <- yvar
            } else {
                # effect_size == "c_index"
                w <- tryCatch(
                    asht::wmwTest(f, data = d_lvl),
                    error = function(e) NULL
                )
                if (is.null(w)) {
                    return(NULL)
                }
                ci <- as.numeric(w$conf.int)
                res <- data.frame(
                    estimate = as.numeric(w$estimate),
                    statistic = NA_real_,
                    p.value = w$p.value,
                    conf.low = ci[1],
                    conf.high = ci[2],
                    method = w$method,
                    alternative = w$alternative,
                    outcome = yvar,
                    stringsAsFactors = FALSE
                )
            }

            for (fc in facet_cols) {
                res[[fc]] <- facet_groups[[fc]][i]
            }
            res
        })
        wilcox_res <- do.call(
            rbind,
            wilcox_list[!vapply(wilcox_list, is.null, logical(1))]
        )

        if (!is.null(wilcox_res)) {
            wilcox_res$qvalue <- stats::p.adjust(
                wilcox_res$p.value,
                method = "fdr"
            )
        }

        subtitle <- NULL # per-panel annotations replace the subtitle
    }

    # -------------------------------------------------------------------------
    # Base plot
    # -------------------------------------------------------------------------
    p <- ggplot2::ggplot(
        d_sub,
        ggplot2::aes(
            x = .data[[group]],
            y = .data[[yvar]],
            fill = .data[[group]]
        )
    ) +
        ggplot2::geom_violin(trim = FALSE, width = 0.8, alpha = alpha) +
        ggplot2::geom_boxplot(width = 0.12, outlier.size = 1, alpha = alpha) +
        ggplot2::labs(subtitle = subtitle, x = group, y = yvar) +
        ggplot2::theme_bw() +
        ggplot2::theme(
            legend.position = "none",
            axis.title.x = ggplot2::element_text(face = "bold"),
            axis.title.y = ggplot2::element_text(face = "bold")
        )

    # -------------------------------------------------------------------------
    # x-axis n= labels  (global when no facet; per-panel geom_text when facet)
    # -------------------------------------------------------------------------
    if (is.null(facet_cols)) {
        # Add text annotations for effect size (top) and n= (bottom)
        p <- p +
            ggplot2::geom_text(
                data = pval_anno,
                ggplot2::aes(x = .x, y = Inf, label = .lbl),
                inherit.aes = FALSE,
                size = 3.2,
                vjust = text_effectsize_vjust,
                parse = TRUE
            ) +
            ggplot2::geom_text(
                data = n_anno,
                ggplot2::aes(x = .x, y = -Inf, label = .lbl),
                inherit.aes = FALSE,
                size = 3.2,
                vjust = text_n_vjust
            ) +
            ggplot2::coord_cartesian(clip = "off")
    } else {
        # Build annotation data frames that retain facet columns so that
        # facet_wrap() / facet_grid() added by the caller routes each label
        # to the correct panel automatically (same pattern as
        # plot_barplot_by_group).
        facet_groups <- unique(d_sub[, facet_cols, drop = FALSE])

        # per-facet y range used for annotation positioning
        y_ranges <- lapply(seq_len(nrow(facet_groups)), function(i) {
            mask <- rep(TRUE, nrow(d_sub))
            for (fc in facet_cols) {
                mask <- mask & (d_sub[[fc]] == facet_groups[[fc]][i])
            }
            range(d_sub[[yvar]][mask], na.rm = TRUE)
        })

        # Stats annotation (top): line 1 = "Median diff: ...", line 2 = p-value
        # Uses plotmath atop() + parse=TRUE for superscript exponents on small p.
        # n= labels are placed separately near the x-axis (see n_anno below).
        pval_anno <- do.call(
            rbind,
            lapply(seq_len(nrow(facet_groups)), function(i) {
                mask <- rep(TRUE, nrow(wilcox_res))
                for (fc in facet_cols) {
                    mask <- mask & (wilcox_res[[fc]] == facet_groups[[fc]][i])
                }
                wr <- wilcox_res[mask, , drop = FALSE]
                if (nrow(wr) == 0L) {
                    return(NULL)
                }
                yr <- y_ranges[[i]]
                est <- as.numeric(wr$estimate[1])
                lo <- as.numeric(wr$conf.low[1])
                hi <- as.numeric(wr$conf.high[1])
                pval <- wr$p.value[1]
                est_str <- sprintf(
                    paste0(
                        "%.",
                        as.integer(digits),
                        "f (%.",
                        as.integer(digits),
                        "f, %.",
                        as.integer(digits),
                        "f)"
                    ),
                    est,
                    lo,
                    hi
                )
                diff_expr <- sprintf("%s%s", text_effectsize_prefix, est_str)
                qval <- if ("qvalue" %in% names(wr)) wr$qvalue[1] else NA_real_

                # Build p-value label
                pval_str <- trimws(format_pvalue(
                    pval
                ))
                p_lbl <- sprintf("%s", pval_str)

                # Build q-value label if available
                q_lbl <- NULL
                if (!is.na(qval)) {
                    if (qval < 0.001) {
                        sci <- formatC(qval, format = "e", digits = 2)
                        parts <- strsplit(sci, "e", fixed = TRUE)[[1]]
                        coef <- trimws(parts[1])
                        exp_val <- as.integer(parts[2])
                        q_lbl <- sprintf("q = %s × 10^%d", coef, exp_val)
                    } else {
                        qval_str <- trimws(format_pvalue(
                            qval,
                            p_text = "q"
                        ))
                        q_lbl <- sprintf("%s", qval_str)
                    }
                }

                # Build label based on facet_pvalue
                if (facet_pvalue == "pvalue") {
                    lbl <- paste(diff_expr, p_lbl, sep = "\n")
                } else if (facet_pvalue == "qvalue") {
                    lbl <- paste(
                        diff_expr,
                        if (!is.null(q_lbl)) q_lbl else p_lbl,
                        sep = "\n"
                    )
                } else if (facet_pvalue == "both") {
                    if (!is.null(q_lbl)) {
                        lbl <- paste(diff_expr, p_lbl, q_lbl, sep = "\n")
                    } else {
                        lbl <- paste(diff_expr, p_lbl, sep = "\n")
                    }
                }
                anno <- data.frame(
                    .x = mean(seq_along(group_levels)),
                    .lbl = lbl,
                    .y = yr[2] + diff(yr) * 0.15,
                    stringsAsFactors = FALSE
                )
                for (fc in facet_cols) {
                    anno[[fc]] <- facet_groups[[fc]][i]
                }
                anno
            })
        )

        # n= labels: one row per group per facet, placed just below the data
        n_anno <- do.call(
            rbind,
            lapply(seq_len(nrow(facet_groups)), function(i) {
                d_mask <- rep(TRUE, nrow(d_sub))
                for (fc in facet_cols) {
                    d_mask <- d_mask & (d_sub[[fc]] == facet_groups[[fc]][i])
                }
                d_lvl <- d_sub[d_mask, , drop = FALSE]
                yr <- y_ranges[[i]]
                rows <- lapply(seq_along(group_levels), function(j) {
                    lv <- group_levels[j]
                    n <- sum(d_lvl[[group]] == lv)
                    ann <- data.frame(
                        .x = lv,
                        .lbl = paste0("n=", n),
                        .y = -Inf,
                        stringsAsFactors = FALSE
                    )
                    for (fc in facet_cols) {
                        ann[[fc]] <- facet_groups[[fc]][i]
                    }
                    ann
                })
                do.call(rbind, rows)
            })
        )

        p <- p +
            ggplot2::geom_text(
                data = pval_anno,
                ggplot2::aes(x = .x, y = Inf, label = .lbl),
                inherit.aes = FALSE,
                size = 3.2,
                vjust = text_effectsize_vjust,
            ) +
            ggplot2::geom_text(
                data = n_anno,
                ggplot2::aes(x = .x, y = -Inf, label = .lbl),
                inherit.aes = FALSE,
                size = 3.2,
                vjust = text_n_vjust
            ) +
            ggplot2::coord_cartesian(clip = "off")
    }

    # -------------------------------------------------------------------------
    # Colour scale
    # -------------------------------------------------------------------------
    if (!is.null(colors)) {
        stopifnot(is.character(colors))
        stopifnot(length(colors) == length(group_levels))
        if (!is.null(names(colors))) {
            stopifnot(setequal(names(colors), group_levels))
        }
        p <- p + ggplot2::scale_fill_manual(values = colors)
    }

    list(ggplot = p, wilcox = wilcox_res)
}
