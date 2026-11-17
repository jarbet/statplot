#' Bar plot by group
#'
#' Create a grouped bar plot (geom_col) from a data frame. Reorders factor levels of
#' `xvar` by `yvar` (descending) and maps fill to `xvar`. Accepts a named or unnamed
#' color vector; unnamed vectors must match the number of groups.
#'
#' @param dat A data.frame containing the data to plot.
#' @param yvar Character. Name of the numeric outcome column.
#' @param xvar Character. Name of the grouping column (will be coerced/reordered as a factor).
#' @param colors Optional character vector of fill colours. If named, names should match group levels.
#'               If unnamed, its length must equal the number of groups. Missing group names are filled with "black".
#' @param sort Character. How to sort the bars: "desc" for descending, "asc" for ascending, or "none" for no reordering.
#' @param title Optional plot title
#' @param xvar_label Optional character label for x variable. If NULL the label is taken from the column's `"label"` attribute (e.g. set with `labelled::var_label()`), falling back to the column name.
#' @param yvar_label Optional character label for y variable. If NULL the label is taken from the column's `"label"` attribute (e.g. set with `labelled::var_label()`), falling back to the column name.
#' @param show_legend Logical. Whether to show the legend for fill colors. Default is FALSE.
#' @examples
#' ggplot2::theme_set(theme_bw2())
#' dat <- data.frame(group = c("A", "B", "C"), score = c(5, 3, 8))
#' plot_bars(dat, yvar = "score", xvar = "group", sort = "desc")
#' @export
plot_bars <- function(
    dat,
    yvar,
    xvar,
    colors = NULL,
    sort = c("desc", "asc", "none"),
    title = NULL,
    xvar_label = NULL,
    yvar_label = NULL,
    show_legend = FALSE
) {
    # basic argument validity checks
    stopifnot(is.data.frame(dat))
    stopifnot(is.character(xvar), length(xvar) == 1)
    stopifnot(is.character(yvar), length(yvar) == 1)
    stopifnot(is.null(colors) || is.character(colors))
    stopifnot(is.character(sort))
    stopifnot(is.null(title) | length(title) == 1)
    stopifnot(
        is.null(xvar_label) ||
            (is.character(xvar_label) && length(xvar_label) == 1)
    )
    stopifnot(
        is.null(yvar_label) ||
            (is.character(yvar_label) && length(yvar_label) == 1)
    )
    stopifnot(is.logical(show_legend), length(show_legend) == 1)

    sort <- match.arg(sort)

    if (!all(c(xvar, yvar) %in% names(dat))) {
        stop("Both 'xvar' and 'yvar' must be column names in 'dat'.")
    }
    dat2 <- dat

    xfac <- as.factor(dat2[[xvar]])
    if (sort != "none") {
        # reorder levels by the median of yvar within each level, ties kept
        # in their original order (equivalent to forcats::fct_reorder())
        med <- tapply(dat2[[yvar]], xfac, stats::median, na.rm = TRUE)
        new_order <- if (sort == "desc") {
            order(-med, na.last = FALSE)
        } else {
            order(med)
        }
        xfac <- factor(xfac, levels = levels(xfac)[new_order])
    }
    dat2[[xvar]] <- xfac
    # restore the variable label (if any) dropped by the factor conversion
    attr(dat2[[xvar]], "label") <- attr(dat[[xvar]], "label", exact = TRUE)

    # variable label stored in the "label" attribute (e.g. set by labelled
    # or haven), falling back to `default`
    get_col_label <- function(vec, default) {
        lab <- attr(vec, "label", exact = TRUE)
        if (is.null(lab) || length(lab) == 0 || identical(lab, "")) {
            default
        } else {
            as.character(lab[[1]])
        }
    }

    # Resolve labels for title: prefer supplied args, then the "label"
    # attribute, then the column name
    if (is.null(yvar_label)) {
        yvar_label <- get_col_label(dat2[[yvar]], yvar)
    } else {
        yvar_label <- as.character(yvar_label)
    }

    if (is.null(xvar_label)) {
        xvar_label <- get_col_label(dat2[[xvar]], xvar)
    } else {
        xvar_label <- as.character(xvar_label)
    }

    if (is.null(title)) {
        title <- paste(yvar_label, "by", xvar_label)
    }

    grp_levels <- levels(dat2[[xvar]])

    if (is.null(colors)) {
        colors <- rep("black", length(grp_levels))
        names(colors) <- grp_levels
    } else {
        if (is.null(names(colors))) {
            if (length(colors) == length(grp_levels)) {
                names(colors) <- grp_levels
            } else {
                stop(
                    "If 'colors' is unnamed it must have length equal to number of groups."
                )
            }
        } else {
            missing_groups <- setdiff(grp_levels, names(colors))
            if (length(missing_groups)) {
                colors[missing_groups] <- "black"
                colors <- colors[grp_levels]
            } else {
                colors <- colors[grp_levels]
            }
        }
    }

    p <- dat2 |>
        ggplot2::ggplot(ggplot2::aes(
            x = .data[[xvar]],
            y = .data[[yvar]],
            fill = .data[[xvar]]
        )) +
        ggplot2::geom_col() +
        ggplot2::scale_fill_manual(values = colors) +
        ggplot2::labs(
            title = title,
            x = xvar_label,
            y = yvar_label,
            fill = xvar_label
        ) +
        ggplot2::theme_minimal() +
        ggplot2::theme(
            plot.title = ggtext::element_markdown(face = "bold"),
            axis.title.x = ggtext::element_markdown(face = "bold"),
            axis.title.y = ggtext::element_markdown(face = "bold"),
            legend.title = ggtext::element_markdown(face = "bold")
        )

    if (!show_legend) {
        p <- p + ggplot2::guides(fill = "none")
    }

    p
}
