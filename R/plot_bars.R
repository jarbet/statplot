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
#' @param xvar_label Optional character label for x variable. If NULL the label is obtained using labelled::get_variable_labels().
#' @param yvar_label Optional character label for y variable. If NULL the label is obtained using labelled::get_variable_labels().
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

    if (sort == "desc") {
        dat2[[xvar]] <- forcats::fct_reorder(
            as.factor(dat2[[xvar]]),
            dat2[[yvar]],
            .desc = TRUE
        )
    } else if (sort == "asc") {
        dat2[[xvar]] <- forcats::fct_reorder(
            as.factor(dat2[[xvar]]),
            dat2[[yvar]],
            .desc = FALSE
        )
    } else if (sort == "none") {
        dat2[[xvar]] <- as.factor(dat2[[xvar]])
    }
    # restore labels if any
    labelled::var_label(dat2) <- labelled::var_label(dat)

    # helper: previous behavior to extract label from attr
    get_col_label <- function(vec, default) {
        lab <- attr(vec, "label", exact = TRUE)
        if (is.null(lab) || length(lab) == 0 || identical(lab, "")) {
            default
        } else {
            as.character(lab)
        }
    }

    # Resolve labels for title: prefer supplied args, then labelled::get_variable_labels(), then fallback
    if (is.null(yvar_label)) {
        y_lab_raw <- labelled::get_variable_labels(dat2[[yvar]])
        if (
            is.null(y_lab_raw) ||
                length(y_lab_raw) == 0 ||
                identical(y_lab_raw, "")
        ) {
            yvar_label <- get_col_label(dat2[[yvar]], yvar)
        } else {
            yvar_label <- as.character(y_lab_raw[[1]])
        }
    } else {
        yvar_label <- as.character(yvar_label)
    }

    if (is.null(xvar_label)) {
        x_lab_raw <- labelled::get_variable_labels(dat2[[xvar]])
        if (
            is.null(x_lab_raw) ||
                length(x_lab_raw) == 0 ||
                identical(x_lab_raw, "")
        ) {
            xvar_label <- get_col_label(dat2[[xvar]], xvar)
        } else {
            xvar_label <- as.character(x_lab_raw[[1]])
        }
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
            plot.title = ggplot2::element_text(face = "bold"),
            axis.title.x = ggplot2::element_text(face = "bold"),
            axis.title.y = ggplot2::element_text(face = "bold"),
            legend.title = ggplot2::element_text(face = "bold")
        )

    if (!show_legend) {
        p <- p + ggplot2::guides(fill = "none")
    }

    p
}
