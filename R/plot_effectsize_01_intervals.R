#' Plot effect-size estimates and confidence intervals.  All values must be between 0 and 1.
#'
#' Create a horizontal interval plot of effect-size estimates (values between 0 and 1)
#' with their lower/upper confidence bounds. The y-axis shows effect labels and the
#' x-axis gives the effect size values.
#'
#' @param d data.frame: data containing the effect labels, estimates and bounds.
#' @param variable_name character(1): column name in `d` with variable names for the y-axis.
#' @param value_name character(1): column name in `d` with estimates (numeric, expected 0..1).
#' @param group_name character(1) or NULL: optional column name in `d` used to color points/lines by group.
#' @param group_label character(1) or NULL: label to show in the legend when `group_name` is used. Defaults to `group_name`.
#' @param colors character(1) or NULL: optional character vector (or named character vector) of colours to map to groups; names should match group values. If NULL (default) uses ggplot2 defaults.
#' @param lower_name character(1): column name in `d` with CI lower bound (numeric, default "lower").
#' @param upper_name character(1): column name in `d` with CI upper bound (numeric, default "upper").
#' @param sort logical: sort rows by the estimate before plotting (default TRUE).
#' @param descending logical: if TRUE place largest estimates at the top (default TRUE).
#' @param xaxis_label character(1): label for the x axis (default "Effect size").
#' @param title character(1): plot title (default "Effect size estimates with 95% confidence intervals").
#' @param dot_size numeric(1): size of the points (default 3). Use smaller values to make points less prominent relative to CI lines.
#' @return A ggplot object (horizontal interval plot).
#' @examples
#' ggplot2::theme_set(theme_bw2())
#' df <- data.frame(
#'   effect = paste0("Effect ", LETTERS[1:5]),
#'   estimate = c(0.12, 0.35, 0.58, 0.74, 0.91),
#'   lower = c(0.05, 0.22, 0.45, 0.60, 0.82),
#'   upper = c(0.20, 0.47, 0.69, 0.85, 0.97),
#'   group = rep(c("One","Two"), length.out = 5)
#' )
#'
#' # Example with no group colors
#' plot_effectsize_01_intervals(
#'   d = df,
#'   variable_name = "effect",
#'   value_name = "estimate",
#'   lower_name = "lower",
#'   upper_name = "upper"
#' )
#'
#' # Example with group colors
#' plot_effectsize_01_intervals(
#'   d = df,
#'   variable_name = "effect",
#'   value_name = "estimate",
#'   group_name = "group",
#'   group_label = "My groups",
#'   lower_name = "lower",
#'   upper_name = "upper",
#'   dot_size = 1.5
#' )
#' @export
plot_effectsize_01_intervals <- function(
    d,
    variable_name, # column in d with effect labels (character/factor)
    value_name, # column in d with estimates (numeric, 0..1)
    group_name = NULL, # optional column in d to color points/lines by
    group_label = group_name, # label to use for legend (defaults to group_name)
    colors = NULL, # optional character vector (or named vector) of colours for groups
    xaxis_label = "Effect size", # x-axis label
    lower_name = "lower", # column in d with CI lower bound (numeric, 0..1)
    upper_name = "upper", # column in d with CI upper bound (numeric, default "upper")
    title = 'Effect size estimates with 95% confidence intervals',
    sort = TRUE, # sort by estimate
    descending = FALSE, # largest at top if TRUE
    dot_size = 3 # numeric(1) size for the point glyph
) {
    ## -- argument type/length checks ------------------------------------------------
    stopifnot(
        is.data.frame(d),
        is.character(variable_name),
        length(variable_name) == 1,
        is.character(value_name),
        length(value_name) == 1,
        is.character(lower_name),
        length(lower_name) == 1,
        is.character(upper_name),
        length(upper_name) == 1,
        is.character(xaxis_label),
        length(xaxis_label) == 1, # required by user
        is.character(title),
        length(title) == 1,
        is.logical(sort),
        length(sort) == 1,
        is.logical(descending),
        length(descending) == 1,
        (is.null(colors) || is.character(colors)),
        is.numeric(dot_size),
        length(dot_size) == 1
    )
    if (dot_size <= 0) {
        stop("`dot_size` must be a positive number.")
    }
    # validate group_name
    if (
        !is.null(group_name) &&
            !(is.character(group_name) && length(group_name) == 1)
    ) {
        stop(
            "`group_name` must be NULL or a single character string naming a column in `d`"
        )
    }
    # validate group_label
    if (
        !is.null(group_label) &&
            !(is.character(group_label) && length(group_label) == 1)
    ) {
        stop(
            "`group_label` must be NULL or a single character string to use as the legend label"
        )
    }
    # validate colors usage
    if (!is.null(colors) && is.null(group_name)) {
        stop(
            "`colors` supplied but `group_name` is NULL. Supply `group_name` to use `colors`."
        )
    }

    # Basic column checks
    cols_needed <- c(variable_name, value_name, lower_name, upper_name)
    if (!is.null(group_name)) {
        cols_needed <- c(cols_needed, group_name)
    }
    missing_cols <- setdiff(cols_needed, names(d))
    if (length(missing_cols) > 0) {
        stop("Missing columns in `d`: ", paste(missing_cols, collapse = ", "))
    }

    # Warn if values fall outside [0, 1]
    rng <- range(
        d[[value_name]],
        d[[lower_name]],
        d[[upper_name]],
        na.rm = TRUE
    )
    if (rng[1] < 0 || rng[2] > 1) {
        stop(
            "All values of should be between 0 and 1."
        )
    }

    # Optional sorting by estimate
    d <- d
    if (sort) {
        ord <- order(d[[value_name]], decreasing = descending, na.last = NA)
        d <- d[ord, , drop = FALSE]
        # Preserve the sorted order on the y-axis
        d[[variable_name]] <- factor(
            d[[variable_name]],
            levels = unique(d[[variable_name]])
        )
    }
    # Build the plot (conditionally add color aesthetic when group_name provided)
    if (is.null(group_name)) {
        p <- ggplot2::ggplot(
            d,
            ggplot2::aes(y = .data[[variable_name]], x = .data[[value_name]])
        ) +
            ggplot2::geom_errorbarh(
                ggplot2::aes(
                    xmin = .data[[lower_name]],
                    xmax = .data[[upper_name]]
                ),
                width = 0.15
            ) +
            ggplot2::geom_point(size = dot_size)
    } else {
        p <- ggplot2::ggplot(
            d,
            ggplot2::aes(
                y = .data[[variable_name]],
                x = .data[[value_name]],
                color = .data[[group_name]]
            )
        ) +
            ggplot2::geom_errorbarh(
                ggplot2::aes(
                    xmin = .data[[lower_name]],
                    xmax = .data[[upper_name]]
                ),
                width = 0.15
            ) +
            ggplot2::geom_point(size = dot_size) +
            ggplot2::labs(color = group_label)

        if (!is.null(colors)) {
            p <- p + ggplot2::scale_color_manual(values = colors)
        }
    }

    p <- p +
        ggplot2::scale_x_continuous(limits = c(0, 1), breaks = seq(0, 1, 0.2)) +
        ggplot2::labs(
            y = NULL,
            x = xaxis_label,
            title = title
        ) +
        ggplot2::theme_bw() +
        ggplot2::theme(
            plot.title = ggplot2::element_text(face = "bold"),
            axis.title.x = ggplot2::element_text(face = "bold")
        )

    return(p)
}
