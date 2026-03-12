#' Violin + boxplot with Wilcoxon rank-sum test
#'
#' Create a violin plot overlaid with a narrow boxplot for two groups and run a
#' Wilcoxon rank-sum test. Returns the ggplot object and a tidy Wilcoxon result.
#'
#' @param yvar character(1) Name of the numeric outcome column in `d`.
#' @param group character(1) Name of the factor column in `d` (must have 2 levels).
#' @param d data.frame Data containing `yvar` and `group`.
#' @param title character Optional plot title. If NULL, a default is generated.
#' @param colors character vector Length-2 vector of fill colours (default c('white', 'white')); if NULL the default ggplot2 fill scale is used.  When supplied, must be a character vector with length equal to the number of groups (two after filtering); if it has names, they must exactly match the factor levels.
#' @param digits numeric(1) Number of decimal places to use in the subtitle statistics.
#' @return A list with elements:
#' \describe{
#'   \item{ggplot}{A ggplot2 object (violin + boxplot).}
#'   \item{wilcox}{A tibble with the Wilcoxon test results (from broom::tidy).}
#' }
#' @examples
#' mtcars$am <- factor(mtcars$am)
#' res <- plot_numeric_by_2groups("mpg", "am", mtcars)
#' res$ggplot
#' res$wilcox
#' @importFrom stats wilcox.test reformulate
#' @importFrom broom tidy
#' @export
plot_numeric_by_2groups <- function(
    yvar,
    group,
    d,
    title = NULL,
    colors = c('white', 'white'),
    digits = 1
) {
    stopifnot(length(yvar) == 1, length(group) == 1, is.data.frame(d))
    stopifnot(all(c(yvar, group) %in% names(d)))
    stopifnot(is.factor(d[[group]]) & length(levels(d[[group]])) == 2)
    stopifnot(is.numeric(digits), length(digits) == 1, digits >= 0)

    d_sub <- d[!is.na(d[[yvar]]) & !is.na(d[[group]]), ]

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

    if (is.null(title)) {
        title <- paste0(yvar, " by ", group)
    }

    # build formula with backticks so names with spaces or symbols are supported
    f <- stats::reformulate(
        sprintf("`%s`", group),
        response = sprintf("`%s`", yvar)
    )
    w <- stats::wilcox.test(
        f,
        data = d_sub,
        conf.int = TRUE
    )
    wilcox_res <- broom::tidy(w, conf.int = TRUE)
    wilcox_res$outcome <- yvar
    pval <- w$p.value
    pval_text <- format_pvalue(pval)

    fmt <- paste0(
        'Median difference: %.',
        as.integer(digits),
        'f (%.',
        as.integer(digits),
        'f, %.',
        as.integer(digits),
        'f), %s'
    )
    subtitle <- sprintf(
        fmt,
        as.numeric(wilcox_res$estimate),
        as.numeric(wilcox_res$conf.low),
        as.numeric(wilcox_res$conf.high),
        pval_text
    )

    p <- ggplot2::ggplot(
        d_sub,
        ggplot2::aes(
            x = .data[[group]],
            y = .data[[yvar]],
            fill = .data[[group]]
        )
    ) +
        ggplot2::geom_violin(trim = FALSE, width = 0.8) +
        ggplot2::geom_boxplot(width = 0.12, outlier.size = 1, alpha = 0.7) +
        ggplot2::labs(title = title, subtitle = subtitle, x = group, y = yvar) +
        ggplot2::theme_bw() +
        ggplot2::theme(
            legend.position = "none",
            plot.title = ggplot2::element_text(face = "bold"),
            axis.title.x = ggplot2::element_text(face = "bold"),
            axis.title.y = ggplot2::element_text(face = "bold")
        )

    # add sample sizes under x-axis group labels
    group_levels <- levels(d_sub[[group]])
    counts <- vapply(
        group_levels,
        function(l) sum(d_sub[[group]] == l),
        integer(1)
    )
    x_labels <- paste0(group_levels, "\n(n=", counts, ")")
    p <- p + ggplot2::scale_x_discrete(labels = x_labels)

    # validate colours if supplied; checks mirror plot_numeric_by_3plusgroups
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
