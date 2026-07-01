#' Kaplan–Meier plot from a Surv object
#'
#' Plot a Kaplan–Meier curve (with confidence intervals and optional risk table)
#' using a supplied \code{Surv} object. If the grouping variable has exactly two
#' levels the function fits a Cox model and annotates hazard ratio (95% CI) and
#' p-value; if more than two groups it displays only the log-rank p-value.
#'
#' @param surv_obj A \code{Surv} object (can be right-censored or left-truncated).
#' @param data A data frame containing the variables referenced by
#'   \code{surv_obj} and \code{group_var}.
#' @param group_var Character, name of the grouping column in \code{data}.
#' @param time_limits Numeric(2), x-axis limits for the plot. If \code{NULL},
#'   sensible limits are estimated from the observed event times.
#' @param x_breaks Numeric vector of x-axis breaks. If \code{NULL}, reasonable
#'   breaks are selected automatically.
#' @param annotate_y Numeric, y position for annotation text (default 0.99).
#' @param annotate_x Numeric or \code{NULL}, x position for annotation; if
#'   \code{NULL} uses the rightmost value of \code{time_limits}.
#' @param x_label Character, label for the x axis.
#' @param y_label Character, label for the y axis. If \code{NULL}, a default
#'   label is chosen based on \code{type}.
#' @param title Character or \code{NULL}, plot title.
#' @param custom_hr_pvalue_text Character or \code{NULL}; if supplied, overrides
#'   the automatically generated HR/log-rank annotation text.
#' @param type Character, one of \code{"survival"} or \code{"risk"}; passed to
#'   \code{ggsurvfit()}. If \code{"survival"}, the default y-axis label is
#'   \code{"Probability Event-Free"}. If \code{"risk"}, the default y-axis
#'   label is \code{"Probability of Event"}.
#' @param show_risktable Logical; if \code{TRUE} (default), display a risk table
#'   beneath the survival curve.
#' @param risktable_stats Character vector specifying statistics shown in the
#'   risk table. Must contain one or more of:
#'   \code{c("n.risk", "cum.event", "cum.censor", "n.event", "n.censor")}.
#'   The default is \code{c("n.risk", "cum.event")}.
#' @param ristable_text_size Numeric, text size for the risk table (default 3.5).
#'
#'   Available statistics:
#'   \itemize{
#'     \item \code{"n.risk"} Number of patients at risk
#'     \item \code{"cum.event"} Cumulative number of observed events
#'     \item \code{"cum.censor"} Cumulative number of censored observations
#'     \item \code{"n.event"} Number of events in each time interval
#'     \item \code{"n.censor"} Number of censored observations in each time interval
#'   }
#'
#' @examples
#' data(cancer, package = "survival")
#'
#' # Example with two groups
#' lung$sex <- factor(lung$sex, labels = c("Male", "Female"))
#' surv_obj <- with(lung, survival::Surv(time, status == 2))
#'
#' plot_survival_curves(
#'     surv_obj,
#'     lung,
#'     group_var = "sex"
#' ) + theme_bw2()
#'
#' # Hide the risk table
#' plot_survival_curves(
#'     surv_obj,
#'     lung,
#'     group_var = "sex",
#'     show_risktable = FALSE
#' ) + theme_bw2()
#'
#' # Show only number at risk
#' plot_survival_curves(
#'     surv_obj,
#'     lung,
#'     group_var = "sex",
#'     risktable_stats = "n.risk"
#' ) + theme_bw2()
#'
#' # Example with more than two groups
#' lung$ph.ecog[lung$ph.ecog == 3] <- NA
#' lung$ph.ecog <- factor(lung$ph.ecog)
#'
#' plot_survival_curves(
#'     surv_obj,
#'     lung,
#'     group_var = "ph.ecog"
#' ) + theme_bw2()
#'
#' # Cumulative incidence plot
#' plot_survival_curves(
#'     surv_obj,
#'     lung,
#'     group_var = "ph.ecog",
#'     type = "risk"
#' ) + theme_bw2()
#'
#' @return A ggsurvfit ggplot object.
#'
#' @importFrom survival Surv coxph survdiff
#' @importFrom broom tidy
#' @importFrom ggsurvfit survfit2 ggsurvfit add_confidence_interval add_risktable
#' @importFrom stats pchisq
#' @importFrom ggtext geom_richtext
#' @export
plot_survival_curves <- function(
    surv_obj,
    data,
    group_var = "met_exercise_guidelines",
    time_limits = NULL,
    x_breaks = NULL,
    annotate_y = 0.99,
    annotate_x = NULL,
    x_label = "Time (units??)",
    y_label = NULL,
    title = NULL,
    custom_hr_pvalue_text = NULL,
    type = c("survival", "risk"),
    show_risktable = TRUE,
    risktable_stats = c("n.risk", "cum.event"),
    ristable_text_size = 3.5
) {
    stopifnot(
        inherits(surv_obj, "Surv"),
        is.data.frame(data),
        length(surv_obj) == nrow(data),
        is.character(group_var),
        length(group_var) == 1,
        group_var %in% names(data),

        (is.null(time_limits) ||
            (is.numeric(time_limits) &&
                length(time_limits) == 2 &&
                is.finite(time_limits[1]) &&
                is.finite(time_limits[2]) &&
                time_limits[1] < time_limits[2])),

        (is.null(x_breaks) ||
            (is.numeric(x_breaks) && length(x_breaks) >= 1)),

        is.numeric(annotate_y),
        length(annotate_y) == 1,
        is.finite(annotate_y),
        annotate_y >= 0,
        annotate_y <= 1,

        (is.null(annotate_x) ||
            (is.numeric(annotate_x) &&
                length(annotate_x) == 1 &&
                is.finite(annotate_x))),

        is.character(x_label),
        length(x_label) == 1,

        (is.null(y_label) ||
            (is.character(y_label) && length(y_label) == 1)),

        (is.null(title) || is.character(title)),

        (is.null(custom_hr_pvalue_text) ||
            length(custom_hr_pvalue_text) == 1),

        is.character(type),
        type %in% c("survival", "risk"),

        is.logical(show_risktable),
        length(show_risktable) == 1,

        is.character(risktable_stats),
        length(risktable_stats) >= 1,

        all(
            risktable_stats %in%
                c(
                    "n.risk",
                    "cum.event",
                    "cum.censor",
                    "n.event",
                    "n.censor"
                )
        ),
        is.numeric(ristable_text_size),
        length(ristable_text_size) == 1,
        is.finite(ristable_text_size),
        ristable_text_size > 0
    )

    type <- match.arg(type)

    if (is.null(y_label)) {
        y_label <- if (identical(type, "survival")) {
            "Probability Event-Free"
        } else {
            "Probability of Event"
        }
    }

    d_sub <- data
    d_sub$.surv_obj <- surv_obj

    keep <- !is.na(d_sub$.surv_obj) &
        !is.na(d_sub[[group_var]])

    d_sub <- d_sub[keep, , drop = FALSE]

    if (nrow(d_sub) == 0) {
        stop("No rows remaining after filtering NA surv or group")
    }

    if (is.null(time_limits) || is.null(x_breaks)) {
        surv_formula_tmp <- as.formula(
            sprintf(".surv_obj ~ `%s`", group_var)
        )

        sf_tmp <- survival::survfit(
            surv_formula_tmp,
            data = d_sub
        )

        times <- sf_tmp$time

        if (length(times) == 0) {
            times <- c(0)
        }

        t_lower <- floor(
            stats::quantile(
                times,
                probs = 0.01,
                na.rm = TRUE
            )
        )

        t_upper <- ceiling(
            stats::quantile(
                times,
                probs = 0.99,
                na.rm = TRUE
            )
        )

        if (
            !is.finite(t_lower) ||
                !is.finite(t_upper) ||
                t_lower == t_upper
        ) {
            t_lower <- floor(min(times, na.rm = TRUE))
            t_upper <- ceiling(max(times, na.rm = TRUE))
        }

        if (
            !is.finite(t_lower) ||
                !is.finite(t_upper) ||
                t_lower >= t_upper
        ) {
            t_lower <- floor(min(times, na.rm = TRUE))

            t_upper <- t_lower +
                max(
                    1,
                    ceiling(
                        max(times, na.rm = TRUE) - t_lower
                    )
                )
        }

        if (is.null(time_limits)) {
            time_limits <- c(t_lower, t_upper)
        }

        if (is.null(x_breaks)) {
            brks <- pretty(time_limits, n = 6)

            brks <- brks[
                brks >= time_limits[1] &
                    brks <= time_limits[2]
            ]

            if (length(brks) == 0) {
                brks <- seq(
                    time_limits[1],
                    time_limits[2],
                    length.out = 5
                )
            }

            x_breaks <- brks
        }
    }

    grp_factor <- factor(d_sub[[group_var]])
    n_groups <- nlevels(grp_factor)

    if (n_groups < 2) {
        stop("Need at least 2 groups in group_var")
    }

    if (n_groups == 2) {
        cox_formula <- as.formula(
            sprintf(".surv_obj ~ `%s`", group_var)
        )

        fit <- survival::coxph(
            cox_formula,
            data = d_sub
        )

        fit_res <- broom::tidy(
            fit,
            conf.int = TRUE,
            exponentiate = TRUE
        )

        hr_text <- sprintf(
            "HR = %.2f (%.2f-%.2f), p",
            fit_res$estimate[1],
            fit_res$conf.low[1],
            fit_res$conf.high[1]
        )

        annot_label <- format_pvalue(
            x = fit_res$p.value[1],
            p_text = hr_text,
            p_symbol = "= "
        )
    } else {
        sd_formula <- as.formula(
            sprintf(".surv_obj ~ `%s`", group_var)
        )

        sd <- survival::survdiff(
            sd_formula,
            data = d_sub
        )

        p_val <- stats::pchisq(
            sd$chisq,
            length(sd$n) - 1,
            lower.tail = FALSE
        )

        annot_label <- format_pvalue(p_val)
    }

    if (!is.null(custom_hr_pvalue_text)) {
        annot_label <- custom_hr_pvalue_text
    }

    surv_formula <- as.formula(
        sprintf(".surv_obj ~ `%s`", group_var)
    )

    kmplot <- ggsurvfit::survfit2(
        surv_formula,
        data = d_sub
    ) |>
        ggsurvfit::ggsurvfit(type = type) +
        ggsurvfit::add_confidence_interval()

    if (show_risktable) {
        kmplot <- kmplot +
            ggsurvfit::add_risktable(
                risktable_stats = risktable_stats,
                size = ristable_text_size
            )
    }

    kmplot <- kmplot +
        ggplot2::scale_x_continuous(
            limits = time_limits,
            breaks = x_breaks
        ) +
        ggplot2::labs(
            x = x_label,
            y = y_label,
            title = title
        )

    if (is.null(annotate_x)) {
        annotate_x <- time_limits[2]
    }

    kmplot <- kmplot +
        ggtext::geom_richtext(
            data = data.frame(
                x = annotate_x,
                y = annotate_y,
                label = annot_label
            ),
            ggplot2::aes(
                x = x,
                y = y,
                label = label
            ),
            hjust = 1,
            vjust = 1,
            size = 3.5,
            fill = NA,
            label.color = NA
        )

    kmplot
}
