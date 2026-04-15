#' Kaplan–Meier plot from a Surv object
#'
#' Plot a Kaplan–Meier curve (with confidence intervals and risk table) using a supplied
#' \code{Surv} object. If the grouping variable has exactly two levels the function fits
#' a Cox model and annotates hazard ratio (95% CI) and p-value; if more than two groups
#' it displays only the log‑rank p-value.
#'
#' @param surv_obj A \code{Surv} object (can be right‑censored or left‑truncated).
#' @param data A data frame containing the variables referenced by \code{surv_obj} and \code{group_var}.
#' @param group_var Character, name of the grouping column in \code{data}.
#' @param time_limits Numeric(2), x-axis limits for the plot (default c(50, 100)).
#' @param x_breaks Numeric, breaks for the x axis.
#' @param annotate_y Numeric, y position for the annotation text (default 0.99).
#' @param annotate_x Numeric or NULL, x position for annotation; if NULL uses rightmost \code{time_limits}.
#' @param x_label Character, label for the x axis (default \"Time\").
#' @param y_label Character, label for the y axis (default \"Probability Event-free\").
#' @param title Character or NULL, plot title (default NULL).
#' @param custom_hr_pvalue_text Character or NULL; if provided, this string will be used for the annotation
#'   instead of the automatically computed HR/p-value (for two groups) or log‑rank p-value (for >2 groups).
#' @param type Character, one of "survival" or "risk"; passed to ggsurvfit(). If
#'   "survival" the default y_label is "Probability Event-Free"; if "risk" the
#'   default y_label is "Probability of Event".
#'
#' @examples
#' ggplot2::theme_set(theme_bw2())
#' data(cancer, package = "survival")
#'
#' # Example with two groups
#' lung$sex <- factor(lung$sex, labels = c("Male", "Female"))
#' surv_obj <- with(lung, survival::Surv(time, status == 2))
#' plot_survival_curves(surv_obj, lung, group_var = "sex")
#'
#' # Example with more than two groups
#' lung$ph.ecog[lung$ph.ecog == 3] <- NA
#' lung$ph.ecog <- factor(lung$ph.ecog)
#' surv_obj <- with(lung, survival::Surv(time, status == 2))
#' plot_survival_curves(surv_obj, lung, group_var = "ph.ecog")
#'
#' # Cumulative incidence plot
#' plot_survival_curves(surv_obj, lung, group_var = "ph.ecog")
#' @return A ggplot object (ggsurvfit).
#'
#' @importFrom survival Surv coxph survdiff
#' @importFrom broom tidy
#' @importFrom ggsurvfit survfit2 ggsurvfit add_confidence_interval add_risktable
#' @importFrom stats pchisq
#' @export
plot_survival_curves <- function(
    surv_obj,
    data,
    group_var = "met_exercise_guidelines",
    time_limits = NULL, # changed: allow NULL to compute from data
    x_breaks = NULL, # changed: allow NULL to compute from data
    annotate_y = 0.99,
    annotate_x = NULL,
    x_label = "Time (units??)",
    y_label = NULL,
    title = NULL,
    custom_hr_pvalue_text = NULL,
    type = c("survival", "risk")
) {
    # argument validity checks using stopifnot()
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
        (is.null(x_breaks) || (is.numeric(x_breaks) && length(x_breaks) >= 1)),
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
        (is.null(y_label) || (is.character(y_label) && length(y_label) == 1)),
        (is.null(title) || is.character(title)),
        (is.null(custom_hr_pvalue_text) || length(custom_hr_pvalue_text) == 1),
        is.character(type),
        type %in% c("survival", "risk")
    )

    type <- match.arg(type)

    # set default y_label based on type if not provided
    if (is.null(y_label)) {
        y_label <- if (identical(type, "survival")) {
            "Probability Event-Free"
        } else {
            "Probability of Event"
        }
    }

    d_sub <- data
    d_sub$.surv_obj <- surv_obj
    keep <- !is.na(d_sub$.surv_obj) & !is.na(d_sub[[group_var]])
    d_sub <- d_sub[keep, , drop = FALSE]

    if (nrow(d_sub) == 0) {
        stop("No rows remaining after filtering NA surv or group")
    }

    # compute sensible defaults for time_limits and x_breaks from the observed event times
    if (is.null(time_limits) || is.null(x_breaks)) {
        surv_formula_tmp <- as.formula(sprintf(".surv_obj ~ `%s`", group_var))
        sf_tmp <- survival::survfit(surv_formula_tmp, data = d_sub)
        times <- sf_tmp$time
        if (length(times) == 0) {
            times <- c(0)
        }
        t_lower <- floor(stats::quantile(times, probs = 0.01, na.rm = TRUE))
        t_upper <- ceiling(stats::quantile(times, probs = 0.99, na.rm = TRUE))

        if (!is.finite(t_lower) || !is.finite(t_upper) || t_lower == t_upper) {
            t_lower <- floor(min(times, na.rm = TRUE))
            t_upper <- ceiling(max(times, na.rm = TRUE))
        }
        if (!is.finite(t_lower) || !is.finite(t_upper) || t_lower >= t_upper) {
            # fallback to a minimal sensible range
            t_lower <- floor(min(times, na.rm = TRUE))
            t_upper <- t_lower +
                max(1, ceiling((max(times, na.rm = TRUE) - t_lower)))
        }

        if (is.null(time_limits)) {
            time_limits <- c(t_lower, t_upper)
        }
        if (is.null(x_breaks)) {
            # use pretty() to pick human-friendly breaks and confine them to the limits
            brks <- pretty(time_limits, n = 6)
            brks <- brks[brks >= time_limits[1] & brks <= time_limits[2]]
            if (length(brks) == 0) {
                brks <- seq(time_limits[1], time_limits[2], length.out = 5)
            }
            x_breaks <- brks
        }
    }

    grp_factor <- factor(d_sub[[group_var]])
    n_groups <- nlevels(grp_factor)
    if (n_groups < 2) {
        stop("Need at least 2 groups in group_var")
    }

    # annotation label
    if (n_groups == 2) {
        cox_formula <- as.formula(sprintf(".surv_obj ~ `%s`", group_var))
        fit <- survival::coxph(cox_formula, data = d_sub)
        fit_res <- broom::tidy(fit, conf.int = TRUE, exponentiate = TRUE)
        hr_text <- sprintf(
            "HR = %.2f (%.2f-%.2f), p",
            fit_res$estimate[1],
            fit_res$conf.low[1],
            fit_res$conf.high[1]
        )
        pval_text <- BoutrosLab.plotting.general::display.statistical.result(
            x = fit_res$p.value[1],
            statistic.type = hr_text,
            symbol = "= "
        )
        annot_label <- as.character(pval_text)[1]
    } else {
        sd_formula <- as.formula(sprintf(".surv_obj ~ `%s`", group_var))
        sd <- survival::survdiff(sd_formula, data = d_sub)
        p_val <- stats::pchisq(sd$chisq, length(sd$n) - 1, lower.tail = FALSE)
        pval_text <- BoutrosLab.plotting.general::display.statistical.result(
            x = p_val,
            statistic.type = "p",
            symbol = "= "
        )
        annot_label <- pval_text[1]
    }

    # if user provided custom text, override the computed annotation
    if (!is.null(custom_hr_pvalue_text)) {
        annot_label <- custom_hr_pvalue_text
    }

    # KM plot using the provided Surv object column
    surv_formula <- as.formula(sprintf(".surv_obj ~ `%s`", group_var))
    kmplot <- ggsurvfit::survfit2(surv_formula, data = d_sub) |>
        ggsurvfit::ggsurvfit(type = type) +
        ggsurvfit::add_confidence_interval() +
        ggsurvfit::add_risktable(risktable_group = "risktable_stats") +
        ggplot2::scale_x_continuous(limits = time_limits, breaks = x_breaks) +
        ggplot2::labs(x = x_label, y = y_label, title = title)

    if (is.null(annotate_x)) {
        annotate_x <- time_limits[2]
    }
    kmplot <- kmplot +
        ggplot2::annotate(
            "text",
            x = annotate_x,
            y = annotate_y,
            label = annot_label,
            hjust = 1,
            vjust = 1,
            size = 3.5
        )

    kmplot
}
