#' Kaplan–Meier plot from a Surv object
#'
#' Plot a Kaplan–Meier curve (with confidence intervals and optional risk table)
#' using a supplied \code{Surv} object. If the grouping variable has exactly two
#' levels the function fits a Cox model and annotates hazard ratio (95% CI) and
#' p-value; if more than two groups it displays only the log-rank p-value.
#' Optional weights (e.g. IPTW) can be supplied to produce weighted survival
#' curves and a weighted Cox model annotation; see \code{weights} below.
#'
#' @param surv_obj A \code{Surv} object (can be right-censored or left-truncated).
#' @param data A data frame containing the variables referenced by
#'   \code{surv_obj} and \code{group_var}.
#' @param group_var Character, name of the grouping column in \code{data}.
#' @param weights Optional weights for producing weighted survival curves
#'   (e.g. inverse probability of treatment weights, IPTW). Either the name
#'   of a numeric column in \code{data}, or a numeric vector with length
#'   equal to \code{nrow(data)}. Weights must be strictly positive (an error
#'   is thrown otherwise); \code{survival::coxph()}, which this function
#'   always fits when weights are supplied, requires weights \code{> 0}, and
#'   \code{survival::survfit.formula()} treats zero weights as ambiguous, so
#'   observations that should be excluded should be filtered out of
#'   \code{data} beforehand rather than given a zero weight. There is no
#'   required scale/normalization (e.g. weights do not need to sum to 1 or
#'   to \code{nrow(data)}); IPTW weights are commonly left unstabilized or
#'   stabilized to a mean of 1, and either is fine here. If \code{NULL}
#'   (default), curves are unweighted. When supplied, the Cox model used for
#'   the HR/p-value annotation is fit with \code{robust = TRUE} (sandwich
#'   variance), as is
#'   standard practice for IPTW-type weights. If \code{group_var} has more
#'   than two levels, the omnibus p-value is a robust Wald test from a
#'   weighted Cox model rather than a log-rank test, since
#'   \code{survival::survdiff()} does not support weights. When
#'   \code{show_risktable = TRUE}, the risk table statistics are rounded to
#'   1 decimal place (they are non-integer "effective" counts when
#'   weighted); see \code{risktable_counts} to show unweighted counts
#'   instead of or alongside the weighted ones. When \code{surv_obj} is a
#'   counting-process (left-truncated) \code{Surv(time1, time2, event)}
#'   object, the robust sandwich variance needs to know which rows belong
#'   to the same subject, so \code{id} (below) is \strong{required} in
#'   that case; it is optional for a plain right-censored
#'   \code{Surv(time, event)} object, where each row is always its own
#'   independent subject.
#' @param id Character name of a subject identifier column in \code{data}.
#'   Only relevant when \code{weights} is supplied, to correctly cluster
#'   rows belonging to the same subject for the robust sandwich variance
#'   used by the weighted Cox model. \strong{Required} when \code{weights}
#'   is supplied and \code{surv_obj} is a counting-process
#'   \code{Surv(time1, time2, event)} object (an error is thrown
#'   otherwise), since such data may have a single subject contributing
#'   multiple \code{(time1, time2]} intervals (e.g. time-varying
#'   covariates) — silently guessing that every row is an independent
#'   subject risks an anti-conservative HR/p-value (standard errors/CIs
#'   too narrow). If every row of \code{data} is already its own
#'   independent subject (e.g. simple left truncation with one row per
#'   subject, such as age at entry/age at exit), add a row-number column
#'   and pass its name here. Optional (default \code{NULL}) when
#'   \code{surv_obj} is a plain right-censored \code{Surv(time, event)}
#'   object, where each row is always its own independent subject and
#'   \code{NULL} is equivalent to a row-number id.
#' @param risktable_counts Character, one of \code{"both"} (default),
#'   \code{"weighted"}, or \code{"unweighted"}. Only relevant when \code{weights}
#'   is supplied and \code{show_risktable = TRUE}. \code{"weighted"} shows
#'   the (rounded) weighted counts, with row labels suffixed
#'   \code{": Weighted"} (e.g. \code{"At Risk: Weighted"}) to flag that
#'   they are non-integer "effective" counts rather than raw subject
#'   counts. \code{"unweighted"} shows the raw unweighted subject counts in
#'   the risk table instead (curves, CI, and the HR/p-value annotation
#'   remain weighted), with unsuffixed row labels (e.g. \code{"At Risk"}).
#'   \code{"both"} shows each cell as \code{"weighted (unweighted)"} with
#'   row labels suffixed \code{": Weighted (Raw)"}; this is exact for
#'   \code{"n.risk"}, \code{"cum.event"}, and \code{"cum.censor"}, but the
#'   raw (non-cumulative) \code{"n.event"}/\code{"n.censor"} counts are
#'   totals over each displayed risk table interval, and the unweighted
#'   side of \code{"both"} can only be computed exactly at actual
#'   event/censoring times. Combining \code{risktable_counts = "both"}
#'   with \code{risktable_stats} containing \code{"n.event"} or
#'   \code{"n.censor"} therefore throws an error; use
#'   \code{"cum.event"}/\code{"cum.censor"} instead (exact in
#'   \code{"both"} mode), or set \code{risktable_counts} to
#'   \code{"weighted"} or \code{"unweighted"}.
#' @param confidence_bands Logical, if \code{TRUE} (default) display confidence bands
#' @param line_size Numeric, line size for the survival curves (default 1).
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
#' # Weighted survival curves (e.g. IPTW)
#' lung$iptw <- runif(nrow(lung), 0.5, 2)
#'
#' # risktable_counts = "both" (the default) shows each risk table cell as
#' # "weighted (unweighted)", so the actual number of observed events/at-risk
#' # subjects stays visible alongside the weighted ("effective") Ns used for
#' # the curves/CI/HR
#' plot_survival_curves(
#'     surv_obj,
#'     lung,
#'     group_var = "sex",
#'     weights = "iptw",
#'     risktable_counts = "both"
#' ) + theme_bw2()
#'
#' # Weighted curves with only the (rounded) weighted Ns in the risk table
#' plot_survival_curves(
#'     surv_obj,
#'     lung,
#'     group_var = "sex",
#'     weights = "iptw",
#'     risktable_counts = "weighted"
#' ) + theme_bw2()
#'
#' # Weighted curves with only the raw unweighted Ns in the risk table
#' plot_survival_curves(
#'     surv_obj,
#'     lung,
#'     group_var = "sex",
#'     weights = "iptw",
#'     risktable_counts = "unweighted"
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
    weights = NULL,
    id = NULL,
    confidence_bands = TRUE,
    line_size = 1,
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
    risktable_counts = c("both", "weighted", "unweighted"),
    ristable_text_size = 3.5
) {
    stopifnot(
        inherits(surv_obj, "Surv"),
        is.data.frame(data),
        length(surv_obj) == nrow(data),
        is.character(group_var),
        length(group_var) == 1,
        group_var %in% names(data),
        group_var != ".weights",

        (is.null(weights) ||
            (is.character(weights) &&
                length(weights) == 1 &&
                weights %in% names(data)) ||
            (is.numeric(weights) && length(weights) == nrow(data))),

        (is.null(id) ||
            (is.character(id) &&
                length(id) == 1 &&
                id %in% names(data))),

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

        is.character(risktable_counts),
        risktable_counts %in% c("weighted", "unweighted", "both"),
        is.numeric(ristable_text_size),
        length(ristable_text_size) == 1,
        is.finite(ristable_text_size),
        ristable_text_size > 0,
        is.numeric(line_size),
        length(line_size) == 1,
        is.finite(line_size),
        line_size > 0,
        is.logical(confidence_bands),
        length(confidence_bands) == 1
    )

    type <- match.arg(type)
    risktable_counts <- match.arg(risktable_counts)

    if (is.null(y_label)) {
        y_label <- if (identical(type, "survival")) {
            "Probability Event-Free"
        } else {
            "Probability of Event"
        }
    }

    has_weights <- !is.null(weights)

    if (
        has_weights &&
            is.null(id) &&
            identical(attr(surv_obj, "type"), "counting")
    ) {
        stop(
            "id is required when weights is supplied and surv_obj is a ",
            "counting-process Surv(time1, time2, event) object (e.g. ",
            "left-truncated data). The robust sandwich variance used for ",
            "the weighted Cox model needs to know which rows belong to ",
            "the same subject: pass the name of a subject identifier ",
            "column via id. If every row of data is already its own ",
            "independent subject (one row per subject, e.g. simple left ",
            "truncation with age at entry/age at exit), add a row-number ",
            "column and pass its name as id."
        )
    }

    if (
        show_risktable &&
            has_weights &&
            risktable_counts == "both" &&
            any(risktable_stats %in% c("n.event", "n.censor"))
    ) {
        stop(
            "risktable_counts = \"both\" cannot be combined with ",
            "risktable_stats = \"n.event\"/\"n.censor\": the unweighted ",
            "count shown in parentheses is only exact at actual ",
            "event/censoring times, not at the risk table's displayed ",
            "times, so it would misrepresent the true interval total. ",
            "Use \"cum.event\"/\"cum.censor\" instead (exact in \"both\" ",
            "mode), or set risktable_counts to \"weighted\" or ",
            "\"unweighted\"."
        )
    }

    d_sub <- data
    d_sub$.surv_obj <- surv_obj

    if (has_weights) {
        d_sub$.weights <- if (is.character(weights)) {
            data[[weights]]
        } else {
            weights
        }

        if (!is.numeric(d_sub$.weights)) {
            stop(
                "weights must be numeric, or the name of a numeric column in data"
            )
        }
    }

    if (has_weights && !is.null(id)) {
        d_sub$.id <- data[[id]]
    }

    keep <- !is.na(d_sub$.surv_obj) &
        !is.na(d_sub[[group_var]])

    if (has_weights) {
        keep <- keep & !is.na(d_sub$.weights)
    }

    if (has_weights && !is.null(id)) {
        keep <- keep & !is.na(d_sub$.id)
    }

    d_sub <- d_sub[keep, , drop = FALSE]

    if (has_weights && any(d_sub$.weights <= 0)) {
        stop(
            "weights must be strictly positive (survival::coxph() requires ",
            "weights > 0; zero weights are also ambiguous in ",
            "survival::survfit.formula() and are better handled by ",
            "filtering the data before calling this function)"
        )
    }

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
            data = d_sub,
            weights = if (has_weights) d_sub$.weights else NULL,
            robust = has_weights,
            id = if (has_weights) {
                if (!is.null(id)) d_sub$.id else seq_len(nrow(d_sub))
            } else {
                NULL
            }
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

        if (has_weights) {
            # survival::survdiff() has no weights argument, so the
            # log-rank test is replaced with a robust Wald test from a
            # weighted Cox model for the omnibus group comparison
            fit <- survival::coxph(
                sd_formula,
                data = d_sub,
                weights = d_sub$.weights,
                robust = TRUE,
                id = if (!is.null(id)) d_sub$.id else seq_len(nrow(d_sub))
            )

            p_val <- summary(fit)$waldtest["pvalue"]
        } else {
            sd <- survival::survdiff(
                sd_formula,
                data = d_sub
            )

            # survdiff() already computes the correct p-value itself
            # (its degrees of freedom is the number of groups with
            # nonzero *expected* events minus 1, which is only
            # equivalent to length(sd$n) - 1 when every group has at
            # least one expected event)
            p_val <- sd$pvalue
        }

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
        data = d_sub,
        weights = if (has_weights) d_sub$.weights else NULL
    ) |>
        ggsurvfit::ggsurvfit(type = type, size = line_size)

    if (confidence_bands) {
        kmplot <- kmplot + ggsurvfit::add_confidence_interval()
    }

    if (show_risktable) {
        if (has_weights) {
            risktable_labels <- c(
                n.risk = "At Risk",
                n.event = "Interval Events",
                n.censor = "Interval Censored",
                cum.event = "Events",
                cum.censor = "Censored"
            )

            if (risktable_counts %in% c("unweighted", "both")) {
                # weighted and unweighted survfit2 fits land on identical
                # time/strata grids (weights don't change event times or
                # grouping, only risk-set sizes), so the unweighted stats
                # can be attached row-for-row to kmplot$data
                unweighted_data <- ggsurvfit::ggsurvfit(
                    ggsurvfit::survfit2(surv_formula, data = d_sub)
                )$data
            }

            if (risktable_counts == "weighted") {
                # weighted n.risk/n.event etc. are non-integer; round them
                # so the risk table stays legible (see
                # ggsurvfit::add_risktable() "Formatting Numbers" docs)
                risktable_stats_fmt <- sprintf(
                    "{trimws(format(round(%s, 1), nsmall = 1))}",
                    risktable_stats
                )

                stats_label <- paste0(
                    unname(risktable_labels[risktable_stats]),
                    ": Weighted"
                )
            } else if (risktable_counts == "unweighted") {
                # add_risktable() always recomputes cum.event/cum.censor
                # as cumsum(n.event)/cumsum(n.censor), so overwriting
                # those columns directly would be silently discarded;
                # overwrite the underlying n.risk/n.event/n.censor
                # instead and let the cumulative stats derive naturally
                base_cols <- c("n.risk", "n.event", "n.censor")

                kmplot$data[base_cols] <- unweighted_data[base_cols]

                risktable_stats_fmt <- risktable_stats

                stats_label <- unname(risktable_labels[risktable_stats])
            } else {
                # "both": add_risktable() always recomputes
                # cum.event/cum.censor as cumsum(n.event)/cumsum(n.censor)
                # and fills gaps in n.risk *upward* but every other column
                # *downward*, so n.risk has to be combined into its native
                # column (inheriting the correct upward fill) rather than a
                # side column; cum.event/cum.censor are exact via a side
                # column since cumulative counts use the same downward
                # fill as the side-column default. Raw (non-cumulative)
                # n.event/n.censor are NOT handled here: they're totals
                # over each displayed interval (via internal binning in
                # add_risktable()), which a row-wise side column can't
                # reproduce, so that combination is rejected above with
                # an error before reaching this branch
                if ("n.risk" %in% risktable_stats) {
                    kmplot$data$n.risk <- sprintf(
                        "%s (%s)",
                        trimws(
                            format(round(kmplot$data$n.risk, 1), nsmall = 1)
                        ),
                        unweighted_data$n.risk
                    )
                }

                other_stats <- setdiff(risktable_stats, "n.risk")

                if (length(other_stats) > 0) {
                    unweighted_cols <- paste0(".unweighted_", other_stats)

                    kmplot$data[unweighted_cols] <-
                        unweighted_data[other_stats]
                }

                risktable_stats_fmt <- ifelse(
                    risktable_stats == "n.risk",
                    "{n.risk}",
                    sprintf(
                        "{trimws(format(round(%s, 1), nsmall = 1))} ({.unweighted_%s})",
                        risktable_stats,
                        risktable_stats
                    )
                )

                stats_label <- paste0(
                    unname(risktable_labels[risktable_stats]),
                    ": Weighted (Raw)"
                )
            }

            kmplot <- kmplot +
                ggsurvfit::add_risktable(
                    risktable_stats = risktable_stats_fmt,
                    stats_label = stats_label,
                    size = ristable_text_size
                )
        } else {
            kmplot <- kmplot +
                ggsurvfit::add_risktable(
                    risktable_stats = risktable_stats,
                    size = ristable_text_size
                )
        }
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
