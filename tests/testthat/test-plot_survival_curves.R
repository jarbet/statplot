# tests/testthat/test-plot_survival_curves.R

testthat::test_that("plot_survival_curves returns a plot for two groups", {
    dat <- survival::lung

    dat$sex <- factor(
        dat$sex,
        labels = c("Male", "Female")
    )

    surv_obj <- with(
        dat,
        survival::Surv(time, status == 2)
    )

    p <- plot_survival_curves(
        surv_obj = surv_obj,
        data = dat,
        group_var = "sex"
    )

    testthat::expect_s3_class(p, "ggsurvfit")
})

testthat::test_that("plot_survival_curves returns a plot for >2 groups", {
    dat <- survival::lung

    dat$ph.ecog <- factor(dat$ph.ecog)

    surv_obj <- with(
        dat,
        survival::Surv(time, status == 2)
    )

    p <- plot_survival_curves(
        surv_obj = surv_obj,
        data = dat,
        group_var = "ph.ecog"
    )

    testthat::expect_s3_class(p, "ggsurvfit")
})

testthat::test_that("type = survival works", {
    dat <- survival::lung

    dat$sex <- factor(dat$sex)

    surv_obj <- with(
        dat,
        survival::Surv(time, status == 2)
    )

    p <- plot_survival_curves(
        surv_obj,
        dat,
        group_var = "sex",
        type = "survival"
    )

    testthat::expect_s3_class(p, "ggsurvfit")
})

testthat::test_that("type = risk works", {
    dat <- survival::lung

    dat$sex <- factor(dat$sex)

    surv_obj <- with(
        dat,
        survival::Surv(time, status == 2)
    )

    p <- plot_survival_curves(
        surv_obj,
        dat,
        group_var = "sex",
        type = "risk"
    )

    testthat::expect_s3_class(p, "ggsurvfit")
})

testthat::test_that("show_risktable = FALSE works", {
    dat <- survival::lung

    dat$sex <- factor(dat$sex)

    surv_obj <- with(
        dat,
        survival::Surv(time, status == 2)
    )

    p <- plot_survival_curves(
        surv_obj,
        dat,
        group_var = "sex",
        show_risktable = FALSE
    )

    testthat::expect_s3_class(p, "ggsurvfit")
})

testthat::test_that("all supported risktable statistics work", {
    dat <- survival::lung

    dat$sex <- factor(dat$sex)

    surv_obj <- with(
        dat,
        survival::Surv(time, status == 2)
    )

    allowed_stats <- list(
        "n.risk",
        "cum.event",
        "cum.censor",
        "n.event",
        "n.censor",
        c("n.risk", "cum.event")
    )

    for (stats in allowed_stats) {
        testthat::expect_no_error(
            plot_survival_curves(
                surv_obj,
                dat,
                group_var = "sex",
                risktable_stats = stats
            )
        )
    }
})

testthat::test_that("invalid risktable statistic throws error", {
    dat <- survival::lung

    dat$sex <- factor(dat$sex)

    surv_obj <- with(
        dat,
        survival::Surv(time, status == 2)
    )

    testthat::expect_error(
        plot_survival_curves(
            surv_obj,
            dat,
            group_var = "sex",
            risktable_stats = "bad_stat"
        )
    )
})

testthat::test_that("custom annotation text works", {
    dat <- survival::lung

    dat$sex <- factor(dat$sex)

    surv_obj <- with(
        dat,
        survival::Surv(time, status == 2)
    )

    testthat::expect_no_error(
        plot_survival_curves(
            surv_obj,
            dat,
            group_var = "sex",
            custom_hr_pvalue_text = "Custom annotation"
        )
    )
})

testthat::test_that("custom axis labels and title work", {
    dat <- survival::lung

    dat$sex <- factor(dat$sex)

    surv_obj <- with(
        dat,
        survival::Surv(time, status == 2)
    )

    p <- plot_survival_curves(
        surv_obj,
        dat,
        group_var = "sex",
        x_label = "Age",
        y_label = "Survival",
        title = "My Plot"
    )

    testthat::expect_s3_class(p, "ggsurvfit")
})

testthat::test_that("automatic time limits and breaks work", {
    dat <- survival::lung

    dat$sex <- factor(dat$sex)

    surv_obj <- with(
        dat,
        survival::Surv(time, status == 2)
    )

    testthat::expect_no_error(
        plot_survival_curves(
            surv_obj,
            dat,
            group_var = "sex",
            time_limits = NULL,
            x_breaks = NULL
        )
    )
})

testthat::test_that("user supplied time limits and breaks work", {
    dat <- survival::lung

    dat$sex <- factor(dat$sex)

    surv_obj <- with(
        dat,
        survival::Surv(time, status == 2)
    )

    testthat::expect_no_error(
        plot_survival_curves(
            surv_obj,
            dat,
            group_var = "sex",
            time_limits = c(0, 1000),
            x_breaks = c(0, 250, 500, 750, 1000)
        )
    )
})

testthat::test_that("invalid time_limits throws error", {
    dat <- survival::lung

    dat$sex <- factor(dat$sex)

    surv_obj <- with(
        dat,
        survival::Surv(time, status == 2)
    )

    testthat::expect_error(
        plot_survival_curves(
            surv_obj,
            dat,
            group_var = "sex",
            time_limits = c(100, 50)
        )
    )
})

testthat::test_that("annotate_hjust propagates to the annotation layer", {
    dat <- survival::lung

    dat$sex <- factor(dat$sex)

    surv_obj <- with(
        dat,
        survival::Surv(time, status == 2)
    )

    p <- plot_survival_curves(
        surv_obj,
        dat,
        group_var = "sex",
        annotate_hjust = 0
    )

    richtext_layer <- p$layers[[
        which(vapply(
            p$layers,
            function(l) inherits(l$geom, "GeomRichText"),
            logical(1)
        ))
    ]]

    testthat::expect_equal(richtext_layer$aes_params$hjust, 0)
})

testthat::test_that("invalid annotate_hjust throws error", {
    dat <- survival::lung

    dat$sex <- factor(dat$sex)

    surv_obj <- with(
        dat,
        survival::Surv(time, status == 2)
    )

    testthat::expect_error(
        plot_survival_curves(
            surv_obj,
            dat,
            group_var = "sex",
            annotate_hjust = c(0, 1)
        )
    )

    testthat::expect_error(
        plot_survival_curves(
            surv_obj,
            dat,
            group_var = "sex",
            annotate_hjust = "left"
        )
    )

    testthat::expect_error(
        plot_survival_curves(
            surv_obj,
            dat,
            group_var = "sex",
            annotate_hjust = NA_real_
        )
    )
})

testthat::test_that("invalid group variable throws error", {
    dat <- survival::lung

    surv_obj <- with(
        dat,
        survival::Surv(time, status == 2)
    )

    testthat::expect_error(
        plot_survival_curves(
            surv_obj,
            dat,
            group_var = "does_not_exist"
        )
    )
})

testthat::test_that("single group throws error", {
    dat <- survival::lung

    dat$grp <- "A"

    surv_obj <- with(
        dat,
        survival::Surv(time, status == 2)
    )

    testthat::expect_error(
        plot_survival_curves(
            surv_obj,
            dat,
            group_var = "grp"
        ),
        "Need at least 2 groups"
    )
})

testthat::test_that("surv length mismatch throws error", {
    dat <- survival::lung

    surv_obj <- with(
        dat[1:10, ],
        survival::Surv(time, status == 2)
    )

    testthat::expect_error(
        plot_survival_curves(
            surv_obj,
            dat,
            group_var = "sex"
        )
    )
})

testthat::test_that("left truncated survival objects are supported", {
    dat <- survival::lung

    dat$entry <- pmax(0, dat$time - 50)

    surv_obj <- with(
        dat,
        survival::Surv(entry, time, status == 2)
    )

    testthat::expect_no_error(
        plot_survival_curves(
            surv_obj,
            dat,
            group_var = "sex"
        )
    )
})

testthat::test_that("weighted survival curves work with left-truncated (counting-process) Surv objects, 2 groups", {
    dat <- survival::lung

    dat$sex <- factor(dat$sex, labels = c("Male", "Female"))
    dat$entry <- pmax(0, dat$time - 50)
    dat$w <- stats::runif(nrow(dat), 0.5, 2)
    dat$subj_id <- seq_len(nrow(dat))

    surv_obj <- with(
        dat,
        survival::Surv(entry, time, status == 2)
    )

    testthat::expect_no_error(
        plot_survival_curves(
            surv_obj,
            dat,
            group_var = "sex",
            weights = "w",
            id = "subj_id"
        )
    )
})

testthat::test_that("weighted survival curves work with left-truncated (counting-process) Surv objects, >2 groups", {
    dat <- survival::lung

    dat$ph.ecog <- factor(dat$ph.ecog)
    dat$entry <- pmax(0, dat$time - 50)
    dat$w <- stats::runif(nrow(dat), 0.5, 2)
    dat$subj_id <- seq_len(nrow(dat))

    surv_obj <- with(
        dat,
        survival::Surv(entry, time, status == 2)
    )

    testthat::expect_no_error(
        plot_survival_curves(
            surv_obj,
            dat,
            group_var = "ph.ecog",
            weights = "w",
            id = "subj_id"
        )
    )
})

testthat::test_that("weighted counting-process Surv objects without id throws an informative error", {
    dat <- survival::lung

    dat$sex <- factor(dat$sex, labels = c("Male", "Female"))
    dat$entry <- pmax(0, dat$time - 50)
    dat$w <- stats::runif(nrow(dat), 0.5, 2)

    surv_obj <- with(
        dat,
        survival::Surv(entry, time, status == 2)
    )

    testthat::expect_error(
        plot_survival_curves(
            surv_obj,
            dat,
            group_var = "sex",
            weights = "w"
        ),
        "id is required"
    )
})

testthat::test_that("rows with missing group values are removed", {
    dat <- survival::lung

    dat$sex <- factor(dat$sex)
    dat$sex[1:10] <- NA

    surv_obj <- with(
        dat,
        survival::Surv(time, status == 2)
    )

    testthat::expect_no_error(
        plot_survival_curves(
            surv_obj,
            dat,
            group_var = "sex"
        )
    )
})

testthat::test_that("id with missing values is ignored (not filtered) for unweighted calls", {
    dat <- survival::lung

    dat$sex <- factor(dat$sex, labels = c("Male", "Female"))
    dat$subj_id <- seq_len(nrow(dat))
    dat$subj_id[1:10] <- NA

    surv_obj <- with(
        dat,
        survival::Surv(time, status == 2)
    )

    p_with_id <- plot_survival_curves(
        surv_obj,
        dat,
        group_var = "sex",
        id = "subj_id"
    )

    p_without_id <- plot_survival_curves(
        surv_obj,
        dat,
        group_var = "sex"
    )

    testthat::expect_equal(
        ggplot2::ggplot_build(p_with_id)$data,
        ggplot2::ggplot_build(p_without_id)$data
    )
})

testthat::test_that("weighted survival curves work with a weights column name", {
    dat <- survival::lung

    dat$sex <- factor(dat$sex, labels = c("Male", "Female"))
    dat$w <- stats::runif(nrow(dat), 0.5, 2)

    surv_obj <- with(
        dat,
        survival::Surv(time, status == 2)
    )

    p <- plot_survival_curves(
        surv_obj,
        dat,
        group_var = "sex",
        weights = "w"
    )

    testthat::expect_s3_class(p, "ggsurvfit")
})

testthat::test_that("weighted survival curves work with a numeric weights vector", {
    dat <- survival::lung

    dat$sex <- factor(dat$sex, labels = c("Male", "Female"))
    w <- stats::runif(nrow(dat), 0.5, 2)

    surv_obj <- with(
        dat,
        survival::Surv(time, status == 2)
    )

    p <- plot_survival_curves(
        surv_obj,
        dat,
        group_var = "sex",
        weights = w
    )

    testthat::expect_s3_class(p, "ggsurvfit")
})

testthat::test_that("weighted survival curves work with >2 groups", {
    dat <- survival::lung

    dat$ph.ecog <- factor(dat$ph.ecog)
    dat$w <- stats::runif(nrow(dat), 0.5, 2)

    surv_obj <- with(
        dat,
        survival::Surv(time, status == 2)
    )

    p <- plot_survival_curves(
        surv_obj,
        dat,
        group_var = "ph.ecog",
        weights = "w"
    )

    testthat::expect_s3_class(p, "ggsurvfit")
})

testthat::test_that("weighted HR annotation matches a robust weighted Cox model", {
    dat <- survival::lung

    dat$sex <- factor(dat$sex, labels = c("Male", "Female"))

    set.seed(42)
    dat$w <- stats::runif(nrow(dat), 0.5, 2)

    surv_obj <- with(
        dat,
        survival::Surv(time, status == 2)
    )

    p <- plot_survival_curves(
        surv_obj,
        dat,
        group_var = "sex",
        weights = "w"
    )

    built <- ggplot2::ggplot_build(p)

    label_layers <- Filter(
        function(d) "label" %in% names(d),
        built$data
    )

    annot_label <- label_layers[[1]]$label

    hr_from_plot <- as.numeric(
        regmatches(
            annot_label,
            regexpr("(?<=HR = )[0-9.]+", annot_label, perl = TRUE)
        )
    )

    fit <- survival::coxph(
        surv_obj ~ dat$sex,
        weights = dat$w,
        robust = TRUE
    )

    hr_expected <- unname(exp(stats::coef(fit))[1])

    testthat::expect_equal(
        hr_from_plot,
        round(hr_expected, 2),
        tolerance = 1e-6
    )
})

testthat::test_that("id correctly clusters the robust variance for multi-row-per-subject counting-process data", {
    set.seed(123)
    n <- 30

    subj_id <- seq_len(n)
    grp <- rep(c("A", "B"), each = n / 2)
    w <- stats::runif(n, 0.5, 2)
    mid <- stats::runif(n, 2, 8)
    final <- mid + stats::runif(n, 2, 8)
    event <- stats::rbinom(n, 1, 0.7)

    dat <- rbind(
        data.frame(
            subj_id = subj_id,
            group = grp,
            w = w,
            time1 = 0,
            time2 = mid,
            event = 0
        ),
        data.frame(
            subj_id = subj_id,
            group = grp,
            w = w,
            time1 = mid,
            time2 = final,
            event = event
        )
    )

    surv_obj <- with(
        dat,
        survival::Surv(time1, time2, event == 1)
    )

    p <- plot_survival_curves(
        surv_obj,
        dat,
        group_var = "group",
        weights = "w",
        id = "subj_id"
    )

    built <- ggplot2::ggplot_build(p)

    label_layers <- Filter(
        function(d) "label" %in% names(d),
        built$data
    )

    annot_label <- label_layers[[1]]$label

    hr_from_plot <- as.numeric(
        regmatches(
            annot_label,
            regexpr("(?<=HR = )[0-9.]+", annot_label, perl = TRUE)
        )
    )

    fit_clustered <- survival::coxph(
        surv_obj ~ dat$group,
        weights = dat$w,
        robust = TRUE,
        id = dat$subj_id
    )

    hr_expected <- unname(exp(stats::coef(fit_clustered))[1])

    testthat::expect_equal(
        hr_from_plot,
        round(hr_expected, 2),
        tolerance = 1e-6
    )

    fit_unclustered <- survival::coxph(
        surv_obj ~ dat$group,
        weights = dat$w,
        robust = TRUE,
        id = seq_len(nrow(dat))
    )

    testthat::expect_false(
        isTRUE(
            all.equal(
                unname(summary(fit_clustered)$coefficients[1, "robust se"]),
                unname(summary(fit_unclustered)$coefficients[1, "robust se"])
            )
        )
    )
})

testthat::test_that("unweighted p-value for >2 groups matches survival::survdiff()", {
    dat <- survival::lung

    dat$ph.ecog[dat$ph.ecog == 3] <- NA
    dat$ph.ecog <- factor(dat$ph.ecog)

    surv_obj <- with(
        dat,
        survival::Surv(time, status == 2)
    )

    p <- plot_survival_curves(
        surv_obj,
        dat,
        group_var = "ph.ecog"
    )

    built <- ggplot2::ggplot_build(p)

    label_layers <- Filter(
        function(d) "label" %in% names(d),
        built$data
    )

    annot_label <- label_layers[[1]]$label

    keep <- !is.na(dat$ph.ecog)

    sd <- survival::survdiff(surv_obj[keep] ~ dat$ph.ecog[keep])

    testthat::expect_identical(
        annot_label,
        format_pvalue(sd$pvalue)
    )
})

testthat::test_that("unweighted p-value for >2 groups uses survdiff's own degrees of freedom, not length(n) - 1", {
    # a group that is censored before any other group's first event time
    # contributes 0 *expected* events; survival::survdiff() correctly
    # drops it from the degrees of freedom (sum(expected > 0) - 1), which
    # differs from naively using length(n) - 1 (this reproduces a bug
    # where the two formulas gave materially different p-values: 0.50
    # vs 0.80)
    dat <- data.frame(
        time = c(5, 6, 7, 8, 5.5, 6.5, 7.5, 8.5, 0.1),
        status = c(1, 1, 0, 1, 1, 0, 1, 1, 0),
        grp = factor(c(rep("A", 4), rep("B", 4), "C"))
    )

    surv_obj <- with(
        dat,
        survival::Surv(time, status == 1)
    )

    sd <- survival::survdiff(surv_obj ~ dat$grp)

    # confirm this data really does trigger the divergence being tested
    testthat::expect_false(sum(sd$exp > 0) - 1 == length(sd$n) - 1)
    testthat::expect_false(
        isTRUE(all.equal(
            sd$pvalue,
            stats::pchisq(sd$chisq, length(sd$n) - 1, lower.tail = FALSE)
        ))
    )

    p <- plot_survival_curves(
        surv_obj,
        dat,
        group_var = "grp"
    )

    built <- ggplot2::ggplot_build(p)

    label_layers <- Filter(
        function(d) "label" %in% names(d),
        built$data
    )

    annot_label <- label_layers[[1]]$label

    testthat::expect_identical(
        annot_label,
        format_pvalue(sd$pvalue)
    )
})

testthat::test_that("weighted p-value for >2 groups matches a robust Wald test", {
    dat <- survival::lung

    dat$ph.ecog[dat$ph.ecog == 3] <- NA
    dat$ph.ecog <- factor(dat$ph.ecog)

    set.seed(42)
    dat$w <- stats::runif(nrow(dat), 0.5, 2)

    surv_obj <- with(
        dat,
        survival::Surv(time, status == 2)
    )

    p <- plot_survival_curves(
        surv_obj,
        dat,
        group_var = "ph.ecog",
        weights = "w"
    )

    built <- ggplot2::ggplot_build(p)

    label_layers <- Filter(
        function(d) "label" %in% names(d),
        built$data
    )

    annot_label <- label_layers[[1]]$label

    keep <- !is.na(surv_obj) & !is.na(dat$ph.ecog)

    fit <- survival::coxph(
        surv_obj[keep] ~ dat$ph.ecog[keep],
        weights = dat$w[keep],
        robust = TRUE
    )

    p_expected <- unname(summary(fit)$waldtest["pvalue"])

    testthat::expect_identical(
        annot_label,
        format_pvalue(p_expected)
    )
})

testthat::test_that("rows with missing weights are removed", {
    dat <- survival::lung

    dat$sex <- factor(dat$sex)
    dat$w <- stats::runif(nrow(dat), 0.5, 2)
    dat$w[1:10] <- NA

    surv_obj <- with(
        dat,
        survival::Surv(time, status == 2)
    )

    testthat::expect_no_error(
        plot_survival_curves(
            surv_obj,
            dat,
            group_var = "sex",
            weights = "w"
        )
    )
})

testthat::test_that("numeric weights vector of the wrong length throws error", {
    dat <- survival::lung

    dat$sex <- factor(dat$sex)

    surv_obj <- with(
        dat,
        survival::Surv(time, status == 2)
    )

    testthat::expect_error(
        plot_survival_curves(
            surv_obj,
            dat,
            group_var = "sex",
            weights = c(1, 2, 3)
        )
    )
})

testthat::test_that("weights column name not present in data throws error", {
    dat <- survival::lung

    dat$sex <- factor(dat$sex)

    surv_obj <- with(
        dat,
        survival::Surv(time, status == 2)
    )

    testthat::expect_error(
        plot_survival_curves(
            surv_obj,
            dat,
            group_var = "sex",
            weights = "does_not_exist"
        )
    )
})

testthat::test_that("non-numeric weights column throws an informative error", {
    dat <- survival::lung

    dat$sex <- factor(dat$sex)
    dat$w <- sample(letters, nrow(dat), replace = TRUE)

    surv_obj <- with(
        dat,
        survival::Surv(time, status == 2)
    )

    testthat::expect_error(
        plot_survival_curves(
            surv_obj,
            dat,
            group_var = "sex",
            weights = "w"
        ),
        "weights must be numeric"
    )
})

testthat::test_that("weighted risk table builds without error for all statistics", {
    dat <- survival::lung

    dat$sex <- factor(dat$sex)
    dat$w <- stats::runif(nrow(dat), 0.5, 2)

    surv_obj <- with(
        dat,
        survival::Surv(time, status == 2)
    )

    testthat::expect_no_error(
        ggplot2::ggplot_build(
            plot_survival_curves(
                surv_obj,
                dat,
                group_var = "sex",
                weights = "w",
                risktable_counts = "unweighted",
                risktable_stats = c(
                    "n.risk",
                    "cum.event",
                    "cum.censor",
                    "n.event",
                    "n.censor"
                )
            )
        )
    )
})

testthat::test_that("risktable_counts = 'unweighted' matches a plain unweighted survfit", {
    dat <- survival::lung

    dat$sex <- factor(dat$sex, labels = c("Male", "Female"))

    set.seed(42)
    dat$w <- stats::runif(nrow(dat), 0.5, 2)

    surv_obj <- with(
        dat,
        survival::Surv(time, status == 2)
    )

    p <- plot_survival_curves(
        surv_obj,
        dat,
        group_var = "sex",
        weights = "w",
        risktable_counts = "unweighted"
    )

    testthat::expect_true(
        all(p$data$n.risk == round(p$data$n.risk))
    )

    fit_unweighted <- survival::survfit(surv_obj ~ dat$sex)
    male_idx <- seq_len(fit_unweighted$strata[1])

    truth <- data.frame(
        time = fit_unweighted$time[male_idx],
        n.risk = fit_unweighted$n.risk[male_idx]
    )

    male_p <- p$data[
        p$data$strata == "Male" & p$data$time > 0,
        c("time", "n.risk")
    ]

    merged <- merge(male_p, truth, by = "time", suffixes = c("_p", "_truth"))

    testthat::expect_equal(nrow(merged), nrow(truth))
    testthat::expect_true(all(merged$n.risk_p == merged$n.risk_truth))
})

testthat::test_that("risktable_counts = 'both' combines weighted and unweighted n.risk correctly", {
    dat <- survival::lung

    dat$sex <- factor(dat$sex, labels = c("Male", "Female"))

    set.seed(42)
    dat$w <- stats::runif(nrow(dat), 0.5, 2)

    surv_obj <- with(
        dat,
        survival::Surv(time, status == 2)
    )

    p <- plot_survival_curves(
        surv_obj,
        dat,
        group_var = "sex",
        weights = "w",
        risktable_counts = "both"
    )

    testthat::expect_type(p$data$n.risk, "character")

    testthat::expect_true(
        all(grepl("^[0-9.]+ \\([0-9]+\\)$", p$data$n.risk))
    )

    fit_unweighted <- survival::survfit(surv_obj ~ dat$sex)
    male_idx <- seq_len(fit_unweighted$strata[1])

    truth <- data.frame(
        time = fit_unweighted$time[male_idx],
        n.risk = fit_unweighted$n.risk[male_idx]
    )

    parsed_unweighted <- as.numeric(
        regmatches(
            p$data$n.risk,
            regexpr("(?<=\\()[0-9]+(?=\\))", p$data$n.risk, perl = TRUE)
        )
    )

    male_rows <- p$data$strata == "Male" & p$data$time > 0

    both_male <- data.frame(
        time = p$data$time[male_rows],
        n.risk_uw = parsed_unweighted[male_rows]
    )

    merged <- merge(both_male, truth, by = "time")

    testthat::expect_equal(nrow(merged), nrow(truth))
    testthat::expect_true(all(merged$n.risk_uw == merged$n.risk))
})

testthat::test_that("risktable_counts = 'both' cum.event matches the unweighted fit's cumulative events", {
    dat <- survival::lung

    dat$sex <- factor(dat$sex, labels = c("Male", "Female"))

    set.seed(42)
    dat$w <- stats::runif(nrow(dat), 0.5, 2)

    surv_obj <- with(
        dat,
        survival::Surv(time, status == 2)
    )

    p <- plot_survival_curves(
        surv_obj,
        dat,
        group_var = "sex",
        weights = "w",
        risktable_counts = "both",
        risktable_stats = c("n.risk", "cum.event")
    )

    testthat::expect_true(".unweighted_cum.event" %in% names(p$data))

    unweighted_data <- ggsurvfit::ggsurvfit(
        ggsurvfit::survfit2(
            surv_obj ~ dat$sex,
            data = dat
        )
    )$data

    testthat::expect_equal(
        p$data$.unweighted_cum.event[p$data$strata == "Male"],
        unweighted_data$cum.event[unweighted_data$strata == "Male"]
    )
})

testthat::test_that("risktable_counts = 'both' works with only n.risk requested", {
    dat <- survival::lung

    dat$sex <- factor(dat$sex)
    dat$w <- stats::runif(nrow(dat), 0.5, 2)

    surv_obj <- with(
        dat,
        survival::Surv(time, status == 2)
    )

    testthat::expect_no_error(
        ggplot2::ggplot_build(
            plot_survival_curves(
                surv_obj,
                dat,
                group_var = "sex",
                weights = "w",
                risktable_counts = "both",
                risktable_stats = "n.risk"
            )
        )
    )
})

testthat::test_that("risktable_counts = 'both' works with only cum.event requested", {
    dat <- survival::lung

    dat$sex <- factor(dat$sex)
    dat$w <- stats::runif(nrow(dat), 0.5, 2)

    surv_obj <- with(
        dat,
        survival::Surv(time, status == 2)
    )

    testthat::expect_no_error(
        ggplot2::ggplot_build(
            plot_survival_curves(
                surv_obj,
                dat,
                group_var = "sex",
                weights = "w",
                risktable_counts = "both",
                risktable_stats = "cum.event"
            )
        )
    )
})

testthat::test_that("risktable_counts = 'both' works with all statistics it supports", {
    dat <- survival::lung

    dat$sex <- factor(dat$sex)
    dat$w <- stats::runif(nrow(dat), 0.5, 2)

    surv_obj <- with(
        dat,
        survival::Surv(time, status == 2)
    )

    testthat::expect_no_error(
        ggplot2::ggplot_build(
            plot_survival_curves(
                surv_obj,
                dat,
                group_var = "sex",
                weights = "w",
                risktable_counts = "both",
                risktable_stats = c("n.risk", "cum.event", "cum.censor")
            )
        )
    )
})

testthat::test_that("risktable_counts = 'both' with n.event/n.censor throws an error", {
    dat <- survival::lung

    dat$sex <- factor(dat$sex)
    dat$w <- stats::runif(nrow(dat), 0.5, 2)

    surv_obj <- with(
        dat,
        survival::Surv(time, status == 2)
    )

    # n.event/n.censor are interval totals in the risk table, but the
    # unweighted side of "both" can only be attached row-wise at actual
    # event/censoring times, so it can't reproduce the interval total;
    # this combination is rejected rather than silently shown wrong
    testthat::expect_error(
        plot_survival_curves(
            surv_obj,
            dat,
            group_var = "sex",
            weights = "w",
            risktable_counts = "both",
            risktable_stats = c("n.risk", "n.event")
        ),
        "n\\.event"
    )

    testthat::expect_error(
        plot_survival_curves(
            surv_obj,
            dat,
            group_var = "sex",
            weights = "w",
            risktable_counts = "both",
            risktable_stats = c("n.risk", "n.censor")
        ),
        "n\\.censor"
    )

    # weighted/unweighted-only modes are unaffected, since there's no
    # side column to misalign with the interval binning
    testthat::expect_no_error(
        ggplot2::ggplot_build(
            plot_survival_curves(
                surv_obj,
                dat,
                group_var = "sex",
                weights = "w",
                risktable_counts = "weighted",
                risktable_stats = c("n.event", "n.censor")
            )
        )
    )

    # the error only applies when the risk table (and thus
    # risktable_stats) is actually used
    testthat::expect_no_error(
        plot_survival_curves(
            surv_obj,
            dat,
            group_var = "sex",
            weights = "w",
            risktable_counts = "both",
            risktable_stats = c("n.event", "n.censor"),
            show_risktable = FALSE
        )
    )
})

testthat::test_that("invalid risktable_counts throws error", {
    dat <- survival::lung

    dat$sex <- factor(dat$sex)
    dat$w <- stats::runif(nrow(dat), 0.5, 2)

    surv_obj <- with(
        dat,
        survival::Surv(time, status == 2)
    )

    testthat::expect_error(
        plot_survival_curves(
            surv_obj,
            dat,
            group_var = "sex",
            weights = "w",
            risktable_counts = "bad_value"
        )
    )
})

testthat::test_that("risktable_counts is ignored without weights", {
    dat <- survival::lung

    dat$sex <- factor(dat$sex)

    surv_obj <- with(
        dat,
        survival::Surv(time, status == 2)
    )

    testthat::expect_no_error(
        plot_survival_curves(
            surv_obj,
            dat,
            group_var = "sex",
            risktable_counts = "unweighted"
        )
    )
})

testthat::test_that("zero or negative weights are rejected with a clear error", {
    dat <- survival::lung

    dat$sex <- factor(dat$sex, labels = c("Male", "Female"))

    surv_obj <- with(
        dat,
        survival::Surv(time, status == 2)
    )

    w_zero <- rep(1, nrow(dat))
    w_zero[1] <- 0

    w_negative <- rep(1, nrow(dat))
    w_negative[1] <- -1

    testthat::expect_error(
        plot_survival_curves(
            surv_obj,
            dat,
            group_var = "sex",
            weights = w_zero
        ),
        "strictly positive"
    )

    testthat::expect_error(
        plot_survival_curves(
            surv_obj,
            dat,
            group_var = "sex",
            weights = w_negative
        ),
        "strictly positive"
    )
})

testthat::test_that("weights need not sum to 1 or to nrow(data)", {
    dat <- survival::lung

    dat$sex <- factor(dat$sex, labels = c("Male", "Female"))

    surv_obj <- with(
        dat,
        survival::Surv(time, status == 2)
    )

    testthat::expect_no_error(
        plot_survival_curves(
            surv_obj,
            dat,
            group_var = "sex",
            weights = rep(0.001, nrow(dat))
        )
    )

    testthat::expect_no_error(
        plot_survival_curves(
            surv_obj,
            dat,
            group_var = "sex",
            weights = rep(1000, nrow(dat))
        )
    )
})

# helper: pull the stats_label vector out of a plot's add_risktable() layer
get_risktable_labels <- function(p) {
    for (l in p$layers) {
        a <- attr(l, "add_risktable")
        if (!is.null(a)) {
            return(a$stats_label)
        }
    }
    NULL
}

testthat::test_that("risktable_counts = 'weighted' labels indicate weighting", {
    dat <- survival::lung

    dat$sex <- factor(dat$sex, labels = c("Male", "Female"))
    dat$w <- stats::runif(nrow(dat), 0.5, 2)

    surv_obj <- with(
        dat,
        survival::Surv(time, status == 2)
    )

    p <- plot_survival_curves(
        surv_obj,
        dat,
        group_var = "sex",
        weights = "w",
        risktable_counts = "weighted",
        risktable_stats = c("n.risk", "cum.event")
    )

    testthat::expect_equal(
        get_risktable_labels(p),
        c("At Risk: Weighted", "Events: Weighted")
    )
})

testthat::test_that("risktable_counts = 'unweighted' labels have no weighting suffix", {
    dat <- survival::lung

    dat$sex <- factor(dat$sex, labels = c("Male", "Female"))
    dat$w <- stats::runif(nrow(dat), 0.5, 2)

    surv_obj <- with(
        dat,
        survival::Surv(time, status == 2)
    )

    p <- plot_survival_curves(
        surv_obj,
        dat,
        group_var = "sex",
        weights = "w",
        risktable_counts = "unweighted",
        risktable_stats = c("n.risk", "cum.event")
    )

    testthat::expect_equal(
        get_risktable_labels(p),
        c("At Risk", "Events")
    )
})

testthat::test_that("risktable_counts = 'both' labels indicate weighted/raw", {
    dat <- survival::lung

    dat$sex <- factor(dat$sex, labels = c("Male", "Female"))
    dat$w <- stats::runif(nrow(dat), 0.5, 2)

    surv_obj <- with(
        dat,
        survival::Surv(time, status == 2)
    )

    p <- plot_survival_curves(
        surv_obj,
        dat,
        group_var = "sex",
        weights = "w",
        risktable_counts = "both",
        risktable_stats = c("n.risk", "cum.event")
    )

    testthat::expect_equal(
        get_risktable_labels(p),
        c("At Risk: Weighted (Raw)", "Events: Weighted (Raw)")
    )
})

testthat::test_that("unweighted risktable labels are unaffected by risktable_counts default", {
    dat <- survival::lung

    dat$sex <- factor(dat$sex)

    surv_obj <- with(
        dat,
        survival::Surv(time, status == 2)
    )

    p <- plot_survival_curves(
        surv_obj,
        dat,
        group_var = "sex",
        risktable_stats = c("n.risk", "cum.event")
    )

    testthat::expect_null(get_risktable_labels(p))
})
