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
