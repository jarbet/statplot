test_that("input validation errors", {
    expect_error(combine_pvalues("not numeric"), "p must be numeric")
    expect_error(combine_pvalues(c(NA_real_)), "no p-values after removing NA")
    # values equal to 0 are invalid; 1 is allowed
    expect_error(combine_pvalues(c(0, 0.5)))
    expect_no_error(combine_pvalues(c(1, 0.5)))
})

test_that("output structure", {
    p <- c(0.001, 0.005, 0.2)
    out <- combine_pvalues(p, methods = "all")
    expect_true(is.numeric(out))
    expect_named(out, c("fisher", "CMC", "MCM", "cauchy", "minp_bonferroni"))
    expect_length(out, 5)
    expect_true(all(out > 0 | out == 0)) # numeric values present (non-NA)
})

test_that("values match known formulas", {
    p <- c(0.001, 0.005, 0.2)
    out <- combine_pvalues(p, methods = "all")

    expected_fisher <- as.numeric(poolr::fisher(p = p)$p)
    expect_equal(as.numeric(out["fisher"]), expected_fisher, tolerance = 1e-12)

    expected_cauchy <- 0.5 - atan(mean(tan((0.5 - p) * pi))) / pi
    expect_equal(as.numeric(out["cauchy"]), expected_cauchy, tolerance = 1e-12)

    expected_minp <- min(1, length(p) * min(p))
    expect_equal(
        as.numeric(out["minp_bonferroni"]),
        expected_minp,
        tolerance = 1e-12
    )

    expected_mcm <- min(1, 2 * min(expected_cauchy, expected_minp))
    expect_equal(as.numeric(out["MCM"]), expected_mcm, tolerance = 1e-12)
})

test_that("NA values are removed before computation", {
    p_with_na <- c(0.01, NA_real_)
    out_with_na <- combine_pvalues(p_with_na, methods = "all")
    out_no_na <- combine_pvalues(0.01, methods = "all")
    expect_equal(out_with_na, out_no_na)
})

test_that("single p-value behaves as expected", {
    p <- 0.05
    out <- combine_pvalues(p, methods = "all")
    # Bonferroni with n = 1 should equal the p itself (or capped at 1)
    expect_equal(as.numeric(out["minp_bonferroni"]), 0.05, tolerance = 1e-12)
    # MCM should follow its definition
    expect_equal(
        as.numeric(out["MCM"]),
        min(
            1,
            2 *
                min(
                    as.numeric(out["cauchy"]),
                    as.numeric(out["minp_bonferroni"])
                )
        ),
        tolerance = 1e-12
    )
})

test_that("matches original authors' function outputs", {
    # https://github.com/zchen2020/Robust-P-value-combination-tests/blob/main/R%20code%20for%20all.R
    og_fn <- function(Pval) {
        p.cau <- 0.5 - atan(mean(tan((0.5 - Pval) * pi))) / pi # p-value from Cauchy
        p.min <- min(1, length(Pval) * min(Pval)) # p-value from MinP
        p.mcm <- min(1, 2 * min(p.cau, p.min)) # p-value from MCM
        p.cmc <- 0.5 -
            atan(mean(c(tan((0.5 - p.cau) * pi), tan((0.5 - p.min) * pi)))) / pi # p-value from CMC
        p.all <- c(
            cauchy = p.cau,
            minp = p.min,
            mcm = p.mcm,
            cmc = p.cmc
        )
        return(p.all)
    }

    p <- c(0.001, 0.005, 0.2)
    ours <- combine_pvalues(p, methods = "all")
    mapped_ours <- c(
        cauchy = as.numeric(ours["cauchy"]),
        minp = as.numeric(ours["minp_bonferroni"]),
        mcm = as.numeric(ours["MCM"]),
        cmc = as.numeric(ours["CMC"])
    )

    expect_equal(mapped_ours, og_fn(p), tolerance = 1e-12)
})
