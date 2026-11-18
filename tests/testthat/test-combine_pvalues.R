test_that("combine_pvalues defaults to CMC", {
    pvals <- c(0.001, 0.005, 0.2)
    res <- combine_pvalues(pvals)
    expect_named(res, "CMC")
    expect_length(res, 1)
})

test_that("methods='fisher' returns only fisher", {
    pvals <- c(0.01, 0.02, 0.5)
    res <- combine_pvalues(pvals, methods = "fisher")
    expect_length(res, 1)
    expect_named(res, "fisher")
    expect_true(is.numeric(res))
})

test_that("invalid method errors", {
    pvals <- c(0.01, 0.02, 0.03)
    expect_error(combine_pvalues(pvals, methods = "unknown"), "unknown method")
})

test_that("methods must be character", {
    pvals <- c(0.01, 0.02, 0.03)
    expect_error(
        combine_pvalues(pvals, methods = 1),
        "methods must be a character"
    )
})

test_that("fisher equals the chi-squared formula and poolr::fisher", {
    withr::local_seed(8)
    for (i in 1:100) {
        p <- stats::runif(sample(2:20, 1))
        res <- combine_pvalues(p, methods = "fisher")
        expect_equal(
            unname(res),
            stats::pchisq(-2 * sum(log(p)), 2 * length(p), lower.tail = FALSE),
            tolerance = 1e-12
        )
        if (requireNamespace("poolr", quietly = TRUE)) {
            expect_equal(
                unname(res),
                as.numeric(poolr::fisher(p)$p),
                tolerance = 1e-12
            )
        }
    }
})
