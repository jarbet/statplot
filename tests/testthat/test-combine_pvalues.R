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
