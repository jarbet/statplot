test_that("format_pvalue default behavior with single value", {
    result <- format_pvalue(0.0005)
    expect_equal(result, "p= 5.00 × 10<sup>-4</sup>")
})

test_that("format_pvalue handles vector input", {
    result <- format_pvalue(c(0.0005, 0.005, 0.02, 0.236))
    expect_equal(length(result), 4)
    expect_equal(result[1], "p= 5.00 × 10<sup>-4</sup>")
    expect_equal(result[2], "p= 0.005")
    expect_equal(result[3], "p= 0.020")
    expect_equal(result[4], "p= 0.24")
})

test_that("format_pvalue with html = FALSE uses scientific notation", {
    result <- format_pvalue(0.0005, html = FALSE)
    expect_equal(result, "p= 5.00e-04")
})

test_that("format_pvalue with truncate_pvalue = TRUE", {
    result <- format_pvalue(0.0005, truncate_pvalue = TRUE)
    expect_equal(result, "p<0.001")
})

test_that("format_pvalue truncate with vector includes both < and =", {
    result <- format_pvalue(
        c(0.0005, 0.005, 0.02, 0.236),
        truncate_pvalue = TRUE
    )
    expect_equal(result[1], "p<0.001")
    expect_equal(result[2], "p= 0.005")
    expect_equal(result[3], "p= 0.020")
    expect_equal(result[4], "p= 0.24")
})

test_that("format_pvalue with custom p_text", {
    result <- format_pvalue(0.0005, p_text = "q")
    expect_equal(result, "q= 5.00 × 10<sup>-4</sup>")
})

test_that("format_pvalue with custom p_symbol", {
    result <- format_pvalue(0.0005, p_symbol = ": ")
    expect_equal(result, "p: 5.00 × 10<sup>-4</sup>")
})

test_that("format_pvalue with empty p_text and p_symbol returns only value", {
    result <- format_pvalue(0.0005, p_text = "", p_symbol = "")
    expect_equal(result, "5.00 × 10<sup>-4</sup>")
})

test_that("format_pvalue with empty p_text", {
    result <- format_pvalue(0.0005, p_text = "", p_symbol = "= ")
    expect_equal(result, "= 5.00 × 10<sup>-4</sup>")
})

test_that("format_pvalue with empty p_symbol", {
    result <- format_pvalue(0.0005, p_text = "p", p_symbol = "")
    expect_equal(result, "p5.00 × 10<sup>-4</sup>")
})

test_that("format_pvalue handles values >= 0.1", {
    result <- format_pvalue(0.15)
    expect_equal(result, "p= 0.15")
})

test_that("format_pvalue handles values between 0.001 and 0.1", {
    result <- format_pvalue(0.005)
    expect_equal(result, "p= 0.005")
    result <- format_pvalue(0.05)
    expect_equal(result, "p= 0.050")
})

test_that("format_pvalue rounds values correctly", {
    result <- format_pvalue(c(0.1234, 0.01234))
    expect_equal(result[1], "p= 0.12")
    expect_equal(result[2], "p= 0.012")
})

test_that("format_pvalue rejects non-numeric input", {
    expect_error(format_pvalue("0.05"), "is.numeric")
})

test_that("format_pvalue rejects out-of-range values", {
    expect_error(format_pvalue(1.5), "x >= 0 & x <= 1")
    expect_error(format_pvalue(-0.05), "x >= 0 & x <= 1")
})

test_that("format_pvalue handles NA values", {
    result <- format_pvalue(c(0.05, NA, 0.01))
    expect_equal(length(result), 3)
    expect_equal(result[1], "p= 0.050")
    expect_equal(result[2], "p= NA")
    expect_equal(result[3], "p= 0.010")
    result <- format_pvalue(1.0)
    expect_equal(result, "p= 1.00")
})

test_that("format_pvalue handles edge case p=0", {
    result <- format_pvalue(0.0)
    expect_equal(result, "p= 0.00 × 10<sup>0</sup>")

    result_no_html <- format_pvalue(0.0, html = FALSE)
    expect_equal(result_no_html, "p= 0.00e+00")
})

test_that("format_pvalue truncate uses < operator in symbol position", {
    # When truncate_pvalue = TRUE and p < 0.001, it uses "<" instead of " = "
    result <- format_pvalue(0.00001, truncate_pvalue = TRUE)
    expect_match(result, "p<0.001")
})

test_that("format_pvalue all parameters work together", {
    result <- format_pvalue(
        0.0005,
        p_text = "α",
        p_symbol = ": ",
        truncate_pvalue = FALSE,
        html = TRUE
    )
    expect_equal(result, "α: 5.00 × 10<sup>-4</sup>")

    result <- format_pvalue(
        0.0005,
        p_text = "α",
        p_symbol = "<",
        truncate_pvalue = TRUE,
        html = TRUE
    )
    expect_equal(result, "α<0.001")
})

test_that("format_pvalue vectorizes correctly with mixed values", {
    result <- format_pvalue(
        c(0.00001, 0.0005, 0.005, 0.05, 0.5),
        truncate_pvalue = TRUE
    )
    expect_equal(result[1], "p<0.001")
    expect_equal(result[2], "p<0.001")
    expect_equal(result[3], "p= 0.005")
    expect_equal(result[4], "p= 0.050")
    expect_equal(result[5], "p= 0.50")
})
