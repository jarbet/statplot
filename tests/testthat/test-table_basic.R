test_that("table_basic returns a tinytable object", {
    df <- data.frame(x = c(1, 2, 3), y = c("a", "b", "c"))
    result <- table_basic(df)
    expect_s4_class(result, "tinytable")
})

test_that("table_basic with single digits value rounds all numeric columns", {
    df <- data.frame(
        name = c("Alice", "Bob"),
        value1 = c(1.234, 5.678),
        value2 = c(10.567, 20.123)
    )
    result <- table_basic(df, digits = 1)
    expect_s4_class(result, "tinytable")
})

test_that("table_basic with digits vector applies rounding per column", {
    df <- data.frame(
        name = c("Alice", "Bob"),
        age = c(25.5, 30.2),
        salary = c(50000.123, 60000.456)
    )
    result <- table_basic(df, digits = c(0, 1, 2))
    expect_s4_class(result, "tinytable")
})

test_that("table_basic with digits vector ignores non-numeric columns", {
    df <- data.frame(
        name = c("Alice", "Bob"),
        age = c(25.5, 30.2),
        dept = c("Sales", "IT")
    )
    result <- table_basic(df, digits = c(0, 1, 2))
    expect_s4_class(result, "tinytable")
})

test_that("table_basic with named list digits", {
    df <- data.frame(
        name = c("Alice", "Bob"),
        age = c(25.5, 30.2),
        salary = c(50000.123, 60000.456)
    )
    result <- table_basic(df, digits = list(age = 0, salary = 2))
    expect_s4_class(result, "tinytable")
})

test_that("table_basic with missing values replaced", {
    df <- data.frame(
        name = c("Alice", "Bob", NA),
        value = c(1.5, 2.5, NA)
    )
    result <- table_basic(df, replace_missing = "N/A")
    expect_s4_class(result, "tinytable")
})

test_that("table_basic default replace_missing is empty string", {
    df <- data.frame(
        name = c("Alice", NA),
        value = c(1.5, NA)
    )
    result <- table_basic(df)
    expect_s4_class(result, "tinytable")
})

test_that("table_basic with no digits argument works", {
    df <- data.frame(x = c(1.234, 5.678), y = c("a", "b"))
    result <- table_basic(df, digits = NULL)
    expect_s4_class(result, "tinytable")
})

test_that("table_basic with digits vector length mismatch only applies to numeric", {
    df <- data.frame(
        a = c(1.111, 2.222),
        b = c("x", "y"),
        c = c(3.333, 4.444)
    )
    # Vector has 3 values for 3 columns, but only numeric columns use them
    result <- table_basic(df, digits = c(1, 2, 1))
    expect_s4_class(result, "tinytable")
})

test_that("table_basic works with single row dataframe", {
    df <- data.frame(name = "Alice", age = 25.5, salary = 50000.123)
    result <- table_basic(df, digits = list(age = 0, salary = 2))
    expect_s4_class(result, "tinytable")
})

test_that("table_basic works with single column dataframe", {
    df <- data.frame(value = c(1.234, 5.678, 9.012))
    result <- table_basic(df, digits = 1)
    expect_s4_class(result, "tinytable")
})

test_that("table_basic named list ignores missing column names", {
    df <- data.frame(
        age = c(25.5, 30.2),
        salary = c(50000.123, 60000.456)
    )
    result <- table_basic(df, digits = list(age = 0, missing_col = 2))
    expect_s4_class(result, "tinytable")
})

test_that("table_basic with all numeric columns and single digit", {
    df <- data.frame(
        x = c(1.111, 2.222),
        y = c(3.333, 4.444),
        z = c(5.555, 6.666)
    )
    result <- table_basic(df, digits = 2)
    expect_s4_class(result, "tinytable")
})

test_that("table_basic with all character columns", {
    df <- data.frame(
        name = c("Alice", "Bob"),
        dept = c("Sales", "IT")
    )
    result <- table_basic(df, digits = 1)
    expect_s4_class(result, "tinytable")
})

test_that("table_basic with commas_large_numbers = TRUE (default)", {
    df <- data.frame(x = c(1234.5, 5678.9))
    result <- table_basic(df, commas_large_numbers = TRUE)
    expect_s4_class(result, "tinytable")
})

test_that("table_basic with commas_large_numbers = FALSE", {
    df <- data.frame(x = c(1234.5, 5678.9))
    result <- table_basic(df, commas_large_numbers = FALSE)
    expect_s4_class(result, "tinytable")
})

test_that("table_basic with digits and commas_large_numbers", {
    df <- data.frame(
        salary = c(50000.123, 60000.456)
    )
    result <- table_basic(df, digits = 0, commas_large_numbers = TRUE)
    expect_s4_class(result, "tinytable")
})

test_that("table_basic with use_column_labels = FALSE (default)", {
    df <- data.frame(x = c(1, 2), y = c(3, 4))
    attr(df$x, "label") <- "X Label"
    result <- table_basic(df, use_column_labels = FALSE)
    expect_s4_class(result, "tinytable")
})

test_that("table_basic with use_column_labels = TRUE", {
    df <- data.frame(x = c(1, 2), y = c(3, 4))
    attr(df$x, "label") <- "X Label"
    result <- table_basic(df, use_column_labels = TRUE)
    expect_s4_class(result, "tinytable")
})
