make_test_data <- function() {
    d <- data.frame(
        name = c("Alice", "Bob", "Carol"),
        age = c(25, 30, 35)
    )
    attr(d$age, "label") <- "Age, years"
    d
}

test_that("table_basic_interactive returns a DT table with the expected settings", {
    d <- make_test_data()
    result <- table_basic_interactive(d, page_length = 2)

    expect_s3_class(result, "datatables")
    expect_equal(result$x$options$pageLength, 2)
    expect_equal(result$x$filter, "top")
    # No row-number column: only the data's own columns are shown.
    expect_equal(ncol(result$x$data), ncol(d))
    expect_equal(result$x$data$name, d$name)
})

test_that("table_basic_interactive uses column labels as headers when present", {
    d <- make_test_data()

    with_labels <- table_basic_interactive(d)
    expect_match(with_labels$x$container, "Age, years", fixed = TRUE)
    # Columns without a label keep their name.
    expect_match(with_labels$x$container, ">name<", fixed = TRUE)

    without_labels <- table_basic_interactive(d, use_column_labels = FALSE)
    expect_no_match(without_labels$x$container, "Age, years", fixed = TRUE)
    expect_match(without_labels$x$container, ">age<", fixed = TRUE)
})

test_that("table_basic_interactive defaults to 10 rows per page", {
    result <- table_basic_interactive(make_test_data())
    expect_equal(result$x$options$pageLength, 10)
})

test_that("table_basic_interactive passes filter to DT", {
    d <- make_test_data()
    expect_equal(table_basic_interactive(d, filter = "none")$x$filter, "none")
    expect_equal(
        table_basic_interactive(d, filter = "bottom")$x$filter,
        "bottom"
    )
})

test_that("table_basic_interactive merges options with the defaults", {
    d <- make_test_data()

    # New options are added and the defaults are kept.
    added <- table_basic_interactive(
        d,
        page_length = 2,
        options = list(lengthMenu = c(2, 5, 10))
    )
    expect_equal(added$x$options$lengthMenu, c(2, 5, 10))
    expect_equal(added$x$options$pageLength, 2)
    expect_true(added$x$options$autoWidth)

    # An option of the same name replaces the default.
    replaced <- table_basic_interactive(d, options = list(autoWidth = FALSE))
    expect_false(replaced$x$options$autoWidth)
})

test_that("table_basic_interactive passes ... to DT::datatable", {
    result <- table_basic_interactive(
        make_test_data(),
        caption = "My caption",
        class = "cell-border stripe"
    )
    expect_match(result$x$caption, "My caption", fixed = TRUE)
    expect_match(result$x$container, "cell-border stripe", fixed = TRUE)
})

test_that("table_basic_interactive output works with DT formatting functions", {
    result <- table_basic_interactive(make_test_data()) |>
        DT::formatRound("age", digits = 1)
    expect_s3_class(result, "datatables")
})

test_that("table_basic_interactive validates its arguments", {
    d <- make_test_data()

    expect_error(
        table_basic_interactive(list(age = 1)),
        "`d` must be a data.frame"
    )
    for (bad_page_length in list(0, 2.5, c(5, 10), NA_real_, "5")) {
        expect_error(
            table_basic_interactive(d, page_length = bad_page_length),
            "`page_length` must be a single positive whole number"
        )
    }
    for (bad_flag in list(NA, c(TRUE, FALSE), "yes")) {
        expect_error(
            table_basic_interactive(d, use_column_labels = bad_flag),
            "`use_column_labels` must be TRUE or FALSE"
        )
    }
    for (bad_filter in list("left", c("top", "none"), NA_character_, 1)) {
        expect_error(
            table_basic_interactive(d, filter = bad_filter),
            "`filter` must be one of"
        )
    }
    expect_error(
        table_basic_interactive(d, options = c(pageLength = 5)),
        "`options` must be a list"
    )
    expect_error(
        table_basic_interactive(d, colnames = c("A", "B")),
        "cannot be passed in `...`"
    )
    expect_error(
        table_basic_interactive(d, rownames = TRUE),
        "cannot be passed in `...`"
    )
})
