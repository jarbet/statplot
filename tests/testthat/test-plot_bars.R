bar_levels <- function(p) levels(p$data[["group"]])

test_that("plot_bars returns a ggplot", {
    dat <- data.frame(group = c("A", "B", "C"), score = c(5, 3, 8))
    expect_s3_class(plot_bars(dat, yvar = "score", xvar = "group"), "ggplot")
})

test_that("sort orders bars by the median of yvar", {
    dat <- data.frame(
        group = c("A", "A", "B", "B", "C", "C"),
        score = c(1, 9, 2, 2, 4, 4) # medians: A = 5, B = 2, C = 4
    )
    expect_equal(
        bar_levels(plot_bars(dat, "score", "group", sort = "desc")),
        c("A", "C", "B")
    )
    expect_equal(
        bar_levels(plot_bars(dat, "score", "group", sort = "asc")),
        c("B", "C", "A")
    )
    expect_equal(
        bar_levels(plot_bars(dat, "score", "group", sort = "none")),
        c("A", "B", "C")
    )
})

test_that("sort keeps tied groups in their original level order", {
    dat <- data.frame(
        group = factor(c("x", "y", "z", "w"), levels = c("z", "y", "x", "w")),
        score = c(1, 2, 1, 2)
    )
    expect_equal(
        bar_levels(plot_bars(dat, "score", "group", sort = "desc")),
        c("y", "w", "z", "x")
    )
    expect_equal(
        bar_levels(plot_bars(dat, "score", "group", sort = "asc")),
        c("z", "x", "y", "w")
    )
})

test_that("sort ignores missing yvar values when computing medians", {
    dat <- data.frame(
        group = c("A", "A", "B", "B"),
        score = c(1, NA, 2, NA)
    )
    expect_equal(
        bar_levels(plot_bars(dat, "score", "group", sort = "desc")),
        c("B", "A")
    )
})

test_that("labels come from the 'label' attribute, then the column name", {
    dat <- data.frame(group = c("A", "B"), score = c(1, 2))
    p <- plot_bars(dat, "score", "group")
    expect_equal(p$labels$title, "score by group")

    attr(dat$group, "label") <- "Treatment group"
    attr(dat$score, "label") <- "Mean score"
    p <- plot_bars(dat, "score", "group")
    expect_equal(p$labels$title, "Mean score by Treatment group")
    # the x label survives the conversion to a sorted factor
    expect_equal(attr(p$data$group, "label"), "Treatment group")

    p <- plot_bars(dat, "score", "group", xvar_label = "Arm", yvar_label = "Y")
    expect_equal(p$labels$title, "Y by Arm")
})

test_that("plot_bars validates inputs", {
    dat <- data.frame(group = c("A", "B"), score = c(1, 2))
    expect_error(plot_bars("x", "score", "group"))
    expect_error(plot_bars(dat, "missing", "group"))
    expect_error(plot_bars(dat, "score", "group", sort = "random"))
})
