test_that("markdown = TRUE uses element_markdown for title/legend-title elements", {
    for (th in list(theme_bw2(), theme_classic2())) {
        for (el in c(
            "plot.title", "plot.subtitle", "plot.caption",
            "axis.title.x", "axis.title.y", "legend.title"
        )) {
            expect_s3_class(th[[el]], "element_markdown")
        }
        expect_equal(th$plot.title$face, "bold")
        expect_equal(th$axis.title.x$face, "bold")
        # axis text and legend text are left as plain text
        expect_false(inherits(th$axis.text.x, "element_markdown"))
        expect_false(inherits(th$axis.text.y, "element_markdown"))
        expect_false(inherits(th$legend.text, "element_markdown"))
        expect_false(inherits(th$strip.text, "element_markdown"))
    }
})

test_that("markdown = FALSE uses plain element_text throughout", {
    for (th in list(
        theme_bw2(markdown = FALSE),
        theme_classic2(markdown = FALSE)
    )) {
        for (el in c(
            "plot.title", "plot.subtitle", "plot.caption",
            "axis.title.x", "axis.title.y", "legend.title"
        )) {
            expect_false(inherits(th[[el]], "element_markdown"))
        }
        expect_equal(th$plot.title$face, "bold")
    }
})

test_that("markdown must be a single logical", {
    expect_error(theme_bw2(markdown = "yes"), "TRUE or FALSE")
    expect_error(theme_classic2(markdown = NA), "TRUE or FALSE")
    expect_error(theme_base(markdown = c(TRUE, FALSE)), "TRUE or FALSE")
})

test_that("markdown labels in titles/axis titles/legend title render", {
    p <- ggplot2::ggplot(mtcars, ggplot2::aes(wt, mpg, colour = factor(cyl))) +
        ggplot2::geom_point() +
        ggplot2::labs(
            title = "Title **bold**",
            x = "Weight (10<sup>3</sup> lbs)",
            y = "*Miles* per gallon",
            colour = "Cyl<sub>n</sub>"
        ) +
        theme_bw2()
    expect_no_error(ggplot2::ggplotGrob(p))
})

test_that("axis text can still opt into markdown manually", {
    p <- ggplot2::ggplot(mtcars, ggplot2::aes(wt, mpg)) +
        ggplot2::geom_point() +
        theme_bw2() +
        ggplot2::theme(
            axis.text.x = ggtext::element_markdown(angle = 45, hjust = 1)
        )
    expect_no_error(ggplot2::ggplotGrob(p))
})
