test_that("patchwork_rowtitle returns a ggplot object", {
    result <- patchwork_rowtitle("Test Label")
    expect_s3_class(result, "ggplot")
})

test_that("patchwork_rowtitle works with default parameters", {
    expect_no_error(patchwork_rowtitle("Test Label"))
})

test_that("patchwork_rowtitle requires a label argument", {
    expect_error(patchwork_rowtitle())
})

test_that("custom size parameter works", {
    result_default <- patchwork_rowtitle("Test")
    result_custom <- patchwork_rowtitle("Test", size = 10)
    expect_s3_class(result_custom, "ggplot")
})

test_that("custom fill parameter works", {
    result_custom <- patchwork_rowtitle("Test", fill = "white")
    expect_s3_class(result_custom, "ggplot")
})

test_that("custom color parameter works", {
    result_custom <- patchwork_rowtitle("Test", color = "red")
    expect_s3_class(result_custom, "ggplot")
})

test_that("custom linewidth parameter works", {
    result_custom <- patchwork_rowtitle("Test", linewidth = 2.5)
    expect_s3_class(result_custom, "ggplot")
})

test_that("custom position parameters work", {
    result_custom <- patchwork_rowtitle(
        "Test",
        x = 0.3,
        y = 0.7,
        hjust = 0.2,
        vjust = 0.8
    )
    expect_s3_class(result_custom, "ggplot")
})

test_that("custom margin parameters work", {
    result_custom <- patchwork_rowtitle(
        "Test",
        margin_top = 10,
        margin_right = 5,
        margin_bottom = 10,
        margin_left = 5
    )
    expect_s3_class(result_custom, "ggplot")
})

test_that("plot has correct number of layers", {
    result <- patchwork_rowtitle("Test Label")
    # Should have: annotate + scale_x + scale_y + theme_void + theme
    expect_equal(length(result$layers), 1)
})

test_that("plot scales are set to 0-1 limits", {
    result <- patchwork_rowtitle("Test Label")
    # Check x scale
    expect_equal(result$scales$get_scales("x")$limits, c(0, 1))
    # Check y scale
    expect_equal(result$scales$get_scales("y")$limits, c(0, 1))
})

test_that("label text is correctly set in annotation layer", {
    result <- patchwork_rowtitle("My Test Label")
    expect_equal(result$layers[[1]]$aes_params$label, "My Test Label")
})

test_that("text position is correctly set in annotation layer", {
    result <- patchwork_rowtitle("Test", x = 0.3, y = 0.8)
    expect_equal(result$layers[[1]]$data$x, 0.3)
    expect_equal(result$layers[[1]]$data$y, 0.8)
})

test_that("text size is correctly set in annotation layer", {
    result <- patchwork_rowtitle("Test", size = 8)
    expect_equal(result$layers[[1]]$aes_params$size, 8)
})

test_that("text hjust and vjust are correctly set", {
    result <- patchwork_rowtitle("Test", hjust = 0.2, vjust = 0.9)
    expect_equal(result$layers[[1]]$aes_params$hjust, 0.2)
    expect_equal(result$layers[[1]]$aes_params$vjust, 0.9)
})

test_that("fill color is applied to plot background", {
    result <- patchwork_rowtitle("Test", fill = "lightblue")
    bg_element <- result$theme$plot.background
    expect_equal(bg_element$fill, "lightblue")
})

test_that("border color is applied to plot background", {
    result <- patchwork_rowtitle("Test", color = "darkred")
    bg_element <- result$theme$plot.background
    expect_equal(bg_element$colour, "darkred")
})

test_that("linewidth is applied to plot background", {
    result <- patchwork_rowtitle("Test", linewidth = 3)
    bg_element <- result$theme$plot.background
    expect_equal(bg_element$linewidth, 3)
})

test_that("margins are correctly applied", {
    result <- patchwork_rowtitle(
        "Test",
        margin_top = 8,
        margin_right = 4,
        margin_bottom = 2,
        margin_left = 1
    )
    margins <- result$theme$plot.margin
    # margins are stored as c(t, r, b, l) in points
    expect_equal(as.numeric(margins), c(8, 4, 2, 1))
})
