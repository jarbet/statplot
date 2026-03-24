make_avail_df <- function() {
    data.frame(
        data_type = rep(c("SNV", "mRNA", "Proteomics"), each = 3),
        group = rep(c("GroupA", "GroupB", "GroupC"), times = 3),
        available = c(1, 1, 0, 1, 0, 1, 0, 1, 1)
    )
}

# ---------------------------------------------------------------------------
# Return type
# ---------------------------------------------------------------------------

test_that("returns a ggplot object", {
    p <- plot_data_avail_by_group(make_avail_df())
    expect_s3_class(p, "ggplot")
})

# ---------------------------------------------------------------------------
# Input validation
# ---------------------------------------------------------------------------

test_that("errors if available column contains values other than 0/1", {
    df <- make_avail_df()
    df$available[1] <- 2
    expect_error(plot_data_avail_by_group(df))
})

test_that("errors if available column contains negative values", {
    df <- make_avail_df()
    df$available[1] <- -1
    expect_error(plot_data_avail_by_group(df))
})

test_that("allows NA values in available column", {
    df <- make_avail_df()
    df$available[1] <- NA
    expect_s3_class(plot_data_avail_by_group(df), "ggplot")
})

test_that("errors if column name arguments are not single strings", {
    expect_error(plot_data_avail_by_group(make_avail_df(), data_type = 1))
    expect_error(plot_data_avail_by_group(
        make_avail_df(),
        group = c("group", "extra")
    ))
    expect_error(plot_data_avail_by_group(
        make_avail_df(),
        available = NA_character_
    ))
})

test_that("errors with informative message if columns are missing from data", {
    df <- make_avail_df()
    expect_error(
        plot_data_avail_by_group(df, data_type = "nonexistent"),
        "nonexistent"
    )
    expect_error(
        plot_data_avail_by_group(df, group = "missing_col"),
        "missing_col"
    )
})

# ---------------------------------------------------------------------------
# Custom column names
# ---------------------------------------------------------------------------

test_that("works with non-default column names", {
    df <- make_avail_df()
    names(df) <- c("dtype", "grp", "avail")
    expect_s3_class(
        plot_data_avail_by_group(
            df,
            data_type = "dtype",
            group = "grp",
            available = "avail"
        ),
        "ggplot"
    )
})

# ---------------------------------------------------------------------------
# Axis label arguments
# ---------------------------------------------------------------------------

test_that("runs without error when xlabel and ylabel are NULL", {
    expect_no_error(plot_data_avail_by_group(make_avail_df()))
})

test_that("runs without error when xlabel and ylabel are supplied", {
    expect_no_error(
        plot_data_avail_by_group(
            make_avail_df(),
            xlabel = "Group",
            ylabel = "Data Type"
        )
    )
})

test_that("xlabel_top_margin does not cause an error", {
    expect_no_error(
        plot_data_avail_by_group(
            make_avail_df(),
            xlabel = "Group",
            xlabel_top_margin = 15
        )
    )
})

# ---------------------------------------------------------------------------
# Other arguments
# ---------------------------------------------------------------------------

test_that("legend_position argument is accepted", {
    expect_no_error(
        plot_data_avail_by_group(make_avail_df(), legend_position = "bottom")
    )
})

test_that("tile_line_color argument is accepted", {
    expect_no_error(
        plot_data_avail_by_group(make_avail_df(), tile_line_color = "grey50")
    )
})
