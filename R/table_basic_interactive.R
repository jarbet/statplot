#' Create a basic interactive table using DT
#'
#' Interactive counterpart of [table_basic()] for HTML output: shows a data
#' frame as a searchable, paged DT table with a filter box above each column
#' and no row-number column. Useful for long tables (e.g. data-cleaning QC
#' tables) that are too long to print in full.
#'
#' @param d data.frame. Dataset to display as a table.
#' @param page_length numeric(1). Number of rows shown per page. Must be a
#'   positive whole number. Default is 10.
#' @param use_column_labels logical(1). If TRUE (default), use the `label`
#'   attribute of columns as headers where available (e.g. set with
#'   `labelled::var_label()`), otherwise use column names.
#' @param filter character(1). Where to show the column filters: `"top"`
#'   (default), `"bottom"` or `"none"`. Passed to `DT::datatable(filter = ...)`.
#' @param options list. DataTables options (see
#'   <https://rstudio.github.io/DT/options.html>), merged with the defaults
#'   `list(pageLength = page_length, autoWidth = TRUE)`: options given here are
#'   added, or replace a default of the same name. For example,
#'   `list(lengthMenu = c(5, 10, 20))` adds a menu for changing the page length.
#' @param ... Further arguments passed to [DT::datatable()], e.g. `caption`,
#'   `class` (e.g. `"cell-border stripe"`), `style` or `extensions`. `data`,
#'   `colnames` and `rownames` are set by this function and cannot be passed.
#'
#' @return A `datatables` htmlwidget. It can be piped into DT's formatting
#'   functions, such as [DT::formatRound()] or [DT::formatStyle()].
#'
#' @examples
#' df <- data.frame(
#'   name = c("Alice", "Bob", "Carol"),
#'   age = c(25.12, 30.55, 35.91)
#' )
#' attr(df$age, "label") <- "Age, years"
#'
#' table_basic_interactive(df)
#'
#' # Show 2 rows per page and use column names as headers
#' table_basic_interactive(df, page_length = 2, use_column_labels = FALSE)
#'
#' # No column filters, a page-length menu and a styled table with a caption
#' table_basic_interactive(
#'   df,
#'   filter = "none",
#'   options = list(lengthMenu = c(2, 5, 10)),
#'   class = "cell-border stripe",
#'   caption = "Participants"
#' )
#'
#' # Round a column with DT's formatting functions
#' table_basic_interactive(df) |>
#'   DT::formatRound("age", digits = 1)
#'
#' @export
table_basic_interactive <- function(
    d,
    page_length = 10,
    use_column_labels = TRUE,
    filter = "top",
    options = list(),
    ...
) {
    fixed_arguments <- c("data", "colnames", "rownames")
    stopifnot(
        "`d` must be a data.frame" = is.data.frame(d),
        "`page_length` must be a single positive whole number" = is.numeric(
            page_length
        ) &&
            length(page_length) == 1 &&
            !is.na(page_length) &&
            page_length >= 1 &&
            page_length == round(page_length),
        "`use_column_labels` must be TRUE or FALSE" = is.logical(
            use_column_labels
        ) &&
            length(use_column_labels) == 1 &&
            !is.na(use_column_labels),
        "`filter` must be one of \"top\", \"bottom\" or \"none\"" = is.character(
            filter
        ) &&
            length(filter) == 1 &&
            filter %in% c("top", "bottom", "none"),
        "`options` must be a list" = is.list(options),
        "`data`, `colnames` and `rownames` are set by table_basic_interactive() and cannot be passed in `...`" = !any(
            names(list(...)) %in% fixed_arguments
        )
    )

    # Headers: each column's label attribute if requested and present,
    # otherwise its name.
    column_headers <- names(d)
    if (use_column_labels) {
        column_labels <- vapply(
            X = d,
            FUN = function(column) {
                label <- attr(column, "label", exact = TRUE)
                if (is.character(label) && length(label) == 1) {
                    label
                } else {
                    NA_character_
                }
            },
            FUN.VALUE = character(1)
        )
        has_label <- !is.na(column_labels)
        column_headers[has_label] <- column_labels[has_label]
    }

    # User options are added to, or replace, the defaults.
    table_options <- utils::modifyList(
        x = list(
            pageLength = page_length,
            autoWidth = TRUE
        ),
        val = options
    )

    DT::datatable(
        data = d,
        colnames = unname(column_headers),
        filter = filter,
        rownames = FALSE,
        options = table_options,
        ...
    )
}
