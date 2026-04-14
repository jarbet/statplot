#' Create overall and by-group summary table
#'
#' The Overall column reports percentages for the overall cohort.
#' The group columns report row-wise percentages.
#'
#' @param d data.frame. Dataset containing variables to summarize.
#' @param name_groupvar character(1). Name of the grouping variable (column
#'   name) in `d` used to produce the grouped summary.
#' @param group_header character(1). Label used as the tab spanner for the
#'   grouped columns in the merged table.
#'
#' @return A gtsummary table object
#'
#' @details The function builds two tbl_summary objects: one for the overall
#'   dataset (with the grouping column removed) and one stratified by the
#'   specified grouping variable. It then merges them into a single table with
#'   a custom tab spanner for the grouped columns.
#'
#' @examples
#' df <- data.frame(
#'   grp = rep(c("A","B"), each = 10),
#'   age = rnorm(20, 50, 10),
#'   sex = sample(c("M","F"), 20, TRUE)
#' )
#' table_overall_and_group(df, "grp")
#'
#' @export
table_overall_and_group <- function(
    d,
    name_groupvar,
    group_header = '**Group**'
) {
    gtsummary::theme_gtsummary_language("en", big.mark = ",")
    gtsummary::theme_gtsummary_compact()
    tab_overall <- gtsummary::tbl_summary(
        data = d[, colnames(d) != name_groupvar],
        type = list(
            gtsummary::all_continuous() ~ 'continuous2',
            gtsummary::all_dichotomous() ~ 'categorical'
        ),
        missing = 'ifany',
        missing_text = 'N missing',
        statistic = list(
            gtsummary::all_continuous() ~ c(
                '{median} ({p25}, {p75})',
                # '{mean} +/- {sd}',    # <-- changed from "#'{...'" to avoid roxygen parsing
                '{min}, {max}'
            ),
            gtsummary::all_categorical() ~ c('{n} ({p}%)')
        ),
        digits = list(
            gtsummary::all_continuous() ~ c(rep(1, 7))
        )
    ) |>
        gtsummary::modify_header(
            gtsummary::all_stat_cols() ~ '**{level}**<br>N = {format(N, big.mark = ",", scientific = FALSE)}'
        ) |>
        gtsummary::modify_footnote(gtsummary::everything() ~ NA) |>
        gtsummary::bold_labels()

    tab_group <- gtsummary::tbl_summary(
        data = d,
        by = name_groupvar,
        percent = "row", # <-- ADDED: use row-wise percentages for categorical variables
        type = list(
            gtsummary::all_continuous() ~ 'continuous2',
            gtsummary::all_dichotomous() ~ 'categorical'
        ),
        missing = 'ifany',
        missing_text = 'N missing',
        statistic = list(
            gtsummary::all_continuous() ~ c(
                '{median} ({p25}, {p75})',
                # '{mean} +/- {sd}',    # <-- changed from "#'{...'" to avoid roxygen parsing
                '{min}, {max}'
            ),
            gtsummary::all_categorical() ~ c('{n} ({p}%)')
        ),
        digits = list(
            gtsummary::all_continuous() ~ c(rep(1, 7))
        )
    ) |>
        gtsummary::modify_header(
            # put Ns on their own line:
            gtsummary::all_stat_cols() ~ '**{level}**<br>n = {format(n, big.mark = ",", scientific = FALSE)}',
            label = ''
        ) |>
        gtsummary::modify_footnote(
            gtsummary::everything() ~ NA
        ) |>
        gtsummary::bold_labels()

    tbl_merged <- gtsummary::tbl_merge(
        list(tab_overall, tab_group),
        tab_spanner = c(NA, group_header)
    )

    return(tbl_merged)
}

#' Create a table that summarizes the entire cohort.
#'
#' Convenience wrapper that builds a gtsummary::tbl_summary for the overall
#' dataset.
#'
#' @param d data.frame Dataset containing variables to summarize.
#' @return A gtsummary::tbl_summary object for the overall cohort.
#' @examples
#' df <- data.frame(age = rnorm(20, 50, 10), sex = sample(c("M","F"), 20, TRUE))
#' table_overall(df)
#' @export
table_overall <- function(d) {
    gtsummary::theme_gtsummary_language("en", big.mark = ",")
    gtsummary::theme_gtsummary_compact()
    tab_overall <- gtsummary::tbl_summary(
        data = d,
        type = list(
            gtsummary::all_continuous() ~ 'continuous2',
            gtsummary::all_dichotomous() ~ 'categorical'
        ),
        missing = 'ifany',
        missing_text = 'N missing',
        statistic = list(
            gtsummary::all_continuous() ~ c(
                '{median} ({p25}, {p75})',
                '{mean} +/- {sd}',
                '{min}, {max}'
            ),
            gtsummary::all_categorical() ~ c('{n} ({p}%)')
        ),
        digits = list(
            gtsummary::all_continuous() ~ c(rep(1, 7))
        )
    ) |>
        gtsummary::modify_header(
            gtsummary::all_stat_cols() ~ '**{level}**<br>N = {format(N, big.mark = ",", scientific = FALSE)}'
        ) |>
        gtsummary::modify_footnote(gtsummary::everything() ~ NA) |>
        gtsummary::bold_labels()

    return(tab_overall)
}

#' Create a basic table using tinytable
#'
#' Wrapper around tinytable functions to create a simple, clean table with
#' a bold header row and optional column-level rounding control.
#'
#' @param d data.frame. Dataset to display as a table.
#' @param digits numeric or list. Rounding specification for columns:
#'   - If a single numeric value, applied to all numeric columns.
#'   - If a numeric vector, one value per column in order. Non-numeric columns
#'     ignore their corresponding value.
#'   - If a list, a named list mapping column names to number of digits (e.g.,
#'     `list(age = 0, salary = 2)`). Unnamed or missing columns keep their
#'     original precision.
#' @param replace_missing character(1). String to replace missing values with.
#'   Default is empty string ("").
#' @param commas_large_numbers logical(1). If TRUE (default), format numeric values
#'   with commas for thousands (e.g., 1,234.56). If FALSE, no comma formatting.
#' @param use_column_labels logical(1). If TRUE, use the
#'   `label` attribute of columns if available, otherwise use column names.
#'   Passed to `tinytable::tt(colnames = ...)`.
#'
#' @return A tinytable object
#'
#' @examples
#' df <- data.frame(
#'   name = c("Alice", "Bob", NA),
#'   age = c(25.5, 30.2, 35.8),
#'   salary = c(50000.123, 60000.456, NA)
#' )
#'
#' # Round specific columns by name
#' table_basic(df, digits = list(age = 1, salary = 0))
#'
#' # Round by column position (ignores non-numeric columns)
#' table_basic(df, digits = c(0, 1, 2))
#'
#' # Disable comma formatting for large numbers
#' table_basic(df, digits = list(age = 1, salary = 0), commas_large_numbers = FALSE)
#'
#' @export
table_basic <- function(
    d,
    digits = NULL,
    replace_missing = "",
    commas_large_numbers = TRUE,
    use_column_labels = TRUE
) {
    # Make a copy to avoid modifying the original
    d_formatted <- d

    # Apply column-level rounding if specified
    if (!is.null(digits)) {
        if (is.numeric(digits)) {
            if (length(digits) == 1) {
                # Single numeric value: apply to all numeric columns
                numeric_cols <- which(sapply(d, is.numeric))
                for (j in numeric_cols) {
                    d_formatted[[j]] <- round(d_formatted[[j]], digits)
                }
            } else if (length(digits) == ncol(d)) {
                # Vector with one value per column: apply to each numeric column
                for (j in seq_len(ncol(d))) {
                    if (is.numeric(d[[j]])) {
                        d_formatted[[j]] <- round(d_formatted[[j]], digits[j])
                    }
                }
            }
        } else if (is.list(digits)) {
            # Named list: apply rounding to specified columns
            for (col_name in names(digits)) {
                if (col_name %in% colnames(d)) {
                    if (is.numeric(d[[col_name]])) {
                        d_formatted[[col_name]] <- round(
                            d_formatted[[col_name]],
                            digits[[col_name]]
                        )
                    }
                }
            }
        }
    }

    # Apply comma formatting for large numbers if specified
    if (commas_large_numbers) {
        numeric_cols <- which(sapply(d_formatted, is.numeric))
        for (j in numeric_cols) {
            d_formatted[[j]] <- format(
                d_formatted[[j]],
                big.mark = ",",
                scientific = FALSE,
                trim = TRUE
            )
        }
    }

    # Build and return the tinytable
    colnames_arg <- if (use_column_labels) "label" else TRUE
    result <- tinytable::tt(d_formatted, colnames = colnames_arg) |>
        tinytable::style_tt(i = 0, j = 1:ncol(d_formatted), bold = TRUE) |>
        tinytable::format_tt(replace = replace_missing)

    return(result)
}
