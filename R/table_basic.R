#' Create a basic table using tinytable
#'
#' Wrapper around tinytable functions to create a simple, clean table with
#' a bold header row and optional column-level rounding control.
#'
#' @param d data.frame. Dataset to display as a table.
#' @param digits numeric or list. Rounding specification for columns:
#'   - If a single numeric value, applied to all numeric columns.
#'   - If a numeric vector, must have length equal to the number of columns in
#'     `d`. Non-numeric columns ignore their corresponding value. Providing a
#'     vector with any other length will raise an error.
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
            } else {
                # Invalid vector length: throw a clear error
                rlang::abort(
                    c(
                        "Invalid `digits` vector length.",
                        "x" = paste0(
                            "The `digits` vector has length ",
                            length(digits),
                            ", but must be either:\n",
                            "  1. A single value (applied to all numeric columns), or\n",
                            "  2. A vector of length ",
                            ncol(d),
                            " (one value per column)"
                        ),
                        "i" = "Alternatively, use a named list: list(col1 = 1, col2 = 2)"
                    )
                )
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

    # Track NA positions and replace them AFTER formatting
    na_positions <- list()
    for (j in seq_len(ncol(d_formatted))) {
        na_positions[[j]] <- which(is.na(d_formatted[[j]]))
    }

    # Apply comma formatting for large numbers if specified
    if (commas_large_numbers) {
        numeric_cols <- which(sapply(d_formatted, is.numeric))
        for (j in numeric_cols) {
            d_formatted[[j]] <- format(
                d_formatted[[j]],
                big.mark = ",",
                scientific = FALSE,
                trim = TRUE,
                na.encode = FALSE
            )
        }
    }

    # Replace NA values (which became "NA" strings after formatting) with replace_missing
    for (j in seq_len(ncol(d_formatted))) {
        if (length(na_positions[[j]]) > 0) {
            d_formatted[[j]][na_positions[[j]]] <- replace_missing
        }
    }

    # Build and return the tinytable
    colnames_arg <- if (use_column_labels) "label" else TRUE
    result <- tinytable::tt(d_formatted, colnames = colnames_arg) |>
        tinytable::style_tt(i = 0, j = 1:ncol(d_formatted), bold = TRUE)

    return(result)
}
