#' Create a table that summarizes the entire cohort.
#'
#' Convenience wrapper that builds a gtsummary::tbl_summary for the overall
#' dataset.
#'
#' @param d data.frame Dataset containing variables to summarize.
#' @param missing character(1) How to display missing data. Options are "ifany", "no", or "always". Default is "ifany".
#' @param stats_col_label character(1) Label for the summary column in the table. Default is "Summary".
#' @param statistic Specifies summary statistics to display for each variable.  See `gtsummary::tbl_summmary` documentation for details.
#' @param digits Specifies the number of decimal places to display for each summary statistic in `statistic`.
#' @param binary_01_only_show1 logical(1) If TRUE, binary variables with values 0 and 1 (or No and Yes) will only show the summary for the "1" (or Yes) value. Default is TRUE.
#' @return A `gtsummary::tbl_summary` object for the overall cohort.
#' @examples
#' df <- data.frame(age = rnorm(20, 50, 10), sex = sample(c("M","F"), 20, TRUE))
#' table_overall(df)
#' @export
table_overall <- function(
    d,
    missing = c("ifany", "no", "always"),
    stats_col_label = 'Summary',
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
    ),
    binary_01_only_show1 = TRUE
) {
    missing <- match.arg(missing)
    stopifnot(is.data.frame(d))
    stopifnot(is.character(stats_col_label) && length(stats_col_label) == 1)
    stopifnot(is.list(statistic))
    stopifnot(is.list(digits))
    stopifnot(
        is.logical(binary_01_only_show1) && length(binary_01_only_show1) == 1
    )
    gtsummary::theme_gtsummary_language("en", big.mark = ",")
    gtsummary::theme_gtsummary_compact()
    if (binary_01_only_show1) {
        type <- list(
            gtsummary::all_continuous() ~ 'continuous2'
        )
    } else {
        type <- list(
            gtsummary::all_continuous() ~ 'continuous2',
            gtsummary::all_dichotomous() ~ 'categorical'
        )
    }
    tab_overall <- gtsummary::tbl_summary(
        data = d,
        type = type,
        missing = missing,
        missing_text = 'N missing',
        statistic = statistic,
        digits = digits
    ) |>
        gtsummary::modify_header(
            gtsummary::all_stat_cols() ~ sprintf(
                '**%s**<br>N = {format(N, big.mark = ",", scientific = FALSE)}',
                stats_col_label
            )
        ) |>
        gtsummary::modify_footnote(gtsummary::everything() ~ NA) |>
        gtsummary::bold_labels()

    return(tab_overall)
}
