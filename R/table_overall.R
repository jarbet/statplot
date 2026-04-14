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
