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
