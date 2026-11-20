#' Visual "Table 1": summary table with inline distribution graphics
#'
#' Summarizes numeric and categorical variables for a cohort (optionally by
#' group) like [table_overall()] / [table_overall_and_group()], but adds compact
#' inline graphics inspired by Furmaga et al. 2026:
#'
#' * **Continuous** variables: by default a horizontal violin plot with an
#'   inner boxplot (box = IQR, thick line = median, whiskers to the most
#'   extreme values within 1.5 x IQR, dots = outliers beyond that) in the
#'   variable's label row, above the numeric summaries. All columns share the
#'   same x-axis (and violin width) scale so groups are directly comparable.
#'   Each violin has the same area (like [ggplot2::geom_violin()]'s default),
#'   so a violin's width shows the shape of that group's distribution, not
#'   its sample size; N is given in each column header.
#' * **Categorical** and **binary** variables: a percentage bar next to each
#'   `n (%)`.
#'
#' Percentages are column percentages (within each group), so bars are
#' comparable across groups. Category percentages exclude missing values, so
#' for variables with missing values counts are shown as `n / N (%)` with N
#' the non-missing count (see `show_denominator`); the "N missing" row gives
#' % missing out of all observations in the column. A table note states this
#' whenever it matters.
#'
#' @param d data.frame. Dataset containing variables to summarize.
#' @param name_groupvar character(1) or `NULL`. Optional name of a grouping
#'   variable in `d`. If `NULL` (default) the entire cohort is summarized.
#'   Rows with a missing group are dropped (and counted in a table note)
#'   unless `missing_group = "show"`.
#' @param overall logical(1). If grouping, also show an Overall column? Default `TRUE`.
#' @param group_header character(1). Spanning header over the group columns.
#' @param stats_col_label character(1). Header label for the overall column.
#' @param missing character(1). When to show an "N missing" row for each
#'   variable: "ifany" (default), "no", or "always". The row shows
#'   `n / N (%)` missing (`n (%)` if `show_denominator = "no"`), where N and
#'   % are out of all observations in the column, with a hatched
#'   bar on the same 0-100% scale as the category bars.
#' @param missing_group character(1). Rows with a missing `name_groupvar`:
#'   `"exclude"` (default) drops them and reports how many in a table note;
#'   `"show"` keeps them as an extra "Missing" group column (colored brown,
#'   `#8C564B`, unless `colors` is named and includes `"Missing"`).
#' @param show_denominator character(1). When to show category counts as
#'   `n / N (%)` instead of `n (%)`, where N is the number of non-missing
#'   values in the column (the percentage's denominator): `"ifmissing"`
#'   (default) for categorical variables with missing values, so the
#'   denominator is visible when it differs from the column N; `"always"` for
#'   all categorical variables; or `"no"`. Only applies to variables whose
#'   `statistic` is the default `"{n} ({p}%)"`. Unless `"no"`, "N missing"
#'   rows also show `n / N (%)`, with N the total number of observations in
#'   the column.
#' @param statistic,digits Passed to [gtsummary::tbl_summary()]. By default
#'   continuous variables show mean (SD) and min; max; the graphic already
#'   shows the median and quartiles. For skewed variables, consider adding a
#'   "Median (Q1, Q3)" row with e.g.
#'   `statistic = list(gtsummary::all_continuous() ~ c("{mean} ({sd})",
#'   "{median} ({p25}, {p75})", "{min}; {max}"),
#'   gtsummary::all_categorical() ~ "{n} ({p}%)")`. `digits = NULL` (default)
#'   lets gtsummary choose the decimals for each variable from its spread, so
#'   variables on a small scale (e.g. 0.03) are not rounded to 0.0; pass e.g.
#'   `list(gtsummary::all_continuous() ~ 1)` to fix them.
#' @param binary_01_only_show1 logical(1). If `TRUE`, binary variables only show
#'   one row (e.g. the "1"/"Yes" level). Default `TRUE`.
#' @param continuous_plot character(1). Graphic for continuous variables:
#'   `"violin_box"` (violin with inner boxplot and outlier dots; default),
#'   `"boxplot"` (boxplot with outlier dots), or `"density"` (density
#'   sparkline with a tick at the median).
#' @param show_outliers logical(1). Draw outliers (values beyond 1.5 x IQR from
#'   the box) as dots? Default `TRUE`. Whiskers still stop at the most extreme
#'   values within 1.5 x IQR when `FALSE`. Has no effect for
#'   `continuous_plot = "density"`.
#' @param show_axis logical(1). Add a small x-axis with a few tick labels
#'   under each continuous-variable graphic to show its range? Default
#'   `TRUE`. The axis is shared by all columns/groups of a variable.
#' @param axis_breaks Optional named list giving tick values for specific
#'   continuous variables when `show_axis = TRUE`, e.g.
#'   `list(age = c(20, 40, 60, 80))`. Names are variable names in `d`;
#'   variables not listed get automatic ticks. Ticks outside the data range
#'   widen that variable's (shared) scale so they are shown. If a vector has
#'   names, they are used as the tick labels, e.g. `c("0" = 0, "1 yr" = 365)`.
#' @param bar_axis logical(1). Add a small 0-100% x-axis under the
#'   percentage bars? Default `FALSE`. The axis is drawn once per variable,
#'   under its last bar (the "N missing" row if shown, otherwise the last
#'   level).
#' @param bar_axis_breaks numeric. Tick positions, in percent (0-100), for
#'   the bar axis when `bar_axis = TRUE`. Default `c(0, 50, 100)`, labelled
#'   "0%", "50%", "100%". If the vector has names, they are used as the tick
#'   labels, e.g. `c("0" = 0, "1/4" = 25, "1/2" = 50)`.
#' @param colors Optional character vector of colors for the group columns
#'   (unnamed in group order, or named by group level). Defaults to the
#'   Okabe-Ito palette.
#' @param overall_color character(1). Color used for the overall column.
#' @param plot_width,plot_height numeric(1). Size of continuous-variable graphics in pixels.
#' @param bar_width numeric(1). Width of percentage bars in pixels
#'   (`plot_layout = "columns"` only).
#' @param plot_layout character(1). Where graphics go when grouping:
#'   `"combined"` (default) adds one "Distribution" column on the right in which the
#'   groups (and Overall) are drawn together on a shared axis, one lane per
#'   group, so they can be compared directly. Continuous variables get stacked
#'   violins/boxplots/densities spanning the variable's label and statistic
#'   rows; each categorical level gets a set of side-by-side bars.
#'   `"columns"` instead draws a separate graphic in each group's column.
#'   Ignored when `name_groupvar` is `NULL`.
#' @param header_align character(1). Alignment of the column headers for the
#'   Overall, group and Distribution columns: `"left"` (default) or `"center"`.
#' @param distribution_label character(1). Header of the combined
#'   distribution column.
#' @param overall_divider logical(1). Draw a vertical line to the right of
#'   the Overall column to set it apart from the group columns? Default
#'   `TRUE`. Only used when grouping with `overall = TRUE`.
#' @param variable_divider logical(1). Draw a thicker horizontal line above
#'   each variable (except the first) so it is clearer where a new variable
#'   starts? Default `TRUE`.
#' @param row_lines logical(1). Draw the thin horizontal lines between the
#'   rows within a variable (e.g. between its statistic rows or levels)?
#'   Default `FALSE`, so only the lines between variables are drawn.
#'   Applies to gt output; the Word (flextable) table has no lines within a
#'   variable.
#' @param output character(1). Table type to return: `"auto"` (default)
#'   returns a flextable when knitting to Word (`.docx`, e.g. Quarto
#'   `format: docx`) and a gt table otherwise; `"gt"` or `"flextable"` force
#'   one. Requires the flextable and xml2 packages for `"flextable"`.
#'
#' @references Furmaga J, et al. A new type of table one: showing instead of
#'   telling. *Journal of Clinical Epidemiology*. 2026;196.
#'
#' @return A `gt::gt_tbl` (`output = "gt"`) or a `flextable` with class
#'   `"table1viz_flextable"` (`output = "flextable"`).
#'
#'   The gt table draws graphics as inline SVG, so it renders in HTML output
#'   (Quarto/R Markdown HTML, RStudio viewer, `gt::gtsave(".html")`).
#'
#'   The flextable is for Word. When knitted to `.docx` it becomes a native,
#'   editable Word table: text, fonts, borders and widths can be edited in
#'   Word, and the graphics are Word vector shapes (drawing groups) inside the
#'   cells, so no image files or post-processing are needed. Graphics are
#'   stored as placeholders until knitting, so print it or knit it as is;
#'   flextable functions that restyle the table (e.g. [flextable::fontsize()])
#'   keep them, but [flextable::save_as_docx()] and other officer exports do
#'   not draw them. When printed or knitted to HTML, the graphics are drawn as
#'   SVG.
#'
#' @examples
#' # Simulated cohort: group, two continuous, two categorical variables
#' set.seed(1)
#' df <- data.frame(
#'   grp = sample(c("A", "B", "C"), 200, TRUE),
#'   age = rnorm(200, 60, 15),
#'   los = rlnorm(200, 5, 0.8),
#'   sex = sample(c("Female", "Male"), 200, TRUE),
#'   diabetes = rbinom(200, 1, 0.4)
#' )
#'
#' # Same data with some missing values
#' df_miss <- df
#' df_miss$los[sample(200, 30)] <- NA
#' df_miss$sex[sample(200, 12)] <- NA
#'
#' # ---- Defaults ----
#'
#' # Whole cohort: one column of summaries with inline graphics
#' table1viz(df)
#'
#' # By group: groups drawn together in one "Distribution" column for direct
#' # comparison
#' table1viz(df, name_groupvar = "grp")
#'
#' # Missing data, whole cohort: "N missing" rows show n / N (%) with hatched bars
#' table1viz(df_miss)
#'
#' # Missing data by group
#' table1viz(df_miss, name_groupvar = "grp")
#'
#' # ---- Other options ----
#'
#' # Separate graphic in each group's column instead of a Distribution column
#' table1viz(df, name_groupvar = "grp", plot_layout = "columns")
#'
#' # Missing data by group, with graphics in each group's column
#' table1viz(df_miss, name_groupvar = "grp", plot_layout = "columns")
#'
#' # Boxplots only (no violin) and no outlier dots
#' table1viz(df, name_groupvar = "grp", continuous_plot = "boxplot", show_outliers = FALSE)
#'
#' # No vertical line between the Overall column and the groups
#' table1viz(df, name_groupvar = "grp", overall_divider = FALSE)
#'
#' # Hide the x-axis tick labels for a more compact table
#' table1viz(df, name_groupvar = "grp", show_axis = FALSE)
#'
#' # Thin lines between the rows within a variable
#' table1viz(df_miss, name_groupvar = "grp", row_lines = TRUE)
#'
#' # No thicker lines between variables
#' table1viz(df_miss, name_groupvar = "grp", variable_divider = FALSE)
#'
#' # Add a median (IQR) row for continuous variables
#' table1viz(
#'   df,
#'   name_groupvar = "grp",
#'   statistic = list(
#'     gtsummary::all_continuous() ~ c("{mean} ({sd})", "{median} ({p25}, {p75})", "{min}; {max}"),
#'     gtsummary::all_categorical() ~ "{n} ({p}%)"
#'   )
#' )
#'
#' # Add a 0-100% axis under the percentage bars, with custom ticks
#' table1viz(df, name_groupvar = "grp", bar_axis = TRUE)
#' table1viz(df, name_groupvar = "grp", bar_axis = TRUE, bar_axis_breaks = c(0, 25, 50, 75, 100))
#'
#' # Word: in a Quarto document with `format: docx`, table1viz() returns an
#' # editable Word table automatically. Force it elsewhere with:
#' if (requireNamespace("flextable", quietly = TRUE) &&
#'     requireNamespace("xml2", quietly = TRUE)) {
#'   table1viz(df, name_groupvar = "grp", output = "flextable")
#' }
#'
#' # Choose your own ticks for some variables (names become tick labels)
#' table1viz(
#'   df,
#'   name_groupvar = "grp",
#'   axis_breaks = list(
#'     age = c(0, 50, 100),
#'     los = c("0" = 0, "1 yr" = 365, "3 yrs" = 1095, "5 yrs" = 1825)
#'   )
#' )
#' @export
table1viz <- function(
    d,
    name_groupvar = NULL,
    overall = TRUE,
    group_header = '**Group**',
    stats_col_label = 'Overall',
    missing = c("ifany", "no", "always"),
    missing_group = c("exclude", "show"),
    show_denominator = c("ifmissing", "always", "no"),
    statistic = list(
        gtsummary::all_continuous() ~ c(
            '{mean} ({sd})',
            '{min}; {max}' # ";", not ",", since numbers use "," for thousands
        ),
        gtsummary::all_categorical() ~ c('{n} ({p}%)')
    ),
    digits = NULL,
    binary_01_only_show1 = TRUE,
    continuous_plot = c("violin_box", "boxplot", "density"),
    show_outliers = TRUE,
    show_axis = TRUE,
    axis_breaks = NULL,
    bar_axis = FALSE,
    bar_axis_breaks = c(0, 50, 100),
    colors = NULL,
    overall_color = '#7F7F7F',
    plot_width = 140,
    plot_height = 30,
    bar_width = 60,
    plot_layout = c("combined", "columns"),
    distribution_label = '**Distribution**',
    header_align = c("left", "center"),
    overall_divider = TRUE,
    variable_divider = TRUE,
    row_lines = FALSE,
    output = c("auto", "gt", "flextable")
) {
    missing <- match.arg(missing)
    missing_group <- match.arg(missing_group)
    show_denominator <- match.arg(show_denominator)
    header_align <- match.arg(header_align)
    continuous_plot <- match.arg(continuous_plot)
    plot_layout <- match.arg(plot_layout)
    output <- match.arg(output)
    if (output == 'auto') {
        output <- if (isTRUE(knitr::pandoc_to('docx'))) 'flextable' else 'gt'
    }
    if (output == 'flextable') {
        rlang::check_installed(
            c('flextable', 'xml2'),
            reason = 'to make a Word table with `table1viz()`.'
        )
    }
    stopifnot(is.data.frame(d))
    stopifnot(
        is.null(name_groupvar) ||
            (is.character(name_groupvar) &&
                length(name_groupvar) == 1 &&
                name_groupvar %in% colnames(d))
    )
    stopifnot(is.logical(overall) && length(overall) == 1)
    stopifnot(
        is.logical(overall_divider) &&
            length(overall_divider) == 1 &&
            !is.na(overall_divider)
    )
    stopifnot(
        is.logical(variable_divider) &&
            length(variable_divider) == 1 &&
            !is.na(variable_divider)
    )
    stopifnot(is.logical(row_lines) && length(row_lines) == 1 && !is.na(row_lines))
    stopifnot(is.list(statistic), is.null(digits) || is.list(digits))
    is_datetime <- vapply(
        d,
        function(x) inherits(x, c('Date', 'POSIXt', 'difftime')),
        logical(1)
    )
    if (any(is_datetime)) {
        stop(
            'Date/time columns are not supported: ',
            paste0('`', names(d)[is_datetime], '`', collapse = ', '),
            '. Convert them to numbers (e.g. days or years) first.',
            call. = FALSE
        )
    }
    stopifnot(
        is.logical(binary_01_only_show1) && length(binary_01_only_show1) == 1
    )
    stopifnot(
        is.numeric(plot_width),
        is.numeric(plot_height),
        is.numeric(bar_width)
    )
    stopifnot(
        is.logical(show_outliers) &&
            length(show_outliers) == 1 &&
            !is.na(show_outliers)
    )
    stopifnot(is.logical(show_axis) && length(show_axis) == 1 && !is.na(show_axis))
    axis_h <- if (show_axis) 12 else 0
    check_axis_breaks(axis_breaks, d)
    stopifnot(is.logical(bar_axis) && length(bar_axis) == 1 && !is.na(bar_axis))
    stopifnot(
        is.numeric(bar_axis_breaks),
        length(bar_axis_breaks) > 0,
        all(is.finite(bar_axis_breaks)),
        all(bar_axis_breaks >= 0 & bar_axis_breaks <= 100)
    )
    bar_axis_h <- if (bar_axis) 12 else 0
    if (is.null(names(bar_axis_breaks))) {
        names(bar_axis_breaks) <- paste0(
            format(bar_axis_breaks, trim = TRUE, drop0trailing = TRUE),
            '%'
        )
    }
    if (!is.null(axis_breaks) && !show_axis) {
        warning('`axis_breaks` is ignored because `show_axis = FALSE`.', call. = FALSE)
        axis_breaks <- NULL
    }

    # Use the package's gtsummary theme for this call only, then restore the
    # caller's theme so later gtsummary tables are unaffected.
    old_theme <- gtsummary::get_gtsummary_theme()
    on.exit(
        suppressMessages({
            gtsummary::reset_gtsummary_theme()
            gtsummary::set_gtsummary_theme(old_theme)
        }),
        add = TRUE
    )
    new_theme <- c(
        gtsummary::theme_gtsummary_language("en", big.mark = ",", set_theme = FALSE),
        gtsummary::theme_gtsummary_compact(set_theme = FALSE)
    )
    new_theme <- new_theme[names(new_theme) != 'pkgwide-str:theme_name']
    # Start from a clean theme so the caller's theme (e.g. percent formatting)
    # can't change the numbers shown; on.exit() restores it afterwards.
    suppressMessages(gtsummary::reset_gtsummary_theme())
    gtsummary::set_gtsummary_theme(new_theme)

    type <- if (binary_01_only_show1) {
        list(gtsummary::all_continuous() ~ 'continuous2')
    } else {
        list(
            gtsummary::all_continuous() ~ 'continuous2',
            gtsummary::all_dichotomous() ~ 'categorical'
        )
    }
    fmt_n <- function(n) format(n, big.mark = ",", scientific = FALSE)

    # ---- column subsets (stat_0 = overall, stat_k = k-th group level) ----
    grouped <- !is.null(name_groupvar)
    combined <- grouped && plot_layout == 'combined'
    n_group_dropped <- 0L
    if (grouped) {
        g_raw <- d[[name_groupvar]]
        is_na_g <- is.na(g_raw)
        real_lev <- levels(droplevels(as.factor(g_raw[!is_na_g])))
        group_colors <- resolve_group_colors(colors, real_lev)
        if (missing_group == 'show' && any(is_na_g)) {
            # keep rows with a missing group as their own "Missing" group
            miss_lab <- if ('Missing' %in% real_lev) '(Missing)' else 'Missing'
            g <- factor(
                ifelse(is_na_g, miss_lab, as.character(g_raw)),
                levels = c(real_lev, miss_lab)
            )
            miss_col <- if (!is.null(names(colors)) && miss_lab %in% names(colors)) {
                colors[[miss_lab]]
            } else {
                '#8C564B' # not in the Okabe-Ito palette or overall grey
            }
            group_colors <- c(group_colors, miss_col)
        } else {
            n_group_dropped <- sum(is_na_g)
            d <- d[!is_na_g, , drop = FALSE]
            g <- droplevels(as.factor(g_raw[!is_na_g]))
        }
        d[[name_groupvar]] <- g
        lev <- levels(g)
        col_rows <- stats::setNames(
            lapply(lev, function(l) which(g == l)),
            paste0('stat_', seq_along(lev))
        )
        col_colors <- stats::setNames(group_colors, names(col_rows))
        if (overall) {
            col_rows <- c(list(stat_0 = seq_len(nrow(d))), col_rows)
            col_colors <- c(stat_0 = overall_color, col_colors)
        }
        col_names <- stats::setNames(
            c(if (overall) stats_col_label, lev),
            names(col_rows)
        )
    } else {
        col_rows <- list(stat_0 = seq_len(nrow(d)))
        col_colors <- c(stat_0 = overall_color)
        col_names <- c(stat_0 = stats_col_label)
    }

    # ---- gtsummary table ----
    make_tbl <- function(statistic, digits) {
        gtsummary::tbl_summary(
            data = d,
            by = if (grouped) dplyr::all_of(name_groupvar) else NULL,
            type = type,
            missing = missing,
            missing_text = 'N missing',
            missing_stat = if (show_denominator == 'no') {
                '{N_miss} ({p_miss}%)'
            } else {
                '{N_miss} / {N_obs} ({p_miss}%)'
            },
            statistic = statistic,
            digits = digits,
            percent = 'column'
        )
    }
    tbl <- make_tbl(statistic, digits)
    # Show the denominator as "n / N (%)" for categorical variables that use
    # the default "{n} ({p}%)" (custom statistics are left as given).
    denom_vars <- character(0)
    if (show_denominator != 'no') {
        tb0 <- tbl$table_body
        cat_v <- unique(tb0$variable[tb0$var_type %in% c('categorical', 'dichotomous')])
        is_default <- vapply(
            cat_v,
            function(v) identical(as.character(tbl$inputs$statistic[[v]]), '{n} ({p}%)'),
            logical(1)
        )
        denom_vars <- cat_v[is_default]
        if (show_denominator == 'ifmissing') {
            denom_vars <- denom_vars[vapply(
                denom_vars,
                function(v) anyNA(d[[v]]),
                logical(1)
            )]
        }
        if (length(denom_vars) > 0) {
            # gtsummary matches unnamed `digits` to a statistic's slots by
            # position, so adding {N} would shift them (c(0, 2) would format
            # N, not p). Reuse each variable's digits as resolved by name in
            # the first table, and format N like the integer N_obs. (Formulas,
            # since gtsummary ignores named entries in a list with formulas.)
            denom_digits <- lapply(denom_vars, function(v) {
                dv <- tbl$inputs$digits[[v]]
                dv$N <- dv$N_obs
                dplyr::all_of(v) ~ dv
            })
            tbl <- make_tbl(
                c(
                    statistic,
                    list(dplyr::all_of(denom_vars) ~ '{n} / {N} ({p}%)')
                ),
                c(digits, denom_digits)
            )
        }
    }
    if (grouped && overall) {
        tbl <- gtsummary::add_overall(tbl, last = FALSE)
    }

    headers <- vapply(
        names(col_rows),
        function(cc) {
            swatch <- if (grouped) {
                sprintf(
                    '<span style="color:%s">&#9632;</span> ',
                    col_colors[[cc]]
                )
            } else {
                ''
            }
            sprintf(
                '%s**%s**<br>N = %s',
                swatch,
                col_names[[cc]],
                fmt_n(length(col_rows[[cc]]))
            )
        },
        character(1)
    )
    tbl <- do.call(
        gtsummary::modify_header,
        c(list(tbl, label = '**Variable**'), as.list(headers))
    ) |>
        gtsummary::modify_footnote(gtsummary::everything() ~ NA) |>
        gtsummary::bold_labels()
    if (grouped) {
        group_cols <- setdiff(names(col_rows), 'stat_0')
        tbl <- gtsummary::modify_spanning_header(
            tbl,
            dplyr::all_of(group_cols) ~ group_header
        )
    }

    # ---- build inline HTML for each cell ----
    tb <- tbl$table_body
    # Raw SVG per cell; bar cells also keep their n (%) text next to the bar
    cell_svg <- lapply(names(col_rows), function(cc) {
        rep(NA_character_, nrow(tb))
    })
    names(cell_svg) <- names(col_rows)
    cell_bar <- lapply(cell_svg, function(z) rep(FALSE, length(z)))
    dist_svg <- rep(NA_character_, nrow(tb))
    span_rows <- integer(0) # distribution cells covered by a plot above
    span_start <- integer(0) # distribution cells holding a spanning plot
    span_px <- rep(NA_real_, nrow(tb)) # gt: row height of a spanned row
    span_groups <- list() # rows (first = plot row) spanned by each plot
    span_row_min_px <- 24 # > height of a one-line text row in gt's compact theme
    lane_height <- max(10, round(plot_height * 0.6))

    for (v in unique(tb$variable)) {
        idx <- which(tb$variable == v)
        var_type <- tb$var_type[idx[1]]
        x_all <- d[[v]]

        # "N missing" row: hatched bars of % missing (of all obs in column)
        miss_row <- idx[tb$row_type[idx] == 'missing']
        if (length(miss_row) == 1) {
            p_miss <- vapply(
                col_rows,
                function(r) if (length(r) == 0) 0 else mean(is.na(x_all[r])),
                numeric(1)
            )
            if (combined) {
                dist_svg[miss_row] <- svg_bars_combined(
                    p_miss,
                    colors = col_colors,
                    lane_names = col_names,
                    width = plot_width,
                    title = 'Missing',
                    hatched = TRUE,
                    axis_height = bar_axis_h,
                    axis_breaks = bar_axis_breaks
                )
            } else {
                for (cc in names(col_rows)) {
                    cell_svg[[cc]][miss_row] <- svg_bar(
                        p_miss[[cc]],
                        color = col_colors[[cc]],
                        width = bar_width,
                        hatched = TRUE,
                        axis_height = bar_axis_h,
                        axis_breaks = bar_axis_breaks
                    )
                    cell_bar[[cc]][miss_row] <- TRUE
                }
            }
        }
        idx <- setdiff(idx, miss_row) # rows a continuous plot may span

        if (var_type %in% c('continuous', 'continuous2')) {
            x_num <- as.numeric(x_all)
            label_row <- idx[tb$row_type[idx] == 'label']
            if (length(label_row) == 0 || all(is.na(x_num))) {
                next
            }
            breaks <- axis_breaks[[v]]
            xlim <- range(c(x_num, breaks), na.rm = TRUE)
            dens <- lapply(col_rows, function(r) {
                compute_density(
                    x_num[r],
                    xlim,
                    trim = continuous_plot == 'violin_box'
                )
            })
            ymax <- max(vapply(
                dens,
                function(z) if (is.null(z)) 0 else max(z$y),
                numeric(1)
            ))
            if (combined) {
                # The plot spans all rows of this variable (label + statistic
                # rows). flextable merges those cells. gt has no rowspan, so
                # the plot sits in the label row and overflows downward over
                # fixed-height spacers below it. Spacers are taller than a
                # text row and their cells have no vertical padding, so they
                # (not the text) set the row heights and the plot can fill
                # the span exactly.
                k <- length(col_rows)
                n_r <- length(idx)
                row_px <- max(
                    span_row_min_px,
                    ceiling((k * lane_height + axis_h) / n_r)
                )
                lane_h <- if (output == 'gt') {
                    (n_r * row_px - axis_h) / k
                } else {
                    lane_height
                }
                dist_svg[label_row] <- svg_continuous_combined(
                    dens,
                    xs = lapply(col_rows, function(r) x_num[r]),
                    xlim = xlim,
                    ymax = ymax,
                    colors = col_colors,
                    lane_names = col_names,
                    width = plot_width,
                    lane_height = lane_h,
                    title = sprintf('Distribution of %s', tb$label[label_row]),
                    type = continuous_plot,
                    show_outliers = show_outliers,
                    axis_height = axis_h,
                    axis_breaks = breaks
                )
                other <- setdiff(idx, label_row)
                span_px[idx] <- row_px
                span_rows <- c(span_rows, other)
                span_start <- c(span_start, label_row)
                span_groups <- c(span_groups, list(c(label_row, other)))
                next
            }
            for (cc in names(col_rows)) {
                cell_svg[[cc]][label_row] <- svg_continuous(
                    dens[[cc]],
                    x = x_num[col_rows[[cc]]],
                    xlim = xlim,
                    ymax = ymax,
                    color = col_colors[[cc]],
                    width = plot_width,
                    height = plot_height,
                    title = sprintf(
                        'Distribution of %s (%s)',
                        tb$label[label_row],
                        col_names[[cc]]
                    ),
                    type = continuous_plot,
                    show_outliers = show_outliers,
                    axis_height = axis_h,
                    axis_breaks = breaks
                )
            }
        } else if (var_type %in% c('categorical', 'dichotomous')) {
            rows <- if (var_type == 'categorical') {
                idx[tb$row_type[idx] == 'level']
            } else {
                idx[tb$row_type[idx] == 'label']
            }
            values <- if (var_type == 'categorical') {
                tb$label[rows]
            } else {
                dichotomous_value(tbl, v)
            }
            x_chr <- as.character(x_all)
            props <- vapply(
                names(col_rows),
                function(cc) {
                    xc <- x_chr[col_rows[[cc]]]
                    xc <- xc[!is.na(xc)]
                    vapply(
                        values,
                        function(val) {
                            if (length(xc) == 0) 0 else mean(xc == val)
                        },
                        numeric(1)
                    )
                },
                numeric(length(values))
            )
            props <- matrix(props, nrow = length(values)) # rows = levels
            # one bar axis per variable, under its last bar
            axis_row <- if (length(miss_row) == 1) 0L else length(rows)
            if (combined) {
                for (j in seq_along(rows)) {
                    dist_svg[rows[j]] <- svg_bars_combined(
                        props[j, ],
                        colors = col_colors,
                        lane_names = col_names,
                        width = plot_width,
                        title = tb$label[rows[j]],
                        axis_height = if (j == axis_row) bar_axis_h else 0,
                        axis_breaks = bar_axis_breaks
                    )
                }
                next
            }
            for (k in seq_along(col_rows)) {
                cc <- names(col_rows)[k]
                for (j in seq_along(rows)) {
                    cell_svg[[cc]][rows[j]] <- svg_bar(
                        props[j, k],
                        color = col_colors[[cc]],
                        width = bar_width,
                        axis_height = if (j == axis_row) bar_axis_h else 0,
                        axis_breaks = bar_axis_breaks
                    )
                    cell_bar[[cc]][rows[j]] <- TRUE
                }
            }
        }
    }

    # ---- notes on denominators / excluded rows ----
    vars <- unique(tb$variable)
    cat_vars <- unique(tb$variable[tb$var_type %in% c('categorical', 'dichotomous')])
    any_miss_cat <- any(vapply(cat_vars, function(v) anyNA(d[[v]]), logical(1)))
    any_miss_row <- any(tb$row_type == 'missing')
    note <- c(
        if (any_miss_cat) {
            if (any(vapply(denom_vars, function(v) anyNA(d[[v]]), logical(1)))) {
                'Category percentages are n / N, where N excludes missing values.'
            } else {
                'Category percentages exclude missing values.'
            }
        },
        if (any_miss_row) '% missing is out of all observations in the column.',
        if (n_group_dropped > 0) {
            sprintf(
                '%s observation%s with missing %s excluded.',
                fmt_n(n_group_dropped),
                if (n_group_dropped == 1) '' else 's',
                name_groupvar
            )
        }
    )

    divider <- overall_divider && grouped && overall
    # first row of each variable after the first
    var_start <- if (variable_divider) {
        which(tb$row_type == 'label')[-1]
    } else {
        integer(0)
    }
    if (output == 'flextable') {
        return(table1viz_flextable(
            tbl,
            cell_svg = cell_svg,
            cell_bar = cell_bar,
            dist_svg = dist_svg,
            span_groups = span_groups,
            combined = combined,
            grouped = grouped,
            col_rows = col_rows,
            col_colors = col_colors,
            col_names = col_names,
            plot_width = plot_width,
            bar_width = bar_width,
            header_align = header_align,
            overall_divider = divider,
            var_start = var_start,
            group_header = group_header,
            distribution_label = distribution_label,
            note = note
        ))
    }

    # ---- render to gt and swap in HTML ----
    html <- lapply(names(cell_svg), function(cc) {
        ifelse(
            cell_bar[[cc]],
            html_bar_cell(tb[[cc]], cell_svg[[cc]]),
            cell_svg[[cc]]
        )
    })
    names(html) <- names(cell_svg)
    dist_html <- dist_svg
    dist_html[span_start] <- sprintf(
        paste0(
            '<div style="position:relative;height:%dpx">',
            '<div style="position:absolute;top:0;left:0">%s</div></div>'
        ),
        span_px[span_start], dist_svg[span_start]
    )
    dist_html[span_rows] <- sprintf(
        '<div style="height:%dpx"></div>',
        span_px[span_rows]
    )
    gt_tbl <- gtsummary::as_gt(tbl)
    for (cc in names(html)) {
        idx <- which(!is.na(html[[cc]]))
        if (length(idx) == 0) {
            next
        }
        gt_tbl <- local({
            vals <- html[[cc]][idx]
            gt::text_transform(
                gt_tbl,
                locations = gt::cells_body(
                    columns = dplyr::all_of(cc),
                    rows = idx
                ),
                fn = function(x) vals
            )
        })
    }
    if (combined) {
        dist_idx <- which(!is.na(dist_html))
        gt_tbl <- gt::cols_add(gt_tbl, distribution = '') |>
            gt::cols_label(distribution = gt::md(distribution_label)) |>
            gt::cols_align('left', columns = 'distribution')
        if (length(dist_idx) > 0) {
            dist_vals <- dist_html[dist_idx]
            gt_tbl <- gt::text_transform(
                gt_tbl,
                locations = gt::cells_body(
                    columns = 'distribution',
                    rows = dist_idx
                ),
                fn = function(x) dist_vals
            )
        }
        if (length(span_start) > 0) {
            # Note: a later character-string tab_style() replaces an earlier
            # one on the same cell, so each cell gets a single CSS string.
            # Keep text on one line so spacer heights (not wrapped text) set
            # the spanned rows' heights; narrow containers scroll instead.
            gt_tbl <- gt::tab_style(
                gt_tbl,
                style = 'white-space: nowrap;',
                locations = gt::cells_body()
            )
            no_pad <- 'white-space: nowrap; padding-top: 0; padding-bottom: 0;'
            # gt clips cell overflow; let the spanning plot extend downward
            gt_tbl <- gt::tab_style(
                gt_tbl,
                style = paste(no_pad, 'overflow: visible;'),
                locations = gt::cells_body(columns = 'distribution', rows = span_start)
            )
            if (length(span_rows) > 0) {
                gt_tbl <- gt::tab_style(
                    gt_tbl,
                    style = no_pad,
                    locations = gt::cells_body(columns = 'distribution', rows = span_rows)
                )
            }
        }
        if (length(span_rows) > 0) {
            # hide row borders crossing a spanning plot
            gt_tbl <- gt::tab_style(
                gt_tbl,
                style = gt::cell_borders(sides = 'top', style = 'hidden'),
                locations = gt::cells_body(columns = 'distribution', rows = span_rows)
            )
        }
    }
    if (length(note) > 0) {
        gt_tbl <- gt::tab_source_note(gt_tbl, paste(note, collapse = ' '))
    }

    gt_tbl <- gt_tbl |>
        gt::cols_align('left', columns = dplyr::all_of(names(col_rows))) |>
        # darker line under the column labels only; gt's option for it also
        # colors the line under the spanner ("Group"), which stays light
        gt::tab_options(table_body.border.top.color = header_line_color) |>
        gt::tab_style(
            style = gt::cell_borders(
                sides = 'bottom',
                color = header_line_color,
                weight = gt::px(2)
            ),
            locations = gt::cells_column_labels()
        ) |>
        gt::tab_style(
            style = gt::cell_text(v_align = 'middle'),
            locations = gt::cells_body()
        ) |>
        gt::tab_style(
            style = gt::cell_text(align = header_align),
            locations = gt::cells_column_labels(
                columns = dplyr::any_of(c(names(col_rows), 'distribution'))
            )
        )

    if (divider) {
        gt_tbl <- gt::tab_style(
            gt_tbl,
            style = gt::cell_borders(
                sides = 'right',
                color = divider_color,
                weight = gt::px(2)
            ),
            locations = list(
                gt::cells_body(columns = 'stat_0'),
                gt::cells_column_labels(columns = 'stat_0')
            )
        )
    }

    if (length(var_start) > 0) {
        gt_tbl <- gt::tab_style(
            gt_tbl,
            style = gt::cell_borders(
                sides = 'top',
                color = divider_color,
                weight = gt::px(2)
            ),
            locations = gt::cells_body(rows = var_start)
        )
    }

    if (!row_lines) {
        # every row except a variable's first (label) row
        within <- which(tb$row_type != 'label')
        if (length(within) > 0) {
            gt_tbl <- gt::tab_style(
                gt_tbl,
                style = gt::cell_borders(sides = 'top', style = 'hidden'),
                locations = gt::cells_body(rows = within)
            )
        }
    }

    return(gt_tbl)
}

# Lines between variables and right of the Overall column (gt's light grey)
divider_color <- '#D3D3D3'
# Line under the column labels: a bit darker so the header stands out
header_line_color <- '#9E9E9E'

# ---- internal helpers -------------------------------------------------------

check_axis_breaks <- function(axis_breaks, d) {
    if (is.null(axis_breaks)) {
        return(invisible())
    }
    if (!is.list(axis_breaks) || is.null(names(axis_breaks)) || any(names(axis_breaks) == '')) {
        stop('`axis_breaks` must be a named list, e.g. list(age = c(20, 40, 60)).', call. = FALSE)
    }
    unknown <- setdiff(names(axis_breaks), colnames(d))
    if (length(unknown) > 0) {
        stop('`axis_breaks` names not found in `d`: ', paste(unknown, collapse = ', '), call. = FALSE)
    }
    ok <- vapply(axis_breaks, function(b) {
        is.numeric(b) && length(b) > 0 && all(is.finite(b))
    }, logical(1))
    if (!all(ok)) {
        stop('`axis_breaks` must contain non-empty finite numeric vectors (bad: ',
             paste(names(axis_breaks)[!ok], collapse = ', '), ').', call. = FALSE)
    }
    invisible()
}

resolve_group_colors <- function(colors, lev) {
    if (is.null(colors)) {
        okabe_ito <- c(
            '#0072B2',
            '#E69F00',
            '#009E73',
            '#CC79A7',
            '#56B4E9',
            '#D55E00',
            '#F0E442',
            '#000000'
        )
        return(rep_len(okabe_ito, length(lev)))
    }
    if (!is.null(names(colors))) {
        if (!all(lev %in% names(colors))) {
            stop(
                '`colors` is named but is missing group level(s): ',
                paste(setdiff(lev, names(colors)), collapse = ', ')
            )
        }
        return(unname(colors[lev]))
    }
    if (length(colors) < length(lev)) {
        stop(
            '`colors` must have at least one color per group (',
            length(lev),
            ').'
        )
    }
    unname(colors[seq_along(lev)])
}

# Kernel density of x on the shared axis range xlim. With trim = TRUE (used
# for violins, like ggplot2::geom_violin's default) the density is evaluated
# only over x's own range, so no violin extends past its group's min/max.
compute_density <- function(x, xlim, trim = FALSE) {
    x <- x[!is.na(x)]
    if (length(x) < 2 || diff(xlim) == 0 || stats::sd(x) == 0) {
        return(NULL)
    }
    from <- xlim[1]
    to <- xlim[2]
    if (trim) {
        from <- max(from, min(x))
        to <- min(to, max(x))
    }
    dd <- stats::density(x, from = from, to = to, n = 128)
    list(x = dd$x, y = dd$y)
}

# Body of one horizontal "lane" (band from y0 to y0 + height) of a
# continuous-variable graphic. All lanes/cells share xlim and ymax.
svg_lane <- function(
    dens,
    x,
    xlim,
    ymax,
    color,
    width,
    y0,
    height,
    type = c('violin_box', 'boxplot', 'density'),
    show_outliers = TRUE
) {
    type <- match.arg(type)
    pad <- 2
    x <- x[!is.na(x)]
    sx <- function(v) {
        if (diff(xlim) == 0) {
            return(rep(width / 2, length(v)))
        }
        pad + (v - xlim[1]) / diff(xlim) * (width - 2 * pad)
    }
    body <- ''
    if (type == 'density') {
        base_y <- y0 + height - pad
        body <- sprintf(
            '<line x1="%.1f" y1="%.1f" x2="%.1f" y2="%.1f" stroke="#BDBDBD" stroke-width="1"/>',
            pad,
            base_y,
            width - pad,
            base_y
        )
        if (!is.null(dens) && ymax > 0) {
            px <- sx(dens$x)
            py <- base_y - dens$y / ymax * (height - 2 * pad)
            body <- paste0(
                body,
                sprintf(
                    '<path d="M%.1f,%.1f L%s L%.1f,%.1f Z" fill="%s" fill-opacity="0.3" stroke="%s" stroke-width="1.2"/>',
                    px[1],
                    base_y,
                    paste(sprintf('%.1f,%.1f', px, py), collapse = ' L'),
                    px[length(px)],
                    base_y,
                    color,
                    color
                )
            )
        }
        if (length(x) > 0) {
            m <- sx(stats::median(x))
            body <- paste0(
                body,
                sprintf(
                    '<line x1="%.1f" y1="%.1f" x2="%.1f" y2="%.1f" stroke="%s" stroke-width="1.5"/>',
                    m,
                    y0 + pad,
                    m,
                    base_y,
                    color
                )
            )
        }
        return(body)
    }
    mid <- y0 + height / 2
    half <- height / 2 - pad
    # violin: density mirrored around the center line (shared width scale)
    if (type == 'violin_box' && !is.null(dens) && ymax > 0) {
        px <- sx(dens$x)
        off <- dens$y / ymax * half
        top <- sprintf('%.1f,%.1f', px, mid - off)
        bot <- sprintf('%.1f,%.1f', rev(px), mid + rev(off))
        body <- paste0(
            body,
            sprintf(
                '<path d="M%s Z" fill="%s" fill-opacity="0.25" stroke="%s" stroke-opacity="0.6" stroke-width="0.8"/>',
                paste(c(top, bot), collapse = ' L'),
                color,
                color
            )
        )
    }
    if (length(x) > 0) {
        # type = 2 matches gtsummary's {p25}/{median}/{p75}
        q <- stats::quantile(x, c(0.25, 0.5, 0.75), names = FALSE, type = 2)
        iqr <- q[3] - q[1]
        lo_fence <- q[1] - 1.5 * iqr
        hi_fence <- q[3] + 1.5 * iqr
        w_lo <- min(x[x >= lo_fence])
        w_hi <- max(x[x <= hi_fence])
        outl <- x[x < lo_fence | x > hi_fence]
        box_half <- if (type == 'violin_box') half * 0.35 else half * 0.6
        fill <- if (type == 'violin_box') '#FFFFFF' else color
        fill_op <- if (type == 'violin_box') 1 else 0.3
        body <- paste0(
            body,
            # whiskers
            sprintf(
                '<line x1="%.1f" y1="%.1f" x2="%.1f" y2="%.1f" stroke="%s" stroke-width="1"/>',
                sx(w_lo),
                mid,
                sx(q[1]),
                mid,
                color
            ),
            sprintf(
                '<line x1="%.1f" y1="%.1f" x2="%.1f" y2="%.1f" stroke="%s" stroke-width="1"/>',
                sx(q[3]),
                mid,
                sx(w_hi),
                mid,
                color
            ),
            # box
            sprintf(
                '<rect x="%.1f" y="%.1f" width="%.1f" height="%.1f" fill="%s" fill-opacity="%s" stroke="%s" stroke-width="1"/>',
                sx(q[1]),
                mid - box_half,
                max(sx(q[3]) - sx(q[1]), 0.5),
                2 * box_half,
                fill,
                fill_op,
                color
            ),
            # median
            sprintf(
                '<line x1="%.1f" y1="%.1f" x2="%.1f" y2="%.1f" stroke="%s" stroke-width="2"/>',
                sx(q[2]),
                mid - box_half,
                sx(q[2]),
                mid + box_half,
                color
            )
        )
        if (show_outliers && length(outl) > 0) {
            # one dot per pixel position keeps the SVG small for large n
            ox <- unique(round(sx(outl)))
            body <- paste0(
                body,
                paste(
                    sprintf(
                        '<circle cx="%.0f" cy="%.1f" r="1.4" fill="%s" fill-opacity="0.6"/>',
                        ox,
                        mid,
                        color
                    ),
                    collapse = ''
                )
            )
        }
    }
    body
}

svg_wrap <- function(body, width, height, title) {
    sprintf(
        '<svg width="%s" height="%s" viewBox="0 0 %s %s" role="img" style="display:block"><title>%s</title>%s</svg>',
        width,
        height,
        width,
        height,
        htmltools::htmlEscape(title),
        body
    )
}

# One graphic per table cell.
svg_continuous <- function(
    dens,
    x,
    xlim,
    ymax,
    color,
    width,
    height,
    title,
    type = c('violin_box', 'boxplot', 'density'),
    show_outliers = TRUE,
    axis_height = 0,
    axis_breaks = NULL
) {
    svg_wrap(
        paste0(
            svg_lane(
            dens,
            x,
            xlim,
            ymax,
            color,
            width,
            y0 = 0,
            height = height,
            type = type,
            show_outliers = show_outliers
        ),
            svg_axis(xlim, width, y0 = height, height = axis_height,
                breaks = axis_breaks)
        ),
        width,
        height + axis_height,
        title
    )
}

# One graphic with a stacked lane per column (overall + groups), shared axis.
svg_continuous_combined <- function(
    dens,
    xs,
    xlim,
    ymax,
    colors,
    lane_names,
    width,
    lane_height,
    title,
    type = c('violin_box', 'boxplot', 'density'),
    show_outliers = TRUE,
    axis_height = 0,
    axis_breaks = NULL
) {
    k <- length(xs)
    lanes <- vapply(
        seq_len(k),
        function(i) {
            sprintf(
                '<g><title>%s</title>%s</g>',
                htmltools::htmlEscape(lane_names[[i]]),
                svg_lane(
                    dens[[i]],
                    xs[[i]],
                    xlim,
                    ymax,
                    colors[[i]],
                    width,
                    y0 = (i - 1) * lane_height,
                    height = lane_height,
                    type = type,
                    show_outliers = show_outliers
                )
            )
        },
        character(1)
    )
    svg_wrap(
        paste0(
            paste(lanes, collapse = ''),
            svg_axis(xlim, width, y0 = k * lane_height, height = axis_height,
                breaks = axis_breaks)
        ),
        width,
        k * lane_height + axis_height,
        title
    )
}

# Small x-axis (baseline, ticks, labels) in the band y0..y0 + height.
# Uses the same x mapping as svg_lane(). Empty when height is 0.
# drop_overlapping: with user `breaks`, keep every tick but hide labels that
# would overlap (end labels kept first, then interior ones that fit).
svg_axis <- function(
    xlim, width, y0, height, pad = 2, font_size = 9, breaks = NULL,
    drop_overlapping = FALSE
) {
    if (height <= 0 || !all(is.finite(xlim))) {
        return('')
    }
    sx <- function(v) {
        if (diff(xlim) == 0) {
            return(rep(width / 2, length(v)))
        }
        pad + (v - xlim[1]) / diff(xlim) * (width - 2 * pad)
    }
    fmt <- function(v) {
        m <- max(abs(v))
        # abbreviate large numbers (e.g. 2,500,000 -> 2.5M) to save space
        div <- if (m >= 1e9) 1e9 else if (m >= 1e6) 1e6 else if (m >= 1e4) 1e3 else 1
        suffix <- c('1' = '', '1000' = 'k', '1e+06' = 'M', '1e+09' = 'B')[[as.character(div)]]
        paste0(
            format(v / div, big.mark = ',', scientific = FALSE, trim = TRUE, drop0trailing = TRUE),
            suffix
        )
    }
    # horizontal extent of each label (approximate glyph width), with
    # edge labels anchored inward so they stay inside the graphic
    layout_ticks <- function(ticks, labs = fmt(ticks)) {
        px <- sx(ticks)
        w <- nchar(labs) * font_size * 0.6
        anchor <- ifelse(px < 12, 'start', ifelse(px > width - 12, 'end', 'middle'))
        left <- ifelse(anchor == 'start', px, ifelse(anchor == 'end', px - w, px - w / 2))
        list(px = px, labs = labs, anchor = anchor, left = left, right = left + w)
    }
    fits <- function(lt) {
        n <- length(lt$px)
        n < 2 || all(lt$left[-1] - lt$right[-n] >= 4)
    }
    if (!is.null(breaks)) {
        # user-supplied ticks: shown as given (names become labels)
        lt <- layout_ticks(
            unname(breaks),
            labs = if (is.null(names(breaks))) fmt(unname(breaks)) else names(breaks)
        )
        if (drop_overlapping && !fits(lt)) {
            # keep the end labels, then interior labels that fit between
            ord <- order(lt$px)
            first <- ord[1]
            last <- ord[length(ord)]
            gap_ok <- function(a, b) lt$left[b] - lt$right[a] >= 4
            keep_last <- gap_ok(first, last)
            keep <- first
            for (i in ord[-c(1, length(ord))]) {
                if (gap_ok(keep[length(keep)], i) && (!keep_last || gap_ok(i, last))) {
                    keep <- c(keep, i)
                }
            }
            if (keep_last) {
                keep <- c(keep, last)
            }
            lt$labs[-keep] <- ''
        }
    } else if (diff(xlim) == 0) {
        lt <- layout_ticks(xlim[1])
    } else {
        lt <- NULL
        # most ticks (from pretty()) whose labels don't overlap
        for (n in c(5, 4, 3, 2, 1)) {
            ticks <- pretty(xlim, n = n)
            ticks <- ticks[ticks >= xlim[1] & ticks <= xlim[2]]
            if (length(ticks) >= 2) {
                cand <- layout_ticks(ticks)
                if (fits(cand)) {
                    lt <- cand
                    break
                }
            }
        }
        if (is.null(lt)) {
            lt <- layout_ticks(signif(mean(xlim), 3))
        }
    }
    px <- lt$px
    labs <- lt$labs
    anchor <- lt$anchor
    col <- '#6B6B6B'
    paste0(
        sprintf(
            '<line x1="%.1f" y1="%.1f" x2="%.1f" y2="%.1f" stroke="%s" stroke-width="0.6"/>',
            pad, y0 + 0.5, width - pad, y0 + 0.5, col
        ),
        paste(
            sprintf(
                '<line x1="%.1f" y1="%.1f" x2="%.1f" y2="%.1f" stroke="%s" stroke-width="0.6"/><text x="%.1f" y="%.1f" font-size="%s" fill="%s" text-anchor="%s">%s</text>',
                px, y0 + 0.5, px, y0 + 3, col,
                px, y0 + height - 1, font_size, col, anchor,
                htmltools::htmlEscape(labs)
            ),
            collapse = ''
        )
    )
}

# Side-by-side (stacked horizontal) bars, one per column, shared 0-100% scale.
# <defs> for a diagonal hatch in `color`; returns list(defs, fill).
# Ids are derived from the color, so repeats on a page are identical.
hatch_pattern <- function(color) {
    id <- paste0('tv-hatch-', gsub('[^A-Za-z0-9]', '', color))
    list(
        defs = sprintf(
            paste0(
                '<defs><pattern id="%s" patternUnits="userSpaceOnUse" width="4" height="4" ',
                'patternTransform="rotate(45)"><rect width="1.6" height="4" fill="%s"/></pattern></defs>'
            ),
            id, color
        ),
        fill = sprintf('url(#%s)', id),
        # outline so small hatched bars stay visible
        stroke = sprintf(' stroke="%s" stroke-width="0.8"', color)
    )
}

svg_bars_combined <- function(
    p,
    colors,
    lane_names,
    width,
    title,
    bar_height = 6,
    gap = 1,
    hatched = FALSE,
    axis_height = 0,
    axis_breaks = NULL
) {
    p <- pmin(pmax(p, 0), 1)
    k <- length(p)
    defs <- ''
    fills <- unname(unlist(colors))
    strokes <- rep('', k)
    if (hatched) {
        pats <- lapply(fills, hatch_pattern)
        defs <- paste(unique(vapply(pats, `[[`, '', 'defs')), collapse = '')
        fills <- vapply(pats, `[[`, '', 'fill')
        strokes <- ifelse(p > 0, vapply(pats, `[[`, '', 'stroke'), '')
    }
    height <- k * bar_height + (k - 1) * gap
    bars <- vapply(
        seq_len(k),
        function(i) {
            y <- (i - 1) * (bar_height + gap)
            sprintf(
                paste0(
                    '<g><title>%s: %.1f%%</title>',
                    '<rect y="%.1f" width="%s" height="%s" fill="#E6E6E6"/>',
                        '<rect y="%.1f" width="%.1f" height="%s" fill="%s"%s/></g>'
                ),
                htmltools::htmlEscape(lane_names[[i]]),
                100 * p[i],
                y,
                width,
                bar_height,
                y,
                p[i] * width,
                bar_height,
                fills[[i]],
                strokes[[i]]
            )
        },
        character(1)
    )
    axis <- svg_axis(c(0, 100), width, y0 = height + 1, height = axis_height,
        pad = 0, breaks = axis_breaks, drop_overlapping = TRUE)
    if (axis_height > 0) {
        height <- height + 1 + axis_height
    }
    svg_wrap(paste0(defs, paste(bars, collapse = ''), axis), width, height, title)
}

# Percentage bar (0-100%) for a single table cell.
svg_bar <- function(
    p, color, width, height = 10, hatched = FALSE,
    axis_height = 0, axis_breaks = NULL
) {
    p <- min(max(p, 0), 1)
    defs <- ''
    fill <- color
    stroke <- ''
    if (hatched) {
        pat <- hatch_pattern(color)
        defs <- pat$defs
        fill <- pat$fill
        if (p > 0) stroke <- pat$stroke
    }
    sprintf(
        paste0(
            '<svg width="%s" height="%s" style="flex:none">%s',
            '<rect width="%s" height="%s" fill="#E6E6E6"/>',
            '<rect width="%.1f" height="%s" fill="%s"%s/>%s</svg>'
        ),
        width,
        if (axis_height > 0) height + 1 + axis_height else height,
        defs,
        width,
        height,
        p * width,
        height,
        fill,
        stroke,
        svg_axis(c(0, 100), width, y0 = height + 1, height = axis_height,
            pad = 0, breaks = axis_breaks, drop_overlapping = TRUE)
    )
}

# HTML cell: bar graphic followed by its n (%) text (vectorized).
html_bar_cell <- function(text, svg) {
    text[is.na(text)] <- ''
    sprintf(
        paste0(
            '<span style="display:inline-flex;align-items:center;gap:6px;white-space:nowrap">',
            '%s<span>%s</span></span>'
        ),
        svg,
        htmltools::htmlEscape(text)
    )
}

# Level displayed for a dichotomous variable, taken from gtsummary's ARD.
dichotomous_value <- function(tbl, v) {
    ard <- tbl$cards$tbl_summary
    hit <- which(ard$variable == v & ard$stat_name == 'n')
    if (length(hit) == 0) {
        return(NA_character_)
    }
    as.character(ard$variable_level[[hit[1]]])
}
