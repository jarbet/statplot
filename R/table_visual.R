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
#' * **Categorical** and **binary** variables: a percentage bar next to each
#'   `n (%)`.
#'
#' Percentages are column percentages (within each group), so bars are
#' comparable across groups. Category percentages exclude missing values; the
#' "N missing" row gives % missing out of all observations in the column. A
#' table note states this whenever it matters.
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
#'   variable: "ifany" (default), "no", or "always". The row shows `n (%)`
#'   missing, where % is out of all observations in the column, with a hatched
#'   bar on the same 0-100% scale as the category bars.
#' @param missing_group character(1). Rows with a missing `name_groupvar`:
#'   `"exclude"` (default) drops them and reports how many in a table note;
#'   `"show"` keeps them as an extra "Missing" group column (colored brown,
#'   `#8C564B`, unless `colors` is named and includes `"Missing"`).
#' @param statistic,digits Passed to [gtsummary::tbl_summary()]. Defaults match [table_overall()].
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
#'   Overall, group and Distribution columns: `"center"` (default) or `"left"`.
#' @param distribution_label character(1). Header of the combined
#'   distribution column.
#'
#' @references Furmaga J, et al. A new type of table one: showing instead of
#'   telling. *Journal of Clinical Epidemiology*. 2026;196.
#'
#' @return A `gt::gt_tbl` object. Graphics are inline SVG, so they render in
#'   HTML output (Quarto/R Markdown HTML, RStudio viewer, `gt::gtsave(".html")`).
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
#' table_visual(df)
#'
#' # By group: groups drawn together in one "Distribution" column for direct
#' # comparison
#' table_visual(df, name_groupvar = "grp")
#'
#' # Missing data, whole cohort: "N missing" rows show n (%) with hatched bars
#' table_visual(df_miss)
#'
#' # Missing data by group
#' table_visual(df_miss, name_groupvar = "grp")
#'
#' # ---- Other options ----
#'
#' # Separate graphic in each group's column instead of a Distribution column
#' table_visual(df, name_groupvar = "grp", plot_layout = "columns")
#'
#' # Missing data by group, with graphics in each group's column
#' table_visual(df_miss, name_groupvar = "grp", plot_layout = "columns")
#'
#' # Boxplots only (no violin) and no outlier dots
#' table_visual(df, name_groupvar = "grp", continuous_plot = "boxplot", show_outliers = FALSE)
#'
#' # Hide the x-axis tick labels for a more compact table
#' table_visual(df, name_groupvar = "grp", show_axis = FALSE)
#'
#' # Choose your own ticks for some variables (names become tick labels)
#' table_visual(
#'   df,
#'   name_groupvar = "grp",
#'   axis_breaks = list(
#'     age = c(0, 50, 100),
#'     los = c("0" = 0, "1 yr" = 365, "3 yrs" = 1095, "5 yrs" = 1825)
#'   )
#' )
#' @export
table_visual <- function(
    d,
    name_groupvar = NULL,
    overall = TRUE,
    group_header = '**Group**',
    stats_col_label = 'Overall',
    missing = c("ifany", "no", "always"),
    missing_group = c("exclude", "show"),
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
    binary_01_only_show1 = TRUE,
    continuous_plot = c("violin_box", "boxplot", "density"),
    show_outliers = TRUE,
    show_axis = TRUE,
    axis_breaks = NULL,
    colors = NULL,
    overall_color = '#7F7F7F',
    plot_width = 140,
    plot_height = 30,
    bar_width = 60,
    plot_layout = c("combined", "columns"),
    distribution_label = '**Distribution**',
    header_align = c("center", "left")
) {
    missing <- match.arg(missing)
    missing_group <- match.arg(missing_group)
    header_align <- match.arg(header_align)
    continuous_plot <- match.arg(continuous_plot)
    plot_layout <- match.arg(plot_layout)
    stopifnot(is.data.frame(d))
    stopifnot(
        is.null(name_groupvar) ||
            (is.character(name_groupvar) &&
                length(name_groupvar) == 1 &&
                name_groupvar %in% colnames(d))
    )
    stopifnot(is.logical(overall) && length(overall) == 1)
    stopifnot(is.list(statistic), is.list(digits))
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
    if (!is.null(axis_breaks) && !show_axis) {
        warning('`axis_breaks` is ignored because `show_axis = FALSE`.', call. = FALSE)
        axis_breaks <- NULL
    }

    gtsummary::theme_gtsummary_language("en", big.mark = ",")
    gtsummary::theme_gtsummary_compact()

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
    tbl <- gtsummary::tbl_summary(
        data = d,
        by = if (grouped) dplyr::all_of(name_groupvar) else NULL,
        type = type,
        missing = missing,
        missing_text = 'N missing',
        missing_stat = '{N_miss} ({p_miss}%)',
        statistic = statistic,
        digits = digits,
        percent = 'column'
    )
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
    html <- lapply(names(col_rows), function(cc) {
        rep(NA_character_, nrow(tb))
    })
    names(html) <- names(col_rows)
    dist_html <- rep(NA_character_, nrow(tb))
    span_rows <- integer(0) # distribution cells covered by a plot above
    span_start <- integer(0) # distribution cells holding a spanning plot
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
                dist_html[miss_row] <- svg_bars_combined(
                    p_miss,
                    colors = col_colors,
                    lane_names = col_names,
                    width = plot_width,
                    title = 'Missing',
                    hatched = TRUE
                )
            } else {
                for (cc in names(col_rows)) {
                    html[[cc]][miss_row] <- html_bar_cell(
                        tb[[cc]][miss_row],
                        p_miss[[cc]],
                        color = col_colors[[cc]],
                        width = bar_width,
                        hatched = TRUE
                    )
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
                compute_density(x_num[r], xlim)
            })
            ymax <- max(vapply(
                dens,
                function(z) if (is.null(z)) 0 else max(z$y),
                numeric(1)
            ))
            if (combined) {
                # The plot spans all rows of this variable (label + statistic
                # rows): gt has no rowspan, so the plot sits in the label row
                # and overflows downward over fixed-height spacers below it.
                # Spacers are taller than a text row and their cells have no
                # vertical padding, so they (not the text) set the row heights
                # and the plot can fill the span exactly.
                k <- length(col_rows)
                n_r <- length(idx)
                row_px <- max(
                    span_row_min_px,
                    ceiling((k * lane_height + axis_h) / n_r)
                )
                svg <- svg_continuous_combined(
                    dens,
                    xs = lapply(col_rows, function(r) x_num[r]),
                    xlim = xlim,
                    ymax = ymax,
                    colors = col_colors,
                    lane_names = col_names,
                    width = plot_width,
                    lane_height = (n_r * row_px - axis_h) / k,
                    title = sprintf('Distribution of %s', tb$label[label_row]),
                    type = continuous_plot,
                    show_outliers = show_outliers,
                    axis_height = axis_h,
                    axis_breaks = breaks
                )
                dist_html[label_row] <- sprintf(
                    paste0(
                        '<div style="position:relative;height:%dpx">',
                        '<div style="position:absolute;top:0;left:0">%s</div></div>'
                    ),
                    row_px, svg
                )
                other <- setdiff(idx, label_row)
                dist_html[other] <- sprintf('<div style="height:%dpx"></div>', row_px)
                span_rows <- c(span_rows, other)
                span_start <- c(span_start, label_row)
                next
            }
            for (cc in names(col_rows)) {
                html[[cc]][label_row] <- svg_continuous(
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
            if (combined) {
                for (j in seq_along(rows)) {
                    dist_html[rows[j]] <- svg_bars_combined(
                        props[j, ],
                        colors = col_colors,
                        lane_names = col_names,
                        width = plot_width,
                        title = tb$label[rows[j]]
                    )
                }
                next
            }
            for (k in seq_along(col_rows)) {
                cc <- names(col_rows)[k]
                for (j in seq_along(rows)) {
                    p <- props[j, k]
                    html[[cc]][rows[j]] <- html_bar_cell(
                        tb[[cc]][rows[j]],
                        p,
                        color = col_colors[[cc]],
                        width = bar_width
                    )
                }
            }
        }
    }

    # ---- render to gt and swap in HTML ----
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
    # ---- notes on denominators / excluded rows ----
    vars <- unique(tb$variable)
    cat_vars <- unique(tb$variable[tb$var_type %in% c('categorical', 'dichotomous')])
    any_miss_cat <- any(vapply(cat_vars, function(v) anyNA(d[[v]]), logical(1)))
    any_miss_row <- any(tb$row_type == 'missing')
    note <- c(
        if (any_miss_cat) 'Category percentages exclude missing values.',
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
    if (length(note) > 0) {
        gt_tbl <- gt::tab_source_note(gt_tbl, paste(note, collapse = ' '))
    }

    gt_tbl <- gt_tbl |>
        gt::cols_align('left', columns = dplyr::all_of(names(col_rows))) |>
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

    return(gt_tbl)
}

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

compute_density <- function(x, xlim) {
    x <- x[!is.na(x)]
    if (length(x) < 2 || diff(xlim) == 0 || stats::sd(x) == 0) {
        return(NULL)
    }
    dd <- stats::density(x, from = xlim[1], to = xlim[2], n = 128)
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
svg_axis <- function(
    xlim, width, y0, height, pad = 2, font_size = 9, breaks = NULL
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
    layout_ticks <- function(ticks) {
        px <- sx(ticks)
        labs <- fmt(ticks)
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
        lt <- layout_ticks(unname(breaks))
        if (!is.null(names(breaks))) {
            lt$labs <- names(breaks)
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
    hatched = FALSE
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
    svg_wrap(paste0(defs, paste(bars, collapse = '')), width, height, title)
}

html_bar_cell <- function(text, p, color, width, height = 10, hatched = FALSE) {
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
            '<span style="display:inline-flex;align-items:center;gap:6px;white-space:nowrap">',
            '<svg width="%s" height="%s" style="flex:none">%s',
            '<rect width="%s" height="%s" fill="#E6E6E6"/>',
            '<rect width="%.1f" height="%s" fill="%s"%s/></svg>',
            '<span>%s</span></span>'
        ),
        width,
        height,
        defs,
        width,
        height,
        p * width,
        height,
        fill,
        stroke,
        htmltools::htmlEscape(if (is.na(text)) '' else text)
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
