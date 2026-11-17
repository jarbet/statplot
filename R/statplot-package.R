#' @keywords internal
"_PACKAGE"

#' @importFrom rlang .data
NULL

# Column names used unquoted inside dplyr/ggplot2 code (non-standard
# evaluation). Declared here so R CMD check doesn't report them as
# undefined global variables.
utils::globalVariables(c(
    ".bar_top", ".est", ".group", ".lbl", ".x", ".y_gap", ".y_max",
    ".y_min", ".y_range", "bar_label", "count", "est_label",
    "events_label", "fill_val", "group_text_color", "label", "lbl",
    "level_bold", "level_label", "membership_key", "n", "n_label", "name",
    "node_fill", "p_combined", "p_label", "p_sig", "pct", "pval", "size",
    "size_val", "stat", "total", "x", "xend", "y", "y_left", "y_max",
    "y_min", "y_pos", "y_range", "y_right", "y_top", "yend", "ymax", "ymin"
))
