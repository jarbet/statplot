devtools::load_all()

data(ex_data_heatmap)

ht_cols_small <- circlize::colorRamp2(
    c(min(1:16), mean(1:16), max(1:16)),
    c("#145afc", "white", "#ee4445")
)

ann_cols_small <- list(
    group = c(G1 = "#1b9e77", G2 = "#d95f02"),
    direction = c(up = "#e41a1c", down = "#4daf4a"),
    is_immune_gene = c(yes = "#fb8072", no = "#d9d9d9"),
    sample_type = c(input = "#8dd3c7", IP = "#80b1d3"),
    condition = c(healthy = "#b3de69", EAE = "#fccde5")
)

# make a categorical heatmap
cat_data <- ex_data_heatmap
cat_data$expression <- as.character(cut(
    cat_data$expression,
    breaks = c(-Inf, 3, 6, Inf),
    labels = c("low", "medium", "high")
))


# default colors
plot_heatmap(
    df = cat_data,
    row_var = external_gene_name,
    col_var = sample,
    value_var = expression,
    row_covariates = c("is_immune_gene", "direction"),
    col_covariates = c("sample_type", "condition", "group"),
    row_split_var = "direction", # up vs down (2 slices)
    col_split_var = "group", # G1 vs G2 (2 slices)
    scale_rows = FALSE,
    cluster_rows = TRUE,
    cluster_columns = TRUE,
    return_details = TRUE,
    row_names_side = "left"
)

# custom colors
heat_cat_colors <- c(
    low = "#313695",
    medium = "#f7f7f7",
    high = "#a50026"
)

plot_heatmap(
    df = cat_data,
    row_var = external_gene_name,
    col_var = sample,
    heatmap_colors = heat_cat_colors,
    value_var = expression,
    row_covariates = c("is_immune_gene", "direction"),
    col_covariates = c("sample_type", "condition", "group"),
    row_split_var = "direction", # up vs down (2 slices)
    col_split_var = "group", # G1 vs G2 (2 slices)
    scale_rows = FALSE,
    cluster_rows = TRUE,
    cluster_columns = TRUE,
    return_details = TRUE,
    row_names_side = "left"
)

######### heatmap with continuous values

# default colors
plot_heatmap(
    df = ex_data_heatmap,
    row_var = external_gene_name,
    col_var = sample,
    value_var = expression,
    row_covariates = c("is_immune_gene", "direction"),
    col_covariates = c("sample_type", "condition", "group"),
    row_split_var = "direction", # up vs down (2 slices)
    col_split_var = "group", # G1 vs G2 (2 slices)
    scale_rows = FALSE,
    cluster_rows = TRUE,
    cluster_columns = TRUE,
    return_details = TRUE,
    row_names_side = "left"
)
# custom colors
plot_heatmap(
    df = ex_data_heatmap,
    row_var = external_gene_name,
    col_var = sample,
    value_var = expression,
    row_covariates = c("is_immune_gene", "direction"),
    col_covariates = c("sample_type", "condition", "group"),
    row_split_var = "direction",
    col_split_var = "group",
    scale_rows = FALSE,
    cluster_rows = FALSE,
    cluster_columns = FALSE,
    heatmap_colors = ht_cols_small,
    anno_colors = ann_cols_small,
    return_details = TRUE
)
### Continuous row and column covariates via colorRamp2 functions
rng_log2fc <- range(ex_data_heatmap$log2fc, na.rm = TRUE)
col_fun_log2fc <- circlize::colorRamp2(
    c(rng_log2fc[1], 0, rng_log2fc[2]),
    c("#762a83", "white", "#e66101")
)

rng_qc <- range(ex_data_heatmap$qc_score, na.rm = TRUE)
col_fun_qc <- circlize::colorRamp2(
    c(rng_qc[1], mean(rng_qc), rng_qc[2]),
    c("#ffffcc", "#41b6c4", "#0c2c84")
)

expr_rng <- range(ex_data_heatmap$expression, na.rm = TRUE)
expr_cols <- circlize::colorRamp2(
    c(expr_rng[1], mean(expr_rng), expr_rng[2]),
    c("#313695", "#f7f7f7", "#a50026")
)

plot_heatmap(
    df = ex_data_heatmap,
    row_var = external_gene_name,
    col_var = sample,
    value_var = expression,
    row_covariates = c("log2fc", "is_immune_gene", "direction"),
    col_covariates = c("qc_score", "condition", "sample_type"),
    col_split_var = "group",
    row_split_var = "direction",
    heatmap_colors = expr_cols,
    anno_colors = list(
        log2fc = col_fun_log2fc,
        qc_score = col_fun_qc,
        is_immune_gene = c(yes = "#e7298a", no = "#a6761d"),
        direction = c(up = "#1f78b4", down = "#e31a1c"),
        condition = c(healthy = "#1b9e77", EAE = "#d95f02"),
        sample_type = c(input = "#7570b3", IP = "#66a61e")
    ),
    scale_rows = FALSE,
    cluster_rows = TRUE,
    cluster_columns = TRUE,
    show_row_names = TRUE,
    row_names_side = "left",
    heatmap_legend_title = "Expression"
)
