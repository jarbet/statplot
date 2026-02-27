## Prepare data_exprs_small: a small tidy expression dataset for examples/tests
## Run this script from the package root, then call usethis::use_data() at the end.

genes <- c("GeneA", "GeneB", "GeneC")
samples <- c("S1", "S2", "S3")

vals_mat <- matrix(
    c(1.2, 3.4, 5.6, 2.1, 4.3, 6.5, 7.8, 9.0, 1.1),
    nrow = 3,
    ncol = 3,
    byrow = TRUE,
    dimnames = list(genes, samples)
)

row_cov <- data.frame(
    external_gene_name = genes,
    is_immune_gene = c("yes", "no", "yes"),
    direction = c("up", "up", "down"),
    log2fc = c(2.3, 1.1, -1.8),
    stringsAsFactors = FALSE
)

col_cov <- data.frame(
    sample = samples,
    sample_type = c("input", "input", "IP"),
    condition = c("healthy", "EAE", "healthy"),
    group = c("G1", "G1", "G2"),
    qc_score = c(0.95, 0.7, 0.5),
    stringsAsFactors = FALSE
)

df_long <- as.data.frame(vals_mat)
df_long$external_gene_name <- rownames(vals_mat)
df_long <- reshape(
    df_long,
    direction = "long",
    varying = samples,
    v.names = "expression",
    idvar = "external_gene_name",
    timevar = "sample",
    times = samples
)
df_long <- merge(df_long, row_cov, by = "external_gene_name")
df_long <- merge(df_long, col_cov, by = "sample")

data_exprs_small <- df_long[
    order(df_long$external_gene_name, df_long$sample),
    c(
        "external_gene_name",
        "sample",
        "expression",
        "is_immune_gene",
        "direction",
        "log2fc",
        "sample_type",
        "condition",
        "group",
        "qc_score"
    )
]
rownames(data_exprs_small) <- NULL

ex_data_heatmap <- data_exprs_small

usethis::use_data(ex_data_heatmap, overwrite = TRUE)
