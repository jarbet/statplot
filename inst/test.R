devtools::load_all()

set.seed(42)
genes <- paste0("gene", 1:6)
df <- expand.grid(
    col = c("Aaaaaaaaaaaaaaaa", "Bbbbbbbbbbbbb", "Cccccccccccccc"),
    row = genes,
    stringsAsFactors = FALSE
)
df$effect <- rnorm(nrow(df), mean = 0, sd = 1.2) # realistic effect sizes
df$mlog10_p <- runif(nrow(df), min = 0, max = 3) # -log10(p) between 0 and 3
df$p <- 10^(-df$mlog10_p)
df$row <- factor(df$row, levels = rev(genes))
plot_dotmap(
    df,
    x = "col",
    y = "row",
    effect = "effect",
    p = "p",
    mlog10_transform_pvalue = TRUE
)
# Add Fisher's combination pvalue barplot on the right which combines p-values across columns for each row category
plot_dotmap(
    df,
    x = "col",
    y = "row",
    effect = "effect",
    p = "p",
    mlog10_transform_pvalue = TRUE,
    add_combined_pvalue_barplot = TRUE,
    combine_pvalue_method = "CMC",
    xlab_angle = 45
)
