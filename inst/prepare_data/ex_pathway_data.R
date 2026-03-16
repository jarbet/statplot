## Prepare ex_expr_pathway and ex_log2fc_pathway
## Simulated expression data for plot_pathway_correlation_network() examples.
## Three small correlated gene modules within MYC_TARGETS_V1 + independent genes.
## Run from the package root to regenerate data/.

set.seed(42)
data("hallmark_t2g")

pathway <- "MYC_TARGETS_V1"
pw_genes <- unique(hallmark_t2g$gene[hallmark_t2g$term == pathway])[1:20]
n_samples <- 40

make_module <- function(n_genes, n_samples, signal_sd = 1, noise_sd = 0.3) {
    signal <- rnorm(n_samples, sd = signal_sd)
    t(replicate(n_genes, signal + rnorm(n_samples, sd = noise_sd)))
}

mod1 <- make_module(5, n_samples)
mod2 <- make_module(5, n_samples)
mod3 <- make_module(4, n_samples)
ind <- matrix(rnorm(6 * n_samples), nrow = 6)

ex_expr_pathway <- rbind(mod1, mod2, mod3, ind)
rownames(ex_expr_pathway) <- pw_genes
colnames(ex_expr_pathway) <- paste0("S", seq_len(n_samples))

ex_log2fc_pathway <- setNames(
    c(
        1.8,
        1.5,
        2.1,
        -0.2,
        1.3,
        -1.6,
        -2.0,
        -0.9,
        -1.4,
        0.3,
        0.8,
        1.1,
        -0.5,
        0.4,
        -0.1,
        0.2,
        -1.8,
        1.2,
        -0.3,
        0.7
    ),
    pw_genes
)

usethis::use_data(ex_expr_pathway, ex_log2fc_pathway, overwrite = TRUE)
