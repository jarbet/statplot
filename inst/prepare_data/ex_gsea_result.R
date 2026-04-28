## Generates data/ex_gsea_result.rda
## Run manually: source("inst/prepare_data/ex_gsea_result.R")

library(statplot)

data(hallmark_t2g)
set.seed(1)
all_genes <- unique(hallmark_t2g$gene)
gene_vec <- setNames(rnorm(length(all_genes)), all_genes)

ex_gsea_result <- run_gsea(gene_vec, term2gene = hallmark_t2g)

usethis::use_data(ex_gsea_result, overwrite = TRUE)
