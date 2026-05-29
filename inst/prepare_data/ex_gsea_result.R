## Generates data/ex_gsea_result.rda
## Run once from the package root: source("inst/prepare_data/ex_gsea_result.R")

library(statplot)

data(hallmark_t2g)
set.seed(1)
all_genes <- unique(hallmark_t2g$gene)
gene_vec  <- setNames(rnorm(length(all_genes)), all_genes)

ex_gsea_result <- run_gsea(gene_vec, term2gene = hallmark_t2g)

save(ex_gsea_result,
     file     = "data/ex_gsea_result.rda",
     compress = "bzip2")
