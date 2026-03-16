## Prepare hallmark_t2g: Hallmark gene sets (Human) from MSigDB v2024.1
## Columns: term (gs_name without "HALLMARK_" prefix), gene (gene_symbol)
## Run once from the package root to regenerate data/hallmark_t2g.rda

library(msigdbr)

hallmark_raw <- msigdbr(species = "Homo sapiens", collection = "H")
hallmark_t2g <- as.data.frame(hallmark_raw[, c("gs_name", "gene_symbol")])
colnames(hallmark_t2g) <- c("term", "gene")
hallmark_t2g$term <- gsub("^HALLMARK_", "", hallmark_t2g$term)
hallmark_t2g <- unique(hallmark_t2g)
hallmark_t2g <- hallmark_t2g[order(hallmark_t2g$term, hallmark_t2g$gene), ]
rownames(hallmark_t2g) <- NULL

usethis::use_data(hallmark_t2g, overwrite = TRUE)
