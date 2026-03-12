devtools::load_all()
library(ggpubr)
library(rstatix) # optional: for tidy tests

# df: columns group (factor), value (numeric), maybe subgroup (factor)
set.seed(123)
df <- data.frame(
    group = rep(c("A", "B", "C"), each = 30),
    value = rnorm(90, mean = rep(c(5.0, 5.8, 6.3), each = 30), sd = 1)
)

# Prism-style barplot with mean ± SE and p-values by pairwise t-tests
ggbarplot(
    df,
    x = "group",
    y = "value",
    add = "mean_se",
    color = "group",
    fill = "group",
    palette = "npg", # or "jco", "aaas", etc.
    width = 0.7
) +
    stat_compare_means(method = "anova") + # overall test
    stat_compare_means(
        comparisons = list(c("A", "B"), c("A", "C"), c("B", "C")),
        method = "t.test",
        label = "p.signif",
        hide.ns = TRUE
    ) +
    labs(x = NULL, y = "Outcome (mean ± SE)") +
    theme_pubr(base_size = 12)
