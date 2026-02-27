# statplot (development version)

## Added

* `plot_heatmap`
* `quarto_html_tabset_list`: convert a named list object to a tabset when knitting to HTML
* `format_pvalue`: function to format pvalues to display in plots
* `combine_pvalues` function to combine pvalues to produce a single overall pvalue. 3 methods are supported: Fisher, Cauchy, and Harmonic Mean
* `plot_pvalue_barplot`: horizontal barplot of pvalues
* `plot_dotmap`: plot heatmap of effect sizes (dots) with cells shaded by pvalue
    + option to add Fisher's combination pvalue barplot on the right
    + option to show q-values for the combined pvalue barplot
    + 3 methods are supported when combining pvalues
    + show an x symbol if dot is missing

## Changed

* ...

## Fixed

* ...

# statplot 0.1.0 - 2026-01-01

Setup first working draft.
