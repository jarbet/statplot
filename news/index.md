# Changelog

## statplot (development version)

### Added

- `plot_heatmap`
  - By default, white gridlines around cells
  - Add support for categorical heatmaps
  - Support for clustering categorical data
- `quarto_html_tabset_list`: convert a named list object to a tabset
  when knitting to HTML
- `format_pvalue`: function to format pvalues to display in plots
- `combine_pvalues` function to combine pvalues to produce a single
  overall pvalue
  - Updated methods for contemporary best practices
- `plot_pvalue_barplot`: horizontal barplot of pvalues
  - By default now shows both pvalues and qvalues stacked
- `plot_dotmap`: plot heatmap of effect sizes (dots) with cells shaded
  by pvalue
  - option to add combined pvalue barplot on the right
    - Changed default combo method to CMC as recommended by literature
  - option to show q-values for the combined pvalue barplot
  - 5 methods are supported when combining pvalues
  - show an x symbol if dot is missing

### Changed

### Fixed

- …

## statplot 0.1.0 - 2026-01-01

Setup first working draft.
