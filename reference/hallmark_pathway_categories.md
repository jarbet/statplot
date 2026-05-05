# Hallmark pathway process categories

A four-column data frame mapping each MSigDB Hallmark gene-set name to
its biological process category, as defined in Table 1 of Liberzon et
al. (2015), along with human-readable pathway labels. Categories are:
`"Cellular component"`, `"Development"`, `"DNA damage"`, `"Immune"`,
`"Metabolic"`, `"Pathway"`, `"Proliferation"`, and `"Signaling"`.

## Usage

``` r
hallmark_pathway_categories
```

## Format

A data frame with 50 rows and 4 columns:

- term:

  Character. Hallmark gene-set name, with the `"HALLMARK_"` prefix
  removed to match the `term` column of
  [`hallmark_t2g`](https://github.com/jarbet/statplot/reference/hallmark_t2g.md).

- process_category:

  Character. Biological process category assigned in Liberzon et al.
  (2015), Table 1, with underscores replaced by spaces.

- label_short:

  Character. Concise human-readable pathway label with Greek symbols (α,
  β, γ, κ) preserved.

- label_long:

  Character. Expanded human-readable pathway label with Greek symbols
  spelled out (alpha, beta, gamma, kappa).

## Source

Process categories taken from Table 1 of Liberzon et al. (2015). Labels
prepared by `inst/prepare_data/hallmark_pathway_categories.R`.

## References

Liberzon, A., Birger, C., Thorvaldsdóttir, H., Ghandi, M., Mesirov, J.
P., & Tamayo, P. (2015). The Molecular Signatures Database hallmark gene
set collection. *Cell Systems*, **1**(6), 417–425.
[doi:10.1016/j.cels.2015.12.004](https://doi.org/10.1016/j.cels.2015.12.004)
