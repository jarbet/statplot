#' Combine p-values
#'
#' Combines a numeric vector of p-values into a single p-value using a variety of methods.  Note CMC or MCM are recommended by Chen 2022.
#'
#' @param p Numeric vector of p-values.
#' @return A named vector of combined p-values for each method
#' @examples
#' pvals <- c(0.001, 0.005, 0.2)
#' combine_pvalues(pvals)
#' @references
#' Chen, Z. Robust tests for combining p-values under arbitrary dependency structures. Sci Rep 12, 3158 (2022). https://doi.org/10.1038/s41598-022-07094-7
#' @export
combine_pvalues <- function(p) {
    if (!is.numeric(p)) {
        stop("p must be numeric")
    }
    p <- p[!is.na(p)]
    if (length(p) == 0) {
        stop("no p-values after removing NA")
    }
    stopifnot(all(p > 0 & p < 1))

    n <- length(p)

    fisher <- as.numeric(poolr::fisher(p = p)$p)

    # https://github.com/zchen2020/Robust-P-value-combination-tests/blob/main/R%20code%20for%20all.R
    cauchy <- 0.5 - atan(mean(tan((0.5 - p) * pi))) / pi
    minp_bonferroni <- min(1, n * min(p))
    mcm <- min(1, 2 * min(cauchy, minp_bonferroni))
    cmc <- 0.5 -
        atan(mean(c(
            tan((0.5 - cauchy) * pi),
            tan((0.5 - minp_bonferroni) * pi)
        ))) /
            pi

    combined_pvalues <- c(
        fisher = fisher,
        CMC = cmc,
        MCM = mcm,
        cauchy = cauchy,
        minp_bonferroni = minp_bonferroni
    )
    return(combined_pvalues)
}
