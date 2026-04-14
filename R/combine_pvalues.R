#' Combine p-values
#'
#' Combines a numeric vector of p-values into a single p-value using a variety of methods.  Note CMC or MCM are recommended by Chen 2022.
#'
#' @param p Numeric vector of p-values.
#' @param methods Character, either 'CMC' (default) to return the CMC combined p-value, `'all'` to return all combined
#' p-values, or the name of one or more methods (fisher, CMC, MCM, cauchy, minp_bonferroni; note CMC or MCM are recommended by Chen 2022) to return only those p-values.
#' @return A named vector of combined p-values for each method, or a single numeric value when
#' `methods` names one method
#' @examples
#' pvals <- c(0.001, 0.005, 0.2)
#' # default returns CMC
#' combine_pvalues(pvals)
#' # get all methods
#' combine_pvalues(pvals, methods = "all")
#' @references
#' Chen, Z. Robust tests for combining p-values under arbitrary dependency structures. Sci Rep 12, 3158 (2022). https://doi.org/10.1038/s41598-022-07094-7
#' @export
combine_pvalues <- function(p, methods = "CMC") {
    if (!is.numeric(p)) {
        stop("p must be numeric")
    }
    p <- p[!is.na(p)]
    if (length(p) == 0) {
        stop("no p-values after removing NA")
    }
    stopifnot(all(p > 0 & p <= 1))

    n <- length(p)

    # allowed method names (do not require computing values to validate)
    .allowed_methods <- c("fisher", "CMC", "MCM", "cauchy", "minp_bonferroni")

    if (!is.character(methods)) {
        stop("methods must be a character")
    }

    if ("all" %in% methods) {
        methods <- "all"
    } else if (!all(methods %in% .allowed_methods)) {
        stop("unknown method")
    }
    methods <- unique(methods)

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
    if ('all' %in% methods) {
        return(combined_pvalues)
    }
    return(combined_pvalues[methods])
}
