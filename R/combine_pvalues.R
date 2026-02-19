#' Combine p-values using Fisher, Cauchy (ACAT), or Harmonic Mean methods
#'
#' Combines a numeric vector of p-values into a single p-value using one of
#' three supported methods: "fisher" (poolr::fisher), "cauchy" (ACAT::ACAT),
#' or "hm" (harmonicmeanp::p.hmp). NAs are removed and all p-values must be
#' strictly between 0 and 1.
#'
#' @param p Numeric vector of p-values.
#' @param method One of "fisher", "cauchy", or "hm".
#' @return A numeric p-value
#' @examples
#' pvals <- c(0.001, 0.005, 0.2)
#' combine_pvalues(pvals, "fisher")
#' combine_pvalues(pvals, "cauchy")
#' combine_pvalues(pvals, "hm")
#' @export
combine_pvalues <- function(p, method = c("fisher", "cauchy", "hm")) {
    method <- match.arg(method)
    if (!is.numeric(p)) {
        stop("p must be numeric")
    }
    p <- p[!is.na(p)]
    if (length(p) == 0) {
        stop("no p-values after removing NA")
    }
    stopifnot(all(p > 0 & p < 1))

    pval <- switch(
        method,
        fisher = as.numeric(poolr::fisher(p = p)$p),
        cauchy = as.numeric(ACAT::ACAT(Pvals = p)),
        hm = {
            res <- harmonicmeanp::p.hmp(p = p, L = length(p))
            if (is.list(res)) {
                if (!is.null(res$p)) {
                    res$p
                } else if (!is.null(res$p.value)) {
                    res$p.value
                } else {
                    as.numeric(res[[1]])
                }
            } else {
                as.numeric(res)
            }
        }
    )

    as.numeric(pmin(pmax(pval, 0), 1))
}
