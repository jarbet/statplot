#' Bias-corrected Cramer's V with a confidence interval
#'
#' Computes Cramer's V for a two-way contingency table using the
#' Bergsma (2013) bias correction, with a confidence interval obtained by
#' inverting the noncentral chi-squared distribution of the Pearson
#' chi-squared statistic. Matches `effectsize::cramers_v(tab, adjust = TRUE,
#' alternative = "two.sided")`.
#'
#' The Pearson chi-squared statistic is computed without Yates' continuity
#' correction (also for 2 x 2 tables). Empty rows and columns are dropped.
#'
#' @param tab A two-way contingency table (e.g. from [table()]) with at least
#'   2 non-empty rows and 2 non-empty columns.
#' @param ci Numeric scalar in (0, 1). Confidence level. Default 0.95.
#' @return A list with elements `estimate`, `ci_low`, `ci_high`.
#' @references Bergsma, W. (2013). A bias-correction for Cramer's V and
#'   Tschuprow's T. Journal of the Korean Statistical Society, 42(3), 323-328.
#' @examples
#' tab <- table(mtcars$cyl, mtcars$gear)
#' statplot:::cramers_v_adjusted(tab)
#' @keywords internal
#' @noRd
cramers_v_adjusted <- function(tab, ci = 0.95) {
    stopifnot(
        length(dim(tab)) == 2,
        is.numeric(tab),
        is.numeric(ci),
        length(ci) == 1,
        ci > 0,
        ci < 1
    )
    # empty rows/columns (e.g. unused factor levels) carry no information
    tab <- tab[rowSums(tab) > 0, colSums(tab) > 0, drop = FALSE]
    stopifnot(nrow(tab) >= 2, ncol(tab) >= 2)

    # Pearson chi-squared without continuity correction
    n <- sum(tab)
    expected <- outer(rowSums(tab), colSums(tab)) / n
    chisq <- sum((tab - expected)^2 / expected)
    r <- nrow(tab)
    k <- ncol(tab)
    df <- (r - 1) * (k - 1)

    # Bergsma (2013) bias correction for phi^2 and the table dimensions
    E <- df / (n - 1)
    r_adj <- r - (r - 1)^2 / (n - 1)
    k_adj <- k - (k - 1)^2 / (n - 1)

    chisq_to_v <- function(chisq_value) {
        phi2_adj <- pmax(0, chisq_value / n - E)
        sqrt(phi2_adj) / sqrt(min(r_adj - 1, k_adj - 1))
    }

    ncp <- noncentral_chisq_ci(chisq, df = df, ci = ci)

    list(
        estimate = chisq_to_v(chisq),
        ci_low = min(1, chisq_to_v(ncp[1])),
        ci_high = min(1, chisq_to_v(ncp[2]))
    )
}

#' Confidence interval for a chi-squared noncentrality parameter
#'
#' Finds the noncentrality parameters `lambda` such that the observed
#' statistic sits at the upper and lower `(1 - ci) / 2` tails of a noncentral
#' chi-squared distribution with `df` degrees of freedom. A bound is set to 0
#' when even the central distribution (`lambda = 0`) cannot place the
#' statistic in the required tail.
#'
#' @param chisq Numeric scalar. Observed chi-squared statistic.
#' @param df Numeric scalar. Degrees of freedom.
#' @param ci Numeric scalar in (0, 1). Confidence level.
#' @return Numeric vector of length 2 (lower, upper noncentrality bounds).
#' @examples
#' statplot:::noncentral_chisq_ci(12, df = 4, ci = 0.95)
#' @keywords internal
#' @noRd
noncentral_chisq_ci <- function(chisq, df, ci = 0.95) {
    stopifnot(
        is.numeric(chisq),
        length(chisq) == 1,
        is.numeric(df),
        length(df) == 1,
        df > 0,
        is.numeric(ci),
        length(ci) == 1
    )
    if (!is.finite(chisq)) {
        return(c(NA_real_, NA_real_))
    }
    alpha <- 1 - ci

    # pchisq(chisq, df, ncp) decreases in ncp; solve pchisq(...) = target
    solve_ncp <- function(target) {
        if (stats::pchisq(chisq, df, ncp = 0) <= target) {
            return(0)
        }
        upper <- max(10, 2 * chisq)
        while (stats::pchisq(chisq, df, ncp = upper) > target) {
            upper <- upper * 2
        }
        stats::uniroot(
            function(ncp) stats::pchisq(chisq, df, ncp = ncp) - target,
            lower = 0,
            upper = upper,
            tol = 1e-10
        )$root
    }

    c(solve_ncp(1 - alpha / 2), solve_ncp(alpha / 2))
}

#' Rank epsilon-squared effect size with a bootstrap confidence interval
#'
#' Computes the rank epsilon-squared effect size for the Kruskal-Wallis test,
#' `H / (n - 1)` (Tomczak & Tomczak, 2014), equivalent to
#' `effectsize::rank_epsilon_squared()`. The confidence interval is a
#' percentile bootstrap in which observations are resampled with replacement
#' within each group (groups of size 1 are kept as is), matching the
#' resampling scheme used by `effectsize`.
#'
#' @param x Numeric vector of outcome values.
#' @param g Grouping vector (factor or coercible to factor), same length as `x`.
#' @param ci Numeric scalar in (0, 1). Confidence level. Default 0.95.
#' @param iterations Positive integer. Number of bootstrap resamples. Default
#'   1000.
#' @return A list with elements `estimate`, `ci_low`, `ci_high`.
#' @references Tomczak, M., & Tomczak, E. (2014). The need to report effect
#'   size estimates revisited. Trends in Sport Sciences, 1(21), 19-25.
#' @examples
#' set.seed(1)
#' statplot:::rank_epsilon_squared(mtcars$mpg, mtcars$cyl, iterations = 200)
#' @keywords internal
#' @noRd
rank_epsilon_squared <- function(x, g, ci = 0.95, iterations = 1000) {
    stopifnot(
        is.numeric(x),
        length(x) == length(g),
        is.numeric(ci),
        length(ci) == 1,
        ci > 0,
        ci < 1,
        is.numeric(iterations),
        length(iterations) == 1,
        iterations >= 1
    )
    keep <- !is.na(x) & !is.na(g)
    x <- x[keep]
    g <- droplevels(as.factor(g[keep]))
    stopifnot(nlevels(g) >= 2)

    n <- length(x)
    epsilon2 <- function(xx) {
        h <- suppressWarnings(stats::kruskal.test(xx, g)$statistic)
        unname(h) / (n - 1)
    }

    est <- epsilon2(x)

    idx_by_group <- split(seq_len(n), g)
    boot_est <- vapply(
        seq_len(iterations),
        function(i) {
            xb <- x
            for (idx in idx_by_group) {
                if (length(idx) >= 2L) {
                    xb[idx] <- x[idx[sample.int(
                        length(idx),
                        length(idx),
                        replace = TRUE
                    )]]
                }
            }
            epsilon2(xb)
        },
        numeric(1)
    )

    alpha <- 1 - ci
    q <- stats::quantile(
        boot_est,
        probs = c(alpha / 2, 1 - alpha / 2),
        na.rm = TRUE,
        names = FALSE
    )

    list(
        estimate = est,
        ci_low = max(0, q[1]),
        ci_high = min(1, q[2])
    )
}

#' Fill a pairwise p-value matrix into a full symmetric matrix
#'
#' Converts the lower-triangular p-value matrix returned by
#' [stats::pairwise.wilcox.test()] (and related `pairwise.*` functions) into a
#' full symmetric square matrix with 1 on the diagonal, the format expected by
#' `multcompView::multcompLetters()`.
#'
#' @param p Numeric matrix of p-values as in `pairwise.wilcox.test()$p.value`:
#'   rows are levels 2..k, columns are levels 1..(k-1), upper triangle `NA`.
#' @return A k x k symmetric numeric matrix with dimnames set to the k levels.
#' @examples
#' pw <- stats::pairwise.wilcox.test(mtcars$mpg, mtcars$cyl, exact = FALSE)
#' statplot:::full_pvalue_matrix(pw$p.value)
#' @keywords internal
#' @noRd
full_pvalue_matrix <- function(p) {
    stopifnot(
        is.matrix(p),
        is.numeric(p),
        !is.null(rownames(p)),
        !is.null(colnames(p))
    )
    lev <- unique(c(colnames(p), rownames(p)))
    out <- matrix(
        NA_real_,
        nrow = length(lev),
        ncol = length(lev),
        dimnames = list(lev, lev)
    )
    out[rownames(p), colnames(p)] <- p
    upper <- upper.tri(out)
    out[upper] <- t(out)[upper]
    diag(out) <- 1
    out
}
