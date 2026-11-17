# Validation of the internal effect-size helpers that replaced `effectsize`,
# `rcompanion::fullPTable()` and `poolr::fisher()`. Reference packages are in
# Suggests; comparisons against them are skipped when they are not installed.

# Random r x c contingency tables of varied shape, size and association
make_random_tables <- function(n_tables, seed) {
    withr::local_seed(seed)
    tabs <- list()
    while (length(tabs) < n_tables) {
        r <- sample(2:5, 1)
        k <- sample(2:5, 1)
        n <- sample(c(15, 30, 60, 200, 1000), 1)
        probs <- matrix(rexp(r * k)^sample(c(0.2, 1, 3), 1), r, k)
        tab <- matrix(stats::rmultinom(1, n, probs), r, k)
        if (all(rowSums(tab) > 0) && all(colSums(tab) > 0)) {
            tabs[[length(tabs) + 1]] <- tab
        }
    }
    tabs
}

# ---------------------------------------------------------------------------
# cramers_v_adjusted()
# ---------------------------------------------------------------------------

test_that("cramers_v_adjusted matches a hand calculation", {
    tab <- matrix(c(20, 10, 5, 15, 10, 20), nrow = 2)
    n <- sum(tab)
    expected <- outer(rowSums(tab), colSums(tab)) / n
    chisq <- sum((tab - expected)^2 / expected)
    phi2_adj <- max(0, chisq / n - (1 * 2) / (n - 1))
    r_adj <- 2 - 1^2 / (n - 1)
    k_adj <- 3 - 2^2 / (n - 1)
    v_hand <- sqrt(phi2_adj / min(r_adj - 1, k_adj - 1))

    res <- statplot:::cramers_v_adjusted(tab)
    expect_equal(res$estimate, v_hand, tolerance = 1e-12)
})

test_that("cramers_v_adjusted uses Pearson chi-squared without Yates correction", {
    tab <- matrix(c(12, 5, 7, 16), nrow = 2)
    chisq <- unname(stats::chisq.test(tab, correct = FALSE)$statistic)
    n <- sum(tab)
    phi2_adj <- max(0, chisq / n - 1 / (n - 1))
    dim_adj <- 2 - 1 / (n - 1)
    expect_equal(
        statplot:::cramers_v_adjusted(tab)$estimate,
        sqrt(phi2_adj / (dim_adj - 1)),
        tolerance = 1e-12
    )
})

test_that("cramers_v_adjusted matches effectsize::cramers_v on 300 random tables", {
    skip_if_not_installed("effectsize")

    # Tail probability P(X <= chisq) implied by a reported CI bound `v`,
    # obtained by inverting the V -> noncentrality conversion
    implied_tail_prob <- function(v, tab) {
        n <- sum(tab)
        r <- nrow(tab)
        k <- ncol(tab)
        df <- (r - 1) * (k - 1)
        dim_adj <- min(r - (r - 1)^2 / (n - 1), k - (k - 1)^2 / (n - 1)) - 1
        ncp <- n * ((v^2) * dim_adj + df / (n - 1))
        expected <- outer(rowSums(tab), colSums(tab)) / n
        chisq <- sum((tab - expected)^2 / expected)
        stats::pchisq(chisq, df, ncp = ncp)
    }

    n_not_converged <- 0
    for (tab in make_random_tables(300, seed = 2024)) {
        ours <- statplot:::cramers_v_adjusted(tab)
        ref <- effectsize::cramers_v(
            tab,
            adjust = TRUE,
            alternative = "two.sided"
        )
        expect_equal(
            ours$estimate,
            ref$Cramers_v_adjusted,
            tolerance = 1e-10
        )
        # effectsize finds the noncentrality bounds with stats::optim(), which
        # stops slightly short of the exact root and occasionally fails to
        # converge at all (its bound then does not solve the defining
        # equation). Compare only bounds where effectsize converged; ours
        # are checked against the defining equation in the
        # noncentral_chisq_ci tests below.
        for (b in list(
            list(ours = ours$ci_low, ref = ref$CI_low, target = 0.975),
            list(ours = ours$ci_high, ref = ref$CI_high, target = 0.025)
        )) {
            ref_ok <- b$ref <= 0 ||
                b$ref >= 1 ||
                abs(implied_tail_prob(b$ref, tab) - b$target) < 0.005
            if (ref_ok) {
                expect_lt(abs(b$ours - b$ref), 0.01)
            } else {
                n_not_converged <- n_not_converged + 1
            }
        }
    }
    # non-convergence in effectsize is rare
    expect_lte(n_not_converged, 6)
})

test_that("cramers_v_adjusted is 0 for independent tables and 1 for perfect association", {
    indep <- outer(c(10, 20, 30), c(2, 3)) # exactly proportional
    expect_equal(statplot:::cramers_v_adjusted(indep)$estimate, 0)
    expect_equal(statplot:::cramers_v_adjusted(indep)$ci_low, 0)

    perfect <- diag(c(30, 30, 30))
    res <- statplot:::cramers_v_adjusted(perfect)
    expect_equal(res$estimate, 1, tolerance = 1e-12)
    expect_equal(res$ci_high, 1)
})

test_that("cramers_v_adjusted CI is ordered and within [0, 1]", {
    for (tab in make_random_tables(100, seed = 7)) {
        res <- statplot:::cramers_v_adjusted(tab)
        expect_gte(res$ci_low, 0)
        expect_lte(res$ci_high, 1)
        expect_lte(res$ci_low, res$estimate + 1e-12)
        expect_gte(res$ci_high, res$estimate - 1e-12)
    }
})

test_that("cramers_v_adjusted drops empty rows and columns", {
    tab <- table(
        factor(c("a", "a", "b", "b", "b"), levels = c("a", "b", "unused")),
        factor(c("x", "y", "y", "y", "x"), levels = c("x", "y", "z"))
    )
    expect_equal(
        statplot:::cramers_v_adjusted(tab),
        statplot:::cramers_v_adjusted(tab[1:2, 1:2])
    )
})

test_that("cramers_v_adjusted rejects tables with fewer than 2 non-empty rows or columns", {
    expect_error(statplot:::cramers_v_adjusted(matrix(1:3, ncol = 1)))
    expect_error(statplot:::cramers_v_adjusted(matrix(c(1, 0, 2, 0), 2)))
    expect_error(statplot:::cramers_v_adjusted(matrix(1:4, 2), ci = 1.5))
})

# ---------------------------------------------------------------------------
# noncentral_chisq_ci()
# ---------------------------------------------------------------------------

test_that("noncentral_chisq_ci bounds solve the defining equations", {
    withr::local_seed(99)
    for (i in 1:200) {
        df <- sample(1:16, 1)
        chisq <- stats::rchisq(1, df, ncp = stats::runif(1, 0, 60))
        bounds <- statplot:::noncentral_chisq_ci(chisq, df = df, ci = 0.95)
        expect_lte(bounds[1], bounds[2])
        # a positive bound puts the observed statistic exactly in the tail;
        # a zero bound means even the central distribution cannot
        if (bounds[1] > 0) {
            expect_equal(
                stats::pchisq(chisq, df, ncp = bounds[1]),
                0.975,
                tolerance = 1e-8
            )
        } else {
            expect_lte(stats::pchisq(chisq, df), 0.975)
        }
        if (bounds[2] > 0) {
            expect_equal(
                stats::pchisq(chisq, df, ncp = bounds[2]),
                0.025,
                tolerance = 1e-8
            )
        } else {
            expect_lte(stats::pchisq(chisq, df), 0.025)
        }
    }
})

test_that("noncentral_chisq_ci returns NA for a non-finite statistic", {
    expect_equal(
        statplot:::noncentral_chisq_ci(NaN, df = 2),
        c(NA_real_, NA_real_)
    )
})

# ---------------------------------------------------------------------------
# rank_epsilon_squared()
# ---------------------------------------------------------------------------

test_that("rank_epsilon_squared estimate equals H / (n - 1)", {
    withr::local_seed(1)
    g <- factor(rep(c("a", "b", "c"), times = c(8, 12, 10)))
    x <- stats::rnorm(30, mean = as.integer(g))
    h <- unname(stats::kruskal.test(x, g)$statistic)
    res <- statplot:::rank_epsilon_squared(x, g, iterations = 50)
    expect_equal(res$estimate, h / (30 - 1), tolerance = 1e-12)
})

test_that("rank_epsilon_squared estimate matches effectsize on 50 random datasets", {
    skip_if_not_installed("effectsize")
    withr::local_seed(11)
    for (i in 1:50) {
        g <- factor(sample(
            letters[1:sample(3:5, 1)],
            sample(c(20, 60, 150), 1),
            replace = TRUE
        ))
        x <- round(stats::rnorm(length(g), as.integer(g) * stats::runif(1)), 1)
        ours <- statplot:::rank_epsilon_squared(x, g, iterations = 10)
        ref <- effectsize::rank_epsilon_squared(
            x ~ g,
            data = data.frame(x, g),
            ci = NULL
        )
        expect_equal(
            ours$estimate,
            ref$rank_epsilon_squared,
            tolerance = 1e-12
        )
    }
})

test_that("rank_epsilon_squared bootstrap CI agrees with effectsize", {
    skip_if_not_installed("effectsize")
    skip_if_not_installed("boot")
    skip_on_cran()
    withr::local_seed(5)
    # Both are percentile bootstraps resampling within groups; with many
    # resamples the remaining difference is Monte Carlo error
    for (i in 1:3) {
        g <- factor(sample(letters[1:4], 60, replace = TRUE))
        x <- stats::rnorm(60, as.integer(g) * 0.4)
        ours <- statplot:::rank_epsilon_squared(x, g, iterations = 4000)
        ref <- effectsize::rank_epsilon_squared(
            x ~ g,
            data = data.frame(x, g),
            alternative = "two.sided",
            iterations = 4000
        )
        expect_lt(abs(ours$ci_low - ref$CI_low), 0.03)
        expect_lt(abs(ours$ci_high - ref$CI_high), 0.03)
    }
})

test_that("rank_epsilon_squared is reproducible with a seed and bounded in [0, 1]", {
    g <- rep(c("a", "b", "c"), each = 10)
    x <- c(1:10, 6:15, 11:20)
    res1 <- withr::with_seed(3, statplot:::rank_epsilon_squared(x, g))
    res2 <- withr::with_seed(3, statplot:::rank_epsilon_squared(x, g))
    expect_identical(res1, res2)
    expect_gte(res1$ci_low, 0)
    expect_lte(res1$ci_high, 1)
    expect_lte(res1$ci_low, res1$estimate)
    expect_gte(res1$ci_high, res1$estimate)
})

test_that("rank_epsilon_squared drops missing values and handles singleton groups", {
    g <- c(rep(c("a", "b", "c"), each = 6), "d", NA)
    x <- c(1:18, 50, 3)
    x[2] <- NA
    keep <- !is.na(x) & !is.na(g)
    h <- unname(stats::kruskal.test(x[keep], factor(g[keep]))$statistic)
    res <- withr::with_seed(
        1,
        statplot:::rank_epsilon_squared(x, g, iterations = 100)
    )
    expect_equal(res$estimate, h / (sum(keep) - 1), tolerance = 1e-12)
    expect_false(anyNA(unlist(res)))
})

test_that("rank_epsilon_squared validates its arguments", {
    expect_error(statplot:::rank_epsilon_squared("a", "b"))
    expect_error(statplot:::rank_epsilon_squared(1:3, c("a", "b")))
    expect_error(statplot:::rank_epsilon_squared(1:4, rep("a", 4)))
    expect_error(statplot:::rank_epsilon_squared(1:4, c("a", "a", "b", "b"), ci = 2))
})

# ---------------------------------------------------------------------------
# full_pvalue_matrix()
# ---------------------------------------------------------------------------

test_that("full_pvalue_matrix fills a pairwise p-value matrix symmetrically", {
    p <- matrix(
        c(0.01, 0.20, NA, 0.03),
        nrow = 2,
        dimnames = list(c("B", "C"), c("A", "B"))
    )
    expected <- matrix(
        c(1, 0.01, 0.20, 0.01, 1, 0.03, 0.20, 0.03, 1),
        nrow = 3,
        dimnames = list(c("A", "B", "C"), c("A", "B", "C"))
    )
    expect_equal(statplot:::full_pvalue_matrix(p), expected)
})

test_that("full_pvalue_matrix works on pairwise.wilcox.test output", {
    withr::local_seed(4)
    g <- factor(sample(LETTERS[1:5], 80, replace = TRUE))
    x <- stats::rnorm(80, as.integer(g) / 3)
    pw <- suppressWarnings(stats::pairwise.wilcox.test(x, g))$p.value
    full <- statplot:::full_pvalue_matrix(pw)
    expect_equal(dimnames(full), list(levels(g), levels(g)))
    expect_equal(full, t(full))
    expect_equal(unname(diag(full)), rep(1, 5))
    for (i in rownames(pw)) {
        for (j in colnames(pw)) {
            if (!is.na(pw[i, j])) expect_equal(full[i, j], pw[i, j])
        }
    }
})

test_that("full_pvalue_matrix output gives the same compact letters", {
    p <- matrix(
        c(0.001, 0.002, NA, 0.6),
        nrow = 2,
        dimnames = list(c("B", "C"), c("A", "B"))
    )
    letters_out <- multcompView::multcompLetters(
        statplot:::full_pvalue_matrix(p),
        Letters = LETTERS
    )$Letters
    expect_equal(letters_out, c(A = "A", B = "B", C = "B"))
})
