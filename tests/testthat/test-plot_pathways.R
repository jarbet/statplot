# ---------------------------------------------------------------------------
# Argument validation: plot_pathways()
# ---------------------------------------------------------------------------

# These tests exercise only the input-validation layer, so they do not need
# enrichplot / clusterProfiler to be installed.

make_fake_gsea <- function() {
    structure(list(), class = "gseaResult")
}

make_fc <- function(n = 10) {
    stats::setNames(stats::rnorm(n), paste0("GENE", seq_len(n)))
}

test_that("plot_pathways() errors when effect_size is not a named numeric", {
    expect_error(
        plot_pathways(make_fake_gsea(), effect_size = 1:5),
        "effect_size must be a named numeric vector"
    )
    expect_error(
        plot_pathways(make_fake_gsea(), effect_size = c(a = "x", b = "y")),
        "effect_size must be a named numeric vector"
    )
})

test_that("plot_pathways() errors when show_pathways is not a positive whole number", {
    fc <- make_fc()
    expect_error(
        plot_pathways(make_fake_gsea(), effect_size = fc, show_pathways = 0),
        "show_pathways"
    )
    expect_error(
        plot_pathways(make_fake_gsea(), effect_size = fc, show_pathways = -1),
        "show_pathways"
    )
    expect_error(
        plot_pathways(make_fake_gsea(), effect_size = fc, show_pathways = 2.5),
        "show_pathways"
    )
    expect_error(
        plot_pathways(
            make_fake_gsea(),
            effect_size = fc,
            show_pathways = c(3L, 5L)
        ),
        "show_pathways"
    )
    expect_error(
        plot_pathways(make_fake_gsea(), effect_size = fc, show_pathways = Inf),
        "show_pathways"
    )
})

test_that("plot_pathways() errors when effect_size_threshold is invalid", {
    fc <- make_fc()
    expect_error(
        plot_pathways(
            make_fake_gsea(),
            effect_size = fc,
            effect_size_threshold = -1
        ),
        "effect_size_threshold"
    )
    expect_error(
        plot_pathways(
            make_fake_gsea(),
            effect_size = fc,
            effect_size_threshold = c(1, 2)
        ),
        "effect_size_threshold"
    )
    expect_error(
        plot_pathways(
            make_fake_gsea(),
            effect_size = fc,
            effect_size_threshold = Inf
        ),
        "effect_size_threshold"
    )
})

test_that("plot_pathways() errors when gene_node_size is invalid", {
    fc <- make_fc()
    expect_error(
        plot_pathways(make_fake_gsea(), effect_size = fc, gene_node_size = 0),
        "gene_node_size"
    )
    expect_error(
        plot_pathways(make_fake_gsea(), effect_size = fc, gene_node_size = -1),
        "gene_node_size"
    )
    expect_error(
        plot_pathways(
            make_fake_gsea(),
            effect_size = fc,
            gene_node_size = c(0.5, 1)
        ),
        "gene_node_size"
    )
})

test_that("plot_pathways() errors when line_size is invalid", {
    fc <- make_fc()
    expect_error(
        plot_pathways(make_fake_gsea(), effect_size = fc, line_size = 0),
        "line_size"
    )
    expect_error(
        plot_pathways(make_fake_gsea(), effect_size = fc, line_size = -0.1),
        "line_size"
    )
    expect_error(
        plot_pathways(
            make_fake_gsea(),
            effect_size = fc,
            line_size = c(0.5, 1)
        ),
        "line_size"
    )
})

test_that("plot_pathways() errors when pathway_label_size is invalid", {
    fc <- make_fc()
    expect_error(
        plot_pathways(
            make_fake_gsea(),
            effect_size = fc,
            pathway_label_size = 0
        ),
        "pathway_label_size"
    )
    expect_error(
        plot_pathways(
            make_fake_gsea(),
            effect_size = fc,
            pathway_label_size = -2
        ),
        "pathway_label_size"
    )
    expect_error(
        plot_pathways(
            make_fake_gsea(),
            effect_size = fc,
            pathway_label_size = c(3, 4)
        ),
        "pathway_label_size"
    )
})

test_that("plot_pathways() errors when gene_label_size is invalid", {
    fc <- make_fc()
    expect_error(
        plot_pathways(make_fake_gsea(), effect_size = fc, gene_label_size = 0),
        "gene_label_size"
    )
    expect_error(
        plot_pathways(make_fake_gsea(), effect_size = fc, gene_label_size = -1),
        "gene_label_size"
    )
    expect_error(
        plot_pathways(
            make_fake_gsea(),
            effect_size = fc,
            gene_label_size = c(2, 3)
        ),
        "gene_label_size"
    )
})

test_that("plot_pathways() errors when legend_fixed_dot_size is invalid", {
    fc <- make_fc()
    expect_error(
        plot_pathways(
            make_fake_gsea(),
            effect_size = fc,
            legend_fixed_dot_size = 0
        ),
        "legend_fixed_dot_size"
    )
    expect_error(
        plot_pathways(
            make_fake_gsea(),
            effect_size = fc,
            legend_fixed_dot_size = -50
        ),
        "legend_fixed_dot_size"
    )
    expect_error(
        plot_pathways(
            make_fake_gsea(),
            effect_size = fc,
            legend_fixed_dot_size = Inf
        ),
        "legend_fixed_dot_size"
    )
    expect_error(
        plot_pathways(
            make_fake_gsea(),
            effect_size = fc,
            legend_fixed_dot_size = "100"
        ),
        "legend_fixed_dot_size"
    )
})

test_that("plot_pathways() errors when pathway_cats is not a named character vector", {
    fc <- make_fc()
    # Unnamed character vector
    expect_error(
        plot_pathways(
            make_fake_gsea(),
            effect_size = fc,
            pathway_cats = c("Cat1", "Cat2"),
            pathway_cat_colors = c(Cat1 = "red", Cat2 = "blue")
        ),
        "pathway_cats"
    )
    # Non-character (numeric)
    expect_error(
        plot_pathways(
            make_fake_gsea(),
            effect_size = fc,
            pathway_cats = c(A = 1L, B = 2L),
            pathway_cat_colors = c(`1` = "red", `2` = "blue")
        ),
        "pathway_cats"
    )
    # Empty named character vector
    expect_error(
        plot_pathways(
            make_fake_gsea(),
            effect_size = fc,
            pathway_cats = stats::setNames(character(0), character(0)),
            pathway_cat_colors = c(Cat1 = "red")
        ),
        "pathway_cats"
    )
})

test_that("plot_pathways() errors when pathway_cat_colors is not a named character vector", {
    fc <- make_fc()
    # Unnamed character vector
    expect_error(
        plot_pathways(
            make_fake_gsea(),
            effect_size = fc,
            pathway_cats = c(A = "Cat1"),
            pathway_cat_colors = c("red", "blue")
        ),
        "pathway_cat_colors"
    )
    # Non-character (numeric)
    expect_error(
        plot_pathways(
            make_fake_gsea(),
            effect_size = fc,
            pathway_cats = c(A = "Cat1"),
            pathway_cat_colors = c(Cat1 = 1L)
        ),
        "pathway_cat_colors"
    )
})

test_that("plot_pathways() errors when only one of pathway_cats / pathway_cat_colors is provided", {
    fc <- make_fc()
    expect_error(
        plot_pathways(
            make_fake_gsea(),
            effect_size = fc,
            pathway_cats = c(A = "Cat1")
        ),
        "pathway_cats and pathway_cat_colors"
    )
    expect_error(
        plot_pathways(
            make_fake_gsea(),
            effect_size = fc,
            pathway_cat_colors = c(Cat1 = "red")
        ),
        "pathway_cats and pathway_cat_colors"
    )
})

test_that("plot_pathways() errors when pathway_cats value not in names(pathway_cat_colors)", {
    fc <- make_fc()
    expect_error(
        plot_pathways(
            make_fake_gsea(),
            effect_size = fc,
            pathway_cats = c(A = "Missing"),
            pathway_cat_colors = c(Cat1 = "red")
        ),
        "pathway_cats"
    )
})

test_that("plot_pathways() errors when legend_pathway_fill_title is invalid", {
    fc <- make_fc()
    expect_error(
        plot_pathways(
            make_fake_gsea(),
            effect_size = fc,
            legend_pathway_fill_title = c("A", "B")
        ),
        "legend_pathway_fill_title"
    )
    expect_error(
        plot_pathways(
            make_fake_gsea(),
            effect_size = fc,
            legend_pathway_fill_title = 42
        ),
        "legend_pathway_fill_title"
    )
})

# ---------------------------------------------------------------------------
# ggplot2 uses a single Scale object for both aesthetic mapping (dot colours)
# and legend construction, so verifying the scale's properties is equivalent
# to verifying that the legend and dots are aligned.
#
# Failure modes these tests guard against:
#  - limits / breaks not propagated to the scale → legend shows wrong range
#  - oob left as censor (default) instead of squish → out-of-bounds genes
#    render as NA (grey) while the legend still shows the squished colour
#  - Two colour scales present → ggplot2 silently drops one, legend and
#    dots could use different scales

local({
    skip_if_not_installed("enrichplot")
    skip_if_not_installed("clusterProfiler")

    data(hallmark_t2g, package = "statplot")
    set.seed(1)
    all_genes <- unique(hallmark_t2g$gene)
    gene_vec <- setNames(rnorm(length(all_genes)), all_genes)
    res <- run_gsea(gene_vec, term2gene = hallmark_t2g)

    # Extract the single colour scale; also asserts there is exactly one.
    .get_colour_scale <- function(p) {
        idx <- which(vapply(
            p$scales$scales,
            function(s) "colour" %in% s$aesthetics,
            logical(1)
        ))
        expect_length(idx, 1L) # exactly one colour scale → legend/dots share it
        p$scales$scales[[idx[1L]]]
    }

    # Functionally test that oob squishes (not censors) values outside limits.
    .oob_squishes <- function(sc, lims) {
        out <- sc$oob(c(lims[1] - 100, lims[2] + 100), range = lims)
        identical(out, lims)
    }

    # ------------------------------------------------------------------
    # 1. Custom diverging scale (color_low / color_mid / color_high path)
    # ------------------------------------------------------------------
    test_that("diverging scale: limits, breaks, and squish oob are set on scale", {
        p <- plot_pathways(
            gsea_result = res$gsea_result,
            effect_size = res$gene_vec,
            show_pathways = 3,
            color_low = "blue",
            color_mid = "white",
            color_high = "red",
            colorkey_limits = c(-2, 2),
            colorkey_breaks = c(-2, -1, 0, 1, 2)
        )
        sc <- .get_colour_scale(p)
        expect_equal(sc$limits, c(-2, 2))
        expect_equal(sc$breaks, c(-2, -1, 0, 1, 2))
        expect_true(
            .oob_squishes(sc, c(-2, 2)),
            label = "oob must squish, not censor, out-of-bounds values"
        )
    })

    test_that("diverging scale: palette maps limits and midpoint to specified colours", {
        p <- plot_pathways(
            gsea_result = res$gsea_result,
            effect_size = res$gene_vec,
            show_pathways = 3,
            color_low = "blue",
            color_mid = "white",
            color_high = "red",
            colorkey_limits = c(-2, 2)
        )
        sc <- .get_colour_scale(p)
        sc$train(c(-2, 2))
        expect_equal(sc$map(-2), "#0000FF") # color_low  = blue
        expect_equal(sc$map(0), "#FFFFFF") # color_mid  = white  (midpoint default = 0)
        expect_equal(sc$map(2), "#FF0000") # color_high = red
    })

    test_that("squish: values beyond colorkey_limits map to the same colour as the limit", {
        lims <- c(-1, 1)
        p <- plot_pathways(
            gsea_result = res$gsea_result,
            effect_size = res$gene_vec,
            show_pathways = 3,
            color_low = "blue",
            color_mid = "white",
            color_high = "red",
            colorkey_limits = lims
        )
        sc <- .get_colour_scale(p)
        sc$train(res$gene_vec)
        expect_equal(sc$map(lims[1] - 10), sc$map(lims[1]))
        expect_equal(sc$map(lims[2] + 10), sc$map(lims[2]))
    })

    # ------------------------------------------------------------------
    # 2. Custom sequential scale (color_low + color_high, no color_mid)
    # ------------------------------------------------------------------
    test_that("sequential scale: limits, breaks, and squish oob are set on scale", {
        p <- plot_pathways(
            gsea_result = res$gsea_result,
            effect_size = res$gene_vec,
            show_pathways = 3,
            color_low = "white",
            color_high = "red",
            colorkey_limits = c(0, 3),
            colorkey_breaks = c(0, 1, 2, 3)
        )
        sc <- .get_colour_scale(p)
        expect_equal(sc$limits, c(0, 3))
        expect_equal(sc$breaks, c(0, 1, 2, 3))
        expect_true(
            .oob_squishes(sc, c(0, 3)),
            label = "oob must squish, not censor, out-of-bounds values"
        )
    })

    # ------------------------------------------------------------------
    # 3. In-place modification only (colorkey_limits / colorkey_breaks,
    #    no custom colour arguments) — cnetplot palette is preserved
    # ------------------------------------------------------------------
    test_that("in-place path: limits and breaks are patched on the cnetplot scale", {
        p <- plot_pathways(
            gsea_result = res$gsea_result,
            effect_size = res$gene_vec,
            show_pathways = 3,
            colorkey_limits = c(-1.5, 1.5),
            colorkey_breaks = c(-1.5, 0, 1.5)
        )
        sc <- .get_colour_scale(p)
        expect_equal(sc$limits, c(-1.5, 1.5))
        expect_equal(sc$breaks, c(-1.5, 0, 1.5))
        expect_true(
            .oob_squishes(sc, c(-1.5, 1.5)),
            label = "oob must squish, not censor, out-of-bounds values"
        )
    })

    test_that("in-place path: out-of-bounds values are squished, not censored to NA", {
        # The default cnetplot scale censors (oob → NA); our patch must replace
        # that with squish so genes outside colorkey_limits still render with colour.
        lims <- c(-0.5, 0.5)
        p <- plot_pathways(
            gsea_result = res$gsea_result,
            effect_size = res$gene_vec,
            show_pathways = 3,
            colorkey_limits = lims
        )
        sc <- .get_colour_scale(p)
        out <- sc$oob(c(lims[1] - 5, lims[2] + 5), range = lims)
        expect_false(anyNA(out), label = "squished values must not be NA")
        expect_equal(out, lims)
    })

    # ------------------------------------------------------------------
    # 4. legend_fixed_dot_size: size scale breaks, limits, and oob
    # ------------------------------------------------------------------
    test_that("legend_fixed_dot_size: breaks and limits are set on the size scale", {
        breaks_in <- c(50, 100, 200)
        p <- plot_pathways(
            gsea_result = res$gsea_result,
            effect_size = res$gene_vec,
            show_pathways = 3,
            legend_fixed_dot_size = breaks_in
        )
        idx_s <- which(vapply(
            p$scales$scales,
            function(s) "size" %in% s$aesthetics,
            logical(1)
        ))
        expect_gte(length(idx_s), 1L)
        sc_s <- p$scales$scales[[idx_s[1L]]]
        expect_equal(sc_s$breaks, breaks_in)
        expect_equal(sc_s$limits, range(breaks_in))
    })

    test_that("legend_fixed_dot_size: values outside range are squished, not NA", {
        breaks_in <- c(50, 100, 200)
        p <- plot_pathways(
            gsea_result = res$gsea_result,
            effect_size = res$gene_vec,
            show_pathways = 3,
            legend_fixed_dot_size = breaks_in
        )
        idx_s <- which(vapply(
            p$scales$scales,
            function(s) "size" %in% s$aesthetics,
            logical(1)
        ))
        sc_s <- p$scales$scales[[idx_s[1L]]]
        lims <- range(breaks_in)
        out <- sc_s$oob(c(lims[1] - 100, lims[2] + 100), range = lims)
        expect_false(anyNA(out), label = "squished values must not be NA")
        expect_equal(out, lims)
    })

    # ------------------------------------------------------------------
    # 5. pathway_cats / pathway_cat_colors: a fill scale is added to the plot
    # ------------------------------------------------------------------
    test_that("pathway_cats: fill identity scale is present with correct breaks", {
        top_ids <- utils::head(res$gsea_result@result$ID, 3)
        pathway_cats <- stats::setNames(
            c("Signaling", "Immune", "Development"),
            top_ids
        )
        cat_colors <- c(
            Signaling = "#e41a1c",
            Immune = "#4daf4a",
            Development = "#377eb8"
        )
        p <- plot_pathways(
            gsea_result = res$gsea_result,
            effect_size = res$gene_vec,
            show_pathways = 3,
            pathway_cats = pathway_cats,
            pathway_cat_colors = cat_colors,
            legend_pathway_fill_title = "Test category"
        )
        idx_fill <- which(vapply(
            p$scales$scales,
            function(s) "fill" %in% s$aesthetics,
            logical(1)
        ))
        expect_gte(length(idx_fill), 1L)
        sc_fill <- p$scales$scales[[idx_fill[1L]]]
        # breaks are the unique color values for the displayed categories;
        # compare as a set to avoid dependence on row-order in p$data
        expect_true(setequal(sc_fill$breaks, unname(cat_colors)))
        expect_equal(sc_fill$name, "Test category")
    })

    test_that("pathway_cats: legend shows deduplicated category labels", {
        top_ids <- utils::head(res$gsea_result@result$ID, 3)
        # Two pathways share the same category -> one legend entry
        pathway_cats <- stats::setNames(
            c("Signaling", "Signaling", "Development"),
            top_ids
        )
        cat_colors <- c(Signaling = "#e41a1c", Development = "#377eb8")
        p <- plot_pathways(
            gsea_result = res$gsea_result,
            effect_size = res$gene_vec,
            show_pathways = 3,
            pathway_cats = pathway_cats,
            pathway_cat_colors = cat_colors,
            legend_pathway_fill_title = "Category"
        )
        idx_fill <- which(vapply(
            p$scales$scales,
            function(s) "fill" %in% s$aesthetics,
            logical(1)
        ))
        sc_fill <- p$scales$scales[[idx_fill[1L]]]
        expect_length(sc_fill$breaks, 2L)
        # sort both to avoid dependence on the order cats_used is built
        expect_equal(sort(sc_fill$labels), sort(c("Signaling", "Development")))
        expect_equal(sc_fill$name, "Category")
    })

    test_that("pathway_cats: colour scale (gene fold-change) is unaffected", {
        top_ids <- utils::head(res$gsea_result@result$ID, 3)
        pathway_cats <- stats::setNames(
            c("Signaling", "Immune", "Development"),
            top_ids
        )
        cat_colors <- c(
            Signaling = "#e41a1c",
            Immune = "#4daf4a",
            Development = "#377eb8"
        )
        p_plain <- plot_pathways(
            gsea_result = res$gsea_result,
            effect_size = res$gene_vec,
            show_pathways = 3
        )
        p_fill <- plot_pathways(
            gsea_result = res$gsea_result,
            effect_size = res$gene_vec,
            show_pathways = 3,
            pathway_cats = pathway_cats,
            pathway_cat_colors = cat_colors
        )
        # Both plots must have exactly one colour scale
        get_colour_scales <- function(p) {
            which(vapply(
                p$scales$scales,
                function(s) "colour" %in% s$aesthetics,
                logical(1)
            ))
        }
        expect_length(get_colour_scales(p_plain), 1L)
        expect_length(get_colour_scales(p_fill), 1L)
    })
})
