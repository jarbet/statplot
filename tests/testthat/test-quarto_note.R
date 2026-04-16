test_that("quarto_note validates text argument", {
    expect_error(
        quarto_note(c("text1", "text2")),
        "`text` must be a length-one character string"
    )
    expect_error(
        quarto_note(123),
        "`text` must be a length-one character string"
    )
    expect_error(
        quarto_note(NULL),
        "`text` must be a length-one character string"
    )
})

test_that("quarto_note validates color argument", {
    expect_error(
        quarto_note("text", color = c("red", "blue")),
        "`color` must be a length-one character string"
    )
    expect_error(
        quarto_note("text", color = 123),
        "`color` must be a length-one character string"
    )
})

test_that("quarto_note validates style argument", {
    expect_error(
        quarto_note("text", style = c("Comment", "Note")),
        "`style` must be a length-one character string"
    )
    expect_error(
        quarto_note("text", style = 123),
        "`style` must be a length-one character string"
    )
})

test_that("quarto_note generates HTML output", {
    with_mocked_bindings(
        is_html_output = function() TRUE,
        is_latex_output = function() FALSE,
        .package = "knitr",
        {
            result <- quarto_note("Test note", color = "red")
            expect_s3_class(result, "knit_asis")
            expect_match(
                as.character(result),
                '<span style="color:red; font-weight: bold;">Test note</span>',
                fixed = TRUE
            )
        }
    )
})

test_that("quarto_note escapes HTML special characters", {
    with_mocked_bindings(
        is_html_output = function() TRUE,
        is_latex_output = function() FALSE,
        .package = "knitr",
        {
            result <- quarto_note("Test & <note>", color = "blue")
            expect_match(
                as.character(result),
                "Test &amp; &lt;note&gt;",
                fixed = TRUE
            )
        }
    )
})

test_that("quarto_note generates LaTeX output", {
    with_mocked_bindings(
        is_html_output = function() FALSE,
        is_latex_output = function() TRUE,
        .package = "knitr",
        {
            result <- quarto_note("Test note", color = "Red")
            expect_s3_class(result, "knit_asis")
            expect_match(
                as.character(result),
                "\\textbf",
                fixed = TRUE
            )
            expect_match(
                as.character(result),
                "\\textcolor",
                fixed = TRUE
            )
        }
    )
})

test_that("quarto_note generates Word output", {
    with_mocked_bindings(
        is_html_output = function() FALSE,
        is_latex_output = function() FALSE,
        .package = "knitr",
        {
            result <- quarto_note("Test note", style = "mynote")
            expect_s3_class(result, "knit_asis")
            expect_match(
                as.character(result),
                'custom-style="mynote"',
                fixed = TRUE
            )
            expect_match(
                as.character(result),
                "Test note",
                fixed = TRUE
            )
        }
    )
})

test_that("quarto_note uses default arguments", {
    with_mocked_bindings(
        is_html_output = function() TRUE,
        is_latex_output = function() FALSE,
        .package = "knitr",
        {
            result <- quarto_note("Test")
            expect_match(
                as.character(result),
                'color:Green',
                fixed = TRUE
            )
        }
    )
})
