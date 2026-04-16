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

test_that("quarto_note validates color format to prevent injection", {
    # Colors with quotes, semicolons, or other special chars should fail
    expect_error(
        quarto_note("text", color = 'red"; font-weight:'),
        "`color` must be a valid CSS color"
    )
    expect_error(
        quarto_note("text", color = "#fff; background:"),
        "`color` must be a valid CSS color"
    )
    expect_error(
        quarto_note("text", color = "red'"),
        "`color` must be a valid CSS color"
    )
    # LaTeX-specific injection attempts should be rejected
    expect_error(
        quarto_note("text", color = "Red}"),
        "`color` must be a valid CSS color"
    )
    expect_error(
        quarto_note("text", color = "Red\\"),
        "`color` must be a valid CSS color"
    )
    expect_error(
        quarto_note("text", color = "Red/"),
        "`color` must be a valid CSS color"
    )
    # Valid hex and named colors should work
    expect_silent(quarto_note("text", color = "#fff"))
    expect_silent(quarto_note("text", color = "#ffffff"))
    expect_silent(quarto_note("text", color = "#ffffff00"))
    expect_silent(quarto_note("text", color = "dark-red"))
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

test_that("quarto_note validates style format to prevent injection", {
    # Styles with quotes or other special chars should fail
    expect_error(
        quarto_note("text", style = 'Comment"'),
        "`style` must contain only alphanumeric characters"
    )
    expect_error(
        quarto_note("text", style = "Comment;"),
        "`style` must contain only alphanumeric characters"
    )
    expect_error(
        quarto_note("text", style = "Comment&test"),
        "`style` must contain only alphanumeric characters"
    )
    # Valid styles with letters, numbers, spaces, and hyphens should work
    with_mocked_bindings(
        is_html_output = function() TRUE,
        is_latex_output = function() FALSE,
        .package = "knitr",
        {
            expect_silent(quarto_note("text", style = "Comment"))
            expect_silent(quarto_note("text", style = "my comment"))
            expect_silent(quarto_note("text", style = "my-comment"))
            expect_silent(quarto_note("text", style = "Note123"))
        }
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

test_that("quarto_note escapes LaTeX special characters", {
    with_mocked_bindings(
        is_html_output = function() FALSE,
        is_latex_output = function() TRUE,
        .package = "knitr",
        {
            # Test various LaTeX special characters
            result <- quarto_note("Test % & _", color = "Red")
            output <- as.character(result)
            # These characters should be escaped by knitr's escape_latex
            # The output contains: Test \% \& \_
            expect_match(output, "\\%", fixed = TRUE)
            expect_match(output, "\\&", fixed = TRUE)
            expect_match(output, "\\_", fixed = TRUE)
        }
    )
})

test_that("quarto_note prevents LaTeX injection via text", {
    with_mocked_bindings(
        is_html_output = function() FALSE,
        is_latex_output = function() TRUE,
        .package = "knitr",
        {
            # Text with potentially malicious LaTeX should be escaped
            result <- quarto_note("Test \\textbf{injection}", color = "Red")
            output <- as.character(result)
            # The backslash and braces should be escaped by knitr's escape_latex
            # Output should contain: Test \textbackslash{}textbf\{injection\}
            expect_match(output, "textbackslash", fixed = TRUE)
            expect_match(output, "\\{", fixed = TRUE)
            expect_match(output, "\\}", fixed = TRUE)
        }
    )
})

test_that("quarto_note prevents LaTeX color injection", {
    with_mocked_bindings(
        is_html_output = function() FALSE,
        is_latex_output = function() TRUE,
        .package = "knitr",
        {
            # LaTeX-breaking characters in color should be rejected
            expect_error(
                quarto_note("Test note", color = "Red}"),
                "`color` must be a valid CSS color"
            )
            expect_error(
                quarto_note("Test note", color = "Red\\"),
                "`color` must be a valid CSS color"
            )
            expect_error(
                quarto_note("Test note", color = "Red/extra"),
                "`color` must be a valid CSS color"
            )
            # Safe colors should work
            result <- quarto_note("Test note", color = "Red")
            expect_match(
                as.character(result),
                "\\textcolor{Red}",
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

test_that("quarto_note escapes ::: in Word/Quarto output", {
    with_mocked_bindings(
        is_html_output = function() FALSE,
        is_latex_output = function() FALSE,
        .package = "knitr",
        {
            # Text containing ::: should be escaped to prevent div closure
            result <- quarto_note("This is :::: note", style = "Comment")
            output <- as.character(result)
            # The ::: should be escaped to \:\:\:
            expect_match(output, "\\:\\:\\:", fixed = TRUE)
            # Verify it's within the div, not the closing delimiter
            expect_match(output, "This is \\:\\:\\:: note", fixed = TRUE)
        }
    )
})

test_that("quarto_note prevents Word document malformation", {
    with_mocked_bindings(
        is_html_output = function() FALSE,
        is_latex_output = function() FALSE,
        .package = "knitr",
        {
            # Multiple ::: sequences should all be escaped
            result <- quarto_note("Start ::: middle ::: end", style = "Note")
            output <- as.character(result)
            # Both ::: in the text should be escaped
            expect_match(
                output,
                "Start \\:\\:\\: middle \\:\\:\\: end",
                fixed = TRUE
            )
            # Should end with the closing :::
            expect_match(output, "\\n:::\\n?$", fixed = FALSE)
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
