# Render a collaborator comment for Quarto output

Format reviewer or collaborator notes so they render as colored text in
HTML, as colored LaTeX in PDF, and as a Word custom style in Docx.

## Usage

``` r
quarto_note(text, color = "Green", style = "Comment")
```

## Arguments

- text:

  Character scalar containing the comment text.

- color:

  Character scalar for the HTML/PDF color name.

- style:

  Character scalar for the Word custom style name.

## Details

When knitting to Word, the referenced Word template must define a
paragraph style matching the `style` argument. The default
`style = "Comment"` requires a Word style named "Comment" in the target
document or template.

## Examples

``` r
if (knitr::is_html_output()) {
  quarto_note("Please review this section")
}
```
