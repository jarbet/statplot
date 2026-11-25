# Create a basic interactive table using DT

Interactive counterpart of
[`table_basic()`](https://github.com/jarbet/statplot/reference/table_basic.md)
for HTML output: shows a data frame as a searchable, paged DT table with
a filter box above each column and no row-number column. Useful for long
tables (e.g. data-cleaning QC tables) that are too long to print in
full.

## Usage

``` r
table_basic_interactive(
  d,
  page_length = 10,
  use_column_labels = TRUE,
  filter = "top",
  options = list(),
  ...
)
```

## Arguments

- d:

  data.frame. Dataset to display as a table.

- page_length:

  numeric(1). Number of rows shown per page. Must be a positive whole
  number. Default is 10.

- use_column_labels:

  logical(1). If TRUE (default), use the `label` attribute of columns as
  headers where available (e.g. set with
  [`labelled::var_label()`](https://larmarange.github.io/labelled/reference/var_label.html)),
  otherwise use column names.

- filter:

  character(1). Where to show the column filters: `"top"` (default),
  `"bottom"` or `"none"`. Passed to `DT::datatable(filter = ...)`.

- options:

  list. DataTables options (see
  <https://rstudio.github.io/DT/options.html>), merged with the defaults
  `list(pageLength = page_length, autoWidth = TRUE)`: options given here
  are added, or replace a default of the same name. For example,
  `list(lengthMenu = c(5, 10, 20))` adds a menu for changing the page
  length.

- ...:

  Further arguments passed to
  [`DT::datatable()`](https://rdrr.io/pkg/DT/man/datatable.html), e.g.
  `caption`, `class` (e.g. `"cell-border stripe"`), `style` or
  `extensions`. `data`, `colnames` and `rownames` are set by this
  function and cannot be passed.

## Value

A `datatables` htmlwidget. It can be piped into DT's formatting
functions, such as
[`DT::formatRound()`](https://rdrr.io/pkg/DT/man/formatCurrency.html) or
[`DT::formatStyle()`](https://rdrr.io/pkg/DT/man/formatCurrency.html).

## Examples

``` r
df <- data.frame(
  name = c("Alice", "Bob", "Carol"),
  age = c(25.12, 30.55, 35.91)
)
attr(df$age, "label") <- "Age, years"

table_basic_interactive(df)

{"x":{"filter":"top","vertical":false,"filterHTML":"<tr>\n  <td data-type=\"character\" style=\"vertical-align: top;\">\n    <div class=\"form-group has-feedback\" style=\"margin-bottom: auto;\">\n      <input type=\"search\" placeholder=\"All\" class=\"form-control\" style=\"width: 100%;\"/>\n      <span class=\"glyphicon glyphicon-remove-circle form-control-feedback\"><\/span>\n    <\/div>\n  <\/td>\n  <td data-type=\"number\" style=\"vertical-align: top;\">\n    <div class=\"form-group has-feedback\" style=\"margin-bottom: auto;\">\n      <input type=\"search\" placeholder=\"All\" class=\"form-control\" style=\"width: 100%;\"/>\n      <span class=\"glyphicon glyphicon-remove-circle form-control-feedback\"><\/span>\n    <\/div>\n    <div style=\"display: none;position: absolute;width: 200px;opacity: 1\">\n      <div data-min=\"25.12\" data-max=\"35.91\" data-scale=\"2\"><\/div>\n      <span style=\"float: left;\"><\/span>\n      <span style=\"float: right;\"><\/span>\n    <\/div>\n  <\/td>\n<\/tr>","data":[["Alice","Bob","Carol"],[25.12,30.55,35.91]],"container":"<table class=\"display\">\n  <thead>\n    <tr>\n      <th>name<\/th>\n      <th>Age, years<\/th>\n    <\/tr>\n  <\/thead>\n<\/table>","options":{"pageLength":10,"autoWidth":true,"columnDefs":[{"className":"dt-right","targets":1},{"name":"name","targets":0},{"name":"age","targets":1}],"order":[],"orderClasses":false,"orderCellsTop":true}},"evals":[],"jsHooks":[]}
# Show 2 rows per page and use column names as headers
table_basic_interactive(df, page_length = 2, use_column_labels = FALSE)

{"x":{"filter":"top","vertical":false,"filterHTML":"<tr>\n  <td data-type=\"character\" style=\"vertical-align: top;\">\n    <div class=\"form-group has-feedback\" style=\"margin-bottom: auto;\">\n      <input type=\"search\" placeholder=\"All\" class=\"form-control\" style=\"width: 100%;\"/>\n      <span class=\"glyphicon glyphicon-remove-circle form-control-feedback\"><\/span>\n    <\/div>\n  <\/td>\n  <td data-type=\"number\" style=\"vertical-align: top;\">\n    <div class=\"form-group has-feedback\" style=\"margin-bottom: auto;\">\n      <input type=\"search\" placeholder=\"All\" class=\"form-control\" style=\"width: 100%;\"/>\n      <span class=\"glyphicon glyphicon-remove-circle form-control-feedback\"><\/span>\n    <\/div>\n    <div style=\"display: none;position: absolute;width: 200px;opacity: 1\">\n      <div data-min=\"25.12\" data-max=\"35.91\" data-scale=\"2\"><\/div>\n      <span style=\"float: left;\"><\/span>\n      <span style=\"float: right;\"><\/span>\n    <\/div>\n  <\/td>\n<\/tr>","data":[["Alice","Bob","Carol"],[25.12,30.55,35.91]],"container":"<table class=\"display\">\n  <thead>\n    <tr>\n      <th>name<\/th>\n      <th>age<\/th>\n    <\/tr>\n  <\/thead>\n<\/table>","options":{"pageLength":2,"autoWidth":true,"columnDefs":[{"className":"dt-right","targets":1},{"name":"name","targets":0},{"name":"age","targets":1}],"order":[],"orderClasses":false,"orderCellsTop":true,"lengthMenu":[2,10,25,50,100]}},"evals":[],"jsHooks":[]}
# No column filters, a page-length menu and a styled table with a caption
table_basic_interactive(
  df,
  filter = "none",
  options = list(lengthMenu = c(2, 5, 10)),
  class = "cell-border stripe",
  caption = "Participants"
)

{"x":{"filter":"none","vertical":false,"caption":"<caption>Participants<\/caption>","data":[["Alice","Bob","Carol"],[25.12,30.55,35.91]],"container":"<table class=\"cell-border stripe\">\n  <thead>\n    <tr>\n      <th>name<\/th>\n      <th>Age, years<\/th>\n    <\/tr>\n  <\/thead>\n<\/table>","options":{"pageLength":10,"autoWidth":true,"lengthMenu":[2,5,10],"columnDefs":[{"className":"dt-right","targets":1},{"name":"name","targets":0},{"name":"age","targets":1}],"order":[],"orderClasses":false}},"evals":[],"jsHooks":[]}
# Round a column with DT's formatting functions
table_basic_interactive(df) |>
  DT::formatRound("age", digits = 1)

{"x":{"filter":"top","vertical":false,"filterHTML":"<tr>\n  <td data-type=\"character\" style=\"vertical-align: top;\">\n    <div class=\"form-group has-feedback\" style=\"margin-bottom: auto;\">\n      <input type=\"search\" placeholder=\"All\" class=\"form-control\" style=\"width: 100%;\"/>\n      <span class=\"glyphicon glyphicon-remove-circle form-control-feedback\"><\/span>\n    <\/div>\n  <\/td>\n  <td data-type=\"number\" style=\"vertical-align: top;\">\n    <div class=\"form-group has-feedback\" style=\"margin-bottom: auto;\">\n      <input type=\"search\" placeholder=\"All\" class=\"form-control\" style=\"width: 100%;\"/>\n      <span class=\"glyphicon glyphicon-remove-circle form-control-feedback\"><\/span>\n    <\/div>\n    <div style=\"display: none;position: absolute;width: 200px;opacity: 1\">\n      <div data-min=\"25.12\" data-max=\"35.91\" data-scale=\"2\"><\/div>\n      <span style=\"float: left;\"><\/span>\n      <span style=\"float: right;\"><\/span>\n    <\/div>\n  <\/td>\n<\/tr>","data":[["Alice","Bob","Carol"],[25.12,30.55,35.91]],"container":"<table class=\"display\">\n  <thead>\n    <tr>\n      <th>name<\/th>\n      <th>Age, years<\/th>\n    <\/tr>\n  <\/thead>\n<\/table>","options":{"pageLength":10,"autoWidth":true,"columnDefs":[{"targets":1,"render":"function(data, type, row, meta) {\n    return type !== 'display' ? data : DTWidget.formatRound(data, 1, 3, \",\", \".\", null);\n  }"},{"className":"dt-right","targets":1},{"name":"name","targets":0},{"name":"age","targets":1}],"order":[],"orderClasses":false,"orderCellsTop":true}},"evals":["options.columnDefs.0.render"],"jsHooks":[]}
```
