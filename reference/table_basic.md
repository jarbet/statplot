# Create a basic table using tinytable

Wrapper around tinytable functions to create a simple, clean table with
a bold header row and optional column-level rounding control.

## Usage

``` r
table_basic(
  d,
  digits = NULL,
  replace_missing = "",
  commas_large_numbers = TRUE,
  use_column_labels = TRUE
)
```

## Arguments

- d:

  data.frame. Dataset to display as a table.

- digits:

  numeric or list. Rounding specification for columns:

  - If a single numeric value, applied to all numeric columns.

  - If a numeric vector, must have length equal to the number of columns
    in `d`. Non-numeric columns ignore their corresponding value.
    Providing a vector with any other length will raise an error.

  - If a list, a named list mapping column names to number of digits
    (e.g., `list(age = 0, salary = 2)`). Unnamed or missing columns keep
    their original precision.

- replace_missing:

  character(1). String to replace missing values with. Default is empty
  string ("").

- commas_large_numbers:

  logical(1). If TRUE (default), format numeric values with commas for
  thousands (e.g., 1,234.56). If FALSE, no comma formatting.

- use_column_labels:

  logical(1). If TRUE, use the `label` attribute of columns if
  available, otherwise use column names. Passed to
  `tinytable::tt(colnames = ...)`.

## Value

A tinytable object

## Examples

``` r
df <- data.frame(
  name = c("Alice", "Bob", NA),
  age = c(25.5, 30.2, 35.8),
  salary = c(50000.123, 60000.456, NA)
)

# Round specific columns by name
table_basic(df, digits = list(age = 1, salary = 0))
#> 
#> +----------+---------+------------+
#> | **name** | **age** | **salary** |
#> +==========+=========+============+
#> | Alice    | 25.5    | 50,000     |
#> +----------+---------+------------+
#> | Bob      | 30.2    | 60,000     |
#> +----------+---------+------------+
#> |          | 35.8    |            |
#> +----------+---------+------------+ 

# Round by column position (ignores non-numeric columns)
table_basic(df, digits = c(0, 1, 2))
#> 
#> +----------+---------+------------+
#> | **name** | **age** | **salary** |
#> +==========+=========+============+
#> | Alice    | 25.5    | 50,000.12  |
#> +----------+---------+------------+
#> | Bob      | 30.2    | 60,000.46  |
#> +----------+---------+------------+
#> |          | 35.8    |            |
#> +----------+---------+------------+ 

# Disable comma formatting for large numbers
table_basic(df, digits = list(age = 1, salary = 0), commas_large_numbers = FALSE)
#> 
#> +----------+---------+------------+
#> | **name** | **age** | **salary** |
#> +==========+=========+============+
#> | Alice    | 25.5    | 50000      |
#> +----------+---------+------------+
#> | Bob      | 30.2    | 60000      |
#> +----------+---------+------------+
#> |          | 35.8    |            |
#> +----------+---------+------------+ 
```
