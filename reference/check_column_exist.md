# Check if Column Exists in Data Frame

Checks for the presence of a column in a data frame, ignoring case
sensitivity in the column name.

## Usage

``` r
check_column_exist(data, name)
```

## Arguments

- data:

  A data frame to check

- name:

  A character string specifying the column name to look for

## Value

Logical value: TRUE if the column exists (ignoring case), FALSE
otherwise

## Examples

``` r
if (FALSE) { # \dontrun{
df <- data.frame(Name = 1:3, Value = 4:6)
check_column_exist(df, "name")  # Returns TRUE
check_column_exist(df, "NAME")  # Returns TRUE
check_column_exist(df, "age")   # Returns FALSE
} # }
```
