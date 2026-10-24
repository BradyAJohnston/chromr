# Interpolate Values in a Column

Performs linear interpolation on values in a specified column of a data
frame. Uses unique values in the column as anchor points for
interpolation.

## Usage

``` r
interpolate_column(data, col)
```

## Arguments

- data:

  A data frame containing the column to interpolate

- col:

  Name of the column to interpolate (unquoted)

## Value

A data frame with the interpolated column

## Examples

``` r
if (FALSE) { # \dontrun{
df <- data.frame(x = c(1, NA, 3, NA, 5))
interpolate_column(df, x)  # Interpolates NA values in column x
} # }
```
