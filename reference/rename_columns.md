# Standardize Column Names

Renames specific columns in a data frame to standardized names. Current
mappings: volume -\> Volume, time -\> Time, fraction -\> Fraction

## Usage

``` r
rename_columns(data)
```

## Arguments

- data:

  A data frame whose columns need to be renamed

## Value

A data frame with standardized column names

## Examples

``` r
if (FALSE) { # \dontrun{
df <- data.frame(volume = 1:3, time = 4:6, other = 7:9)
renamed_df <- rename_columns(df)  # 'volume' becomes 'Volume', 'time' becomes 'Time'
} # }
```
