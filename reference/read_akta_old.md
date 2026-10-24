# Read a .asc chromatogram from an old AKTA Pure

Read a .asc chromatogram from an old AKTA Pure

## Usage

``` r
read_akta_old(file, fraction_size = 2)
```

## Arguments

- file:

  Filepath to read

- fraction_size:

  Currently unused.

## Value

a [tibble](https://tibble.tidyverse.org/reference/tibble-package.html)

## Examples

``` r
fl <- system.file('extdata', 'example_akta_2.asc', package = 'chromr')

read_akta_old(fl)
#> New names:
#> • `` -> `...2`
#> • `` -> `...4`
#> • `` -> `...6`
#> • `` -> `...8`
#> • `` -> `...10`
#> • `` -> `...12`
#> • `` -> `...14`
#> • `` -> `...16`
#> • `` -> `...18`
#> • `` -> `...20`
#> Rows: 963 Columns: 20
#> ── Column specification ────────────────────────────────────────────────────────
#> Delimiter: "\t"
#> chr  (2): ...16, ...20
#> dbl (18): loop001:10_UV, ...2, loop001:10_Cond, ...4, loop001:10_Cond%, ...6...
#> 
#> ℹ Use `spec()` to retrieve the full column specification for this data.
#> ℹ Specify the column types or set `show_col_types = FALSE` to quiet this message.
#> Warning: NAs introduced by coercion
```
