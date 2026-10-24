# The volumes each fraction covers

Gives the start and end volume of each fraction in a chromatogram. A
fraction starts at the first point collected into it and ends at the
first point after it, so the fractions of a run meet end to end. These
are the sampled volumes, so they are within one point of the volumes the
fraction collector recorded.

## Usage

``` r
chrom_fractions(data)
```

## Arguments

- data:

  A chromatogram from `chrom_read_*()`, with `volume` and `fraction`
  columns. Appended runs, with a `run` column, are handled run by run.

## Value

A [tibble](https://tibble.tidyverse.org/reference/tibble-package.html)
with a row for each fraction and the columns `fraction`, `start` and
`end`, and `run` if `data` has one.

## Examples

``` r
fl <- system.file("extdata", "example_akta_2.asc", package = "chromr")
read_akta_old(fl) |>
  chrom_fractions()
#> # A tibble: 73 × 3
#>    fraction start  end
#>      <frac>  <mL> <mL>
#>  1        1  9.11 11.0
#>  2        2 11.0  13.1
#>  3        3 13.1  15.0
#>  4        4 15.0  17.1
#>  5        5 17.1  19.0
#>  6        6 19.0  21.1
#>  7        7 21.1  23.0
#>  8        8 23.0  25.1
#>  9        9 25.1  27.2
#> 10       10 27.2  29.1
#> # ℹ 63 more rows
```
