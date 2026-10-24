# Read the fraction table from the BioRad NGC

The NGC exports the fractions of a run as a `.txt` table, with the rack
and tube each fraction was collected into and the volumes it covers.

## Usage

``` r
chrom_read_ngc_fractions(file)
```

## Arguments

- file:

  File path to the `.txt` file.

## Value

A [tibble](https://tibble.tidyverse.org/reference/tibble-package.html)
with a row for each fraction and the columns `fraction`, `rack`, `tube`,
`start`, `end` and `collected`, the collected volume. `fraction` is a
[fraction](https://bradyajohnston.github.io/chromr/reference/fraction.md)
vector, and the volumes are
[quantity](https://bradyajohnston.github.io/chromr/reference/quantity.md)
vectors in mL. Like
[`chrom_fractions()`](https://bradyajohnston.github.io/chromr/reference/chrom_fractions.md),
it has the columns `fraction`, `start` and `end`.

## Examples

``` r
fl <- system.file("extdata", "ngc_sec_fractions.txt", package = "chromr")
chrom_read_ngc_fractions(fl)
#> # A tibble: 21 × 6
#>    fraction rack   tube start  end collected
#>      <frac> <chr> <int>  <mL> <mL>      <mL>
#>  1        1 A         1  25.3 26.8      1.51
#>  2        2 A         2  26.8 28.3      1.49
#>  3        3 A         3  28.3 29.8      1.51
#>  4        4 A         4  29.8 31.3      1.5 
#>  5        5 A         5  31.3 32.8      1.49
#>  6        6 A         6  32.8 34.3      1.5 
#>  7        7 A         7  34.3 35.8      1.5 
#>  8        8 A         8  35.8 37.2      1.42
#>  9        9 A         9  61.6 63.0      1.5 
#> 10       10 A        10  63.0 64.6      1.5 
#> # ℹ 11 more rows
```
