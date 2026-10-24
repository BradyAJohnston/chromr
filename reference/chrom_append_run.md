# Append a Chromatogram to Another Run

Sums the volume and the time columns. Optionally continues the numbering
of the fractions after the previous runs.

## Usage

``` r
chrom_append_run(.data, ..., adjust_fractions = FALSE)
```

## Arguments

- .data:

  A data frame created through `chrom_read_*()`

- ...:

  Runs created through `chrom_read_*()`

- adjust_fractions:

  Logical. FALSE keeps the fraction numbering as-is. TRUE starts the
  number of the subsequent fractions by adding the previous fractions.

## Value

a [tibble](https://tibble.tidyverse.org/reference/tibble-package.html)
which combines the given runs.

## Examples

``` r

fl1 <- system.file("extdata",
  "example_quadtec_1.TXT",
  package = "chromr"
)
fl2 <- system.file("extdata",
  "example_quadtec_2.TXT",
  package = "chromr"
)
df1 <- chrom_read_quadtech(fl1)
df2 <- chrom_read_quadtech(fl2)


df1 |>
  chrom_append_run(df2)
#> # A tibble: 23,581 × 11
#>      run  time percent_b m_s_cm   psi  volume fraction     a280     a260    a205
#>    <dbl> <dbl>     <dbl>  <dbl> <dbl>   <dbl>    <dbl>    <dbl>    <dbl>   <dbl>
#>  1     1     0         0   42.3     5 0              0  1.95e-4  1.38e-4 -0.0142
#>  2     1     4         0   42.1    18 0.00474        0 -4.83e-3 -5.78e-3 -0.0263
#>  3     1     8         0   42.0    18 0.00947        0 -3.86e-3 -4.79e-3 -0.0263
#>  4     1    12         0   41.9    19 0.0142         0 -3.35e-3 -4.30e-3 -0.0300
#>  5     1    16         0   41.9    20 0.0189         0 -3.08e-3 -4.02e-3 -0.0301
#>  6     1    20         0   41.7    20 0.0237         0 -2.29e-3 -3.48e-3 -0.0301
#>  7     1    24         0   41.6    20 0.0284         0 -1.86e-3 -2.68e-3 -0.0302
#>  8     1    28         0   41.6    20 0.0332         0 -2.03e-3 -2.78e-3 -0.0337
#>  9     1    32         0   41.5    20 0.0379         0 -2.29e-3 -3.05e-3 -0.0337
#> 10     1    36         0   41.4    20 0.0426         0 -2.47e-3 -3.27e-3 -0.0338
#> # ℹ 23,571 more rows
#> # ℹ 1 more variable: a488 <dbl>
```
