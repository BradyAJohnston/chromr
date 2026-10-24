# Append a Chromatogram to Another Run

Joins runs end to end: the time and volume of each run continue from the
end of the run before it. Optionally continues the numbering of the
fractions after the previous runs.

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

  Logical. FALSE keeps the fraction numbering as-is. TRUE renumbers the
  fractions of a run that starts again from a fraction number already
  used, so they continue after the fractions of the runs before it. Runs
  whose fractions already continue the numbering are left as they are.

## Value

a [tibble](https://tibble.tidyverse.org/reference/tibble-package.html)
which combines the given runs, with a `run` column giving the number of
the run each row comes from. If all the runs are chromatograms, it is a
chromatogram whose metadata has the run, date, method and file of each
run; see
[`chrom_meta()`](https://bradyajohnston.github.io/chromr/reference/chrom_meta.md).

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
#> # A chromatogram: 23,581 × 11
#> # Instrument:     BioRad QuadTec
#> # Run:            20220809_S200_16600_sfpqTEVdig,
#> #   20220809_S200_16600_sfpqTEVdig_part2
#> # Date:           04:43:33 PM 08-09-22, 09:38:14 AM 08-10-22
#>      run time  volume fraction      a280      a260    a205      a488 percent_b
#>    <dbl>  <s>    <mL>   <frac>      <AU>      <AU>    <AU>      <AU>       <%>
#>  1     1    0 0             NA  0.000195  0.000138 -0.0142  0.000955         0
#>  2     1    4 0.00474       NA -0.00483  -0.00578  -0.0263 -0.00351          0
#>  3     1    8 0.00947       NA -0.00386  -0.00479  -0.0263 -0.00224          0
#>  4     1   12 0.0142        NA -0.00335  -0.00430  -0.0300 -0.00161          0
#>  5     1   16 0.0189        NA -0.00308  -0.00402  -0.0301 -0.00150          0
#>  6     1   20 0.0237        NA -0.00229  -0.00348  -0.0301 -0.00124          0
#>  7     1   24 0.0284        NA -0.00186  -0.00268  -0.0302 -0.00093          0
#>  8     1   28 0.0332        NA -0.00204  -0.00278  -0.0337 -0.000881         0
#>  9     1   32 0.0379        NA -0.00229  -0.00305  -0.0337 -0.00120          0
#> 10     1   36 0.0426        NA -0.00247  -0.00327  -0.0338 -0.00114          0
#> # ℹ 23,571 more rows
#> # ℹ 2 more variables: conductivity <mS/cm>, pressure <psi>
```
