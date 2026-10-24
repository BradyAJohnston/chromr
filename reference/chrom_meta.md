# Chromatogram metadata

The readers return a chromatogram: a
[tibble](https://tibble.tidyverse.org/reference/tibble-package.html)
that also records where the run came from. It works with dplyr, tidyr
and ggplot2 as any tibble does, and prints the instrument and run above
the data. `chrom_meta()` gives the metadata as a list:

- `instrument`: the instrument, such as `"BioRad QuadTec"`.

- `run`: the name of the run.

- `date` and `method`: the date and method of the run, as recorded, if
  the instrument records them.

- `file`: the name of the file it was read from.

The units of the columns are part of the columns: see
[`quantity()`](https://bradyajohnston.github.io/chromr/reference/quantity.md)
and
[`absorbance()`](https://bradyajohnston.github.io/chromr/reference/absorbance.md).

Appended runs, from
[`chrom_append_run()`](https://bradyajohnston.github.io/chromr/reference/chrom_append_run.md),
have one `run`, `date`, `method` and `file` for each run.

Results that aren't a chromatogram, such as from
[`dplyr::summarise()`](https://dplyr.tidyverse.org/reference/summarise.html)
or
[`tidyr::pivot_longer()`](https://tidyr.tidyverse.org/reference/pivot_longer.html),
are plain tibbles.
[`dplyr::ungroup()`](https://dplyr.tidyverse.org/reference/group_by.html)
also gives a plain tibble, so use the `.by` argument of dplyr verbs to
work by group and keep the metadata.

## Usage

``` r
chrom_meta(x)
```

## Arguments

- x:

  A chromatogram.

## Value

A list, or `NULL` for a data frame that isn't a chromatogram.

## Examples

``` r
fl <- system.file("extdata", "example_quadtec_1.TXT", package = "chromr")
dat <- chrom_read_quadtech(fl)
dat
#> # A chromatogram: 15,160 × 10
#> # Instrument:     BioRad QuadTec
#> # Run:            20220809_S200_16600_sfpqTEVdig
#> # Date:           04:43:33 PM 08-09-22
#>    time  volume fraction      a280      a260    a205      a488 percent_b
#>     <s>    <mL>   <frac>      <AU>      <AU>    <AU>      <AU>       <%>
#>  1    0 0             NA  0.000195  0.000138 -0.0142  0.000955         0
#>  2    4 0.00474       NA -0.00483  -0.00578  -0.0263 -0.00351          0
#>  3    8 0.00947       NA -0.00386  -0.00479  -0.0263 -0.00224          0
#>  4   12 0.0142        NA -0.00335  -0.00430  -0.0300 -0.00161          0
#>  5   16 0.0189        NA -0.00308  -0.00402  -0.0301 -0.00150          0
#>  6   20 0.0237        NA -0.00229  -0.00348  -0.0301 -0.00124          0
#>  7   24 0.0284        NA -0.00186  -0.00268  -0.0302 -0.00093          0
#>  8   28 0.0332        NA -0.00204  -0.00278  -0.0337 -0.000881         0
#>  9   32 0.0379        NA -0.00229  -0.00305  -0.0337 -0.00120          0
#> 10   36 0.0426        NA -0.00247  -0.00327  -0.0338 -0.00114          0
#> # ℹ 15,150 more rows
#> # ℹ 2 more variables: conductivity <mS/cm>, pressure <psi>

chrom_meta(dat)
#> $instrument
#> [1] "BioRad QuadTec"
#> 
#> $run
#> [1] "20220809_S200_16600_sfpqTEVdig"
#> 
#> $date
#> [1] "04:43:33 PM 08-09-22"
#> 
#> $method
#> [1] "S200_16/600_overnight_pumpA"
#> 
#> $file
#> [1] "example_quadtec_1.TXT"
#> 
```
