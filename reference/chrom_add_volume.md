# Add Volume Column From Time Units

Adds a column with calculated volums from the time column given a
particular flow rate. Currently only constant flow rates are supported.

## Usage

``` r
chrom_add_volume(.data, flow_rate = 0.5, time = "second")
```

## Arguments

- .data:

  Data frame or tibble with a column called 'time'.

- flow_rate:

  Flow rate in ml/min.

- time:

  Time unit when exported, used when the time column doesn't record its
  unit, as columns from `chrom_read_*()` do.

## Value

a [tibble](https://tibble.tidyverse.org/reference/tibble-package.html)

## Examples

``` r
fl <- system.file(
  "extdata",
  "example_quadtec_4.txt",
  package = "chromr"
)
# read just the data
dat <- fl |>
  chrom_read_quadtech()
dat
#> # A chromatogram: 6,260 × 3
#> # Instrument:     BioRad QuadTec
#> # Run:            180725_dPPR10_only
#> # Date:           12:40:53 PM 07-26-18
#>    time      a280      a260
#>     <s>      <AU>      <AU>
#>  1  0    0         0       
#>  2  0.2  0         0       
#>  3  0.4  0         0       
#>  4  0.6  0         0       
#>  5  0.8  0         0       
#>  6  1    0         0       
#>  7  1.2  0         0.000003
#>  8  1.4  0         0.000003
#>  9  1.6 -0.000006 -0.000001
#> 10  1.8  0.000001  0.000004
#> # ℹ 6,250 more rows
# add a volume given a constant flow rate
dat |>
  chrom_add_volume(0.3)
#> # A chromatogram: 6,260 × 4
#> # Instrument:     BioRad QuadTec
#> # Run:            180725_dPPR10_only
#> # Date:           12:40:53 PM 07-26-18
#>    time volume      a280      a260
#>     <s>   <mL>      <AU>      <AU>
#>  1  0    0      0         0       
#>  2  0.2  0.001  0         0       
#>  3  0.4  0.002  0         0       
#>  4  0.6  0.003  0         0       
#>  5  0.8  0.004  0         0       
#>  6  1    0.005  0         0       
#>  7  1.2  0.006  0         0.000003
#>  8  1.4  0.007  0         0.000003
#>  9  1.6  0.008 -0.000006 -0.000001
#> 10  1.8  0.009  0.000001  0.000004
#> # ℹ 6,250 more rows
```
