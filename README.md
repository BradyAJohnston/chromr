
<!-- README.md is generated from README.Rmd. Please edit that file -->

# chromr

<!-- badges: start -->

[![Codecov test
coverage](https://codecov.io/gh/BradyAJohnston/chromr/branch/main/graph/badge.svg)](https://app.codecov.io/gh/BradyAJohnston/chromr?branch=main)
[![R-CMD-check](https://github.com/BradyAJohnston/chromr/actions/workflows/R-CMD-check.yaml/badge.svg)](https://github.com/BradyAJohnston/chromr/actions/workflows/R-CMD-check.yaml)
<!-- badges: end -->

The goal of chromr is to …

## Installation

You can install the development version of chromr from
[GitHub](https://github.com/) with:

``` r
# install.packages("devtools")
devtools::install_github("BradyAJohnston/chromr")
```

## Example

Reading of chromatogram files and plotting from the BioRad QuadTech.

``` r
library(chromr)
## basic example code
```

``` r
fl <- system.file(
  "extdata",
  "sec.txt", 
  package = "chromr"
)

dat <- fl %>% 
  chrom_read_quadtech()
dat
#> # A tibble: 7,680 × 6
#>     time  volume name               value unit              wl
#>    <dbl>   <dbl> <chr>              <dbl> <chr>          <dbl>
#>  1     0 0       QuadTec 1       0        (280.0 nm), AU   280
#>  2     0 0       QuadTec 2       0        (260.0 nm), AU   260
#>  3     0 0       Gradient Pump   0        %B                NA
#>  4     0 0       QuadTec 3       0.00550  (550.0 nm), AU   550
#>  5     0 0       QuadTec 4       0        (650.0 nm), AU   650
#>  6     0 0       UV              0        AU                NA
#>  7     0 0       Conductivity   16.0      mS/cm             NA
#>  8     0 0       GP Pressure   265        PSI               NA
#>  9     1 0.00501 QuadTec 1       0.000004 (280.0 nm), AU   280
#> 10     1 0.00501 QuadTec 2       0.000013 (260.0 nm), AU   260
#> # … with 7,670 more rows
#> # ℹ Use `print(n = ...)` to see more rows
dat %>%
  chrom_plot()
```

<img src="man/figures/README-unnamed-chunk-2-1.png" width="100%" />

``` r
fl <- system.file(
  "extdata",
  "sec_no_volume.txt", 
  package = "chromr"
)

fl %>% 
  chrom_read_quadtech() %>% 
  chrom_add_volume(0.3) %>% 
  chrom_plot(xlim = c(0, 3), ylim = c(NA, 0.01))
```

<img src="man/figures/README-unnamed-chunk-3-1.png" width="100%" />
