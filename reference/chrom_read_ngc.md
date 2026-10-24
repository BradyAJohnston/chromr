# Read .csv Chromatogram from the BioRad NGC

Read .csv Chromatogram from the BioRad NGC

## Usage

``` r
chrom_read_ngc(file, skip = 1)
```

## Arguments

- file:

  File path to the `.csv` file.

- skip:

  Number of lines to skip before begin reading. Usually 1 line.

## Value

a [tibble](https://tibble.tidyverse.org/reference/tibble-package.html)

## Examples

``` r
fl <- system.file(
  "extdata",
  "ngc_sec.csv",
  package = "chromr"
)

dat <- chrom_read_ngc(fl)
#> Adding missing grouping variables: `id`
dat
#> # A tibble: 32,550 × 5
#> # Groups:   type, id [32,550]
#>       id   volume type          value name               
#>    <int>    <dbl> <chr>         <dbl> <chr>              
#>  1     1 0.000500 uv1           0.747 uv1_260_nm_m_au    
#>  2     1 0.000500 uv2           0     uv2_280_nm_m_au    
#>  3     1 0.000500 conductivity 28.0   conductivity_m_s_cm
#>  4     1 0        percent       0     percent_b_percent  
#>  5     1 0.00503  system        5     system_pressure_psi
#>  6     2 0.00503  uv1           1.10  uv1_260_nm_m_au    
#>  7     2 0.00503  uv2           0     uv2_280_nm_m_au    
#>  8     2 0.00503  conductivity 28.0   conductivity_m_s_cm
#>  9     2 0.0247   percent       0     percent_b_percent  
#> 10     2 0.0347   system        5     system_pressure_psi
#> # ℹ 32,540 more rows
```
