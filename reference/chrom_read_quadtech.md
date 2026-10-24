# Read BioRad QuadTech Chromatogram Files

Read BioRad QuadTech Chromatogram Files

## Usage

``` r
chrom_read_quadtech(file, interp_volume = TRUE)
```

## Arguments

- file:

  Exported `.TXT` chromatogram file from the BioRad QuadTec.

- interp_volume:

  Logical. If TRUE, interpolates the values in the volume column based
  on the values in the time column.

## Value

A chromatogram: a
[tibble](https://tibble.tidyverse.org/reference/tibble-package.html)
with the run's metadata, see
[`chrom_meta()`](https://bradyajohnston.github.io/chromr/reference/chrom_meta.md).

## Columns

The readers all give one row per sampled point, with the columns the
instrument recorded:

- `time`, in seconds, when the instrument records it, and `volume`, in
  mL.

- `fraction`, the fraction each row was collected into, or `NA` when it
  wasn't collected.

- Absorbance, named after its wavelength in nm, such as `a280`, or `uv`
  when the instrument doesn't record the wavelength.

- The other traces under standard names: `conductivity`,
  `conductivity_percent`, `percent_b`, `pressure`, `flow` and
  `temperature`.

`fraction` is a
[fraction](https://bradyajohnston.github.io/chromr/reference/fraction.md)
vector and the absorbance columns are
[absorbance](https://bradyajohnston.github.io/chromr/reference/absorbance.md)
vectors, which record their wavelength and unit. The other columns are
[quantity](https://bradyajohnston.github.io/chromr/reference/quantity.md)
vectors with the unit recorded in the file, such as `s`, `mL`, `mS/cm`,
`psi` or `MPa`.

## Examples

``` r
fl <- system.file("extdata",
  "example_quadtec_3.txt",
  package = "chromr"
)
# just read
fl |>
  chrom_read_quadtech()
#> # A chromatogram: 960 × 10
#> # Instrument:     BioRad QuadTec
#> # Run:            20220801_dsnPPR10_Cy3_1_AF647_2
#> # Date:           02:10:17 PM 08-01-22
#>    time  volume     a280     a260     a550     a650   uv percent_b conductivity
#>     <s>    <mL>     <AU>     <AU>     <AU>     <AU> <AU>       <%>      <mS/cm>
#>  1    0 0         0       0        5.51e-3  0          0         0         16.0
#>  2    1 0.00501   4  e-6  1.3 e-5 -1.6 e-4  1.87e-3    0         0         16.0
#>  3    2 0.0100    0      -8   e-6 -6.40e-5  1.80e-3    0         0         16.0
#>  4    3 0.0150   -1.3e-5 -2.20e-5 -6.40e-5  3.39e-3    0         0         16.0
#>  5    4 0.0200   -6  e-6 -1.5 e-5 -9.5 e-5  3.91e-3    0         0         16.0
#>  6    5 0.0250   -6  e-6 -1.80e-5 -1.28e-4  4.13e-3    0         0         16.0
#>  7    6 0.0300   -6  e-6 -2.90e-5 -1.93e-4  3.76e-3    0         0         16.0
#>  8    7 0.0350   -2  e-6 -8   e-6 -1.28e-4  1.92e-3    0         0         16.0
#>  9    8 0.0400    3  e-6 -1.10e-5 -6.40e-5  8.83e-4    0         0         16.0
#> 10    9 0.0450   -8  e-6 -9   e-6 -9.60e-5 -3.2 e-4    0         0         16.0
#> # ℹ 950 more rows
#> # ℹ 1 more variable: pressure <psi>

# read without interpolating volume
fl |>
  chrom_read_quadtech(interp_volume = FALSE)
#> # A chromatogram: 960 × 10
#> # Instrument:     BioRad QuadTec
#> # Run:            20220801_dsnPPR10_Cy3_1_AF647_2
#> # Date:           02:10:17 PM 08-01-22
#>    time volume      a280     a260     a550     a650   uv percent_b conductivity
#>     <s>   <mL>      <AU>     <AU>     <AU>     <AU> <AU>       <%>      <mS/cm>
#>  1    0      0  0         0        5.51e-3  0          0         0         16.0
#>  2    1      0  0.000004  1.3 e-5 -1.6 e-4  1.87e-3    0         0         16.0
#>  3    2      0  0        -8   e-6 -6.40e-5  1.80e-3    0         0         16.0
#>  4    3      0 -0.000013 -2.20e-5 -6.40e-5  3.39e-3    0         0         16.0
#>  5    4      0 -0.000006 -1.5 e-5 -9.5 e-5  3.91e-3    0         0         16.0
#>  6    5      0 -0.000006 -1.80e-5 -1.28e-4  4.13e-3    0         0         16.0
#>  7    6      0 -0.000006 -2.90e-5 -1.93e-4  3.76e-3    0         0         16.0
#>  8    7      0 -0.000002 -8   e-6 -1.28e-4  1.92e-3    0         0         16.0
#>  9    8      0  0.000003 -1.10e-5 -6.40e-5  8.83e-4    0         0         16.0
#> 10    9      0 -0.000008 -9   e-6 -9.60e-5 -3.2 e-4    0         0         16.0
#> # ℹ 950 more rows
#> # ℹ 1 more variable: pressure <psi>
# read then plot
fl |>
  chrom_read_quadtech() |>
  chrom_plot()
```
