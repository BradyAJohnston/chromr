# Read .csv Chromatogram from the BioRad NGC

Each trace in an NGC export is recorded at its own volumes. The traces
are combined into one row per volume of the first absorbance trace, and
the other traces are interpolated onto those volumes.

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

A chromatogram: a
[tibble](https://tibble.tidyverse.org/reference/tibble-package.html)
with the run's metadata, see
[`chrom_meta()`](https://bradyajohnston.github.io/chromr/reference/chrom_meta.md).
Exports of several runs have the run, date, method and file of each run.

## Details

An export can hold several runs side by side. Each run is read on its
own, and the runs are stacked, at the volumes each was recorded at, with
a `run` column first: the name of each run, or its number if the runs
don't each have their own name. A new run starts where the run named on
the first line changes, or where a trace repeats.

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
fl <- system.file(
  "extdata",
  "ngc_sec.csv",
  package = "chromr"
)

dat <- chrom_read_ngc(fl)
dat
#> # A chromatogram: 6,510 × 6
#> # Instrument:     BioRad NGC
#> # Run:            20191126_IMAC
#>      volume   a260   a280 conductivity percent_b pressure
#>        <mL>  <mAU>  <mAU>      <mS/cm>       <%>    <psi>
#>  1 0.000500  0.747   0            28.0         0    NA   
#>  2 0.00503   1.10    0            28.0         0     5   
#>  3 0.0104    0.784 NaN            28.0         0     5   
#>  4 0.0147    0.784 NaN            28.0         0     5   
#>  5 0.0213   -1.26  NaN            28.0         0     5   
#>  6 0.0280   -3.21  NaN            28.0         0     5   
#>  7 0.0347   -4.56   -8.57         27.7         0     5   
#>  8 0.0413   -5.45   -8.58         27.8         0     5.55
#>  9 0.0480   -6.05   -8.39         27.7         0     6.09
#> 10 0.0547   -6.39   -8.12         27.7         0     6.64
#> # ℹ 6,500 more rows
```
