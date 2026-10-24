# Read chromatograms from UNICORN 6 and later

ÄKTA systems run by UNICORN 6 and later save each run as a result, which
UNICORN can export as a `.zip` archive or as curves in a `.csv` file.

- `chrom_read_akta_result()` reads a result archive, the `.zip` file
  from "Export" in UNICORN's Evaluation module. It needs the xml2
  package.

- `chrom_read_akta_csv()` reads curves exported to a `.csv` file.

Each trace is recorded at its own volumes. The traces are combined into
one row per volume of the UV trace, and the other traces are
interpolated onto those volumes. The fraction marks give the fraction of
each row; rows before the first mark, or after a mark such as "Waste",
have no fraction. The run log and injection marks are dropped.

For exports from UNICORN 5 and earlier, see
[`read_akta_old()`](https://bradyajohnston.github.io/chromr/reference/read_akta_old.md).

## Usage

``` r
chrom_read_akta_result(file)

chrom_read_akta_csv(file)
```

## Arguments

- file:

  Path to the `.zip` result or `.csv` export.

## Value

A chromatogram: a
[tibble](https://tibble.tidyverse.org/reference/tibble-package.html)
with the run's metadata, see
[`chrom_meta()`](https://bradyajohnston.github.io/chromr/reference/chrom_meta.md).
Results record the run's name and when it was created.

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
fl <- system.file("extdata", "example_akta_3.csv", package = "chromr")
chrom_read_akta_csv(fl)
#> # A chromatogram: 9,572 × 2
#> # Instrument:     ÄKTA
#>    volume    uv
#>      <mL> <mAU>
#>  1  0.224  2.02
#>  2  0.237  2.03
#>  3  0.25   2.05
#>  4  0.264  2.06
#>  5  0.277  2.1 
#>  6  0.29   2.12
#>  7  0.304  2.16
#>  8  0.317  2.21
#>  9  0.33   2.25
#> 10  0.344  2.30
#> # ℹ 9,562 more rows
```
