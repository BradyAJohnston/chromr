# Read a .asc chromatogram from an old AKTA Pure

Each trace in an ÄKTA export is recorded at its own volumes. The traces
are combined into one row per volume of the UV trace, and the other
traces are interpolated onto those volumes. The fraction marks give the
fraction of each row; rows before the first mark, or after a mark such
as "Waste", have no fraction. Injection marks and the logbook are
dropped.

## Usage

``` r
read_akta_old(file, fraction_size = deprecated())
```

## Arguments

- file:

  Filepath to read

- fraction_size:

  **\[deprecated\]** Not used: the end of each fraction is given by the
  next mark.

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

## See also

[`chrom_read_akta_result()`](https://bradyajohnston.github.io/chromr/reference/chrom_read_akta_result.md)
and
[`chrom_read_akta_csv()`](https://bradyajohnston.github.io/chromr/reference/chrom_read_akta_result.md)
for results and exports from UNICORN 6 and later.

## Examples

``` r
fl <- system.file('extdata', 'example_akta_2.asc', package = 'chromr')

read_akta_old(fl)
#> # A chromatogram: 962 × 9
#> # Instrument:     ÄKTA
#> # Run:            Emma K 15032024 SFPQ FL 2x 800 mL cultures 9 mL loop001:10
#>    volume fraction    uv conductivity conductivity_percent percent_b pressure
#>      <mL>   <frac> <mAU>      <mS/cm>                  <%>       <%>    <MPa>
#>  1  0           NA 2007.         54.8                 54.8       100        0
#>  2  0.16        NA 2007.         54.9                 54.9       100        0
#>  3  0.32        NA 2007.         54.9                 54.9       100        0
#>  4  0.48        NA 2007.         54.9                 54.9       100        0
#>  5  0.639       NA 2007.         54.9                 54.9       100        0
#>  6  0.799       NA 2007.         54.8                 54.8       100        0
#>  7  0.959       NA 2007.         54.8                 54.8       100        0
#>  8  1.12        NA 2007.         54.8                 54.8       100        0
#>  9  1.28        NA 2007.         54.8                 54.8       100        0
#> 10  1.44        NA 2007.         54.8                 54.8       100        0
#> # ℹ 952 more rows
#> # ℹ 2 more variables: flow <mL/min>, temperature <°C>
```
