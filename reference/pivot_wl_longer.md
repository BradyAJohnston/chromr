# Pivot Wavelength Data to Long Format

Converts wavelength data from wide to long format, with a row for each
absorbance column of each point. The absorbance columns are those of the
[absorbance](https://bradyajohnston.github.io/chromr/reference/absorbance.md)
type, or for plain data frames, columns named like `"a280"` or `"uv"`.
The values keep their unit, converted to mAU if the columns have
different units.

## Usage

``` r
pivot_wl_longer(data, values_to = "abs", names_to = "wl")
```

## Arguments

- data:

  A data frame containing wavelength measurements in wide format

- values_to:

  Name of the column to create for the values (default: "abs")

- names_to:

  Name of the column to create for the wavelength identifiers (default:
  "wl")

## Value

A data frame in long format with wavelength measurements

## Examples

``` r
if (FALSE) { # \dontrun{
# With default column names
pivot_wl_longer(spectral_data)

# With custom column names
pivot_wl_longer(spectral_data, values_to = "absorbance", names_to = "wavelength")
} # }
```
