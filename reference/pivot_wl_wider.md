# Pivot Wavelength Data to Wide Format

Converts wavelength data from long to wide format, specifically handling
columns containing absorbance values and wavelength identifiers. By
default, looks for absorbance columns matching "abs\|absorbance" and
wavelength columns matching "wl\|wavelength".

## Usage

``` r
pivot_wl_wider(
  data,
  values_from = "abs|absorbance",
  names_from = "wl|wavelength"
)
```

## Arguments

- data:

  A data frame containing wavelength measurements in long format

- values_from:

  A regular expression pattern identifying the column(s) containing the
  values to be pivoted (default: "abs\|absorbance")

- names_from:

  A regular expression pattern identifying the column(s) containing the
  wavelength identifiers (default: "wl\|wavelength")

## Value

A data frame in wide format where each wavelength becomes a separate
column

## Examples

``` r
if (FALSE) { # \dontrun{
# With default column patterns
pivot_wl_wider(long_spectral_data)

# With custom column patterns
pivot_wl_wider(
  long_spectral_data,
  values_from = "measurement",
  names_from = "wavelength_nm"
)
} # }
```
