# Pivot Wavelength Data to Long Format

Converts wavelength data from wide to long format, specifically handling
columns that match the pattern `"(a|A)\\d{2,3}"` (e.g., "A280", "a340").

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
