# Reads Metadata from QuadTech Chromatogram

Reads Metadata from QuadTech Chromatogram

## Usage

``` r
chrom_get_meta_quadtech(file, start_line)
```

## Arguments

- file:

  Path to the chromatogram file.

- start_line:

  Start of the data and thus end of the metadata. Determined with
  [`chromr::chrom_find_data_start_line()`](https://bradyajohnston.github.io/chromr/reference/chrom_find_data_start_line.md)

## Value

a [tibble](https://tibble.tidyverse.org/reference/tibble-package.html)
