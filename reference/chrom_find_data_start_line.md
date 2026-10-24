# Find the Line Where Data Begins

Finds the line where the tabular data begins. This is then used to start
the reading of the data with
[`readr::read_csv()`](https://readr.tidyverse.org/reference/read_delim.html)
and end the reading of the metadata.

## Usage

``` r
chrom_find_data_start_line(file, n_lines = 50)
```

## Arguments

- file:

  File path to the file to read.

- n_lines:

  Number of lines to search for the start of the data.

## Value

Single integer of the start of the data.
