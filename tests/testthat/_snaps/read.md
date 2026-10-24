# read_akta_old() deprecates fraction_size

    Code
      dat <- read_akta_old(example_file("example_akta_1.asc"), fraction_size = 2)
    Condition
      Warning:
      The `fraction_size` argument of `read_akta_old()` is deprecated as of chromr 0.3.0.
      i The end of each fraction is given by the next mark.

# fraction_at() follows the fraction marks

    Code
      out <- fraction_at(c(1, 2), c(1, 2), c("1", "1.A.2"))
    Condition
      Warning:
      Can't find the fraction number of 1 fraction mark.
      i Points after it have no fraction: "1.A.2".

# read_akta_old() errors for other UNICORN exports

    Code
      read_akta_old(path)
    Condition
      Error in `read_akta_old()`:
      ! '<file>' isn't an export of chromatogram curves.
      i It is another UNICORN export, such as a method or a peak table.

# chrom_read_quadtech() errors when the column names don't match

    Code
      chrom_read_quadtech(path)
    Condition
      Error in `chrom_read_quadtech()`:
      ! Can't read the column names of '<file>'.
      i Line 4 should name each column of the data, but it has 1 name for 2 columns.

