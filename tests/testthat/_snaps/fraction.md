# fraction() makes 0 missing and checks its input

    Code
      fraction(-1)
    Condition
      Error in `fraction()`:
      ! `x` can't contain negative fraction numbers.
    Code
      fraction(1.5)
    Condition
      Error in `fraction()`:
      ! Can't convert from `x` <double> to <integer> due to loss of precision.
      * Locations: 1
    Code
      fraction("a")
    Condition
      Error in `fraction()`:
      ! Can't convert `x` <character> to <integer>.

# fractions do arithmetic and compare as numbers

    Code
      -x
    Condition
      Error in `vec_arith()`:
      ! <fraction> - <MISSING> is not permitted

# fractions print and plot

    Code
      x
    Output
      <fraction[3]>
      [1] NA  1  2
    Code
      tibble::tibble(fraction = x)
    Output
      # A tibble: 3 x 1
        fraction
          <frac>
      1       NA
      2        1
      3        2

# chrom_fractions() needs volume and fraction

    Code
      chrom_fractions(tibble::tibble(volume = 1))
    Condition
      Error in `chrom_fractions()`:
      ! `data` must have the fraction column.

