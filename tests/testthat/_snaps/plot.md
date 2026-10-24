# chrom_plot_fractions() checks its arguments

    Code
      chrom_plot_fractions(dat, frac_include = 3)
    Condition
      Error in `chrom_plot_fractions()`:
      ! `frac_include` must be "all" or a numeric vector of length 2, not a number.
    Code
      chrom_plot_fractions(dat, frac_labelling = 0)
    Condition
      Error in `chrom_plot_fractions()`:
      ! `frac_labelling` must be a whole number larger than or equal to 1, not the number 0.

# chromatograms without a volume are plotted against time

    Code
      chrom_plot(tibble::tibble(a280 = 1))
    Condition
      Error in `chrom_plot()`:
      ! `data` must have a volume or time column.
      i Add a volume to runs that only record the time with `chrom_add_volume()`.

