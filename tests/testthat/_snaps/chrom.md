# chromatograms print their metadata

    Code
      print(dat, n = 3)
    Output
      # A chromatogram: 6,510 x 6
      # Instrument:     BioRad NGC
      # Run:            20191126_IMAC
          volume  a260  a280 conductivity percent_b pressure
           <dbl> <mAU> <mAU>        <dbl>     <dbl>    <dbl>
      1 0.000500 0.747     0         28.0         0       NA
      2 0.00503  1.10      0         28.0         0        5
      3 0.0104   0.784   NaN         28.0         0        5
      # i 6,507 more rows

