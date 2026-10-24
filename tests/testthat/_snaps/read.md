# read_akta_old() deprecates fraction_size

    Code
      dat <- read_akta_old(example_file("example_akta_1.asc"), fraction_size = 2)
    Condition
      Warning:
      The `fraction_size` argument of `read_akta_old()` is deprecated as of chromr 0.3.0.
      i The end of each fraction is given by the next mark.

