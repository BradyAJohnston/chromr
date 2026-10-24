# Compute Identity Statistics for Fraction Bars

Internal function that processes data for fraction-based area plots,
handling periodic highlighting and group interactions.

## Usage

``` r
.comput_frac_identity(data, scales, period = 5, initial = 0L, lines = NA)
```

## Arguments

- data:

  Input data frame.

- scales:

  Scale objects from the plot.

- period:

  Integer specifying the highlighting interval.

- initial:

  Integer specifying the starting point.

- lines:

  Vector of line colors to include.

## Value

A data frame with computed groups and fill values.
