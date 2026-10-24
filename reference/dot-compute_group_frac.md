# Compute Group Statistics for Fraction Labels

Internal function that computes the positions for fraction labels based
on specified grouping and positioning rules.

## Usage

``` r
.compute_group_frac(
  data,
  scales,
  period = 5,
  xpos = mean,
  ypos = 0,
  lines = NA,
  initial = 0L
)
```

## Arguments

- data:

  Input data frame.

- scales:

  Scale objects from the plot.

- period:

  Integer specifying the interval between labels.

- xpos:

  Function or static value for x position.

- ypos:

  Function or static value for y position.

- lines:

  Vector of line colors to include.

- initial:

  Integer specifying the starting point.

## Value

A data frame with computed positions and labels.
