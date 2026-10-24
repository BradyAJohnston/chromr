# Create Fraction-Based Area Plots with Periodic Highlighting

Creates filled area plots that visually highlight periodic patterns in
fractional data. The highlighting is controlled by the period parameter
and can be filtered by line colors.

## Usage

``` r
geom_fraction_bars(
  mapping = NULL,
  data = NULL,
  position = "identity",
  na.rm = FALSE,
  show.legend = FALSE,
  alpha = 0.3,
  period = 5,
  initial = 0L,
  lines = NA,
  inherit.aes = TRUE,
  ...
)
```

## Arguments

- mapping:

  Set of aesthetic mappings created by
  [`aes`](https://ggplot2.tidyverse.org/reference/aes.html). If
  specified and `inherit.aes = TRUE`, is combined with the default
  mapping at the top level of the plot.

- data:

  The data to be displayed in this layer. If `NULL`, the default, the
  data is inherited from the plot data as specified in the call to
  [`ggplot`](https://ggplot2.tidyverse.org/reference/ggplot.html).

- position:

  Position adjustment, either as a string, or the result of a call to a
  position adjustment function. Default is "identity".

- na.rm:

  If `FALSE`, the default, missing values are removed with a warning. If
  `TRUE`, missing values are silently removed.

- show.legend:

  logical. Should this layer be included in the legends? `NA`, the
  default, includes if any aesthetics are mapped.

- alpha:

  Numeric value between 0 and 1 for the transparency of the area fills.
  Default is 0.3.

- period:

  Integer specifying the interval for highlighting fractions. Default is
  5.

- initial:

  Integer specifying the starting point for fraction counting. Default
  is 0.

- lines:

  Vector of line colors to include in the visualization. Default is NA.

- inherit.aes:

  If `FALSE`, overrides the default aesthetics, rather than combining
  with them.

- ...:

  Other arguments passed to
  [`layer`](https://ggplot2.tidyverse.org/reference/layer.html).

## Value

A ggplot2 layer.

## Examples

``` r
if (FALSE) { # \dontrun{
ggplot(data, aes(x = x, y = y, fraction = frac)) +
  geom_fraction_bars(period = 5)

# With custom period and initial value
ggplot(data, aes(x = x, y = y, fraction = frac)) +
  geom_fraction_bars(period = 3, initial = 1, alpha = 0.2)
} # }
```
