# Create Labels for Fractional Data

Adds labels to plots at specified fractional intervals. Labels can be
positioned using either static values or functions for both x and y
coordinates.

## Usage

``` r
geom_fraction_label(
  mapping = NULL,
  data = NULL,
  position = "identity",
  na.rm = FALSE,
  show.legend = NA,
  xpos = mean,
  ypos = 0,
  period = 5,
  initial = 0L,
  inherit.aes = TRUE,
  ...
)
```

## Arguments

- mapping:

  Set of aesthetic mappings created by
  [`aes`](https://ggplot2.tidyverse.org/reference/aes.html).

- data:

  The data to be displayed in this layer.

- position:

  Position adjustment, either as a string, or the result of a call to a
  position adjustment function. Default is "identity".

- na.rm:

  If `FALSE`, the default, missing values are removed with a warning.

- show.legend:

  logical. Should this layer be included in the legends?

- xpos:

  Function or static value determining x position of labels. Default is
  mean.

- ypos:

  Function or static value determining y position of labels. Default is
  0.

- period:

  Integer specifying the interval between labels. Default is 5.

- initial:

  Integer specifying the starting point for fraction counting. Default
  is 0.

- inherit.aes:

  If `FALSE`, overrides the default aesthetics.

- ...:

  Other arguments passed to
  [`layer`](https://ggplot2.tidyverse.org/reference/layer.html).

## Value

A ggplot2 layer.

## Examples

``` r
if (FALSE) { # \dontrun{
# Basic usage with default positioning
ggplot(data, aes(x = x, y = y, fraction = frac)) +
  geom_fraction_label()

# Custom positioning using functions
ggplot(data, aes(x = x, y = y, fraction = frac)) +
  geom_fraction_label(xpos = mean, ypos = max)
} # }
```
