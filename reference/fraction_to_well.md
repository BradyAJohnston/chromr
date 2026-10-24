# The well each fraction was collected into

Fraction collectors fill deep-well plates one well per fraction, so
fraction 1 goes into A01, fraction 2 into A02, and so on.
`fraction_to_well()` gives the well of each fraction as well IDs from
the [wellr](https://bradyajohnston.github.io/wellr/) package, so
fractions can be joined to plate-reader data.

## Usage

``` r
fraction_to_well(x, plate = 96, colwise = FALSE)
```

## Arguments

- x:

  Fraction numbers.

- plate:

  The plate the fractions were collected into: the number of wells in a
  standard plate, or the number of rows and columns. See
  [`wellr::well()`](https://bradyajohnston.github.io/wellr/reference/well.html).

- colwise:

  Whether the collector fills the plate down the columns, from A01 to
  B01, instead of along the rows, from A01 to A02.

## Value

A `wellr_well` vector of well IDs, `NA` where `x` is `NA`.

## Examples

``` r
fraction_to_well(fraction(c(1, 2, 13, NA)))
#> <well<96>[4]>
#> [1] A01  A02  B01  <NA>
fraction_to_well(1:3, colwise = TRUE)
#> <well<96>[3]>
#> [1] A01 B01 C01
```
