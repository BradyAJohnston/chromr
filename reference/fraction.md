# Fraction numbers

A `chromr_fraction` vector holds the number of the fraction each point
of a chromatogram was collected into. It is an integer vector
underneath, so fractions compare, sort and do arithmetic as numbers, but
points that weren't collected into a fraction are always `NA`.
Instruments record these as 0 or leave them out; `fraction()` turns 0
into `NA`.

- `fraction()` creates fraction numbers.

- `is_fraction()` tests whether an object is a vector of fraction
  numbers.

Adding or subtracting numbers gives fraction numbers, as when continuing
the numbering of a second run. Combining fraction numbers with other
numbers gives numbers. Use
[`as.integer()`](https://rdrr.io/r/base/integer.html) to get the plain
integer vector.

## Usage

``` r
fraction(x = integer())

is_fraction(x)
```

## Arguments

- x:

  For `fraction()`, a vector of whole numbers. For `is_fraction()`, an
  object to test.

## Value

`fraction()` returns a `chromr_fraction` vector. `is_fraction()` returns
a single logical.

## See also

[`chrom_fractions()`](https://bradyajohnston.github.io/chromr/reference/chrom_fractions.md)
for the volumes each fraction covers, and
[`fraction_to_well()`](https://bradyajohnston.github.io/chromr/reference/fraction_to_well.md)
for the well each fraction was collected into.

## Examples

``` r
x <- fraction(c(0, 0, 1, 1, 2, 2, 0))
x
#> <fraction[7]>
#> [1] NA NA  1  1  2  2 NA

x + 10
#> <fraction[7]>
#> [1] NA NA 11 11 12 12 NA
x == 2
#> [1]    NA    NA FALSE FALSE  TRUE  TRUE    NA
```
