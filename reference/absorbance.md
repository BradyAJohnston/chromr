# Absorbance that knows its wavelength and unit

A `chromr_absorbance` vector holds absorbance values along with the
wavelength they were measured at and their unit, AU or mAU. It is a
double vector underneath, so it does arithmetic, compares and plots as
numbers. The readers return it, so columns such as `a280` keep their
wavelength and unit when they are combined, and plots label their axes
with the unit.

- `absorbance()` creates absorbance values.

- `is_absorbance()` tests whether an object is absorbance.

- `absorbance_wavelength()` and `absorbance_unit()` give the wavelength
  in nm and the unit.

- `absorbance_convert()` converts to another unit.

Combining absorbance in AU with absorbance in mAU, for example appending
runs from different instruments, converts it to mAU. Combining
absorbance at different wavelengths, as when pivoting to one column with
[`pivot_wl_longer()`](https://bradyajohnston.github.io/chromr/reference/pivot_wl_longer.md),
gives absorbance with no wavelength. Combining with plain numbers gives
numbers.

Adding, subtracting, and scaling by numbers gives absorbance, so a
baseline can be subtracted. Dividing absorbance by absorbance gives a
plain ratio, such as A260/A280, after converting both to the same unit.

## Usage

``` r
absorbance(x = double(), wavelength = NA, unit = c("mAU", "AU"))

is_absorbance(x)

absorbance_wavelength(x)

absorbance_unit(x)

absorbance_convert(x, unit = c("mAU", "AU"))
```

## Arguments

- x:

  For `absorbance()`, a numeric vector. For the other functions, an
  object to test, or absorbance.

- wavelength:

  The wavelength in nm, or `NA` if it isn't known.

- unit:

  The unit, `"AU"` or `"mAU"`.

## Value

`absorbance()` and `absorbance_convert()` return a `chromr_absorbance`
vector. `is_absorbance()` returns a single logical.
`absorbance_wavelength()` returns a number and `absorbance_unit()` a
string.

## Examples

``` r
x <- absorbance(c(0.1, 0.25), wavelength = 280, unit = "AU")
x
#> <absorbance<280 nm, AU>[2]>
#> [1] 0.10 0.25
absorbance_convert(x, "mAU")
#> <absorbance<280 nm, mAU>[2]>
#> [1] 100 250

# A260/A280 ratio
a260 <- absorbance(c(150, 400), wavelength = 260, unit = "mAU")
a260 / x
#> [1] 1.5 1.6

# Read from a file
fl <- system.file("extdata", "example_quadtec_3.txt", package = "chromr")
dat <- chrom_read_quadtech(fl)
absorbance_unit(dat$a280)
#> [1] "AU"
```
