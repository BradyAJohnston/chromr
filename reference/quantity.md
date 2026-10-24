# Numbers with a unit

A `chromr_quantity` vector holds numbers along with their unit, such as
the volume in mL or the pressure in psi. The readers give every column
other than `fraction` and absorbance (see
[`absorbance()`](https://bradyajohnston.github.io/chromr/reference/absorbance.md))
the unit recorded in the file. It is a double vector underneath, so it
compares, filters and does arithmetic with plain numbers: `volume > 10`
works as it always has.

- `quantity()` creates numbers with a unit.

- `is_quantity()` tests whether an object is numbers with a unit.

- `quantity_unit()` gives the unit.

- `quantity_convert()` converts to another unit of the same kind.

The units chromr knows, and can convert between, are:

- time: `s`, `min`, `h`

- volume: `mL`, `L`, `µL`

- flow: `mL/min`, `mL/h`, `L/min`

- pressure: `MPa`, `kPa`, `bar`, `psi`

- conductivity: `mS/cm`, `µS/cm`, `S/m`

- `%`, and temperature in `°C`

Other units are kept as they are, but can't be converted. Common
spellings in instrument files, such as "ml", "Second" and "PSI", become
the units above.

Combining numbers in different units of the same kind, as when appending
a run recorded in MPa to one in psi, converts them to the unit of the
first. Combining with plain numbers gives plain numbers. Adding,
subtracting and scaling by numbers keeps the unit, and dividing by
numbers of the same kind gives a plain ratio.

## Usage

``` r
quantity(x = double(), unit)

is_quantity(x)

quantity_unit(x)

quantity_convert(x, unit)
```

## Arguments

- x:

  For `quantity()`, a numeric vector. For the other functions, an object
  to test, or numbers with a unit.

- unit:

  The unit, such as `"mL"`.

## Value

`quantity()` and `quantity_convert()` return a `chromr_quantity` vector.
`is_quantity()` returns a single logical and `quantity_unit()` a string.

## Examples

``` r
x <- quantity(c(0.5, 1.2), "MPa")
x
#> <quantity<MPa>[2]>
#> [1] 0.5 1.2
quantity_convert(x, "psi")
#> <quantity<psi>[2]>
#> [1]  72.51884 174.04522

x > 1
#> [1] FALSE  TRUE
x * 2
#> <quantity<MPa>[2]>
#> [1] 1.0 2.4

# Read from a file
fl <- system.file("extdata", "example_akta_2.asc", package = "chromr")
dat <- read_akta_old(fl)
quantity_unit(dat$pressure)
#> [1] "MPa"
```
