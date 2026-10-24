#' Numbers with a unit
#'
#' @description
#' A `chromr_quantity` vector holds numbers along with their unit, such as the
#' volume in mL or the pressure in psi. The readers give every column other
#' than `fraction` and absorbance (see [absorbance()]) the unit recorded in the
#' file. It is a double vector underneath, so it compares, filters and does
#' arithmetic with plain numbers: `volume > 10` works as it always has.
#'
#' * `quantity()` creates numbers with a unit.
#' * `is_quantity()` tests whether an object is numbers with a unit.
#' * `quantity_unit()` gives the unit.
#' * `quantity_convert()` converts to another unit of the same kind.
#'
#' The units chromr knows, and can convert between, are:
#'
#' * time: `s`, `min`, `h`
#' * volume: `mL`, `L`, `µL`
#' * flow: `mL/min`, `mL/h`, `L/min`
#' * pressure: `MPa`, `kPa`, `bar`, `psi`
#' * conductivity: `mS/cm`, `µS/cm`, `S/m`
#' * `%`, and temperature in `°C`
#'
#' Other units are kept as they are, but can't be converted. Common spellings
#' in instrument files, such as "ml", "Second" and "PSI", become the units
#' above.
#'
#' Combining numbers in different units of the same kind, as when appending a
#' run recorded in MPa to one in psi, converts them to the unit of the first.
#' Combining with plain numbers gives plain numbers. Adding, subtracting and
#' scaling by numbers keeps the unit, and dividing by numbers of the same kind
#' gives a plain ratio.
#'
#' @param x For `quantity()`, a numeric vector. For the other functions, an
#'   object to test, or numbers with a unit.
#' @param unit The unit, such as `"mL"`.
#' @returns `quantity()` and `quantity_convert()` return a `chromr_quantity`
#'   vector. `is_quantity()` returns a single logical and `quantity_unit()` a
#'   string.
#' @export
#' @examples
#' x <- quantity(c(0.5, 1.2), "MPa")
#' x
#' quantity_convert(x, "psi")
#'
#' x > 1
#' x * 2
#'
#' # Read from a file
#' fl <- system.file("extdata", "example_akta_2.asc", package = "chromr")
#' dat <- read_akta_old(fl)
#' quantity_unit(dat$pressure)
quantity <- function(x = double(), unit) {
  x <- vctrs::vec_cast(x, double(), x_arg = "x")
  check_string(unit)
  new_quantity(x, standard_unit(unit))
}

new_quantity <- function(x = double(), unit = "") {
  vctrs::new_vctr(x, unit = unit, class = "chromr_quantity")
}

#' @rdname quantity
#' @export
is_quantity <- function(x) {
  inherits(x, "chromr_quantity")
}

#' @rdname quantity
#' @export
quantity_unit <- function(x) {
  check_is_quantity(x)
  attr(x, "unit")
}

#' @rdname quantity
#' @export
quantity_convert <- function(x, unit) {
  check_is_quantity(x)
  check_string(unit)
  unit <- standard_unit(unit)
  from <- attr(x, "unit")
  if (identical(from, unit)) {
    return(x)
  }
  if (!units_convertible(from, unit)) {
    cli::cli_abort("Can't convert {.val {from}} to {.val {unit}}.")
  }
  new_quantity(vctrs::vec_data(x) * unit_factor(from) / unit_factor(unit), unit)
}

check_is_quantity <- function(
  x,
  arg = rlang::caller_arg(x),
  call = rlang::caller_env()
) {
  if (!is_quantity(x)) {
    stop_input_type(x, "numbers with a unit", arg = arg, call = call)
  }
}

known_units <- tibble::tribble(
  ~unit,
  ~kind,
  ~factor,
  "s",
  "time",
  1,
  "min",
  "time",
  60,
  "h",
  "time",
  3600,
  "mL",
  "volume",
  1,
  "L",
  "volume",
  1000,
  "\u00b5L",
  "volume",
  0.001,
  "mL/min",
  "flow",
  1,
  "mL/h",
  "flow",
  1 / 60,
  "L/min",
  "flow",
  1000,
  "MPa",
  "pressure",
  1,
  "kPa",
  "pressure",
  0.001,
  "bar",
  "pressure",
  0.1,
  "psi",
  "pressure",
  0.00689476,
  "mS/cm",
  "conductivity",
  1,
  "\u00b5S/cm",
  "conductivity",
  0.001,
  "S/m",
  "conductivity",
  10,
  "%",
  "percent",
  1,
  "\u00b0C",
  "temperature",
  1
)

unit_aliases <- c(
  ml = "mL",
  second = "s",
  seconds = "s",
  sec = "s",
  minute = "min",
  minutes = "min",
  hour = "h",
  hours = "h",
  "%b" = "%",
  "ml/min" = "mL/min",
  "c" = "\u00b0C",
  "degc" = "\u00b0C",
  "deg c" = "\u00b0C",
  ul = "\u00b5L",
  "\u03bcl" = "\u00b5L"
)

# The standard spelling of a unit: a known unit matched ignoring case, or a
# common alias, or else the unit as given.
standard_unit <- function(unit) {
  unit <- stringr::str_squish(unit)
  key <- tolower(unit)
  known <- known_units$unit[match(key, tolower(known_units$unit))]
  alias <- unname(unit_aliases[key])
  dplyr::coalesce(known, alias, unit)
}

unit_kind <- function(unit) {
  known_units$kind[match(unit, known_units$unit)]
}

unit_factor <- function(unit) {
  known_units$factor[match(unit, known_units$unit)]
}

units_convertible <- function(x, y) {
  identical(x, y) ||
    (!is.na(unit_kind(x)) && identical(unit_kind(x), unit_kind(y)))
}

#' @export
format.chromr_quantity <- function(x, ...) {
  format(vctrs::vec_data(x), ...)
}

#' @exportS3Method vctrs::vec_ptype_abbr
vec_ptype_abbr.chromr_quantity <- function(x, ...) {
  attr(x, "unit")
}

#' @exportS3Method vctrs::vec_ptype_full
vec_ptype_full.chromr_quantity <- function(x, ...) {
  paste0("quantity<", attr(x, "unit"), ">")
}

#' @exportS3Method pillar::pillar_shaft
pillar_shaft.chromr_quantity <- function(x, ...) {
  pillar::pillar_shaft(vctrs::vec_data(x), ...)
}

#' @exportS3Method vctrs::vec_ptype2
vec_ptype2.chromr_quantity.chromr_quantity <- function(
  x,
  y,
  ...,
  x_arg = "",
  y_arg = "",
  call = rlang::caller_env()
) {
  if (!units_convertible(attr(x, "unit"), attr(y, "unit"))) {
    vctrs::stop_incompatible_type(
      x,
      y,
      x_arg = x_arg,
      y_arg = y_arg,
      call = call
    )
  }
  new_quantity(double(), attr(x, "unit"))
}
#' @exportS3Method vctrs::vec_ptype2
vec_ptype2.chromr_quantity.double <- function(x, y, ...) double()
#' @exportS3Method vctrs::vec_ptype2
vec_ptype2.double.chromr_quantity <- function(x, y, ...) double()
#' @exportS3Method vctrs::vec_ptype2
vec_ptype2.chromr_quantity.integer <- function(x, y, ...) double()
#' @exportS3Method vctrs::vec_ptype2
vec_ptype2.integer.chromr_quantity <- function(x, y, ...) double()

#' @exportS3Method vctrs::vec_cast
vec_cast.chromr_quantity.chromr_quantity <- function(
  x,
  to,
  ...,
  x_arg = "",
  to_arg = "",
  call = rlang::caller_env()
) {
  if (!units_convertible(attr(x, "unit"), attr(to, "unit"))) {
    vctrs::stop_incompatible_cast(
      x,
      to,
      x_arg = x_arg,
      to_arg = to_arg,
      call = call
    )
  }
  quantity_convert(x, attr(to, "unit"))
}
#' @exportS3Method vctrs::vec_cast
vec_cast.chromr_quantity.double <- function(x, to, ...) {
  new_quantity(x, attr(to, "unit"))
}
#' @exportS3Method vctrs::vec_cast
vec_cast.chromr_quantity.integer <- function(x, to, ...) {
  new_quantity(as.double(x), attr(to, "unit"))
}
#' @exportS3Method vctrs::vec_cast
vec_cast.double.chromr_quantity <- function(x, to, ...) {
  vctrs::vec_data(x)
}

#' @exportS3Method vctrs::vec_arith
vec_arith.chromr_quantity <- function(op, x, y, ...) {
  UseMethod("vec_arith.chromr_quantity", y)
}

#' @export
#' @method vec_arith.chromr_quantity default
vec_arith.chromr_quantity.default <- function(op, x, y, ...) {
  vctrs::stop_incompatible_op(op, x, y)
}

#' @export
#' @method vec_arith.chromr_quantity chromr_quantity
vec_arith.chromr_quantity.chromr_quantity <- function(op, x, y, ...) {
  if (!op %in% c("+", "-", "/")) {
    vctrs::stop_incompatible_op(op, x, y)
  }
  y <- vctrs::vec_cast(y, x)
  out <- vctrs::vec_arith_base(op, vctrs::vec_data(x), vctrs::vec_data(y))
  if (op == "/") out else vctrs::vec_restore(out, x)
}

#' @export
#' @method vec_arith.chromr_quantity numeric
vec_arith.chromr_quantity.numeric <- function(op, x, y, ...) {
  out <- vctrs::vec_arith_base(op, vctrs::vec_data(x), y)
  if (op %in% c("+", "-", "*", "/")) vctrs::vec_restore(out, x) else out
}

#' @export
#' @method vec_arith.chromr_quantity MISSING
vec_arith.chromr_quantity.MISSING <- function(op, x, y, ...) {
  switch(
    op,
    "-" = vctrs::vec_restore(-vctrs::vec_data(x), x),
    "+" = x,
    vctrs::stop_incompatible_op(op, x, y)
  )
}

#' @exportS3Method vctrs::vec_arith.numeric
vec_arith.numeric.chromr_quantity <- function(op, x, y, ...) {
  out <- vctrs::vec_arith_base(op, x, vctrs::vec_data(y))
  if (op %in% c("+", "-", "*")) vctrs::vec_restore(out, y) else out
}

#' @exportS3Method vctrs::vec_math
vec_math.chromr_quantity <- function(.fn, .x, ...) {
  out <- vctrs::vec_math_base(.fn, vctrs::vec_data(.x), ...)
  keep <- c("sum", "mean", "min", "max", "range", "cumsum", "cummin", "cummax")
  if (.fn %in% keep) vctrs::vec_restore(out, .x) else out
}

#' @exportS3Method ggplot2::scale_type
scale_type.chromr_quantity <- function(x) {
  "continuous"
}

#' @exportS3Method scales::rescale
rescale.chromr_quantity <- function(x, ...) {
  scales::rescale(vctrs::vec_data(x), ...)
}

# An axis title with the unit of a column, if it has one.
unit_title <- function(title, x) {
  if (is_quantity(x)) paste0(title, " (", quantity_unit(x), ")") else title
}
