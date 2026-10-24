#' Absorbance that knows its wavelength and unit
#'
#' @description
#' A `chromr_absorbance` vector holds absorbance values along with the
#' wavelength they were measured at and their unit, AU or mAU. It is a double
#' vector underneath, so it does arithmetic, compares and plots as numbers. The
#' readers return it, so columns such as `a280` keep their wavelength and unit
#' when they are combined, and plots label their axes with the unit.
#'
#' * `absorbance()` creates absorbance values.
#' * `is_absorbance()` tests whether an object is absorbance.
#' * `absorbance_wavelength()` and `absorbance_unit()` give the wavelength in nm
#'   and the unit.
#' * `absorbance_convert()` converts to another unit.
#'
#' Combining absorbance in AU with absorbance in mAU, for example appending
#' runs from different instruments, converts it to mAU. Combining absorbance at
#' different wavelengths, as when pivoting to one column with
#' [pivot_wl_longer()], gives absorbance with no wavelength. Combining with
#' plain numbers gives numbers.
#'
#' Adding, subtracting, and scaling by numbers gives absorbance, so a baseline
#' can be subtracted. Dividing absorbance by absorbance gives a plain ratio,
#' such as A260/A280, after converting both to the same unit.
#'
#' @param x For `absorbance()`, a numeric vector. For the other functions, an
#'   object to test, or absorbance.
#' @param wavelength The wavelength in nm, or `NA` if it isn't known.
#' @param unit The unit, `"AU"` or `"mAU"`.
#' @returns `absorbance()` and `absorbance_convert()` return a
#'   `chromr_absorbance` vector. `is_absorbance()` returns a single logical.
#'   `absorbance_wavelength()` returns a number and `absorbance_unit()` a
#'   string.
#' @export
#' @examples
#' x <- absorbance(c(0.1, 0.25), wavelength = 280, unit = "AU")
#' x
#' absorbance_convert(x, "mAU")
#'
#' # A260/A280 ratio
#' a260 <- absorbance(c(150, 400), wavelength = 260, unit = "mAU")
#' a260 / x
#'
#' # Read from a file
#' fl <- system.file("extdata", "example_quadtec_3.txt", package = "chromr")
#' dat <- chrom_read_quadtech(fl)
#' absorbance_unit(dat$a280)
absorbance <- function(x = double(), wavelength = NA, unit = c("mAU", "AU")) {
  x <- vctrs::vec_cast(x, double(), x_arg = "x")
  check_number_decimal(wavelength, min = 0, allow_na = TRUE)
  unit <- rlang::arg_match(unit)
  new_absorbance(x, as.numeric(wavelength), unit)
}

new_absorbance <- function(x = double(), wavelength = NA_real_, unit = "mAU") {
  vctrs::new_vctr(
    x,
    wavelength = wavelength,
    unit = unit,
    class = "chromr_absorbance"
  )
}

#' @rdname absorbance
#' @export
is_absorbance <- function(x) {
  inherits(x, "chromr_absorbance")
}

#' @rdname absorbance
#' @export
absorbance_wavelength <- function(x) {
  check_is_absorbance(x)
  attr(x, "wavelength")
}

#' @rdname absorbance
#' @export
absorbance_unit <- function(x) {
  check_is_absorbance(x)
  attr(x, "unit")
}

#' @rdname absorbance
#' @export
absorbance_convert <- function(x, unit = c("mAU", "AU")) {
  check_is_absorbance(x)
  unit <- rlang::arg_match(unit)
  scale <- unit_scale(attr(x, "unit")) / unit_scale(unit)
  new_absorbance(vctrs::vec_data(x) * scale, attr(x, "wavelength"), unit)
}

unit_scale <- function(unit) {
  c(AU = 1000, mAU = 1)[[unit]]
}

check_is_absorbance <- function(
  x,
  arg = rlang::caller_arg(x),
  call = rlang::caller_env()
) {
  if (!is_absorbance(x)) {
    stop_input_type(x, "absorbance", arg = arg, call = call)
  }
}

#' @export
format.chromr_absorbance <- function(x, ...) {
  format(vctrs::vec_data(x), ...)
}

#' @exportS3Method vctrs::vec_ptype_abbr
vec_ptype_abbr.chromr_absorbance <- function(x, ...) {
  attr(x, "unit")
}

#' @exportS3Method vctrs::vec_ptype_full
vec_ptype_full.chromr_absorbance <- function(x, ...) {
  wavelength <- attr(x, "wavelength")
  if (is.na(wavelength)) {
    paste0("absorbance<", attr(x, "unit"), ">")
  } else {
    paste0("absorbance<", wavelength, " nm, ", attr(x, "unit"), ">")
  }
}

#' @exportS3Method pillar::pillar_shaft
pillar_shaft.chromr_absorbance <- function(x, ...) {
  pillar::pillar_shaft(vctrs::vec_data(x), ...)
}

#' @exportS3Method vctrs::vec_ptype2
vec_ptype2.chromr_absorbance.chromr_absorbance <- function(x, y, ...) {
  wavelength <- attr(x, "wavelength")
  if (!identical(wavelength, attr(y, "wavelength"))) {
    wavelength <- NA_real_
  }
  unit <- attr(x, "unit")
  if (!identical(unit, attr(y, "unit"))) {
    unit <- "mAU"
  }
  new_absorbance(double(), wavelength, unit)
}
#' @exportS3Method vctrs::vec_ptype2
vec_ptype2.chromr_absorbance.double <- function(x, y, ...) double()
#' @exportS3Method vctrs::vec_ptype2
vec_ptype2.double.chromr_absorbance <- function(x, y, ...) double()
#' @exportS3Method vctrs::vec_ptype2
vec_ptype2.chromr_absorbance.integer <- function(x, y, ...) double()
#' @exportS3Method vctrs::vec_ptype2
vec_ptype2.integer.chromr_absorbance <- function(x, y, ...) double()

#' @exportS3Method vctrs::vec_cast
vec_cast.chromr_absorbance.chromr_absorbance <- function(
  x,
  to,
  ...,
  x_arg = "",
  to_arg = "",
  call = rlang::caller_env()
) {
  to_wavelength <- attr(to, "wavelength")
  if (
    !is.na(to_wavelength) && !identical(attr(x, "wavelength"), to_wavelength)
  ) {
    vctrs::stop_incompatible_cast(
      x,
      to,
      x_arg = x_arg,
      to_arg = to_arg,
      call = call
    )
  }
  x <- absorbance_convert(x, attr(to, "unit"))
  attr(x, "wavelength") <- to_wavelength
  x
}
#' @exportS3Method vctrs::vec_cast
vec_cast.chromr_absorbance.double <- function(x, to, ...) {
  new_absorbance(x, attr(to, "wavelength"), attr(to, "unit"))
}
#' @exportS3Method vctrs::vec_cast
vec_cast.chromr_absorbance.integer <- function(x, to, ...) {
  new_absorbance(as.double(x), attr(to, "wavelength"), attr(to, "unit"))
}
#' @exportS3Method vctrs::vec_cast
vec_cast.double.chromr_absorbance <- function(x, to, ...) {
  vctrs::vec_data(x)
}

#' @exportS3Method vctrs::vec_arith
vec_arith.chromr_absorbance <- function(op, x, y, ...) {
  UseMethod("vec_arith.chromr_absorbance", y)
}

#' @export
#' @method vec_arith.chromr_absorbance default
vec_arith.chromr_absorbance.default <- function(op, x, y, ...) {
  vctrs::stop_incompatible_op(op, x, y)
}

#' @export
#' @method vec_arith.chromr_absorbance chromr_absorbance
vec_arith.chromr_absorbance.chromr_absorbance <- function(op, x, y, ...) {
  ptype <- vctrs::vec_ptype2(x, y)
  args <- vctrs::vec_cast_common(x, y, .to = ptype)
  out <- vctrs::vec_arith_base(
    op,
    vctrs::vec_data(args[[1]]),
    vctrs::vec_data(args[[2]])
  )
  switch(
    op,
    "+" = ,
    "-" = vctrs::vec_restore(out, ptype),
    "/" = out,
    vctrs::stop_incompatible_op(op, x, y)
  )
}

#' @export
#' @method vec_arith.chromr_absorbance numeric
vec_arith.chromr_absorbance.numeric <- function(op, x, y, ...) {
  out <- vctrs::vec_arith_base(op, vctrs::vec_data(x), y)
  if (op %in% c("+", "-", "*", "/")) vctrs::vec_restore(out, x) else out
}

#' @export
#' @method vec_arith.chromr_absorbance MISSING
vec_arith.chromr_absorbance.MISSING <- function(op, x, y, ...) {
  switch(
    op,
    "-" = vctrs::vec_restore(-vctrs::vec_data(x), x),
    "+" = x,
    vctrs::stop_incompatible_op(op, x, y)
  )
}

#' @exportS3Method vctrs::vec_arith.numeric
vec_arith.numeric.chromr_absorbance <- function(op, x, y, ...) {
  out <- vctrs::vec_arith_base(op, x, vctrs::vec_data(y))
  if (op %in% c("+", "-", "*")) vctrs::vec_restore(out, y) else out
}

#' @exportS3Method vctrs::vec_math
vec_math.chromr_absorbance <- function(.fn, .x, ...) {
  out <- vctrs::vec_math_base(.fn, vctrs::vec_data(.x), ...)
  keep <- c("sum", "mean", "min", "max", "range", "cumsum", "cummin", "cummax")
  if (.fn %in% keep) vctrs::vec_restore(out, .x) else out
}

#' @exportS3Method ggplot2::scale_type
scale_type.chromr_absorbance <- function(x) {
  "continuous"
}

#' @exportS3Method scales::rescale
rescale.chromr_absorbance <- function(x, ...) {
  scales::rescale(vctrs::vec_data(x), ...)
}

# The absorbance columns of a data frame: absorbance vectors, or for plain data
# frames, columns named like `a280` or `uv`.
absorbance_columns <- function(data) {
  typed <- purrr::map_lgl(data, is_absorbance)
  if (any(typed)) {
    return(names(data)[typed])
  }
  names(data)[is_absorbance_name(tolower(names(data)))]
}

# Axis title for the absorbance columns of a data frame, with their unit if
# they share one.
absorbance_title <- function(data, columns = absorbance_columns(data)) {
  units <- unique(unlist(purrr::map(data[columns], attr, "unit")))
  if (length(units) == 1) {
    paste0("Absorbance (", units, ")")
  } else {
    "Absorbance"
  }
}
