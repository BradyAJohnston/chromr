#' Fraction numbers
#'
#' @description
#' A `chromr_fraction` vector holds the number of the fraction each point of a
#' chromatogram was collected into. It is an integer vector underneath, so
#' fractions compare, sort and do arithmetic as numbers, but points that
#' weren't collected into a fraction are always `NA`. Instruments record these
#' as 0 or leave them out; `fraction()` turns 0 into `NA`.
#'
#' * `fraction()` creates fraction numbers.
#' * `is_fraction()` tests whether an object is a vector of fraction numbers.
#'
#' Adding or subtracting numbers gives fraction numbers, as when continuing the
#' numbering of a second run. Combining fraction numbers with other numbers
#' gives numbers. Use `as.integer()` to get the plain integer vector.
#'
#' @param x For `fraction()`, a vector of whole numbers. For `is_fraction()`,
#'   an object to test.
#' @returns `fraction()` returns a `chromr_fraction` vector. `is_fraction()`
#'   returns a single logical.
#' @seealso [chrom_fractions()] for the volumes each fraction covers, and
#'   [fraction_to_well()] for the well each fraction was collected into.
#' @export
#' @examples
#' x <- fraction(c(0, 0, 1, 1, 2, 2, 0))
#' x
#'
#' x + 10
#' x == 2
fraction <- function(x = integer()) {
  if (is_fraction(x)) {
    return(x)
  }
  x <- vctrs::vec_cast(x, integer(), x_arg = "x")
  if (any(x < 0, na.rm = TRUE)) {
    cli::cli_abort("{.arg x} can't contain negative fraction numbers.")
  }
  x[x %in% 0L] <- NA_integer_
  new_fraction(x)
}

new_fraction <- function(x = integer()) {
  vctrs::new_vctr(x, class = "chromr_fraction")
}

#' @rdname fraction
#' @export
is_fraction <- function(x) {
  inherits(x, "chromr_fraction")
}

#' @export
format.chromr_fraction <- function(x, ...) {
  format(vctrs::vec_data(x), ...)
}

#' @export
as.character.chromr_fraction <- function(x, ...) {
  as.character(vctrs::vec_data(x))
}

#' @exportS3Method vctrs::vec_ptype_abbr
vec_ptype_abbr.chromr_fraction <- function(x, ...) {
  "frac"
}

#' @exportS3Method vctrs::vec_ptype_full
vec_ptype_full.chromr_fraction <- function(x, ...) {
  "fraction"
}

#' @exportS3Method pillar::pillar_shaft
pillar_shaft.chromr_fraction <- function(x, ...) {
  pillar::pillar_shaft(vctrs::vec_data(x), ...)
}

#' @exportS3Method vctrs::vec_ptype2
vec_ptype2.chromr_fraction.chromr_fraction <- function(x, y, ...) {
  new_fraction()
}
#' @exportS3Method vctrs::vec_ptype2
vec_ptype2.chromr_fraction.integer <- function(x, y, ...) integer()
#' @exportS3Method vctrs::vec_ptype2
vec_ptype2.integer.chromr_fraction <- function(x, y, ...) integer()
#' @exportS3Method vctrs::vec_ptype2
vec_ptype2.chromr_fraction.double <- function(x, y, ...) double()
#' @exportS3Method vctrs::vec_ptype2
vec_ptype2.double.chromr_fraction <- function(x, y, ...) double()

#' @exportS3Method vctrs::vec_cast
vec_cast.chromr_fraction.chromr_fraction <- function(x, to, ...) x
#' @exportS3Method vctrs::vec_cast
vec_cast.chromr_fraction.integer <- function(x, to, ...) fraction(x)
#' @exportS3Method vctrs::vec_cast
vec_cast.integer.chromr_fraction <- function(x, to, ...) vctrs::vec_data(x)
#' @exportS3Method vctrs::vec_cast
vec_cast.chromr_fraction.double <- function(x, to, ...) fraction(x)
#' @exportS3Method vctrs::vec_cast
vec_cast.double.chromr_fraction <- function(x, to, ...) {
  as.double(vctrs::vec_data(x))
}

#' @exportS3Method vctrs::vec_arith
vec_arith.chromr_fraction <- function(op, x, y, ...) {
  UseMethod("vec_arith.chromr_fraction", y)
}

#' @export
#' @method vec_arith.chromr_fraction default
vec_arith.chromr_fraction.default <- function(op, x, y, ...) {
  vctrs::stop_incompatible_op(op, x, y)
}

#' @export
#' @method vec_arith.chromr_fraction numeric
vec_arith.chromr_fraction.numeric <- function(op, x, y, ...) {
  out <- vctrs::vec_arith_base(op, vctrs::vec_data(x), y)
  if (op %in% c("+", "-") && rlang::is_integerish(y)) fraction(out) else out
}

#' @export
#' @method vec_arith.chromr_fraction chromr_fraction
vec_arith.chromr_fraction.chromr_fraction <- function(op, x, y, ...) {
  vctrs::vec_arith_base(op, vctrs::vec_data(x), vctrs::vec_data(y))
}

#' @export
#' @method vec_arith.chromr_fraction MISSING
vec_arith.chromr_fraction.MISSING <- function(op, x, y, ...) {
  if (op == "+") x else vctrs::stop_incompatible_op(op, x, y)
}

#' @exportS3Method vctrs::vec_arith.numeric
vec_arith.numeric.chromr_fraction <- function(op, x, y, ...) {
  out <- vctrs::vec_arith_base(op, x, vctrs::vec_data(y))
  if (op == "+" && rlang::is_integerish(x)) fraction(out) else out
}

#' @exportS3Method vctrs::vec_math
vec_math.chromr_fraction <- function(.fn, .x, ...) {
  out <- vctrs::vec_math_base(.fn, vctrs::vec_data(.x), ...)
  if (.fn %in% c("min", "max", "range", "cummin", "cummax")) {
    new_fraction(as.integer(out))
  } else {
    out
  }
}

#' @exportS3Method ggplot2::scale_type
scale_type.chromr_fraction <- function(x) {
  "continuous"
}

#' @exportS3Method scales::rescale
rescale.chromr_fraction <- function(x, ...) {
  scales::rescale(vctrs::vec_data(x), ...)
}

#' The volumes each fraction covers
#'
#' Gives the start and end volume of each fraction in a chromatogram. A
#' fraction starts at the first point collected into it and ends at the first
#' point after it, so the fractions of a run meet end to end. These are the
#' sampled volumes, so they are within one point of the volumes the fraction
#' collector recorded. A fraction collected into more than once, as when the
#' collector runs out of tubes and starts again at the first, has a row for
#' each time.
#'
#' @param data A chromatogram from `chrom_read_*()`, with `volume` and
#'   `fraction` columns. Appended runs, with a `run` column, are handled run by
#'   run.
#' @returns A [tibble][tibble::tibble-package] with a row for each fraction and
#'   the columns `fraction`, `start` and `end`, and `run` if `data` has one.
#' @export
#' @examples
#' fl <- system.file("extdata", "example_akta_2.asc", package = "chromr")
#' read_akta_old(fl) |>
#'   chrom_fractions()
chrom_fractions <- function(data) {
  check_data_frame(data)
  check_columns(data, c("volume", "fraction"))

  by <- if ("run" %in% names(data)) "run" else character()
  data |>
    dplyr::select(dplyr::all_of(c(by, "volume", "fraction"))) |>
    dplyr::reframe(
      fraction_bounds(.data$volume, .data$fraction),
      .by = dplyr::all_of(by)
    ) |>
    dplyr::mutate(fraction = fraction(.data$fraction))
}

fraction_bounds <- function(volume, fraction) {
  ord <- order(volume)
  volume <- volume[ord]
  fraction <- vctrs::vec_data(fraction(fraction))[ord]

  runs <- rle(dplyr::coalesce(fraction, -1L))
  last <- cumsum(runs$lengths)
  first <- last - runs$lengths + 1
  next_point <- pmin(last + 1, length(volume))

  tibble::tibble(
    fraction = runs$values,
    start = volume[first],
    end = volume[next_point]
  ) |>
    dplyr::filter(.data$fraction > 0)
}

#' The well each fraction was collected into
#'
#' Fraction collectors fill deep-well plates one well per fraction, so fraction
#' 1 goes into A01, fraction 2 into A02, and so on. `fraction_to_well()` gives
#' the well of each fraction as well IDs from the
#' [wellr](https://bradyajohnston.github.io/wellr/) package, so fractions can be
#' joined to plate-reader data.
#'
#' @param x Fraction numbers.
#' @param plate The plate the fractions were collected into: the number of
#'   wells in a standard plate, or the number of rows and columns. See
#'   [wellr::well()].
#' @param colwise Whether the collector fills the plate down the columns, from
#'   A01 to B01, instead of along the rows, from A01 to A02.
#' @returns A `wellr_well` vector of well IDs, `NA` where `x` is `NA`.
#' @export
#' @examplesIf rlang::is_installed("wellr")
#' fraction_to_well(fraction(c(1, 2, 13, NA)))
#' fraction_to_well(1:3, colwise = TRUE)
fraction_to_well <- function(x, plate = 96, colwise = FALSE) {
  rlang::check_installed("wellr", version = "0.4.0")
  x <- fraction(x)
  check_bool(colwise)

  out <- wellr::well(rep(NA_character_, length(x)), plate = plate)
  collected <- !is.na(x)
  out[collected] <- wellr::well_from_index(
    vctrs::vec_data(x)[collected],
    plate = plate,
    colwise = colwise
  )
  out
}
