#' Read .csv Chromatogram from the BioRad NGC
#'
#' Each trace in an NGC export is recorded at its own volumes. The traces are
#' combined into one row per volume of the first absorbance trace, and the
#' other traces are interpolated onto those volumes.
#'
#' @param file File path to the `.csv` file.
#' @param skip Number of lines to skip before begin reading. Usually 1 line.
#' @inheritSection chrom_read_quadtech Columns
#' @return A chromatogram: a [tibble][tibble::tibble-package] with the run's
#'   metadata, see [chrom_meta()].
#' @export
#'
#' @examples
#' fl <- system.file(
#'   "extdata",
#'   "ngc_sec.csv",
#'   package = "chromr"
#' )
#'
#' dat <- chrom_read_ngc(fl)
#' dat
#'
chrom_read_ngc <- function(file, skip = 1) {
  check_number_whole(skip, min = 0)
  header <- readr::read_lines(file, skip = skip, n_max = 1)
  header <- readr::read_csv(I(header), col_names = FALSE, col_types = "c")
  header <- unlist(header, use.names = FALSE)
  dat <- readr::read_csv(
    file,
    skip = skip + 1,
    col_names = FALSE,
    col_types = readr::cols(.default = "d")
  )

  run <- if (skip > 0) {
    first <- readr::read_lines(file, n_max = 1)
    stringr::str_extract(first, "^[^,]*")
  } else {
    NA_character_
  }

  # Columns come in pairs: "<trace>_volume" and "<trace>_<unit>".
  traces <- purrr::map(seq(1, length(header), by = 2), \(i) {
    list(
      name = stringr::str_remove(header[[i + 1]], "_[^_]*$"),
      unit = stringr::str_extract(header[[i + 1]], "[^_]*$"),
      volume = dat[[i]],
      value = dat[[i + 1]]
    )
  })

  combine_traces(traces, instrument = "BioRad NGC", run = run, file = file)
}

#' Read BioRad QuadTech Chromatogram Files
#'
#' @param file Exported `.TXT` chromatogram file from the BioRad QuadTec.
#' @param interp_volume Logical. If TRUE, interpolates the values in the volume
#'   column based on the values in the time column.
#'
#' @section Columns:
#' The readers all give one row per sampled point, with the columns the
#' instrument recorded:
#'
#' * `time`, in seconds, when the instrument records it, and `volume`, in mL.
#' * `fraction`, the fraction each row was collected into, or `NA` when it
#'   wasn't collected.
#' * Absorbance, named after its wavelength in nm, such as `a280`, or `uv` when
#'   the instrument doesn't record the wavelength.
#' * The other traces under standard names: `conductivity`,
#'   `conductivity_percent`, `percent_b`, `pressure`, `flow` and
#'   `temperature`.
#'
#' `fraction` is a [fraction] vector and the absorbance columns are
#' [absorbance] vectors, which record their wavelength and unit. The other
#' columns are [quantity] vectors with the unit recorded in the file, such as
#' `s`, `mL`, `mS/cm`, `psi` or `MPa`.
#'
#' @return A chromatogram: a [tibble][tibble::tibble-package] with the run's
#'   metadata, see [chrom_meta()].
#' @export
#'
#' @examples
#' fl <- system.file("extdata",
#'   "example_quadtec_3.txt",
#'   package = "chromr"
#' )
#' # just read
#' fl |>
#'   chrom_read_quadtech()
#'
#' # read without interpolating volume
#' fl |>
#'   chrom_read_quadtech(interp_volume = FALSE)
#' # read then plot
#' fl |>
#'   chrom_read_quadtech() |>
#'   chrom_plot()
chrom_read_quadtech <- function(file, interp_volume = TRUE) {
  check_bool(interp_volume)
  start_line <- chrom_find_data_start_line(file, n_lines = 50)

  data <- readr::read_csv(
    file = file,
    skip = start_line - 2,
    col_types = readr::cols()
  )

  channels <- chrom_get_meta_quadtech(file, start_line = start_line)
  units <- channels$unit[match(names(data), channels$name)]
  units <- dplyr::coalesce(units, "")
  names(data) <- trace_column(names(data), units)
  names(units) <- names(data)

  if (interp_volume && "volume" %in% names(data)) {
    data$volume <- stats::approx(unique(data$volume), n = nrow(data))$y
  }

  info <- function(name) {
    value <- channels$unit[channels$name == name]
    if (length(value) == 0) NA_character_ else value[[1]]
  }
  tidy_traces(
    data,
    units,
    instrument = "BioRad QuadTec",
    run = info("Run Name"),
    date = stringr::str_squish(info("Run Date")),
    method = info("Method Name"),
    file = file
  )
}


#' Reads the Channels of a QuadTech Chromatogram
#'
#' @param file Path to the chromatogram file.
#' @param start_line Start of the data and thus end of the metadata. Determined
#'   with `chromr::chrom_find_data_start_line()`
#'
#' @return a [tibble][tibble::tibble-package] with the `name` and `unit` of
#'   each channel, such as "QuadTec 1" and "(280.0 nm), AU".
#' @noRd
chrom_get_meta_quadtech <- function(file, start_line) {
  lines <- readr::read_lines(file, n_max = start_line - 2)
  lines <- stringr::str_split_fixed(lines, ",", 2)

  tibble::tibble(
    name = stringr::str_squish(lines[, 1]),
    unit = stringr::str_squish(lines[, 2])
  )
}


#' Find the Line Where Data Begins
#'
#' Finds the line where the tabular data begins. This is then used to
#' start the reading of the data with `readr::read_csv()` and end the reading
#' of the metadata.
#'
#' @param file File path to the file to read.
#' @param n_lines Number of lines to search for the start of the data.
#'
#' @return Single integer of the start of the data.
#' @noRd
chrom_find_data_start_line <- function(file, n_lines = 50) {
  start_line <- readr::read_lines(file, n_max = n_lines) |>
    stringr::str_trim() |>
    stringr::str_which("^\\d") |>
    min()

  start_line
}

#' Add Volume Column From Time Units
#'
#' Adds a column with calculated volums from the time column given a particular
#' flow rate. Currently only constant flow rates are supported.
#'
#' @param .data Data frame or tibble with a column called 'time'.
#' @param flow_rate Flow rate in ml/min.
#' @param time Time unit when exported, used when the time column doesn't
#'   record its unit, as columns from `chrom_read_*()` do.
#'
#' @return a [tibble][tibble::tibble-package]
#' @export
#'
#' @examples
#' fl <- system.file(
#'   "extdata",
#'   "example_quadtec_4.txt",
#'   package = "chromr"
#' )
#' # read just the data
#' dat <- fl |>
#'   chrom_read_quadtech()
#' dat
#' # add a volume given a constant flow rate
#' dat |>
#'   chrom_add_volume(0.3)
chrom_add_volume <-
  function(.data, flow_rate = 0.5, time = "second") {
    minutes <- if (is_quantity(.data$time)) {
      vctrs::vec_data(quantity_convert(.data$time, "min"))
    } else {
      time_adjust <- switch(time, "second" = 60, "minute" = 1, "hour" = 1 / 60)
      .data$time / time_adjust
    }

    .data |>
      dplyr::mutate(volume = quantity(minutes * flow_rate, "mL")) |>
      dplyr::select("time", "volume", dplyr::everything())
  }
