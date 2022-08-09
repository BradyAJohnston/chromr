#' Interpolate Volume from Time
#'
#' Interpolates the volume column values, based on the time column values.
#'
#' Some FPLC systems don't report accurate volume data, but do report accurate
#' timing data. This function takes the time and volume data and interpolates
#' the otherwise constant volume data in accordance with the time points.
#'
#' @param .data Data frame with a volume column to be interpolated.
#' @param time Name of the time column to use for interpolation.
#' @param volume Name of the volumn column to interpolate along the time.
#'
#' @importFrom rlang :=
#' @return a [tibble][tibble::tibble-package]
#' @export
#'
#' @examples
#' fl <- system.file(
#'   "extdata",
#'   "sec.txt",
#'   package = "chromr"
#' )
#'
#' dat <- fl |>
#'   chrom_read_quadtech(interp_volume = FALSE)
#'
#' dat
#'
#' dat |>
#'   chrom_interp_volume(time, volume)
#'
chrom_interp_volume <- function(.data, time, volume) {
  .data |>
    dplyr::select({{ time }}, {{ volume }}) |>
    unique() |>
    dplyr::mutate(
      same = {{ volume }} != dplyr::lag({{ volume }}),
      same = dplyr::if_else(is.na(same), TRUE, FALSE),
      group = cumsum(same)
    ) |>
    dplyr::group_by(group, {{ volume }}) |>
    tidyr::nest() |>
    dplyr::ungroup() |>
    dplyr::mutate(vol_new = dplyr::lead({{ volume }})) |>
    tidyr::unnest(data) |>
    dplyr::group_by({{ volume }}) |>
    dplyr::mutate(
      row = dplyr::row_number(),
      factor = row / max(row),
      vol_adjusted = {{ volume }} + factor * (vol_new - {{ volume }})
    ) |>
    dplyr::ungroup() |>
    dplyr::select({{ time }}, {{ volume }} := vol_adjusted)
}

#' Read BioRad QuadTech Chromatogram Files
#'
#' @param file Exported `.TXT` chromatogram file from the BioRad QuadTech.
#' @param interp_volume Logical. If TRUE, interpolates the values in the volume
#'   column based on the values in the time column.
#'
#' @return a [tibble][tibble::tibble-package]
#' @export
#'
#' @examples
#' fl <- system.file("extdata",
#'                   "sec.txt",
#'                   package = "chromr")
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
  start_line <- chrom_find_data_start_line(file, n_lines = 50)

  data <- readr::read_csv(
    file = file,
    skip = start_line - 2,
    col_types = readr::cols()
  )

  met <- chrom_get_meta_quadtech(file, start_line = start_line)

  wavelengths <- met |>
    dplyr::filter(stringr::str_detect(meta, "Quad")) |>
    dplyr::mutate(
      wl = as.numeric(stringr::str_extract(value, "\\d{3}")),
      channel = as.numeric(stringr::str_extract(meta, "\\d$"))
    )


  data <- data |>
    janitor::clean_names() |>
    tidyr::pivot_longer(
      cols = dplyr::contains("quad"),
      values_to = "abs"
    ) |>
    dplyr::mutate(name = as.numeric(stringr::str_extract(name, "\\d$"))) |>
    dplyr::rename(channel = name) |>
    dplyr::left_join(wavelengths,
      by = c("channel" = "channel")
    )

  volume_present <- as.logical(sum(stringr::str_detect(colnames(data), "volume")))

  if (interp_volume & volume_present) {
    volume_interp <- chrom_interp_volume(data, time, volume)

    data <- data |>
      dplyr::select(-volume) |>
      dplyr::left_join(volume_interp, by = c("time" = "time"))
  }

  if (volume_present) {
    data <- data |>
      dplyr::select(
        time,
        volume,
        wl,
        abs,
        dplyr::everything(),
        -meta,
        -value
      )
  } else {
    data <- data |>
      dplyr::select(
        time,
        wl,
        abs,
        dplyr::everything(),
        -meta,
        -value
      )
  }

  data
}


#' Reads Metadata from QuadTech Chromatogram
#'
#' @param file Path to the chromatogram file.
#' @param start_line Start of the data and thus end of the metadata. Determined
#'   with `chromr::chrom_find_data_start_line()`
#'
#' @return a [tibble][tibble::tibble-package]
chrom_get_meta_quadtech <- function(file, start_line) {
  met <-
    purrr::quietly(readr::read_csv)(
      file,
      n_max = start_line - 3,
      col_names = FALSE,
      col_types = readr::cols()
    )

  met <- met$result |>
    dplyr::rename(meta = 1, value = 2)

  met
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
#' @param time Time unit when exported.
#'
#' @return a [tibble][tibble::tibble-package]
#' @export
#'
#' @examples
#' fl <- system.file(
#'   "extdata",
#'   "sec_no_volume.txt",
#'   package = "chromr"
#' )
#' # read just the data
#' dat <- fl |>
#'   chrom_read_quadtech()
#' dat
#' # add a volume given a constant flow rate
#' dat |>
#'   chrom_add_volume(0.3)
chrom_add_volume <- function(.data, flow_rate = 0.5, time = "second") {
  time_adjust <- switch(time,
    "second" = 60,
    "minute" = 1,
    "hour" = 1 / 60
  )

  .data |>
    dplyr::mutate(
      volume = time / time_adjust * flow_rate
    )
}
