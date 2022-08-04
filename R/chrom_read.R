#' Interpolate Volume from Time
#'
#' Interpolates the volume column values, based on the time column values.
#'
#' Some FPLC systems don't report accurate volume data, but do report accurate
#' timing data. This function takes the time and volume data and interpolates
#' the otherwise constant volume data in accordance with the time points.
#'
#' @param df
#'
#' @importFrom rlang :=
#' @return
#' @export
#'
#' @examples
chrom_interp_volume <- function(df, time, volume) {
  df |>
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
#' @return
#' @export
#'
#' @examples
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
                     by = c("channel" = "channel"))

  volume_present <- sum(stringr::str_detect(colnames(data), "volume"))

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
        gradient = gradient_pump,
        pressure = gp_pressure,
        cond = conductivity,
        dplyr::everything(),
        -meta,
        -value
      )
  } else {
    data <- data |>
      dplyr::select(
        time,
        volume,
        wl,
        abs,
        gradient = gradient_pump,
        pressure = gp_pressure,
        cond = conductivity,
        dplyr::everything(),
        -meta,
        -value
      )
  }

  data
}


#' Title
#'
#' @param file
#'
#' @return
#' @export
#'
#' @examples
chrom_get_meta_quadtech <- function(file, start_line) {
  met <-
    readr::read_csv(
      file,
      n_max = start_line - 3,
      col_names = FALSE,
      col_types = readr::cols()
    ) |>
    dplyr::rename(meta = 1, value = 2)

  met
}


#' Title
#'
#' @param file
#' @param n_lines
#'
#' @return
#' @export
#'
#' @examples
chrom_find_data_start_line <- function(file, n_lines = 50) {
  start_line <- readr::read_lines(file, n_max = n_lines) |>
    stringr::str_trim() |>
    stringr::str_which("^\\d") |>
    min()
}

#' Title
#'
#' @param df
#' @param flow_rate
#' @param time
#'
#' @return
#' @export
#'
#' @examples
chrom_add_volume <- function(df, flow_rate = 0.5, time = "second") {

  time_adjust <- switch(time,
                        "second" = 60,
                        "minute" = 1,
                        "hour" = 1 / 60)

  df |>
    dplyr::mutate(
      volume = time / time_adjust * flow_rate
    )

}
