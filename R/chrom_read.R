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
#' @param interp_volume Logical. If TRUE, interpolates the values in the volume column based on the values in the time column.
#'
#' @return
#' @export
#'
#' @examples
chrom_read_quadtech <- function(file, interp_volume = TRUE) {
  start_line <- readr::read_lines(file, n_max = 50) |>
    stringr::str_trim() |>
    stringr::str_which("^\\d") |>
    min()

  data <-
    readr::read_csv(file, skip = start_line - 2, col_types = readr::cols())

  met <-
    readr::read_csv(
      file,
      n_max = start_line - 3,
      col_names = FALSE,
      col_types = readr::cols()
    ) |>
    dplyr::rename(category = 1, meta = 2)

  met <- met |>
    dplyr::filter(stringr::str_detect(category, "Quad")) |>
    dplyr::mutate(
      wl = as.numeric(stringr::str_extract(meta, "\\d{3}")),
      channel = as.numeric(stringr::str_extract(category, "\\d$"))
    )


  data <- data |>
    janitor::clean_names() |>
    tidyr::pivot_longer(cols = dplyr::contains("quad")) |>
    dplyr::mutate(name = as.numeric(stringr::str_extract(name, "\\d$"))) |>
    dplyr::rename(channel = name) |>
    dplyr::left_join(met,
                     by = c("channel" = "channel"))

  if (interp_volume) {
    volume_interp <- chrom_interp_volume(data, time, volume)

    data <- data |>
      dplyr::select(-volume) |>
      dplyr::left_join(volume_interp, by = c("time" = "time"))
  }

  data <- data |>
    dplyr::select(
      time,
      volume,
      wl,
      abs = value,
      gradient = gradient_pump,
      pressure = gp_pressure,
      cond = conductivity,
      dplyr::everything(),
      -category,
      -meta
    )

  data
}
