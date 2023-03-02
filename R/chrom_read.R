#' Read .csv Chromatogram from the BioRad NGC
#'
#' @param file File path to the `.csv` file.
#' @param skip Number of lines to skip before begin reading. Usually 1 line.
#' @importFrom rlang .data
#' @return a [tibble][tibble::tibble-package]
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
  dat <- readr::read_csv(file = file,
                         skip = skip,
                         col_types = readr::cols())

  dat <- janitor::clean_names(dat)

  dat <- dat %>%
    dplyr::mutate(id = dplyr::row_number()) %>%
    tidyr::pivot_longer(-.data$id) %>%
    dplyr::mutate(
      type = stringr::str_extract(.data$name, "^[^\\_]+"),
      volume = stringr::str_detect(.data$name, "volume")
    ) %>%
    dplyr::group_by(.data$type, .data$id) %>%
    dplyr::mutate(volume = dplyr::if_else(.data$volume, .data$value, NaN)) %>%
    tidyr::fill(.data$volume) %>%
    dplyr::filter(stringr::str_detect(.data$name, "volume", negate = TRUE)) %>%
    dplyr::select("volume", "type", "value", "name")

  dat
}

#' Read BioRad QuadTech Chromatogram Files
#'
#' @param file Exported `.TXT` chromatogram file from the BioRad QuadTech.
#' @param interp_volume Logical. If TRUE, interpolates the values in the volume
#'   column based on the values in the time column.
#' @importFrom rlang .data
#' @return a [tibble][tibble::tibble-package]
#' @export
#'
#' @examples
#' fl <- system.file("extdata",
#'   "sec.txt",
#'   package = "chromr"
#' )
#' # just read
#' fl %>%
#'   chrom_read_quadtech()
#'
#' # read without interpolating volume
#' fl %>%
#'   chrom_read_quadtech(interp_volume = FALSE)
#' # read then plot
#' fl %>%
#'   chrom_read_quadtech() %>%
#'   chrom_plot()
chrom_read_quadtech <- function(file, interp_volume = TRUE) {
  start_line <- chrom_find_data_start_line(file, n_lines = 50)

  data <- readr::read_csv(file = file,
                          skip = start_line - 2,
                          col_types = readr::cols()) %>%
    rename_columns()

  met <- chrom_get_meta_quadtech(file, start_line = start_line)





  wavelengths <- met %>%
    dplyr::filter(stringr::str_detect(.data$meta, "Quad")) %>%
    dplyr::mutate(
      wl = as.numeric(stringr::str_extract(.data$value, "\\d{3}")),
      channel = as.numeric(stringr::str_extract(.data$meta, "\\d$"))
    )

  volume_present <- check_column_exist(data, "volume")
  fraction_present <- check_column_exist(data, "fraction")

  if (interp_volume & volume_present) {
    data <- data %>%
      dplyr::mutate(volume = stats::approx(unique(.data$volume), n = nrow(data))$y)
  }

  col_names <- dplyr::pull(met, .data$value, .data$meta)

  data <- data |>
    dplyr::rename_with(.fn = ~ col_names[.x],
                       .cols = dplyr::any_of(names(col_names))) |>
    janitor::clean_names() |>
    dplyr::rename_with(~ paste0("a", stringr::str_extract(.x, "\\d{1,3}")),
                       .cols = dplyr::matches("nm_au")) |>
    dplyr::select(-dplyr::matches("a\\d{1,3}"), dplyr::everything())

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

  met <- met$result %>%
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
  start_line <- readr::read_lines(file, n_max = n_lines) %>%
    stringr::str_trim() %>%
    stringr::str_which("^\\d") %>%
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
#' dat <- fl %>%
#'   chrom_read_quadtech()
#' dat
#' # add a volume given a constant flow rate
#' dat %>%
#'   chrom_add_volume(0.3)
chrom_add_volume <-
  function(.data,
           flow_rate = 0.5,
           time = "second") {
    time_adjust <- switch(time,
                          "second" = 60,
                          "minute" = 1,
                          "hour" = 1 / 60)

    .data %>%
      dplyr::mutate(volume = .data$time / time_adjust * flow_rate) |>
      dplyr::select("time", "volume", dplyr::everything())
  }
