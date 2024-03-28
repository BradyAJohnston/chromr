#' Find common start of multiple strings
#'
#' Walks along all strings until there is a character that is not shared
#' between all of them, then returns the string up until there.
#'
#' @param strings Vector of strings to be processed
#'
#' @return Single character that is common to the start of all the strings.
#'  Returns empty string if nothing in common.
#' @export
#'
#' @keywords internal
.find_common_start <- function(strings) {
  strings <- unique(strings)
  split_letters <- stringr::str_split(strings[1], '')[[1]]
  found_longest <- FALSE
  i <- 1
  while (!found_longest) {
    lets <- paste0(split_letters[seq(i)], collapse = '')
    all_common_start <- all(stringr::str_detect(strings, stringr::str_glue("^{lets}")))
    if (all_common_start) {
      i <- i + 1
    } else {
      found_longest <- TRUE
      i <- i - 1
    }
  }
  if (i == 0) {
    ""
  } else {
    paste0(split_letters[seq(i)], collapse = '')
  }
}

#' Read a .asc chromatogram from an old AKTA Pure
#'
#' @param file Filepath to read
#'
#' @return a [tibble][tibble::tibble-package]
#' @export
#'
#' @examples
#' fl <- system.file('extdata', 'example_akta_old.asc', package = 'wellr')
#'
#' read_akta_old(fl)
read_akta_old <- function(file, fraction_size = 2) {
  .split_columns <- function(.data) {

    purrr::map(seq(1, ncol(.data), by = 2), \(x) {
      .data[, c(x, x + 1)]
    })
  }


  read_the_file <- function(file) {
    info_lines <- readr::read_lines(file, n_max = 3) |>
      stringr::str_split(pattern = "\t")

    unit_lines <- info_lines[[3]]

    info <- info_lines[[2]] |>
      unlist() |>
      stringr::str_split(pattern = " ") |>
      purrr::map(\(x) x[length(x)]) |>
      unlist()

    readr::read_tsv(file, skip = 3, col_names = info)
  }

  raw_dat <- read_the_file(file) |>
    janitor::clean_names()

  bad_columns <-
    which(stringr::str_detect(colnames(raw_dat), "logbook|inject"))

  raw_dat[, -c(bad_columns, bad_columns + 1)] |>

    .split_columns() |>
    purrr::map(\(x) {
      name <- colnames(x)[1]
      colnames(x) <- c("volume", name)
      janitor::clean_names(x) |>
        dplyr::mutate(id = dplyr::row_number()) |>
        tidyr::pivot_longer(-c('volume', 'id'), values_transform = as.numeric)
    }) |>
    dplyr::bind_rows() |>
    tidyr::drop_na() -> dat

  # cleanup names
  dat$name <- stringr::str_remove(dat$name, .find_common_start(dat$name))

  fractions <- dat |>
    dplyr::filter(stringr::str_detect(.data$name, "fraction"))

  # get the volumes for starting and ending fractions,
  # adding in the final stop which will be usually 2ml + last start volume
  volumes <- fractions$volume
  n <- length(volumes)
  fraction_breaks <- c(volumes, volumes[n] + (volumes[n]- volumes[n - 1]))

  dat <-
    dplyr::filter(dat, stringr::str_detect(.data$name, "fraction", negate = TRUE)) |>
    dplyr::mutate(
      volume = mean(.data$volume),
      fraction = as.numeric(cut(
        .data$volume,
        breaks = fraction_breaks,
        labels = fractions$id,
        include.lowest = TRUE
        )),
      .by = .data$id
    ) |>
    dplyr::select(-"id") |>
    tidyr::pivot_wider(values_from = 'value', names_from = 'name')

}

