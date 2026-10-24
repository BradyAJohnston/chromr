#' Chromatogram metadata
#'
#' @description
#' The readers return a chromatogram: a [tibble][tibble::tibble-package] that
#' also records where the run came from. It works with dplyr, tidyr and ggplot2
#' as any tibble does, and prints the instrument and run above the data.
#' `chrom_meta()` gives the metadata as a list:
#'
#' * `instrument`: the instrument, such as `"BioRad QuadTec"`.
#' * `run`: the name of the run.
#' * `date` and `method`: the date and method of the run, as recorded, if the
#'   instrument records them.
#' * `file`: the name of the file it was read from.
#'
#' The units of the columns are part of the columns: see [quantity()] and
#' [absorbance()].
#'
#' Appended runs, from [chrom_append_run()], have one `run`, `date`, `method`
#' and `file` for each run.
#'
#' Results that aren't a chromatogram, such as from `dplyr::summarise()` or
#' `tidyr::pivot_longer()`, are plain tibbles. `dplyr::ungroup()` also gives a
#' plain tibble, so use the `.by` argument of dplyr verbs to work by group and
#' keep the metadata.
#'
#' @param x A chromatogram.
#' @returns A list, or `NULL` for a data frame that isn't a chromatogram.
#' @export
#' @examples
#' fl <- system.file("extdata", "example_quadtec_1.TXT", package = "chromr")
#' dat <- chrom_read_quadtech(fl)
#' dat
#'
#' chrom_meta(dat)
chrom_meta <- function(x) {
  attr(x, "chrom_meta", exact = TRUE)
}

new_chrom <- function(data, meta) {
  tibble::new_tibble(data, chrom_meta = meta, class = "chromr_chrom")
}

chrom_meta_list <- function(
  instrument,
  run = NA_character_,
  date = NA_character_,
  method = NA_character_,
  file = NA_character_
) {
  list(
    instrument = instrument,
    run = run,
    date = date,
    method = method,
    file = basename(file)
  )
}

# Combines the metadata of appended runs.
chrom_meta_append <- function(runs) {
  metas <- purrr::map(runs, chrom_meta)
  if (any(purrr::map_lgl(metas, is.null))) {
    return(NULL)
  }
  field <- function(name) purrr::map_chr(metas, name)
  list(
    instrument = unique(field("instrument")),
    run = field("run"),
    date = field("date"),
    method = field("method"),
    file = field("file")
  )
}

#' @exportS3Method pillar::tbl_sum
tbl_sum.chromr_chrom <- function(x, ...) {
  header <- NextMethod()
  names(header)[[1]] <- "A chromatogram"
  meta <- chrom_meta(x)
  if (is.null(meta)) {
    return(header)
  }
  info <- list(Instrument = meta$instrument, Run = meta$run, Date = meta$date)
  info <- info[!purrr::map_lgl(info, \(x) all(is.na(x)))]
  c(header, purrr::map_chr(info, \(x) paste(x, collapse = ", ")))
}

as_plain_tibble <- function(data) {
  data <- tibble::as_tibble(data)
  attr(data, "chrom_meta") <- NULL
  data
}
