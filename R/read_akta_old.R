#' Find common start of multiple strings
#'
#' Walks along all strings until there is a character that is not shared
#' between all of them, then returns the string up until there.
#'
#' @param strings Vector of strings to be processed
#'
#' @return Single character that is common to the start of all the strings.
#'  Returns empty string if nothing in common.
#'
#' @keywords internal
.find_common_start <- function(strings) {
  strings <- unique(strings)
  split_letters <- stringr::str_split(strings[1], '')[[1]]
  found_longest <- FALSE
  i <- 1
  while (!found_longest) {
    lets <- paste0(split_letters[seq(i)], collapse = '')
    all_common_start <-
      all(stringr::str_detect(strings, stringr::str_glue("^{lets}")))
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
#' Each trace in an ÄKTA export is recorded at its own volumes. The traces are
#' combined into one row per volume of the UV trace, and the other traces are
#' interpolated onto those volumes. The fraction marks give the fraction of
#' each row; rows before the first mark, or after a mark such as "Waste", have
#' no fraction. Injection marks and the logbook are dropped.
#'
#' @param file Filepath to read
#' @param fraction_size `r lifecycle::badge("deprecated")` Not used: the end of
#'   each fraction is given by the next mark.
#' @inheritSection chrom_read_quadtech Columns
#'
#' @return A chromatogram: a [tibble][tibble::tibble-package] with the run's
#'   metadata, see [chrom_meta()].
#' @export
#'
#' @examples
#' fl <- system.file('extdata', 'example_akta_2.asc', package = 'chromr')
#'
#' read_akta_old(fl)
read_akta_old <- function(file, fraction_size = deprecated()) {
  if (lifecycle::is_present(fraction_size)) {
    lifecycle::deprecate_warn(
      "0.3.0",
      "read_akta_old(fraction_size)",
      details = "The end of each fraction is given by the next mark."
    )
  }

  locale <- readr::locale(encoding = akta_encoding(file))
  if (
    stringr::str_starts(readr::read_lines(file, n_max = 1), "UNICORN start")
  ) {
    cli::cli_abort(c(
      "{.file {file}} isn't an export of chromatogram curves.",
      i = "It is another UNICORN export, such as a method or a peak table."
    ))
  }
  header <- readr::read_lines(file, skip = 1, n_max = 2, locale = locale) |>
    stringr::str_split("\t")
  dat <- readr::read_tsv(
    file,
    skip = 3,
    col_names = FALSE,
    col_types = readr::cols(.default = "c"),
    locale = locale,
    trim_ws = TRUE
  )

  # Columns come in pairs, volume then value, named "<run>_<trace>" with the
  # unit of each column on the line below.
  starts <- seq(1, length(header[[1]]) - 1, by = 2)
  labels <- header[[1]][starts]
  # Some exports name traces "<run>_<trace>", and others just "<trace>".
  prefix <- stringr::str_extract(.find_common_start(labels), "^.*_")
  prefix <- dplyr::coalesce(prefix, "")
  traces <- purrr::map(starts, \(i) {
    list(
      name = stringr::str_sub(header[[1]][[i]], nchar(prefix) + 1),
      unit = header[[2]][[i + 1]],
      volume = as.numeric(dat[[i]]),
      value = dat[[i + 1]]
    )
  })
  # Drop the injection marks and the logbook.
  events <- paste(
    purrr::map_chr(traces, "name"),
    purrr::map_chr(traces, "unit")
  )
  traces <- traces[
    !stringr::str_detect(
      events,
      "(?i)inject|logbook|run log|set marks"
    )
  ]
  is_fraction <- stringr::str_detect(
    purrr::map_chr(traces, "name"),
    "(?i)^fraction"
  )
  traces[!is_fraction] <- purrr::map(traces[!is_fraction], \(x) {
    x$value <- as.numeric(x$value)
    x
  })

  combine_traces(
    traces,
    instrument = "\u00c4KTA",
    run = if (nzchar(prefix)) stringr::str_remove(prefix, "_$") else NA,
    file = file
  )
}

akta_encoding <- function(file) {
  guess <- readr::guess_encoding(file, n_max = 3)$encoding
  if (length(guess) == 0 || guess[[1]] == "ASCII") "UTF-8" else guess[[1]]
}
