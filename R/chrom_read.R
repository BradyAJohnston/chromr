#' Read .csv Chromatogram from the BioRad NGC
#'
#' Each trace in an NGC export is recorded at its own volumes. The traces are
#' combined into one row per volume of the first absorbance trace, and the
#' other traces are interpolated onto those volumes.
#'
#' An export can hold several runs side by side. Each run is read on its own,
#' and the runs are stacked, at the volumes each was recorded at, with a `run`
#' column first: the name of each run, or its number if the runs don't each
#' have their own name. A new run starts where the run named on the first line
#' changes, or where a trace repeats.
#'
#' @param file File path to the `.csv` file.
#' @param skip Number of lines to skip before begin reading. Usually 1 line.
#' @inheritSection chrom_read_quadtech Columns
#' @return A chromatogram: a [tibble][tibble::tibble-package] with the run's
#'   metadata, see [chrom_meta()]. Exports of several runs have the run, date,
#'   method and file of each run.
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

  # The first line names the run of each column.
  run_names <- if (skip > 0) {
    first <- readr::read_lines(file, n_max = 1)
    first <- readr::read_csv(I(first), col_names = FALSE, col_types = "c")
    unlist(first, use.names = FALSE)
  } else {
    rep(NA_character_, length(header))
  }

  # Columns come in pairs: "<trace>_volume" and "<trace>_<unit>".
  starts <- seq(1, length(header), by = 2)
  traces <- purrr::map(starts, \(i) {
    list(
      name = stringr::str_remove(header[[i + 1]], "_[^_]*$"),
      unit = stringr::str_extract(header[[i + 1]], "[^_]*$"),
      volume = dat[[i]],
      value = dat[[i + 1]]
    )
  })

  run <- ngc_runs(run_names[starts], purrr::map_chr(traces, "name"))
  runs <- purrr::map(unname(split(seq_along(traces), run)), \(i) {
    combine_traces(
      traces[i],
      instrument = "BioRad NGC",
      run = run_names[starts][[i[[1]]]],
      file = file
    )
  })
  if (length(runs) == 1) {
    return(runs[[1]])
  }

  # Runs are identified by their names, or else by number.
  meta <- chrom_meta_append(runs)
  named <- !anyNA(meta$run) && !anyDuplicated(meta$run)
  runs <- purrr::map(runs, as_plain_tibble)
  out <- if (named) {
    dplyr::bind_rows(rlang::set_names(runs, meta$run), .id = "run")
  } else {
    dplyr::bind_rows(runs, .id = "run") |>
      dplyr::mutate(run = as.numeric(.data$run))
  }
  new_chrom(out, meta)
}

# The number of the run each trace of an NGC export belongs to: a new run
# starts where the run name changes, or where a trace repeats.
ngc_runs <- function(run_names, trace_names) {
  run <- integer(length(trace_names))
  current <- 1L
  seen <- character()
  for (i in seq_along(trace_names)) {
    changed <- i > 1 && !identical(run_names[[i]], run_names[[i - 1]])
    if (changed || trace_names[[i]] %in% seen) {
      current <- current + 1L
      seen <- character()
    }
    seen <- c(seen, trace_names[[i]])
    run[[i]] <- current
  }
  run
}

#' Read the fraction table from the BioRad NGC
#'
#' The NGC exports the fractions of a run as a `.txt` table, with the rack and
#' tube each fraction was collected into and the volumes it covers.
#'
#' @param file File path to the `.txt` file.
#' @return A [tibble][tibble::tibble-package] with a row for each fraction and
#'   the columns `fraction`, `rack`, `tube`, `start`, `end` and `collected`,
#'   the collected volume. `fraction` is a [fraction] vector, and the volumes
#'   are [quantity] vectors in mL. Like [chrom_fractions()], it has the
#'   columns `fraction`, `start` and `end`.
#' @export
#'
#' @examples
#' fl <- system.file("extdata", "ngc_sec_fractions.txt", package = "chromr")
#' chrom_read_ngc_fractions(fl)
chrom_read_ngc_fractions <- function(file) {
  first <- readr::read_lines(file, n_max = 1)
  dat <- readr::read_tsv(
    file,
    skip = 1,
    col_types = readr::cols(.default = "c"),
    trim_ws = TRUE
  )
  if (!stringr::str_starts(first, "Run:") || !"Rack/Tube" %in% names(dat)) {
    cli::cli_abort(c(
      "{.file {file}} isn't an NGC fraction table.",
      i = "It should start with the run's name, then a table of fractions with
        a {.field Rack/Tube} column."
    ))
  }

  volume <- function(pattern) {
    column <- stringr::str_subset(names(dat), pattern)[[1]]
    unit <- stringr::str_match(column, "\\(([^)]*)\\)")[, 2]
    quantity(as.numeric(dat[[column]]), dplyr::coalesce(unit, "mL"))
  }
  tibble::tibble(
    fraction = fraction(as.integer(dat[["#"]])),
    rack = stringr::str_extract(dat[["Rack/Tube"]], "^[^/]*"),
    tube = as.integer(stringr::str_extract(dat[["Rack/Tube"]], "[^/]*$")),
    start = volume("^Start"),
    end = volume("^End"),
    collected = volume("^Collected")
  )
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
  header <- readr::read_lines(file, skip = start_line - 2, n_max = 1)
  first <- readr::read_lines(file, skip = start_line - 1, n_max = 1)
  if (stringr::str_count(header, ",") != stringr::str_count(first, ",")) {
    cli::cli_abort(c(
      "Can't read the column names of {.file {file}}.",
      i = "Line {start_line - 1} should name each column of the data, but it
        has {stringr::str_count(header, ',') + 1} name{?s} for
        {stringr::str_count(first, ',') + 1} columns."
    ))
  }

  channels <- chrom_get_meta_quadtech(file, start_line = start_line)
  units <- channels$unit[match(names(data), channels$name)]
  units <- dplyr::coalesce(units, "")
  names(data) <- trace_columns(names(data), units)
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
    file = file,
    absorbance_unit = "AU"
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
