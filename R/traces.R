# Helpers shared by the readers, which turn the traces an instrument records
# into one row per sampled volume with the columns:
#
# * `time` and `volume`;
# * `fraction`, the fraction each row was collected into;
# * absorbance, named `a` and the wavelength (e.g. `a280`), or `uv` when the
#   instrument doesn't record the wavelength;
# * the other traces, under standard names where they have one
#   (`conductivity`, `conductivity_percent`, `percent_b`, `pressure`, `flow`,
#   `temperature`), and otherwise cleaned with janitor.

# The standard column name for a trace, from the instrument's name and unit for
# it.
trace_column <- function(name, unit) {
  label <- tolower(stringr::str_squish(name))
  unit <- tolower(stringr::str_squish(unit))
  wavelength <- trace_wavelength(name, unit)

  dplyr::case_when(
    label == "time" ~ "time",
    label == "volume" ~ "volume",
    stringr::str_detect(label, "^fraction") ~ "fraction",
    !is.na(wavelength) ~ paste0("a", wavelength),
    stringr::str_detect(label, "^(uv|quad ?tec)") | unit %in% c("au", "mau") ~
      "uv",
    stringr::str_detect(label, "^cond") & unit == "%" ~ "conductivity_percent",
    stringr::str_detect(label, "^cond") ~ "conductivity",
    stringr::str_detect(label, "gradient|%b|^conc") | unit == "%b" ~
      "percent_b",
    stringr::str_detect(label, "pressure") ~ "pressure",
    stringr::str_detect(label, "flow") ~ "flow",
    stringr::str_detect(label, "^temp") ~ "temperature",
    .default = janitor::make_clean_names(name, allow_dupes = TRUE)
  )
}

# Column names for traces, numbering repeats: a second UV trace is `uv_2`.
trace_columns <- function(name, unit) {
  columns <- trace_column(name, unit)
  n <- stats::ave(seq_along(columns), columns, FUN = seq_along)
  ifelse(n > 1, paste0(columns, "_", n), columns)
}

# Absorbance column names: `a280`, `uv`, and repeats such as `uv_2`.
is_absorbance_name <- function(x) {
  stringr::str_detect(x, "^(a\\d+|uv)(_\\d+)?$")
}

# The absorbance unit in a unit string such as "(280.0 nm), AU" or " mAU", or
# `default` if it has none.
absorbance_unit_from <- function(unit, default) {
  unit <- stringr::str_extract(unit, "(?i)\\bm?au\\b")
  if (is.na(unit)) {
    default
  } else if (tolower(unit) == "mau") {
    "mAU"
  } else {
    "AU"
  }
}

# The wavelength in nm of an absorbance trace, from names and units such as
# "UV1 (260 nm)", "(280.0 nm), AU" or "UV1_280nm".
trace_wavelength <- function(name, unit) {
  text <- paste(name, unit)
  nm <- stringr::str_match(text, "(?i)(\\d{3})(?:\\.\\d+)?\\s*nm")[, 2]
  uv <- stringr::str_match(text, "(?i)^uv\\d?_(\\d{3})")[, 2]
  as.numeric(dplyr::coalesce(nm, uv))
}

# Gives the columns their types, with their units, puts the standard columns
# first (time, volume, fraction, then absorbance), and makes a chromatogram.
# `units` are the instrument's units for each column, named by column, and
# `...` the metadata for `chrom_meta_list()`. Absorbance whose unit isn't
# recorded is in `absorbance_unit`.
tidy_traces <- function(data, units, ..., absorbance_unit = "mAU") {
  if ("fraction" %in% names(data)) {
    data$fraction <- fraction(data$fraction)
  }
  absorbance <- names(data)[is_absorbance_name(names(data))]
  for (col in absorbance) {
    data[[col]] <- absorbance(
      data[[col]],
      wavelength = wl_number(col),
      unit = absorbance_unit_from(units[[col]] %||% "", absorbance_unit)
    )
  }
  data <- dplyr::relocate(
    data,
    dplyr::any_of(c("time", "volume", "fraction")),
    dplyr::all_of(absorbance)
  )
  units <- as.list(units)
  if ("volume" %in% names(data)) {
    units[["volume"]] <- "mL"
  }
  other <- setdiff(names(data), c(absorbance, "fraction"))
  for (col in intersect(other, names(units))) {
    if (is.numeric(data[[col]]) && nzchar(stringr::str_squish(units[[col]]))) {
      data[[col]] <- quantity(data[[col]], units[[col]])
    }
  }
  new_chrom(data, chrom_meta_list(...))
}

# Combines traces that were each recorded at their own volumes, as NGC and
# ÄKTA exports are, into one row per volume. `traces` is a list of lists with
# the elements `name`, `unit`, `volume` and `value`, with volumes in mL.
# `...` is the metadata for `chrom_meta_list()`.
#
# The volumes are those of the first absorbance trace, or otherwise the trace
# with the most points; the other traces are interpolated onto them. Fraction
# marks give the fraction each volume was collected into: the last mark at or
# before it, or `NA` before the first mark and after a mark that isn't a
# fraction, such as "Waste". Marks are fraction numbers or tubes, such as "12"
# or "T12".
combine_traces <- function(traces, ...) {
  columns <- trace_columns(
    purrr::map_chr(traces, "name"),
    purrr::map_chr(traces, "unit")
  )
  is_fraction <- columns == "fraction"

  numeric <- which(!is_fraction)
  absorbance <- numeric[is_absorbance_name(columns[numeric])]
  points <- purrr::map_int(traces[numeric], \(x) sum(!is.na(x$volume)))
  grid <- if (length(absorbance) > 0) {
    absorbance[[1]]
  } else {
    numeric[[which.max(points)]]
  }
  volume <- traces[[grid]]$volume
  keep <- !is.na(volume)
  volume <- volume[keep]

  out <- list(volume = volume)
  units <- list()
  for (i in which(is_fraction)) {
    out$fraction <- fraction_at(volume, traces[[i]]$volume, traces[[i]]$value)
  }
  for (i in numeric) {
    units[[columns[[i]]]] <- traces[[i]]$unit
    out[[columns[[i]]]] <- if (i == grid) {
      as.numeric(traces[[i]]$value[keep])
    } else {
      interpolate_trace(traces[[i]], volume)
    }
  }

  tidy_traces(tibble::as_tibble(out), units, ...)
}

interpolate_trace <- function(trace, volume) {
  value <- as.numeric(trace$value)
  # Traces recorded at the same volumes keep their values.
  recorded <- !is.na(trace$volume)
  if (identical(trace$volume[recorded], volume)) {
    return(value[recorded])
  }
  ok <- recorded & !is.na(value)
  if (sum(ok) < 2) {
    return(rep(NA_real_, length(volume)))
  }
  stats::approx(
    trace$volume[ok],
    value[ok],
    xout = volume,
    ties = mean
  )$y
}

fraction_at <- function(volume, mark_volume, mark_label) {
  ok <- !is.na(mark_volume)
  ord <- order(mark_volume[ok])
  mark_volume <- mark_volume[ok][ord]
  mark_label <- mark_label[ok][ord]
  mark_label <- stringr::str_squish(stringr::str_remove_all(mark_label, '"'))
  # Fractions are marked with their number, or with a tube such as "T12".
  number <- as.integer(stringr::str_match(mark_label, "^T?(\\d+)$")[, 2])
  unknown <- unique(mark_label[is.na(number) & tolower(mark_label) != "waste"])
  if (length(unknown) > 0) {
    cli::cli_warn(c(
      "Can't find the fraction number of {length(unknown)} fraction mark{?s}.",
      i = "Points after {?it/them} have no fraction: {.val {unknown}}."
    ))
  }

  i <- findInterval(volume, mark_volume)
  out <- rep(NA_integer_, length(volume))
  out[i > 0] <- number[i[i > 0]]
  out
}
