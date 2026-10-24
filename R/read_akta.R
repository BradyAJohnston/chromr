#' Read chromatograms from UNICORN 6 and later
#'
#' @description
#' ÄKTA systems run by UNICORN 6 and later save each run as a result, which
#' UNICORN can export as a `.zip` archive or as curves in a `.csv` file.
#'
#' * `chrom_read_akta_result()` reads a result archive, the `.zip` file from
#'   "Export" in UNICORN's Evaluation module. It needs the xml2 package.
#' * `chrom_read_akta_csv()` reads curves exported to a `.csv` file.
#'
#' Each trace is recorded at its own volumes. The traces are combined into one
#' row per volume of the UV trace, and the other traces are interpolated onto
#' those volumes. The fraction marks give the fraction of each row; rows before
#' the first mark, or after a mark such as "Waste", have no fraction. The run
#' log and injection marks are dropped.
#'
#' For exports from UNICORN 5 and earlier, see [read_akta_old()].
#'
#' @param file Path to the `.zip` result or `.csv` export.
#' @inheritSection chrom_read_quadtech Columns
#' @return A chromatogram: a [tibble][tibble::tibble-package] with the run's
#'   metadata, see [chrom_meta()]. Results record the run's name and when it
#'   was created.
#' @export
#' @examples
#' fl <- system.file("extdata", "example_akta_3.csv", package = "chromr")
#' chrom_read_akta_csv(fl)
chrom_read_akta_result <- function(file) {
  rlang::check_installed("xml2", reason = "to read UNICORN results.")
  dir <- tempfile("chromr")
  on.exit(unlink(dir, recursive = TRUE), add = TRUE)
  contents <- tryCatch(utils::unzip(file, list = TRUE)$Name, error = \(cnd) {
    character()
  })
  if (!"Result.xml" %in% contents) {
    cli::cli_abort(c(
      "{.file {file}} isn't a UNICORN result.",
      i = "Results are {.file .zip} archives with a {.file Result.xml} file."
    ))
  }

  # Only the result, the chromatogram and its curves are extracted.
  extract <- function(name) {
    utils::unzip(file, files = name, exdir = dir)
    file.path(dir, name)
  }
  result <- xml2::read_xml(extract("Result.xml"))
  info <- function(xpath) {
    xml2::xml_text(xml2::xml_find_first(result, xpath))
  }
  chrom_file <- info("//Chromatograms/Chromatogram/ChromatogramFileName")
  chrom <- xml2::read_xml(extract(paste0(chrom_file, ".Xml")))

  curves <- xml2::xml_find_all(chrom, "/Chromatogram/Curves/Curve")
  traces <- purrr::map(curves, \(curve) {
    field <- function(xpath) {
      xml2::xml_text(xml2::xml_find_first(curve, xpath))
    }
    points <- read_akta_curve_points(
      extract(field(".//BinaryCurvePointsFileName"))
    )
    list(
      name = field("Name"),
      unit = field("AmplitudeUnit"),
      volume = points$volume,
      value = points$value
    )
  })

  fractions <- xml2::xml_find_all(
    chrom,
    "/Chromatogram/EventCurves/EventCurve[@EventCurveType='Fraction']/Events/Event"
  )
  if (length(fractions) > 0) {
    traces <- c(
      traces,
      list(list(
        name = "Fraction",
        unit = "",
        volume = as.numeric(xml2::xml_text(
          xml2::xml_find_all(fractions, "EventVolume")
        )),
        value = xml2::xml_text(xml2::xml_find_all(fractions, "EventText"))
      ))
    )
  }

  combine_akta_traces(
    traces,
    run = info("/Result/Name"),
    date = info("/Result/Created"),
    file = file
  )
}

#' @rdname chrom_read_akta_result
#' @export
chrom_read_akta_csv <- function(file) {
  locale <- readr::locale(encoding = akta_encoding(file))
  header <- readr::read_lines(file, n_max = 3, locale = locale)
  header <- readr::read_csv(
    I(header),
    col_names = FALSE,
    col_types = readr::cols(.default = "c")
  )
  if (!isTRUE(stringr::str_detect(header[[1, 1]], "^Chrom\\.\\d+$"))) {
    cli::cli_abort(c(
      "{.file {file}} isn't a UNICORN export of chromatogram curves.",
      i = "Its first line should name the chromatogram of each curve, such as
        {.val Chrom.1}."
    ))
  }
  dat <- readr::read_csv(
    file,
    skip = 3,
    col_names = FALSE,
    col_types = readr::cols(.default = "c"),
    locale = locale
  )

  # Columns come in pairs, volume then value, with the curve's name on the
  # second line and the unit of each column on the third.
  starts <- which(!is.na(unlist(header[1, ])))
  traces <- purrr::map(starts, \(i) {
    list(
      name = header[[i]][[2]],
      unit = header[[i + 1]][[3]],
      volume = as.numeric(dat[[i]]),
      value = dat[[i + 1]]
    )
  })

  combine_akta_traces(traces, run = NA, file = file)
}

# The volumes and values of a curve in a UNICORN result, which are stored as an
# archive of .NET arrays.
read_akta_curve_points <- function(path) {
  dir <- tempfile("chromr")
  on.exit(unlink(dir, recursive = TRUE), add = TRUE)
  dir.create(dir)
  # Some archives are padded with zeros after their end record, too many for
  # unzip to find it, so they're cut at the end record.
  bytes <- readBin(path, "raw", n = file.size(path))
  end <- zip_end(bytes)
  archive <- file.path(dir, "curve.zip")
  writeBin(bytes[seq_len(end)], archive)
  utils::unzip(archive, exdir = dir)
  list(
    volume = read_net_array(file.path(dir, "CoordinateData.Volumes")),
    value = read_net_array(file.path(dir, "CoordinateData.Amplitudes"))
  )
}

# The position of the last byte of a zip archive: the end of its end of
# central directory record, which is 22 bytes and a comment.
zip_end <- function(bytes) {
  n <- length(bytes)
  i <- seq_len(max(n - 21, 0))
  start <- i[
    bytes[i] == as.raw(0x50) &
      bytes[i + 1] == as.raw(0x4b) &
      bytes[i + 2] == as.raw(0x05) &
      bytes[i + 3] == as.raw(0x06)
  ]
  if (length(start) == 0) {
    return(n)
  }
  start <- start[[length(start)]]
  comment <- readBin(
    bytes[start + 20:21],
    "integer",
    size = 2,
    signed = FALSE,
    endian = "little"
  )
  min(start + 21 + comment, n)
}

# Reads a number array saved with .NET's BinaryFormatter: a 17-byte header,
# then the array record (0x0F), its object ID, its length and the type of its
# elements, single (11) or double (6) precision, followed by the numbers.
read_net_array <- function(path, call = rlang::caller_env()) {
  bytes <- readBin(path, "raw", n = file.size(path))
  if (length(bytes) < 27 || bytes[[18]] != as.raw(0x0f)) {
    cli::cli_abort(
      "Can't read the curve points in {.file {path}}.",
      call = call
    )
  }
  n <- readBin(bytes[23:26], "integer", size = 4, endian = "little")
  size <- switch(as.character(as.integer(bytes[[27]])), "11" = 4L, "6" = 8L)
  if (is.null(size) || length(bytes) < 27 + n * size) {
    cli::cli_abort(
      "Can't read the curve points in {.file {path}}.",
      call = call
    )
  }
  readBin(
    bytes[seq(28, length.out = n * size)],
    "double",
    n = n,
    size = size,
    endian = "little"
  )
}
