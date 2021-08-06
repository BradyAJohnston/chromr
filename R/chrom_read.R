
#' Read chromatogram file.
#'
#' @param file Path to file.
#' @param tidy Logical, whether or not to return a "tidy" tibble.
#' @param header Position of header values (NULL attempts to auto-detect).
#' @param ID String to identify the particular chromatogram / run.
#' @param df Return just dataframe, or list containing dataframe and other run details.
#'
#' @return
#' @export
#'
#' @examples
chrom_read <- function(
  file,
  ID = NULL,
  df = FALSE,
  tidy = TRUE,
  header = NULL
) {
  if (is.null(header)){
    header <- max(stringr::str_which(readLines(file), "[:alpha:]"))
  }
  # print(header)

  head_info <- readLines(file, n = header-1)

  splits <- stringr::str_split(head_info, ",")

  details <- as.data.frame(do.call(rbind, splits))

  colnames(details) <- c("detail", "value", "extra")

  data <- read.csv(file, skip = header - 1)


  run <- list(
    details = details,
    data = data
  )

  # print(colnames(run$data))

  wavelengths <- run$details[grep("Quadtec", run$details$detail, ignore.case = TRUE), ]

  wavelengths$channel <- as.numeric(stringr::str_extract(wavelengths$detail, "\\d"))

  wavelengths$wl <- as.numeric(stringr::str_extract(wavelengths$value, "\\d+(?=.)"))

  colnames(run$data) <- ifelse(
    stringr::str_detect(colnames(run$data), "QuadTec.\\d"),
    wavelengths$wl[as.numeric(stringr::str_extract(colnames(run$data), "\\d"))],
    colnames(data)
  )

  print(paste("Detected Column names are as follows:", colnames(run$data)))

  if (tidy) {
    run$data <- run$data %>%
      tidyr::pivot_longer(
        cols = stringr::str_which(
          colnames(.),
          "\\d\\d\\d"
        ),
        values_to = "abs",
        names_to = "wl"
      )
  }

  if (!is.null(ID)) {
    run$data$ID <- ID
  }

  run$wl <- wavelengths

  if (df) {
    run$data
  } else {
    df
  }

}
