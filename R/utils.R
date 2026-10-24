#' Pivot Wavelength Data to Long Format
#'
#' @description
#' Converts wavelength data from wide to long format, specifically handling
#' columns that match the pattern `"(a|A)\\d{2,3}"` (e.g., "A280", "a340").
#'
#' @param data A data frame containing wavelength measurements in wide format
#' @param values_to Name of the column to create for the values (default: "abs")
#' @param names_to Name of the column to create for the wavelength identifiers (default: "wl")
#'
#' @return A data frame in long format with wavelength measurements
#' @export
#'
#' @examples
#' \dontrun{
#' # With default column names
#' pivot_wl_longer(spectral_data)
#'
#' # With custom column names
#' pivot_wl_longer(spectral_data, values_to = "absorbance", names_to = "wavelength")
#' }
pivot_wl_longer <- function(data, values_to = "abs", names_to = "wl") {
  tidyr::pivot_longer(
    data = data,
    dplyr::matches("(a|A)\\d{2,3}"),
    values_to = values_to,
    names_to = names_to
  )
}

#' Pivot Wavelength Data to Wide Format
#'
#' @description
#' Converts wavelength data from long to wide format, specifically handling
#' columns containing absorbance values and wavelength identifiers. By default,
#' looks for absorbance columns matching "abs|absorbance" and wavelength columns
#' matching "wl|wavelength".
#'
#' @param data A data frame containing wavelength measurements in long format
#' @param values_from A regular expression pattern identifying the column(s) containing
#'        the values to be pivoted (default: "abs|absorbance")
#' @param names_from A regular expression pattern identifying the column(s) containing
#'        the wavelength identifiers (default: "wl|wavelength")
#'
#' @return A data frame in wide format where each wavelength becomes a separate column
#' @export
#'
#' @examples
#' \dontrun{
#' # With default column patterns
#' pivot_wl_wider(long_spectral_data)
#'
#' # With custom column patterns
#' pivot_wl_wider(
#'   long_spectral_data,
#'   values_from = "measurement",
#'   names_from = "wavelength_nm"
#' )
#' }
pivot_wl_wider <- function(
  data,
  values_from = "abs|absorbance",
  names_from = "wl|wavelength"
) {
  tidyr::pivot_wider(
    data = data,
    values_from = dplyr::matches(values_from),
    names_from = dplyr::matches(names_from)
  )
}

#' Check if Column Exists in Data Frame
#'
#' @description
#' Checks for the presence of a column in a data frame, ignoring case sensitivity
#' in the column name.
#'
#' @param data A data frame to check
#' @param name A character string specifying the column name to look for
#'
#' @return Logical value: TRUE if the column exists (ignoring case), FALSE otherwise
#'
#' @examples
#' \dontrun{
#' df <- data.frame(Name = 1:3, Value = 4:6)
#' check_column_exist(df, "name")  # Returns TRUE
#' check_column_exist(df, "NAME")  # Returns TRUE
#' check_column_exist(df, "age")   # Returns FALSE
#' }
#' @keywords internal
check_column_exist <- function(data, name) {
  detected_vector <- stringr::str_detect(
    colnames(data),
    stringr::fixed(name, ignore_case = TRUE)
  )
  is_present <- TRUE %in% detected_vector
  is_present
}

check_columns <- function(
  data,
  columns,
  arg = rlang::caller_arg(data),
  call = rlang::caller_env()
) {
  missing <- setdiff(columns, names(data))
  if (length(missing) > 0) {
    cli::cli_abort(
      "{.arg {arg}} must have the {.field {missing}} column{?s}.",
      call = call
    )
  }
}
