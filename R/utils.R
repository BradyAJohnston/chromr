#' Pivot Wavelength Data to Long Format
#'
#' @description
#' Converts wavelength data from wide to long format, specifically handling
#' columns that match the pattern "(a|A)\\d{2,3}" (e.g., "A280", "a340").
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

pivot_wl_wider <- function(data, values_from = "abs|absorbance", names_from = "wl|wavelength") {
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
#' @export
#'
#' @examples
#' \dontrun{
#' df <- data.frame(Name = 1:3, Value = 4:6)
#' check_column_exist(df, "name")  # Returns TRUE
#' check_column_exist(df, "NAME")  # Returns TRUE
#' check_column_exist(df, "age")   # Returns FALSE
#' }
check_column_exist <- function(data, name) {
  detected_vector <- stringr::str_detect(
    colnames(data),
    stringr::fixed(name, ignore_case = TRUE)
  )
  is_present <- TRUE %in% detected_vector
  is_present
}

#' Standardize Column Names
#'
#' @description
#' Renames specific columns in a data frame to standardized names.
#' Current mappings: volume -> Volume, time -> Time, fraction -> Fraction
#'
#' @param data A data frame whose columns need to be renamed
#'
#' @return A data frame with standardized column names
#' @export
#'
#' @examples
#' \dontrun{
#' df <- data.frame(volume = 1:3, time = 4:6, other = 7:9)
#' renamed_df <- rename_columns(df)  # 'volume' becomes 'Volume', 'time' becomes 'Time'
#' }
rename_columns <- function(data) {
  nicer_column_names <- c(
    "Volume" = "volume",
    "Time" = "time",
    "Fraction" = "fraction"
  )
  rename_function <- function(name, new_names) {
    dplyr::if_else(
      name %in% names(new_names),
      new_names[name],
      name
    )
  }
  dplyr::rename_with(data, rename_function, new_names = nicer_column_names)
}

#' Interpolate Values in a Column
#'
#' @description
#' Performs linear interpolation on values in a specified column of a data frame.
#' Uses unique values in the column as anchor points for interpolation.
#'
#' @param data A data frame containing the column to interpolate
#' @param col Name of the column to interpolate (unquoted)
#'
#' @return A data frame with the interpolated column
#' @export
#' @importFrom rlang :=
#'
#' @examples
#' \dontrun{
#' df <- data.frame(x = c(1, NA, 3, NA, 5))
#' interpolate_column(df, x)  # Interpolates NA values in column x
#' }
interpolate_column <- function(data, col) {
  dplyr::mutate(
    data,
    {{ col }} := stats::approx(unique({{ col }}), n = nrow(data))$y
  )
}
