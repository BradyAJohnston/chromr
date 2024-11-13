
pivot_wl_longer <- function(data, values_to = "abs", names_to = "wl") {
  tidyr::pivot_longer(
    data = data,
    dplyr::matches("(a|A)\\d{2,3}"),
    values_to = values_to,
    names_to = names_to
  )
}

#' Check if Column is Present in DataFrame Ignoring Case
#'
#' @param data Dataframe
#' @param name string of column name
#'
#' @return Logical TRUE/FALSE if the column exists.
check_column_exist <- function(data, name) {
  detected_vector <- stringr::str_detect(
    colnames(data),
    stringr::fixed(name, ignore_case = TRUE)
  )
  is_present <- TRUE %in% detected_vector
  is_present
}

#' Title
#'
#' @param data Dataframe to rename the columns of.
#'
#' @return Dataframe with renamed column.
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

#' Interpolate Interpolate Given Column
#'
#' @param data Dataframe with column to interpolate.
#' @param col Name of the column to interpolate.
#'
#' @importFrom rlang :=
#' @return a [tibble][tibble::tibble-package]

interpolate_column <- function(data, col) {
  dplyr::mutate(
    data,
    {{ col }} := stats::approx(unique({{ col }}), n = nrow(data))$y
  )
}
