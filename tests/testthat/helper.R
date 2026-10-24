example_file <- function(file) {
  system.file("extdata", file, package = "chromr", mustWork = TRUE)
}

# Drops the chromatogram class and the absorbance, fraction and quantity types, to
# compare values with plain numbers.
plain <- function(data) {
  dplyr::mutate(
    as_plain_tibble(data),
    dplyr::across(
      dplyr::where(\(x) is_absorbance(x) || is_fraction(x) || is_quantity(x)),
      vctrs::vec_data
    )
  )
}
