#' Append a Chromatogram to Another Run
#'
#' Sums the volume and the time columns. Optionally continues the numbering of
#' the fractions after the previous runs.
#'
#' @param .data A data frame created through `chrom_read_*()`
#' @param ... Runs created through `chrom_read_*()`
#' @param adjust_fractions Logical. FALSE keeps the fraction numbering as-is.
#' TRUE starts the number of the subsequent fractions by adding the previous
#' fractions.
#'
#' @importFrom rlang .data
#' @return a [tibble][tibble::tibble-package] which combines the given runs.
#' @export
#'
#' @examples
#'
#' fl1 <- system.file("extdata",
#'   "20220809_SFPQfl_TEVdig_S200_part1.TXT",
#'   package = "chromr"
#' )
#' fl2 <- system.file("extdata",
#'   "20220809_SFPQfl_TEVdig_S200_part2.TXT",
#'   package = "chromr"
#' )
#' df1 <- chrom_read_quadtech(fl1)
#' df2 <- chrom_read_quadtech(fl2)
#'
#'
#' df1 %>%
#'   chrom_append_run(df2)
chrom_append_run <- function(.data, ..., adjust_fractions = FALSE) {
  new_runs <- list(...)

  .data <- dplyr::bind_rows(.data, new_runs, .id = "run") %>%
    dplyr::mutate(run = as.numeric(.data$run))

  max_values <- .data %>%
    dplyr::group_by(.data$run) %>%
    dplyr::summarise(dplyr::across(tidyr::matches("time|volume|fraction"), max))

  .data <- .data %>%
    dplyr::mutate(
      time = dplyr::if_else(
        .data$run == 1,
        .data$time,
        .data$time + max_values$time[dplyr::if_else(
          .data$run == 1,
          1,
          .data$run - 1)]
      ),
      volume = dplyr::if_else(
        .data$run == 1,
        .data$volume,
        .data$volume + max_values$volume[dplyr::if_else(
          .data$run == 1,
          1,
          .data$run - 1)]
      )
    )

  if (adjust_fractions) {
    .data <- .data %>%
      dplyr::mutate(
        fraction = dplyr::case_when(
          .data$fraction == 0 ~ 0,
          .data$run == 1 ~ .data$fraction,
          TRUE ~ .data$fraction + max_values[run - 1, ]$fraction
        )
      )
  }

  .data
}
