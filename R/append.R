#' Append a Chromatogram to Another Run
#'
#' Joins runs end to end: the time and volume of each run continue from the end
#' of the run before it. Optionally continues the numbering of the fractions
#' after the previous runs.
#'
#' @param .data A data frame created through `chrom_read_*()`
#' @param ... Runs created through `chrom_read_*()`
#' @param adjust_fractions Logical. FALSE keeps the fraction numbering as-is.
#'   TRUE renumbers the fractions of a run that starts again from a fraction
#'   number already used, so they continue after the fractions of the runs
#'   before it. Runs whose fractions already continue the numbering are left as
#'   they are.
#'
#' @return a [tibble][tibble::tibble-package] which combines the given runs,
#'   with a `run` column giving the number of the run each row comes from.
#'   If all the runs are chromatograms, it is a chromatogram whose metadata has
#'   the run, date, method and file of each run; see [chrom_meta()].
#' @export
#'
#' @examples
#'
#' fl1 <- system.file("extdata",
#'   "example_quadtec_1.TXT",
#'   package = "chromr"
#' )
#' fl2 <- system.file("extdata",
#'   "example_quadtec_2.TXT",
#'   package = "chromr"
#' )
#' df1 <- chrom_read_quadtech(fl1)
#' df2 <- chrom_read_quadtech(fl2)
#'
#'
#' df1 |>
#'   chrom_append_run(df2)
chrom_append_run <- function(.data, ..., adjust_fractions = FALSE) {
  check_bool(adjust_fractions)
  runs <- c(list(.data), list(...))

  ends <- list(time = 0, volume = 0, fraction = 0)
  for (i in seq_along(runs)) {
    run <- runs[[i]]
    for (col in c("time", "volume")) {
      if (i > 1 && col %in% names(run)) {
        # Offset in the unit of the first run.
        first <- runs[[1]][[col]]
        if (is_quantity(first) && is_quantity(run[[col]])) {
          run[[col]] <- quantity_convert(run[[col]], quantity_unit(first))
        }
        run[[col]] <- run[[col]] + ends[[col]]
      }
    }
    if (adjust_fractions && i > 1 && "fraction" %in% names(run)) {
      collected <- is_collected(run$fraction)
      if (any(collected) && min(run$fraction[collected]) <= ends$fraction) {
        run$fraction[collected] <- run$fraction[collected] + ends$fraction
      }
    }
    for (col in names(ends)) {
      if (col %in% names(run) && any(!is.na(run[[col]]))) {
        ends[[col]] <- max(ends[[col]], run[[col]], na.rm = TRUE)
      }
    }
    runs[[i]] <- run
  }

  meta <- chrom_meta_append(runs)
  runs <- purrr::map(runs, as_plain_tibble)
  out <- dplyr::bind_rows(runs, .id = "run") |>
    dplyr::mutate(run = as.numeric(.data$run))
  if (is.null(meta)) out else new_chrom(out, meta)
}

# Rows collected into a fraction: instruments record 0 or NA when they aren't
# collecting.
is_collected <- function(x) {
  !is.na(x) & x != 0
}
