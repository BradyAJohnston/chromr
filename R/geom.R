#' Create Fraction-Based Area Plots with Periodic Highlighting
#'
#' @description
#' Creates filled area plots that visually highlight periodic patterns in fractional data.
#' The highlighting is controlled by the period parameter and can be filtered by line colors.
#'
#' @param mapping Set of aesthetic mappings created by \code{\link[ggplot2]{aes}}. If specified and
#'   \code{inherit.aes = TRUE}, is combined with the default mapping at the top level of the plot.
#' @param data The data to be displayed in this layer. If \code{NULL}, the default, the data
#'   is inherited from the plot data as specified in the call to \code{\link[ggplot2]{ggplot}}.
#' @param position Position adjustment, either as a string, or the result of a call to a
#'   position adjustment function. Default is "identity".
#' @param na.rm If \code{FALSE}, the default, missing values are removed with a warning.
#'   If \code{TRUE}, missing values are silently removed.
#' @param show.legend logical. Should this layer be included in the legends?
#'   \code{NA}, the default, includes if any aesthetics are mapped.
#' @param alpha Numeric value between 0 and 1 for the transparency of the area fills.
#'   Default is 0.3.
#' @param period Integer specifying the interval for highlighting fractions. Default is 5.
#' @param initial Integer specifying the starting point for fraction counting. Default is 0.
#' @param lines Vector of line colors to include in the visualization. Default is NA.
#' @param inherit.aes If \code{FALSE}, overrides the default aesthetics,
#'   rather than combining with them.
#' @param ... Other arguments passed to \code{\link[ggplot2]{layer}}.
#'
#' @return A ggplot2 layer.
#' @export
#'
#' @examples
#' \dontrun{
#' ggplot(data, aes(x = x, y = y, fraction = frac)) +
#'   geom_fraction_bars(period = 5)
#'
#' # With custom period and initial value
#' ggplot(data, aes(x = x, y = y, fraction = frac)) +
#'   geom_fraction_bars(period = 3, initial = 1, alpha = 0.2)
#' }
geom_fraction_bars <- function(mapping = NULL,
                               data = NULL,
                               position = "identity",
                               na.rm = FALSE,
                               show.legend = FALSE,
                               alpha = 0.3,
                               period = 5,
                               initial = 0L,
                               lines = NA,
                               inherit.aes = TRUE,
                               ...) {
  ggplot2::layer(
    stat = StatFraction,
    geom = ggplot2::GeomArea,
    data = data,
    mapping = mapping,
    position = position,
    show.legend = show.legend,
    inherit.aes = inherit.aes,
    params = list(
      na.rm = na.rm,
      alpha = alpha,
      period = period,
      initial = initial,
      lines = lines,
      ...
    )
  )
}

#' Create Labels for Fractional Data
#'
#' @description
#' Adds labels to plots at specified fractional intervals. Labels can be positioned
#' using either static values or functions for both x and y coordinates.
#'
#' @param mapping Set of aesthetic mappings created by \code{\link[ggplot2]{aes}}.
#' @param data The data to be displayed in this layer.
#' @param position Position adjustment, either as a string, or the result of a call to a
#'   position adjustment function. Default is "identity".
#' @param na.rm If \code{FALSE}, the default, missing values are removed with a warning.
#' @param show.legend logical. Should this layer be included in the legends?
#' @param xpos Function or static value determining x position of labels. Default is mean.
#' @param ypos Function or static value determining y position of labels. Default is 0.
#' @param period Integer specifying the interval between labels. Default is 5.
#' @param initial Integer specifying the starting point for fraction counting. Default is 0.
#' @param inherit.aes If \code{FALSE}, overrides the default aesthetics.
#' @param ... Other arguments passed to \code{\link[ggplot2]{layer}}.
#'
#' @return A ggplot2 layer.
#' @export
#'
#' @examples
#' \dontrun{
#' # Basic usage with default positioning
#' ggplot(data, aes(x = x, y = y, fraction = frac)) +
#'   geom_fraction_label()
#'
#' # Custom positioning using functions
#' ggplot(data, aes(x = x, y = y, fraction = frac)) +
#'   geom_fraction_label(xpos = mean, ypos = max)
#' }
geom_fraction_label <- function(mapping = NULL,
                                data = NULL,
                                position = "identity",
                                na.rm = FALSE,
                                show.legend = NA,
                                xpos = mean,
                                ypos = 0,
                                period = 5,
                                initial = 0L,
                                inherit.aes = TRUE,
                                ...) {
  ggplot2::layer(
    stat = StatFractionGroup,
    geom = ggplot2::GeomLabel,
    data = data,
    mapping = mapping,
    position = position,
    show.legend = show.legend,
    inherit.aes = inherit.aes,
    params = list(
      na.rm = na.rm,
      period = period,
      initial = initial,
      xpos = xpos,
      ypos = ypos,
      ...
    )
  )
}

#' Compute Group Statistics for Fraction Labels
#'
#' @description
#' Internal function that computes the positions for fraction labels based on
#' specified grouping and positioning rules.
#'
#' @param data Input data frame.
#' @param scales Scale objects from the plot.
#' @param period Integer specifying the interval between labels.
#' @param xpos Function or static value for x position.
#' @param ypos Function or static value for y position.
#' @param lines Vector of line colors to include.
#' @param initial Integer specifying the starting point.
#'
#' @return A data frame with computed positions and labels.
#' @keywords internal
.compute_group_frac <- function(data,
                                scales,
                                period = 5,
                                xpos = mean,
                                ypos = 0,
                                lines = NA,
                                initial = 0L) {
  if (rlang::is_callable(xpos)) {
    xfun <- function(x) {
      xpos(x)
    }
  } else {
    xfun <- function(x) {
      xpos
    }
  }
  if (rlang::is_callable(ypos)) {
    yfun <- function(y) {
      ypos(y)
    }
  } else {
    yfun <- function(y) {
      ypos
    }
  }

  if (any(is.na(lines)) | !check_column_exist(data, "colour")) {
    mask <- rep(TRUE, nrow(data))
  } else {
    mask <- purrr::map_lgl(data$colour, \(x) x %in% lines)
  }

  data |>
    dplyr::filter(mask) |>
    dplyr::summarise(x = xfun(x), y = yfun(y), .by = fraction) |>
    dplyr::filter(fraction %% period == initial, fraction != 0) |>
    dplyr::mutate(label = fraction)
}

#' Compute Identity Statistics for Fraction Bars
#'
#' @description
#' Internal function that processes data for fraction-based area plots,
#' handling periodic highlighting and group interactions.
#'
#' @param data Input data frame.
#' @param scales Scale objects from the plot.
#' @param period Integer specifying the highlighting interval.
#' @param initial Integer specifying the starting point.
#' @param lines Vector of line colors to include.
#'
#' @return A data frame with computed groups and fill values.
#' @keywords internal
.comput_frac_identity <- function(data,
                                  scales,
                                  period = 5,
                                  initial = 0L,
                                  lines = NA) {
  if (any(is.na(lines)) | !check_column_exist(data, "colour")) {
    mask <- rep(TRUE, nrow(data))
  } else {
    mask <- purrr::map_lgl(data$colour, \(x) x %in% lines)
  }

  data |>
    dplyr::filter(fraction != 0, mask) |>
    dplyr::mutate(fill = factor((fraction - initial) %% period),
                  group = interaction(group, fraction))
}

#' Fraction Label Stat for ggplot2
#'
#' @description
#' ggproto object defining the statistical transformation for fraction labels.
#'
#' @format A ggproto object extending 'Stat'
#' @keywords internal
StatFractionGroup <- ggplot2::ggproto(
  `_class` = "StatFractionLabel",
  `_inherit` = ggplot2::Stat,
  required_aes = c("x", "y", "fraction"),
  compute_group = .compute_group_frac
)

#' Fraction Bar Stat for ggplot2
#'
#' @description
#' ggproto object defining the statistical transformation for fraction bars.
#'
#' @format A ggproto object extending 'Stat'
#' @keywords internal
StatFraction <- ggplot2::ggproto(
  `_class` = "StatFraction",
  `_inherit` = ggplot2::Stat,
  required_aes = c("x", "y", "fraction"),
  compute_group = .comput_frac_identity
)
