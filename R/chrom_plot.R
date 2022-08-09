#' Plot a Chromatogram
#'
#' @param data Data frame that contains columns for wavelength ('wl'), volume
#'   ('volume') and absorbance ('abs').
#' @param channels Character vector of wavelengths to plot (e.g. \code{c("280",
#'   "260")}). If none supplied, all wavelengths plotted.
#' @param xlim Limits for the x axis.
#' @param ylim Limits for the y axis.
#'
#' @importFrom rlang .data
#' @return a `ggplot2::ggplot()` plot.
#' @export
#'
#' @examples
#' fl <- system.file(
#'   "extdata",
#'   "sec_no_volume.txt",
#'   package = "chromr"
#' )
#'
#' fl %>%
#'   chrom_read_quadtech() %>%
#'   chrom_add_volume(0.3) %>%
#'   chrom_plot(xlim = c(0, 3), ylim = c(NA, 0.01))
chrom_plot <-
  function(data,
           channels = NULL,
           xlim = NULL,
           ylim = NULL) {
    if (is.null(channels)) {
      channels <- base::unique(data$wl)
    }

    data %>%
      dplyr::filter(.data$wl %in% channels) %>%
      ggplot2::ggplot(ggplot2::aes(.data$volume, .data$abs, colour = factor(.data$wl))) +
      ggplot2::geom_line() +
      ggplot2::theme_bw() +
      ggplot2::scale_x_continuous(expand = c(0, 0)) +
      ggplot2::coord_cartesian(
        xlim = xlim,
        ylim = ylim
      ) +
      ggplot2::theme(
        legend.position = c(0.05, 0.95),
        legend.justification = c(0, 1),
        panel.grid.minor = ggplot2::element_blank(),
        legend.background = ggplot2::element_rect(
          colour = "gray20",
          fill = scales::alpha("white", 0.9)
        )
      ) +
      ggplot2::labs(
        x = "Volume (mL)",
        y = "Absorbance (AU)",
        colour = "Wavelength (nm)"
      )
  }
