#' Create basic plot of chromatogram.
#'
#' @param chrom Chrom object from \code{read_chrom()}.
#' @param channels Character vector of valengths to plot (e.g. \code{c("280", "260")}).
#'
#' @return
#' @export
#'
#' @examples
chrom_plot <- function(chrom, channels = NULL) {

  if (is.null(channels)) {
    channels <- base::unique(chrom$data$wl)
  }

  chrom$data %>%
    dplyr::filter(wl %in% channels) %>%
    ggplot2::ggplot(ggplot2::aes(Time/60, abs, colour = wl)) +
    ggplot2::geom_line() +
    ggplot2::theme_minimal() +
    ggplot2::scale_x_continuous(expand = c(0,0)) +
    ggplot2::theme(legend.position = c(0.05,0.95),
          legend.justification = c(0,1),
          panel.grid.minor = ggplot2::element_blank(),
          legend.background = ggplot2::element_rect(colour = "gray20",
                                           fill = scales::alpha("white", 0.9))) +
    ggplot2::labs(x = "Time (min)",
         y = "Absorbance (AU)",
         colour = "Wavelength (nm)")

}
