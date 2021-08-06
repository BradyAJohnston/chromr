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
    filter(wl %in% channels) %>%
    ggplot(aes(Time/60, abs, colour = wl)) +
    geom_line() +
    theme_minimal() +
    scale_x_continuous(expand = c(0,0)) +
    theme(legend.position = c(0.05,0.95),
          legend.justification = c(0,1),
          panel.grid.minor = element_blank(),
          legend.background = element_rect(colour = "gray20",
                                           fill = scales::alpha("white", 0.9))) +
    labs(x = "Time (min)",
         y = "Absorbance (AU)",
         colour = "Wavelength (nm)")

}
