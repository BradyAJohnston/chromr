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
      dplyr::filter(.data$name %in% channels) %>%
      ggplot2::ggplot(ggplot2::aes(.data$volume, .data$value, colour = factor(.data$name))) +
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

#' Plot Chromatogram with Fractions
#'
#' @param data Datafram containing values.
#' @param wl_show Wavelengths to show on the plot.
#' @param frac_wl Wavelengths to show the fractionation scheme for.
#' @param frac_labelling How often to label the fraction scheme.
#'
#' @importFrom rlang .data
#' @return a ggplot object.
#' @export
#'
#' @examples
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
#' dat <- chrom_append_run(df1, df2)
#' chrom_plot_fractions(dat, frac_wl = c(280, 488))
chrom_plot_fractions <- function(data,
                                 wl_show = NULL,
                                 frac_wl = 280,
                                 frac_labelling = 5) {
  lab_data <- data %>%
    dplyr::filter(
      .data$fraction %% frac_labelling == 0,
      .data$wl %in% frac_wl,
      .data$fraction != 0
    ) %>%
    dplyr::group_by(.data$fraction) %>%
    dplyr::summarise(volume = mean(.data$volume), value = max(.data$value))

  if (!is.null(wl_show)) {
    data <- data %>%
      dplyr::filter(.data$wl %in% wl_show)
  }

  data %>%
    ggplot2::ggplot(ggplot2::aes(.data$volume, .data$value)) +
    ggplot2::geom_area(
      position = "identity",
      data = dplyr::filter(data, .data$fraction != 0, .data$wl %in% frac_wl),
      ggplot2::aes(fill = factor(.data$fraction %% 5),
                   group = interaction(.data$fraction, .data$wl, .data$run)),
    ) +
    ggplot2::geom_text(
      data = lab_data,
      mapping = ggplot2::aes(label = .data$fraction, y = 0),
      vjust = 1.8
    ) +
    ggplot2::geom_line(
      size = 0.8,
      ggplot2::aes(group = factor(.data$wl), colour = factor(.data$wl)),
      alpha = 0.8
    ) +
    ggplot2::guides(fill = "none") +
    ggplot2::coord_cartesian(ylim = c(0, NA)) +
    ggplot2::labs(
      colour = "Wavelength (nm)",
      x = "Volume (mL)",
      y = "Absorbance (AU)"
    ) +
    ggplot2::scale_x_continuous(breaks = scales::pretty_breaks(10)) +
    ggplot2::scale_fill_manual( # values = RColorBrewer::brewer.pal(7, "Greys")[2:6]
      values = grDevices::gray(seq(0.85, 0.65, length.out = 5))
    ) +
    # theme_bw(base_size = 20) +
    ggplot2::theme_classic(base_size = 20) +
    ggplot2::theme(
      legend.position = c(0.05, 0.95),
      legend.justification = c(0, 1),
      legend.background = ggplot2::element_rect(colour = "gray30", size = 0.8)
    )
}
