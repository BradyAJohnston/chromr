#' Plot a Chromatogram
#'
#' @param data Data frame that contains columns for wavelength ('wl'), volume
#'   ('volume') and absorbance ('abs').
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
           xlim = NULL,
           ylim = NULL) {
    dat <- tidyr::pivot_longer(data, dplyr::matches("a\\d{2,3}"), names_to = "wl")

      plt <- ggplot2::ggplot(
        data = dat,
        ggplot2::aes(.data$volume, .data$value, colour = factor(.data$wl))
        ) +
      ggplot2::geom_line() +
      ggplot2::scale_x_continuous(expand = c(0, 0)) +
      ggplot2::scale_colour_discrete(
        labels = function(x) stringr::str_extract(x, "\\d+")
      ) +
      ggplot2::coord_cartesian(
        xlim = xlim,
        ylim = ylim
      ) +
      ggplot2::theme_bw() +
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

      plt
  }

#' Plot Chromatogram with Fractions
#'
#' @param data Datafram containing values.
#' @param wl_frac Wavelengths to show the fractionation scheme for.
#' @param fractions Logical, whether to incude fractions on the plot.
#' @param frac_include Specific fractions to include. Either "all" for all
#'   fractions, or a numeric vector of length 2, specifying the limits for the
#'   fractions to be included (e.g. c(10, 30) includes fractions from 10 till
#'   30, including both).
#' @param frac_labelling How often to label the fractions. Every $n_{th}$
#'   fraction is labelled.
#' @param frac_text_size Size of the labels for the fractionation.
#' @param frac_text_adjust `vjust` for the labels for the fractionation.
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
#' df2 <- chrom_read_quadtech(fl2)matches
#'
#' dat <- chrom_append_run(df1, df2)
#' chrom_plot_fractions(dat, wl_frac = c(280, 488))
chrom_plot_fractions <- function(data,
                                 wl_frac = 280,
                                 fractions = TRUE,
                                 frac_include = "all",
                                 frac_labelling = 5,
                                 frac_text_size = 3,
                                 frac_text_adjust = 1.3) {
  if (fractions) {
    frac_numbers <- data$fraction

    if (!("all" %in% frac_include)) {
      stopifnot(is.numeric(frac_include))
      frac_limits <- length(frac_include)
      if (frac_limits == 2) {
        frac_numbers <- frac_numbers[frac_numbers >= frac_include[1] & frac_numbers <= frac_include[2]]
      }
    }
  }

  fractions_present <- TRUE %in% stringr::str_detect(colnames(data), "frac") & fractions

  data <- tidyr::pivot_longer(data, dplyr::matches("a\\d{2,3}"), names_to = "wl")

  plt <- ggplot2::ggplot(data, ggplot2::aes(.data$volume, .data$value))

  if (fractions_present) {
    lab_data <- data %>%
      dplyr::filter(
        .data$fraction != 0,
        .data$fraction %in% frac_numbers,
        .data$fraction %% frac_labelling == 0,
        .data$wl %in% wl_frac
      ) %>%
      dplyr::group_by(.data$fraction) %>%
      dplyr::summarise(volume = mean(.data$volume))

    plt <- plt +
      ggplot2::geom_area(
        position = "identity",
        data = dplyr::filter(data, .data$fraction != 0,
                             stringr::str_detect(.data$wl, paste(wl_frac, collapse = "|")),
                             .data$fraction %in% frac_numbers),
        ggplot2::aes(
          fill = factor(.data$fraction %% 5),
          group = interaction(.data$fraction, .data$wl)
        ),
        alpha = 0.5,
      ) +
      ggplot2::geom_text(
        data = lab_data,
        mapping = ggplot2::aes(label = .data$fraction, y = 0),
        size = frac_text_size,
        vjust = frac_text_adjust
      )
  }
  plt +
    ggplot2::geom_line(
      size = 0.6,
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
    ggplot2::scale_colour_discrete(
      labels = function(x) stringr::str_extract(x, "\\d{3}"),
    ) +
    ggplot2::scale_fill_manual( # values = RColorBrewer::brewer.pal(7, "Greys")[2:6]
      values = grDevices::gray(seq(0.85, 0.65, length.out = 5))
    ) +
    # theme_bw(base_size = 20) +
    ggplot2::theme_classic() +
    ggplot2::theme(
      legend.position = c(0.05, 0.95),
      legend.justification = c(0, 1),
      legend.background = ggplot2::element_rect(colour = "gray30", size = 0.8)
    )
}
