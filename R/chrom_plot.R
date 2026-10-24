#' Plot a Chromatogram
#'
#' @param data A chromatogram from `chrom_read_*()`, or a data frame with a
#'   `volume` column and absorbance columns named like `a280`. Chromatograms
#'   without a volume, such as QuadTec runs that only record the time, are
#'   plotted against `time`.
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
#'   "example_quadtec_4.txt",
#'   package = "chromr"
#' )
#'
#' fl |>
#'   chrom_read_quadtech() |>
#'   chrom_add_volume(0.3) |>
#'   chrom_plot(xlim = c(0, 3), ylim = c(NA, 0.01))
chrom_plot <-
  function(data, xlim = NULL, ylim = NULL) {
    absorbance <- absorbance_columns(data)
    x <- plot_x(data)
    dat <- tidyr::pivot_longer(
      data,
      dplyr::all_of(absorbance),
      names_to = "wl"
    )

    plt <- ggplot2::ggplot(
      data = dat,
      ggplot2::aes(.data[[x$column]], .data$value, colour = factor(.data$wl))
    ) +
      ggplot2::geom_line(ggplot2::aes(group = !!line_group(dat, "wl"))) +
      ggplot2::scale_x_continuous(expand = c(0, 0)) +
      ggplot2::scale_colour_discrete(labels = wl_label) +
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
        x = x$title,
        y = absorbance_title(data, absorbance),
        colour = "Wavelength (nm)"
      )

    plt
  }

#' Plot Chromatogram with Fractions
#'
#' @param data A chromatogram from `chrom_read_*()`, plotted against `volume`,
#'   or `time` if it has no volume.
#' @param wl_frac Wavelengths to show the fractionation scheme for.
#' @param fractions Logical, whether to incude fractions on the plot.
#' @param frac_include Specific fractions to include. Either "all" for all
#'   fractions, or a numeric vector of length 2, specifying the limits for the
#'   fractions to be included (e.g. c(10, 30) includes fractions from 10 till
#'   30, including both).
#' @param frac_labelling How often to label the fractions. Every nth
#'   fraction is labelled, and the shading of the fractions repeats every nth
#'   fraction.
#' @param frac_text_size Size of the labels for the fractionation.
#' @param frac_text_adjust `vjust` for the labels for the fractionation.
#'
#' @importFrom rlang .data
#' @return a ggplot object.
#' @export
#'
#' @examples
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
#' dat <- chrom_append_run(df1, df2)
#' chrom_plot_fractions(dat, wl_frac = c(280, 488))
chrom_plot_fractions <- function(
  data,
  wl_frac = 280,
  fractions = TRUE,
  frac_include = "all",
  frac_labelling = 5,
  frac_text_size = 3,
  frac_text_adjust = 1.3
) {
  check_bool(fractions)
  check_number_whole(frac_labelling, min = 1)
  if (fractions) {
    frac_numbers <- data$fraction

    if (!identical(frac_include, "all")) {
      if (!is.numeric(frac_include) || length(frac_include) != 2) {
        cli::cli_abort(
          "{.arg frac_include} must be {.val all} or a numeric vector of length 2, not {.obj_type_friendly {frac_include}}."
        )
      }
      frac_numbers <- frac_numbers[
        frac_numbers >= frac_include[1] & frac_numbers <= frac_include[2]
      ]
    }
  }
  # Shade the fractions in cycles that start at each labelled fraction.
  shade_period <- max(frac_labelling, 2)

  fractions_present <- TRUE %in%
    stringr::str_detect(colnames(data), "frac") &
    fractions

  absorbance <- absorbance_columns(data)
  y_title <- absorbance_title(data, absorbance)
  x <- plot_x(data)
  data <- tidyr::pivot_longer(
    data,
    dplyr::all_of(absorbance),
    names_to = "wl"
  )

  plt <- ggplot2::ggplot(data, ggplot2::aes(.data[[x$column]], .data$value))

  if (fractions_present) {
    lab_data <- data |>
      dplyr::filter(
        is_collected(.data$fraction),
        .data$fraction %in% frac_numbers,
        .data$fraction %% frac_labelling == 0,
        wl_number(.data$wl) %in% wl_frac
      ) |>
      dplyr::group_by(dplyr::across(dplyr::any_of(c("run", "fraction")))) |>
      dplyr::summarise("{x$column}" := mean(.data[[x$column]]))

    plt <- plt +
      ggplot2::geom_area(
        position = "identity",
        data = dplyr::filter(
          data,
          is_collected(.data$fraction),
          wl_number(.data$wl) %in% wl_frac,
          .data$fraction %in% frac_numbers
        ),
        ggplot2::aes(
          fill = factor(.data$fraction %% shade_period),
          group = !!line_group(data, c("fraction", "wl"))
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
      linewidth = 0.6,
      ggplot2::aes(
        group = !!line_group(data, "wl"),
        colour = factor(.data$wl)
      ),
      alpha = 0.8
    ) +
    ggplot2::guides(fill = "none") +
    ggplot2::coord_cartesian(ylim = c(0, NA)) +
    ggplot2::labs(
      colour = "Wavelength (nm)",
      x = x$title,
      y = y_title
    ) +
    ggplot2::scale_x_continuous(breaks = scales::pretty_breaks(10)) +
    ggplot2::scale_colour_discrete(labels = wl_label) +
    ggplot2::scale_fill_manual(
      # values = RColorBrewer::brewer.pal(7, "Greys")[2:6]
      values = grDevices::gray(seq(0.85, 0.65, length.out = shade_period))
    ) +
    # theme_bw(base_size = 20) +
    ggplot2::theme_classic() +
    ggplot2::theme(
      legend.position = c(0.05, 0.95),
      legend.justification = c(0, 1),
      legend.background = ggplot2::element_rect(
        colour = "gray30",
        linewidth = 0.8
      )
    )
}

# The column to plot along the x axis, `volume` or else `time`, and its axis
# title.
plot_x <- function(data, call = rlang::caller_env()) {
  column <- intersect(c("volume", "time"), names(data))
  if (length(column) == 0) {
    cli::cli_abort(
      c(
        "{.arg data} must have a {.field volume} or {.field time} column.",
        i = "Add a volume to runs that only record the time with
          {.fn chrom_add_volume}."
      ),
      call = call
    )
  }
  column <- column[[1]]
  title <- if (column == "volume") "Volume" else "Time"
  list(column = column, title = unit_title(title, data[[column]]))
}

# Groups for lines and areas: one for each value of `columns`, in each run, so
# the lines of runs read together aren't joined.
line_group <- function(data, columns) {
  columns <- c(columns, if ("run" %in% names(data)) "run")
  rlang::expr(interaction(!!!rlang::syms(columns), drop = TRUE))
}

# The wavelength of absorbance column names such as "a280".
wl_number <- function(x) {
  as.numeric(stringr::str_match(x, "^a(\\d+)")[, 2])
}

# Legend labels for absorbance column names: the wavelength, or "UV".
wl_label <- function(x) {
  dplyr::coalesce(stringr::str_match(x, "^a(\\d+)")[, 2], toupper(x))
}
