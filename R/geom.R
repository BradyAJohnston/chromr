.compute_group_frac <- function(data, scales, period=5, ypos = 0, initial = 0L) {
  data |>
    dplyr::select(x, y, fraction) |>
    dplyr::summarise(
      x = mean(x),
      y = ypos,
      .by = fraction
    ) |>
    dplyr::filter(
      fraction %% period == initial,
      fraction != 0
      ) |>
    dplyr::mutate(
      label = fraction
    )
}

StatFractionGroup <- ggplot2::ggproto(
  `_class` = "StatFractionLabel",
  `_inherit` = ggplot2::Stat,
  required_aes = c("x", "y", "fraction"),
  compute_group = .compute_group_frac
)

.comput_frac_identity <- function(data, scales, period = 5, initial = 0L, lines = NA) {
  if (any(is.na(lines))) {
    mask <- rep(TRUE, nrow(data))
  } else {
    mask <- sapply(data$colour, \(x) x %in% lines, simplify = TRUE)
  }

  data |>
    dplyr::filter(fraction != 0, mask) |>
    dplyr::mutate(
      fill = factor((fraction - initial) %% period),
      group = interaction(group, fraction)
    )
}

StatFraction <- ggplot2::ggproto(
  `_class` = "StatFraction",
  `_inherit` = ggplot2::Stat,
  required_aes = c("x", "y", "fraction"),
  compute_group = .comput_frac_identity
)

geom_fraction_label <- function(mapping = NULL,
                                data = NULL,
                                position = "identity",
                                na.rm = FALSE,
                                show.legend = NA,
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
    params = list(na.rm = na.rm, period = period, initial = initial, ...)
  )
}

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
    params = list(na.rm = na.rm, alpha=alpha, period = period, initial = initial, lines = lines, ...)
  )
}
