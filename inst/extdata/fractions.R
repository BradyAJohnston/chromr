library(tidyverse)

fl <- "inst/extdata/20220809_SFPQfl_TEVdig_S200_part1.TXT"

# fl <- system.file(
#   "extdata",
#   "sec.txt",
#   package = "chromr"
# )

df <- chrom_read_quadtech(fl, interp_volume = TRUE)

df |>
  filter(!is.na(wl)) |>
  filter(wl != 205) |>
  qplot(volume, value, data = _, colour = factor(wl), geom = "line")

fl <- "inst/extdata/20220809_SFPQfl_TEVdig_S200_part2.TXT"



df2 <- chrom_read_quadtech(fl)


chrom_bind_run <- function(data, ...) {

  new_runs <- list(...)

  columns <- c("time", "fraction", "volume")
  names(columns) <- columns

  find_max_values <- function(data, columns) {
    lapply(columns, function(x) {
      if (check_column_exist(data, x)) {
        max(data[, x])
      }
    })
  }

  previous <- find_max_values(data, columns)

  new_runs_updated <- lapply(new_runs, function(x) {
    for (col in names(previous)) {
      x[, col] <- x[, col] + previous[col]
    }
    previous <- find_max_values(x, names(previous))
    x
  })

  dplyr::bind_rows(data, new_runs_updated)
  # new_runs_updated

}

df <- chrom_bind_run(df, df2)
df |>
  filter(!is.na(wl)) |>
  filter(volume > 40, volume < 60) |>
  qplot(volume, value, data = _, geom = "line", group = interaction(wl, fraction),
        fill = factor(fraction %% 5),
        # alpha = wl == 280
        # colour = factor(fraction %% 5)
        ) +
  geom_area(aes(alpha = wl == 280)) +
  geom_line() +
  # facet_wrap(~wl) +
  geom_label(
    data = fracs |>
      filter(volume > 40, volume < 60 , fraction %% 5 == 0),
    aes(label = fraction, y = 0.01, group = fraction),
    fill = "white"
  ) +
  # scale_fill_viridis_d(option = "A")
  NULL

fracs <- df |>
  group_by(fraction) |>
  summarise(volume = mean(volume))
