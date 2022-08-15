library(dplyr)
library(tidyr)
library(ggplot2)



fl1 <- "inst/extdata/20220809_SFPQfl_TEVdig_S200_part1.TXT"
df <- chrom_read_quadtech(fl)
fl2 <- "inst/extdata/20220809_SFPQfl_TEVdig_S200_part2.TXT"
df2 <- chrom_read_quadtech(fl)


df1 %>%
  chrom_append_run(df2)




df
df2


dat <- df |>
  chrom_append_run(chrom_read_quadtech(fl)) |>
  filter(!is.na(wl))

lab_data <- dplyr::filter(
  dat, fraction %% 5 == 0, wl == 280
) |>
  group_by(fraction) |>
  summarise(volume = min(volume), value = mean(value))

dat |>
  ggplot(aes(volume, value)) +
  geom_area(
    position = "identity",
    data = dplyr::filter(dat, fraction != 0, wl == 280),
    aes(fill = factor(fraction %% 5), group = interaction(fraction, wl)),
    alpha = 0.3
  ) +
  geom_vline(
    data = lab_data |> filter(fraction %% 10 == 0),
    aes(xintercept = volume)
  ) +
  geom_label(
    data = lab_data,
    mapping = aes(label = fraction, y = -0.005),
    hjust = -0.2
  ) +
  geom_line(colour = "black", aes(group = factor(wl))) +
  theme_bw() +
  coord_cartesian(ylim = c(0, NA)) +
  scale_fill_viridis_d(option = "D")
