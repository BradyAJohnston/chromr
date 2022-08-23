library(dplyr)
library(tidyr)
library(ggplot2)



fl1 <- "inst/extdata/20220809_SFPQfl_TEVdig_S200_part1.TXT"
df1 <- chrom_read_quadtech(fl1)
fl2 <- "inst/extdata/20220809_SFPQfl_TEVdig_S200_part2.TXT"
df2 <- chrom_read_quadtech(fl2)


dat <- df1 %>%
  chrom_append_run(chrom_read_quadtech(fl2)) %>%
  filter(!is.na(wl))



chrom_plot_fractions(dat, wl_frac = c(280, 488))
