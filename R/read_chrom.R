fl <- "~/Dropbox/BondLab/Data/quadtec_chromatograms/Chromatograms/S200_10300_Anal/180720_dPPR10-C2_5mM_DTT.TXT"

# read.csv(fl, skip = 20) |> head()

# readLines(fl, 30)

as.data.frame(do.call(rbind, stringr::str_split(readLines(fl, 20), ",")))


read_chrom <- function(
  file,
  header = 21
) {

  head_info <- readLines(file, header-1)

  splits <- stringr::str_split(head_info, ",")

  details <- as.data.frame(do.call(rbind, splits))

  colnames(details) <- c("detail", "value", "extra")

  data <- read.csv(file, skip = header-1)

  run <- list(
    details = details,
    data = data
  )

  run

}

dl <- read_chrom(fl)
wl <- grep("QuadTec", dl$details$detail, ignore.case = TRUE)

readr::parse_number(dl$details$detail[wl])
library(dplyr)
colnames(dl$data) %>% stringr::str_split("QuadTec.") %>%
  unlist() %>%
  readr::parse_number()
