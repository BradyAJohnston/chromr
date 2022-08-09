pkgname <- "chromr"
source(file.path(R.home("share"), "R", "examples-header.R"))
options(warn = 1)
library('chromr')

base::assign(".oldSearch", base::search(), pos = 'CheckExEnv')
base::assign(".old_wd", base::getwd(), pos = 'CheckExEnv')
cleanEx()
nameEx("chrom_add_volume")
### * chrom_add_volume

flush(stderr()); flush(stdout())

### Name: chrom_add_volume
### Title: Add Volume Column From Time Units
### Aliases: chrom_add_volume

### ** Examples

fl <- system.file(
  "extdata",
  "sec_no_volume.txt",
  package = "chromr"
)
# read just the data
dat <- fl |>
  chrom_read_quadtech()
dat
# add a volume given a constant flow rate
dat |>
  chrom_add_volume(0.3)



cleanEx()
nameEx("chrom_interp_volume")
### * chrom_interp_volume

flush(stderr()); flush(stdout())

### Name: chrom_interp_volume
### Title: Interpolate Volume from Time
### Aliases: chrom_interp_volume

### ** Examples

fl <- system.file(
  "extdata",
  "sec.txt",
  package = "chromr"
)

dat <- fl |>
  chrom_read_quadtech(interp_volume = FALSE)

dat

dat |>
  chrom_interp_volume(time, volume)




cleanEx()
nameEx("chrom_plot")
### * chrom_plot

flush(stderr()); flush(stdout())

### Name: chrom_plot
### Title: Plot a Chromatogram
### Aliases: chrom_plot

### ** Examples

fl <- system.file(
  "extdata",
  "sec_no_volume.txt",
  package = "chromr"
)

fl |>
  chrom_read_quadtech() |>
  chrom_add_volume(0.3) |>
  chrom_plot(xlim = c(0, 3), ylim = c(NA, 0.01))



cleanEx()
nameEx("chrom_read_ngc")
### * chrom_read_ngc

flush(stderr()); flush(stdout())

### Name: chrom_read_ngc
### Title: Read .csv Chromatogram from the BioRad NGC
### Aliases: chrom_read_ngc

### ** Examples

fl <- system.file(
  "extdata",
  "ngc_sec.csv",
  package = "chromr"
)

dat <- chrom_read_ngc(fl)
dat



cleanEx()
nameEx("chrom_read_quadtech")
### * chrom_read_quadtech

flush(stderr()); flush(stdout())

### Name: chrom_read_quadtech
### Title: Read BioRad QuadTech Chromatogram Files
### Aliases: chrom_read_quadtech

### ** Examples

fl <- system.file("extdata",
                  "sec.txt",
                  package = "chromr")
# just read
fl |>
  chrom_read_quadtech()

# read without interpolating volume
fl |>
  chrom_read_quadtech(interp_volume = FALSE)
# read then plot
fl |>
  chrom_read_quadtech() |>
  chrom_plot()



### * <FOOTER>
###
cleanEx()
options(digits = 7L)
base::cat("Time elapsed: ", proc.time() - base::get("ptime", pos = 'CheckExEnv'),"\n")
grDevices::dev.off()
###
### Local variables: ***
### mode: outline-minor ***
### outline-regexp: "\\(> \\)?### [*]+" ***
### End: ***
quit('no')
