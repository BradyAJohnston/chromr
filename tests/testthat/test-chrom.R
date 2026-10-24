test_that("readers record the run's metadata", {
  quadtec <- chrom_meta(chrom_read_quadtech(example_file(
    "example_quadtec_1.TXT"
  )))
  expect_equal(quadtec$instrument, "BioRad QuadTec")
  expect_equal(quadtec$run, "20220809_S200_16600_sfpqTEVdig")
  expect_equal(quadtec$date, "04:43:33 PM 08-09-22")
  expect_equal(quadtec$method, "S200_16/600_overnight_pumpA")
  expect_equal(quadtec$file, "example_quadtec_1.TXT")
  ngc <- chrom_meta(chrom_read_ngc(example_file("ngc_sec.csv")))
  expect_equal(ngc$instrument, "BioRad NGC")
  expect_equal(ngc$run, "20191126_IMAC")

  akta <- chrom_meta(read_akta_old(example_file("example_akta_1.asc")))
  expect_equal(akta$instrument, "ÄKTA")
  expect_equal(akta$run, "20220817 DAZAP1 WA construct 2001:10")
})

test_that("chromatograms print their metadata", {
  dat <- chrom_read_ngc(example_file("ngc_sec.csv"))

  expect_snapshot(print(dat, n = 3))
})

test_that("dplyr verbs keep the metadata", {
  dat <- chrom_read_ngc(example_file("ngc_sec.csv"))
  meta <- chrom_meta(dat)

  expect_equal(chrom_meta(dplyr::filter(dat, volume > 1)), meta)
  expect_equal(chrom_meta(dplyr::mutate(dat, ratio = a260 / a280)), meta)
  expect_equal(chrom_meta(dplyr::select(dat, volume, a280)), meta)
  expect_equal(chrom_meta(dat[1:10, ]), meta)
  expect_equal(
    chrom_meta(dplyr::mutate(dat, base = a280 - min(a280), .by = NULL)),
    meta
  )
  expect_null(chrom_meta(dplyr::summarise(dat, max = max(a280))))
})

test_that("chrom_meta() is NULL for other data frames", {
  expect_null(chrom_meta(tibble::tibble(volume = 1)))
})

test_that("chrom_append_run() combines the metadata", {
  df1 <- chrom_read_quadtech(example_file("example_quadtec_1.TXT"))
  df2 <- chrom_read_quadtech(example_file("example_quadtec_2.TXT"))

  meta <- chrom_meta(chrom_append_run(df1, df2))
  expect_equal(meta$instrument, "BioRad QuadTec")
  expect_equal(meta$run, c(chrom_meta(df1)$run, chrom_meta(df2)$run))
  expect_equal(meta$file, c("example_quadtec_1.TXT", "example_quadtec_2.TXT"))

  plain_runs <- chrom_append_run(tibble::tibble(volume = 1), df1)
  expect_null(chrom_meta(plain_runs))
  expect_s3_class(plain_runs, "tbl_df")
})

test_that("readers give the other columns their units", {
  quadtec <- chrom_read_quadtech(example_file("example_quadtec_1.TXT"))
  ngc <- chrom_read_ngc(example_file("ngc_sec.csv"))
  akta <- read_akta_old(example_file("example_akta_2.asc"))

  units <- function(data) {
    purrr::map_chr(
      dplyr::select(data, dplyr::where(is_quantity)),
      quantity_unit
    )
  }
  expect_equal(
    units(quadtec),
    c(
      time = "s",
      volume = "mL",
      percent_b = "%",
      conductivity = "mS/cm",
      pressure = "psi"
    )
  )
  expect_equal(
    units(ngc),
    c(volume = "mL", conductivity = "mS/cm", percent_b = "%", pressure = "psi")
  )
  expect_equal(
    units(akta),
    c(
      volume = "mL",
      conductivity = "mS/cm",
      conductivity_percent = "%",
      percent_b = "%",
      pressure = "MPa",
      flow = "mL/min",
      temperature = "\u00b0C"
    )
  )
})

test_that("chrom_add_volume() uses the unit of time", {
  dat <- chrom_read_quadtech(example_file("example_quadtec_4.txt"))

  with_volume <- chrom_add_volume(dat, 0.3)
  expect_equal(quantity_unit(with_volume$volume), "mL")
  # 0.2 s at 0.3 mL/min
  expect_equal(vctrs::vec_data(with_volume$volume[2]), 0.2 / 60 * 0.3)
  expect_equal(
    chrom_add_volume(plain(dat), 0.3, time = "minute")$volume[2],
    quantity(0.2 * 0.3, "mL")
  )
})

test_that("chrom_append_run() offsets runs in the unit of the first", {
  run1 <- tibble::tibble(time = quantity(c(0, 60), "s"))
  run2 <- tibble::tibble(time = quantity(c(0, 1), "min"))

  expect_equal(
    chrom_append_run(run1, run2)$time,
    quantity(c(0, 60, 60, 120), "s")
  )
})
