test_that("every reader gives the standard columns", {
  expect_named(
    chrom_read_quadtech(example_file("example_quadtec_1.TXT")),
    c(
      "time",
      "volume",
      "fraction",
      "a280",
      "a260",
      "a205",
      "a488",
      "percent_b",
      "conductivity",
      "pressure"
    )
  )
  expect_named(
    chrom_read_ngc(example_file("ngc_sec.csv")),
    c("volume", "a260", "a280", "conductivity", "percent_b", "pressure")
  )
  expect_named(
    chrom_read_ngc(example_file("example_ngc_2.csv")),
    c("volume", "a260", "a280", "percent_b")
  )
  expect_named(
    read_akta_old(example_file("example_akta_2.asc")),
    c(
      "volume",
      "fraction",
      "uv",
      "conductivity",
      "conductivity_percent",
      "percent_b",
      "pressure",
      "flow",
      "temperature"
    )
  )
  expect_named(
    read_akta_old(example_file("example_akta_1.asc")),
    c("volume", "fraction", "uv")
  )
})

test_that("chrom_read_ngc() gives one ungrouped row per volume", {
  dat <- chrom_read_ngc(example_file("ngc_sec.csv"))

  expect_false(dplyr::is_grouped_df(dat))
  expect_equal(nrow(dat), 6510)
  expect_equal(
    dat$a260[1:3],
    c(0.746999983675778, 1.10100000165403, 0.783999974373728)
  )
  # Pressure is recorded at fewer volumes, and is interpolated.
  expect_true(is.na(dat$pressure[1]))
  expect_equal(dat$pressure[2], 5)
})

test_that("read_akta_old() aligns the traces by volume", {
  dat <- read_akta_old(example_file("example_akta_2.asc"))

  # The UV trace gives the volumes; conductivity is recorded at other volumes.
  expect_equal(dat$volume[1:3], c(0, 0.16, 0.32))
  expect_equal(nrow(dat), 962)
  # Only the last UV point is past the end of the conductivity trace.
  expect_equal(which(is.na(dat$conductivity)), 962)
})

test_that("read_akta_old() gives each row the fraction it was collected into", {
  dat <- read_akta_old(example_file("example_akta_2.asc"))

  # The first fraction mark is at 9.02 mL, and "Waste" ends the last at 153.79.
  expect_true(all(is.na(dat$fraction[dat$volume < 9.02])))
  expect_equal(unique(dat$fraction[dat$volume >= 9.02 & dat$volume < 11.02]), 1)
  expect_equal(max(dat$fraction, na.rm = TRUE), 73)
  expect_no_warning(read_akta_old(example_file("example_akta_2.asc")))
})

test_that("read_akta_old() deprecates fraction_size", {
  expect_snapshot({
    dat <- read_akta_old(example_file("example_akta_1.asc"), fraction_size = 2)
  })
})

test_that("trace_column() gives standard names", {
  expect_equal(
    trace_column(
      c("QuadTec 1", "UV1 (260 nm)", "UV", "Cond", "Cond%", "Conc", "%B"),
      c("(280.0 nm), AU", "mAU", "mAU", "mS/cm", "%", "%B", "%")
    ),
    c(
      "a280",
      "a260",
      "uv",
      "conductivity",
      "conductivity_percent",
      "percent_b",
      "percent_b"
    )
  )
  expect_equal(trace_column("System Pressure", "psi"), "pressure")
  expect_equal(trace_column("Some Trace", "x"), "some_trace")
})

test_that("fraction_at() follows the fraction marks", {
  expect_equal(
    fraction_at(c(0, 1, 2, 3, 4), c(3, 1, 2.5), c('"2"', '"1"', '"Waste"')),
    c(NA, 1L, 1L, 2L, 2L)
  )
  expect_equal(
    fraction_at(c(0, 1, 3), c(0.5, 2), c("1", "Waste")),
    c(NA, 1L, NA)
  )
})
