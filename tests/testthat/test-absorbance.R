test_that("absorbance() records the wavelength and unit", {
  x <- absorbance(c(0.1, 0.25), wavelength = 280, unit = "AU")

  expect_true(is_absorbance(x))
  expect_equal(absorbance_wavelength(x), 280)
  expect_equal(absorbance_unit(x), "AU")
  expect_equal(absorbance_unit(absorbance(1)), "mAU")
  expect_true(is.na(absorbance_wavelength(absorbance(1))))
  expect_snapshot(error = TRUE, {
    absorbance(1, unit = "OD")
    absorbance(1, wavelength = "280")
    absorbance_unit(1)
  })
})

test_that("absorbance_convert() converts between AU and mAU", {
  x <- absorbance(c(0.1, 0.25), wavelength = 280, unit = "AU")

  expect_equal(
    absorbance_convert(x, "mAU"),
    absorbance(c(100, 250), wavelength = 280, unit = "mAU")
  )
  expect_equal(absorbance_convert(absorbance_convert(x, "mAU"), "AU"), x)
})

test_that("combining absorbance converts units and drops differing wavelengths", {
  au <- absorbance(0.1, wavelength = 280, unit = "AU")
  mau <- absorbance(150, wavelength = 280, unit = "mAU")
  a260 <- absorbance(0.2, wavelength = 260, unit = "AU")

  expect_equal(vctrs::vec_c(au, mau), absorbance(c(100, 150), 280, "mAU"))
  expect_equal(vctrs::vec_c(au, au), absorbance(c(0.1, 0.1), 280, "AU"))
  expect_equal(vctrs::vec_c(au, a260), absorbance(c(0.1, 0.2), NA, "AU"))
  expect_identical(vctrs::vec_c(au, 1), c(0.1, 1))
})

test_that("absorbance can't be assigned values at another wavelength", {
  x <- absorbance(c(0.1, 0.25), wavelength = 280, unit = "AU")

  x[1] <- absorbance(150, wavelength = 280, unit = "mAU")
  expect_equal(x, absorbance(c(0.15, 0.25), 280, "AU"))
  expect_snapshot(error = TRUE, {
    x[1] <- absorbance(1, wavelength = 260)
  })
})

test_that("absorbance does arithmetic", {
  x <- absorbance(c(0.1, 0.25), wavelength = 280, unit = "AU")
  a260 <- absorbance(c(150, 400), wavelength = 260, unit = "mAU")

  expect_equal(x - 0.1, absorbance(c(0, 0.15), 280, "AU"))
  expect_equal(2 * x, absorbance(c(0.2, 0.5), 280, "AU"))
  expect_equal(x / 2, absorbance(c(0.05, 0.125), 280, "AU"))
  expect_equal(-x, absorbance(c(-0.1, -0.25), 280, "AU"))
  expect_equal(a260 / x, c(1.5, 1.6))
  expect_equal(x - x, absorbance(c(0, 0), 280, "AU"))
  expect_equal(mean(x), absorbance(0.175, 280, "AU"))
  expect_equal(max(x), absorbance(0.25, 280, "AU"))
  expect_equal(log(x), log(c(0.1, 0.25)))
  expect_snapshot(error = TRUE, x * a260)
})

test_that("absorbance prints with its unit", {
  x <- absorbance(c(0.1, 0.25), wavelength = 280, unit = "AU")

  expect_snapshot({
    x
    absorbance(1)
    tibble::tibble(a280 = x)
  })
})

test_that("readers give absorbance in the instrument's unit", {
  quadtec <- chrom_read_quadtech(example_file("example_quadtec_3.txt"))
  ngc <- chrom_read_ngc(example_file("ngc_sec.csv"))
  akta <- read_akta_old(example_file("example_akta_2.asc"))

  expect_equal(absorbance_unit(quadtec$a280), "AU")
  expect_equal(absorbance_wavelength(quadtec$a550), 550)
  expect_equal(absorbance_unit(quadtec$uv), "AU")
  expect_true(is.na(absorbance_wavelength(quadtec$uv)))
  expect_equal(absorbance_unit(ngc$a260), "mAU")
  expect_equal(absorbance_wavelength(ngc$a260), 260)
  expect_equal(absorbance_unit(akta$uv), "mAU")
})

test_that("appending runs in different units converts to mAU", {
  quadtec <- chrom_read_quadtech(example_file("example_quadtec_3.txt"))
  ngc <- chrom_read_ngc(example_file("ngc_sec.csv"))

  dat <- chrom_append_run(quadtec, ngc)
  expect_equal(absorbance_unit(dat$a280), "mAU")
  expect_equal(
    vctrs::vec_data(dat$a280[dat$run == 1]),
    vctrs::vec_data(quadtec$a280) * 1000
  )
})

test_that("pivoting keeps the unit and gets back the wavelengths", {
  dat <- chrom_read_quadtech(example_file("example_quadtec_3.txt"))

  long <- pivot_wl_longer(dat)
  expect_equal(absorbance_unit(long$abs), "AU")
  expect_equal(unique(long$wl), c("a280", "a260", "a550", "a650", "uv"))
  expect_equal(pivot_wl_wider(long)[names(dat)], as_plain_tibble(dat))
})

test_that("plots label absorbance with its unit", {
  quadtec <- chrom_read_quadtech(example_file("example_quadtec_3.txt"))
  ngc <- chrom_read_ngc(example_file("ngc_sec.csv"))

  expect_equal(chrom_plot(quadtec)$labels$y, "Absorbance (AU)")
  expect_equal(chrom_plot(ngc)$labels$y, "Absorbance (mAU)")
  expect_equal(
    chrom_plot(plain(quadtec))$labels$y,
    "Absorbance"
  )

  built <- ggplot2::ggplot_build(chrom_plot(quadtec))
  expect_equal(
    built$plot$scales$get_scales("colour")$get_labels(),
    c("260", "280", "550", "650", "UV")
  )
})
