test_that("fraction() makes 0 missing and checks its input", {
  x <- fraction(c(0, 1, 2, NA))

  expect_true(is_fraction(x))
  expect_equal(as.integer(x), c(NA, 1L, 2L, NA))
  expect_identical(fraction(x), x)
  expect_snapshot(error = TRUE, {
    fraction(-1)
    fraction(1.5)
    fraction("a")
  })
})

test_that("fractions do arithmetic and compare as numbers", {
  x <- fraction(c(0, 1, 2))

  expect_equal(x + 10L, fraction(c(NA, 11, 12)))
  expect_equal(10 + x, fraction(c(NA, 11, 12)))
  expect_equal(x - 1, fraction(c(NA, 0, 1)))
  expect_equal(x %% 2, c(NA, 1, 0))
  expect_equal(x + 0.5, c(NA, 1.5, 2.5))
  expect_equal(x - x, c(NA, 0L, 0L))
  expect_equal(x == 2, c(NA, FALSE, TRUE))
  expect_equal(x %in% 2, c(FALSE, FALSE, TRUE))
  expect_equal(max(x, na.rm = TRUE), fraction(2))
  expect_snapshot(error = TRUE, -x)
})

test_that("fractions combine with numbers as numbers", {
  x <- fraction(1:2)

  expect_identical(vctrs::vec_c(x, x), fraction(c(1:2, 1:2)))
  expect_identical(vctrs::vec_c(x, 3L), 1:3)
  expect_identical(vctrs::vec_c(x, 3), c(1, 2, 3))
  expect_identical(dplyr::if_else(c(TRUE, FALSE), x, 0), c(1, 0))
})

test_that("fractions print and plot", {
  x <- fraction(c(0, 1, 2))

  expect_snapshot({
    x
    tibble::tibble(fraction = x)
  })
  expect_equal(levels(factor(x)), c("1", "2"))

  dat <- tibble::tibble(volume = 1:3, fraction = x)
  built <- ggplot2::ggplot_build(
    ggplot2::ggplot(dat, ggplot2::aes(.data$fraction, .data$volume)) +
      ggplot2::geom_point(na.rm = TRUE)
  )
  expect_equal(built$data[[1]]$x, c(NA, 1, 2))
})

test_that("readers give fraction numbers", {
  quadtec <- chrom_read_quadtech(example_file("example_quadtec_2.TXT"))
  akta <- read_akta_old(example_file("example_akta_2.asc"))

  expect_true(is_fraction(quadtec$fraction))
  expect_true(is.na(quadtec$fraction[1]))
  expect_equal(quadtec$fraction[2], fraction(33))
  expect_true(is_fraction(akta$fraction))
})

test_that("chrom_fractions() gives where each fraction starts and ends", {
  dat <- tibble::tibble(
    volume = c(0, 1, 2, 3, 4, 5),
    fraction = fraction(c(0, 1, 1, 2, 0, 0))
  )

  expect_equal(
    chrom_fractions(dat),
    tibble::tibble(fraction = fraction(1:2), start = c(1, 3), end = c(3, 4))
  )

  fractions <- chrom_fractions(read_akta_old(example_file(
    "example_akta_2.asc"
  )))
  expect_equal(fractions$fraction, fraction(1:73))
  # The fraction collector marked fraction 1 at 9.02 mL and fraction 2 at
  # 11.02 mL; the points are 0.16 mL apart.
  expect_equal(vctrs::vec_data(fractions$start[1]), 9.02, tolerance = 0.2)
  expect_equal(vctrs::vec_data(fractions$end[1]), 11.02, tolerance = 0.2)
  expect_equal(fractions$end[-73], fractions$start[-1])
})

test_that("chrom_fractions() works run by run", {
  run <- tibble::tibble(volume = 1:3, fraction = c(0, 1, 2))
  dat <- chrom_append_run(run, run)

  fractions <- chrom_fractions(dat)
  expect_equal(fractions$run, c(1, 1, 2, 2))
  expect_equal(fractions$start, c(2, 3, 5, 6))
})

test_that("chrom_fractions() needs volume and fraction", {
  expect_snapshot(error = TRUE, {
    chrom_fractions(tibble::tibble(volume = 1))
  })
})

test_that("fraction_to_well() gives the well of each fraction", {
  skip_if_not_installed("wellr", "0.4.0")

  wells <- fraction_to_well(fraction(c(1, 2, 13, 0)))
  expect_true(wellr::is_well(wells))
  expect_equal(as.character(wells), c("A01", "A02", "B01", NA))
  expect_equal(
    as.character(fraction_to_well(1:2, colwise = TRUE)),
    c("A01", "B01")
  )
  expect_equal(wellr::well_plate_size(fraction_to_well(1, plate = 24)), 24)
})
