test_that("quantity() records a standard unit", {
  expect_true(is_quantity(quantity(1, "mL")))
  expect_equal(quantity_unit(quantity(1, "ml")), "mL")
  expect_equal(quantity_unit(quantity(1, "Second")), "s")
  expect_equal(quantity_unit(quantity(1, "PSI")), "psi")
  expect_equal(quantity_unit(quantity(1, " %B")), "%")
  expect_equal(quantity_unit(quantity(1, "furlongs")), "furlongs")
  expect_snapshot(error = TRUE, {
    quantity(1, 2)
    quantity("a", "mL")
    quantity_unit(1)
  })
})

test_that("quantity_convert() converts units of the same kind", {
  expect_equal(
    quantity_convert(quantity(1, "MPa"), "psi"),
    quantity(145.0378, "psi"),
    tolerance = 1e-6
  )
  expect_equal(quantity_convert(quantity(90, "s"), "min"), quantity(1.5, "min"))
  expect_equal(quantity_convert(quantity(1.5, "L"), "mL"), quantity(1500, "mL"))
  expect_equal(
    quantity_convert(quantity(1, "S/m"), "mS/cm"),
    quantity(10, "mS/cm")
  )
  expect_snapshot(error = TRUE, {
    quantity_convert(quantity(1, "mL"), "s")
    quantity_convert(quantity(1, "furlongs"), "mL")
  })
})

test_that("quantities compare and do arithmetic with numbers", {
  x <- quantity(c(5, 15), "mL")

  expect_equal(x > 10, c(FALSE, TRUE))
  expect_equal(x == 5, c(TRUE, FALSE))
  expect_equal(x + 1, quantity(c(6, 16), "mL"))
  expect_equal(1 + x, quantity(c(6, 16), "mL"))
  expect_equal(x * 2, quantity(c(10, 30), "mL"))
  expect_equal(x / 5, quantity(c(1, 3), "mL"))
  expect_equal(-x, quantity(c(-5, -15), "mL"))
  expect_equal(x - quantity(0.005, "L"), quantity(c(0, 10), "mL"))
  expect_equal(x / quantity(5, "mL"), c(1, 3))
  expect_equal(max(x), quantity(15, "mL"))
  expect_equal(diff(x), quantity(10, "mL"))
  expect_equal(sqrt(quantity(4, "mL")), 2)
  expect_snapshot(error = TRUE, {
    x * x
    x + quantity(1, "s")
  })
})

test_that("combining quantities converts to the unit of the first", {
  psi <- quantity(10, "psi")
  mpa <- quantity(1, "MPa")

  expect_equal(
    vctrs::vec_c(psi, mpa),
    quantity(c(10, 145.0377), "psi"),
    tolerance = 1e-6
  )
  expect_identical(vctrs::vec_c(psi, 1), c(10, 1))
  expect_snapshot(error = TRUE, vctrs::vec_c(psi, quantity(1, "mL")))
})

test_that("quantities print and plot with their unit", {
  dat <- tibble::tibble(volume = quantity(c(1, 2.5), "mL"))
  expect_snapshot({
    dat$volume
    dat
  })

  built <- ggplot2::ggplot_build(
    ggplot2::ggplot(dat, ggplot2::aes(.data$volume, .data$volume)) +
      ggplot2::geom_point()
  )
  expect_equal(built$data[[1]]$x, c(1, 2.5))
  expect_equal(
    chrom_plot(chrom_read_ngc(example_file("ngc_sec.csv")))$labels$x,
    "Volume (mL)"
  )
})

test_that("appending runs converts their units", {
  ngc <- chrom_read_ngc(example_file("ngc_sec.csv"))
  akta <- read_akta_old(example_file("example_akta_2.asc"))
  akta$pressure <- akta$pressure + 0.1

  dat <- chrom_append_run(ngc, akta)
  expect_equal(quantity_unit(dat$pressure), "psi")
  expect_equal(
    vctrs::vec_data(dat$pressure[dat$run == 2][1]),
    vctrs::vec_data(quantity_convert(akta$pressure[1], "psi"))
  )
})
