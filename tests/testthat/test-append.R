test_that("chrom_append_run() continues time and volume from each run", {
  run <- tibble::tibble(
    time = 0:2,
    volume = c(0, 0.5, 1),
    fraction = c(0, 1, 2)
  )

  dat <- chrom_append_run(run, run, run)
  expect_equal(dat$run, rep(1:3, each = 3))
  expect_equal(dat$time, c(0:2, 2:4, 4:6))
  expect_equal(dat$volume, c(0, 0.5, 1, 1, 1.5, 2, 2, 2.5, 3))
  expect_equal(dat$fraction, rep(c(0, 1, 2), 3))
})

test_that("chrom_append_run() renumbers fractions that start again", {
  run <- tibble::tibble(volume = 1:3, fraction = c(0, 1, 2))

  dat <- chrom_append_run(run, run, run, adjust_fractions = TRUE)
  expect_equal(dat$fraction, c(0, 1, 2, 0, 3, 4, 0, 5, 6))
})

test_that("chrom_append_run() keeps fractions that already continue", {
  df1 <- chrom_read_quadtech(example_file("example_quadtec_1.TXT"))
  df2 <- chrom_read_quadtech(example_file("example_quadtec_2.TXT"))

  expect_no_warning(
    adjusted <- chrom_append_run(df1, df2, adjust_fractions = TRUE)
  )
  expect_equal(adjusted, chrom_append_run(df1, df2))
})

test_that("chrom_append_run() works without a time column", {
  run <- tibble::tibble(volume = 1:3)

  expect_equal(chrom_append_run(run, run)$volume, c(1:3, 4:6))
})
