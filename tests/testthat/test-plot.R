test_that("chrom_plot_fractions() labels and shades every nth fraction", {
  dat <- chrom_read_quadtech(example_file("example_quadtec_1.TXT"))

  built <- ggplot2::ggplot_build(chrom_plot_fractions(dat))
  expect_equal(built$data[[2]]$label, seq(5, 30, by = 5))
  expect_length(unique(built$data[[1]]$fill), 5)

  built <- ggplot2::ggplot_build(chrom_plot_fractions(dat, frac_labelling = 10))
  expect_equal(built$data[[2]]$label, c(10, 20, 30))
  expect_length(unique(built$data[[1]]$fill), 10)
})

test_that("chrom_plot_fractions() checks its arguments", {
  dat <- chrom_read_quadtech(example_file("example_quadtec_1.TXT"))

  expect_snapshot(error = TRUE, {
    chrom_plot_fractions(dat, frac_include = 3)
    chrom_plot_fractions(dat, frac_labelling = 0)
  })
})
