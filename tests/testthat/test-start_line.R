test_that("finds data start line", {
  expect_equal(
    {
      fl <- system.file(
        "extdata",
        "example_quadtec_3.txt",
        package = "chromr"
      )

      chromr:::chrom_find_data_start_line(fl)
    },
    22
  )
  expect_equal(
    {
      fl <- system.file(
        "extdata",
        "example_quadtec_4.txt",
        package = "chromr"
      )

      chromr:::chrom_find_data_start_line(fl)
    },
    22
  )
})
