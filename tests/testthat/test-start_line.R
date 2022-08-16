test_that("finds data start line", {
  expect_equal(
    {
      fl <- system.file(
        "extdata",
        "sec.txt",
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
        "sec_no_volume.txt",
        package = "chromr"
      )

      chromr:::chrom_find_data_start_line(fl)
    },
    22
  )
})
