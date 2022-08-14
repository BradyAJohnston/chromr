test_that("read quadtech file no volume", {
  expect_equal(
    {
      fl <- system.file(
        "extdata",
        "sec_no_volume.txt",
        package = "chromr"
      )

      head(chrom_read_quadtech(fl))
    },
    tibble::tribble(
      ~time, ~name, ~value, ~unit, ~wl,
      0, "QuadTec 1", 0L, "(280.0 nm), AU", 280L,
      0, "QuadTec 2", 0L, "(260.0 nm), AU", 260L,
      0.2, "QuadTec 1", 0L, "(280.0 nm), AU", 280L,
      0.2, "QuadTec 2", 0L, "(260.0 nm), AU", 260L,
      0.4, "QuadTec 1", 0L, "(280.0 nm), AU", 280L,
      0.4, "QuadTec 2", 0L, "(260.0 nm), AU", 260L
    )
  )

  expect_equal(
    {
      fl <- system.file(
        "extdata",
        "sec_no_volume.txt",
        package = "chromr"
      )

      fl %>%
        chrom_read_quadtech() %>%
        chrom_add_volume(0.3) %>%
        head()
    },
    tibble::tribble(
      ~time, ~name, ~value, ~unit, ~wl, ~volume,
      0, "QuadTec 1", 0L, "(280.0 nm), AU", 280L, 0,
      0, "QuadTec 2", 0L, "(260.0 nm), AU", 260L, 0,
      0.2, "QuadTec 1", 0L, "(280.0 nm), AU", 280L, 0.001,
      0.2, "QuadTec 2", 0L, "(260.0 nm), AU", 260L, 0.001,
      0.4, "QuadTec 1", 0L, "(280.0 nm), AU", 280L, 0.002,
      0.4, "QuadTec 2", 0L, "(260.0 nm), AU", 260L, 0.002
    )
  )
})

test_that("read quadtech file with volume", {
  expect_equal(
    {
      fl <- system.file(
        "extdata",
        "sec.txt",
        package = "chromr"
      )

      fl %>%
        chrom_read_quadtech() %>%
        head()
    },
    tibble::tribble(
      ~time, ~volume, ~name, ~value, ~unit, ~wl,
      0L, 0.01, "QuadTec 1", 0, "(280.0 nm), AU", 280L,
      0L, 0.01, "QuadTec 2", 0, "(260.0 nm), AU", 260L,
      0L, 0.01, "Gradient Pump", 0, "%B", NA,
      0L, 0.01, "QuadTec 3", 0.005505, "(550.0 nm), AU", 550L,
      0L, 0.01, "QuadTec 4", 0, "(650.0 nm), AU", 650L,
      0L, 0.01, "UV", 0, "AU", NA
    )
  )

  expect_equal(
    {
      fl <- system.file(
        "extdata",
        "sec.txt",
        package = "chromr"
      )

      fl %>%
        chrom_read_quadtech(interp_volume = FALSE) %>%
        head()
    },
    tibble::tribble(
      ~time, ~volume, ~name, ~value, ~unit, ~wl,
      0L, 0L, "QuadTec 1", 0, "(280.0 nm), AU", 280L,
      0L, 0L, "QuadTec 2", 0, "(260.0 nm), AU", 260L,
      0L, 0L, "Gradient Pump", 0, "%B", NA,
      0L, 0L, "QuadTec 3", 0.005505, "(550.0 nm), AU", 550L,
      0L, 0L, "QuadTec 4", 0, "(650.0 nm), AU", 650L,
      0L, 0L, "UV", 0, "AU", NA
    )
  )
})
