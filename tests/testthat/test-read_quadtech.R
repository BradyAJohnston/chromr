test_that("read quadtech file no volume", {
  expect_equal({
    fl <- system.file(
      "extdata",
      "sec_no_volume.txt",
      package = "chromr"
    )

    head(chrom_read_quadtech(fl))
  },
  tibble::tribble(
    ~time,  ~wl, ~abs, ~channel,
        0, 280L,   0L,       1L,
        0, 260L,   0L,       2L,
      0.2, 280L,   0L,       1L,
      0.2, 260L,   0L,       2L,
      0.4, 280L,   0L,       1L,
      0.4, 260L,   0L,       2L
    )
  )

  expect_equal({
    fl <- system.file(
      "extdata",
      "sec_no_volume.txt",
      package = "chromr"
    )

    fl |>
      chrom_read_quadtech() |>
      chrom_add_volume(0.3) |>
      head()

  },
  tibble::tribble(
    ~time,  ~wl, ~abs, ~channel, ~volume,
        0, 280L,   0L,       1L,       0,
        0, 260L,   0L,       2L,       0,
      0.2, 280L,   0L,       1L,   0.001,
      0.2, 260L,   0L,       2L,   0.001,
      0.4, 280L,   0L,       1L,   0.002,
      0.4, 260L,   0L,       2L,   0.002
    )

  )
})

test_that("read quadtech file with volume", {
  expect_equal({
    fl <- system.file(
      "extdata",
      "sec.txt",
      package = "chromr"
    )

    fl |>
      chrom_read_quadtech() |>
      head()

  },
  tibble::tribble(
    ~time, ~volume,  ~wl,     ~abs, ~gradient_pump, ~uv, ~conductivity, ~gp_pressure, ~channel,
       0L,    0.01, 280L,        0,             0L,  0L,        15.967,         265L,       1L,
       0L,    0.01, 260L,        0,             0L,  0L,        15.967,         265L,       2L,
       0L,    0.01, 550L, 0.005505,             0L,  0L,        15.967,         265L,       3L,
       0L,    0.01, 650L,        0,             0L,  0L,        15.967,         265L,       4L,
       1L,    0.02, 280L,    4e-06,             0L,  0L,        15.969,         266L,       1L,
       1L,    0.02, 260L,  1.3e-05,             0L,  0L,        15.969,         266L,       2L
    )

  )

  expect_equal({
    fl <- system.file(
      "extdata",
      "sec.txt",
      package = "chromr"
    )

    fl |>
      chrom_read_quadtech(interp_volume = FALSE) |>
      head()

  },
  tibble::tribble(
    ~time, ~volume,  ~wl,     ~abs, ~gradient_pump, ~uv, ~conductivity, ~gp_pressure, ~channel,
       0L,      0L, 280L,        0,             0L,  0L,        15.967,         265L,       1L,
       0L,      0L, 260L,        0,             0L,  0L,        15.967,         265L,       2L,
       0L,      0L, 550L, 0.005505,             0L,  0L,        15.967,         265L,       3L,
       0L,      0L, 650L,        0,             0L,  0L,        15.967,         265L,       4L,
       1L,      0L, 280L,    4e-06,             0L,  0L,        15.969,         266L,       1L,
       1L,      0L, 260L,  1.3e-05,             0L,  0L,        15.969,         266L,       2L
    )

  )
})
