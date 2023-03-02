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
      ~time, ~a280, ~a260,
          0,    0L,    0L,
        0.2,    0L,    0L,
        0.4,    0L,    0L,
        0.6,    0L,    0L,
        0.8,    0L,    0L,
          1,    0L,    0L
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
      ~time, ~volume, ~a280, ~a260,
          0,       0,    0L,    0L,
        0.2,   0.001,    0L,    0L,
        0.4,   0.002,    0L,    0L,
        0.6,   0.003,    0L,    0L,
        0.8,   0.004,    0L,    0L,
          1,   0.005,    0L,    0L
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
      ~time, ~percent_b, ~au, ~m_s_cm, ~psi,             ~volume,    ~a280,    ~a260,     ~a550,    ~a650,
         0L,         0L,  0L,  15.967, 265L,                   0,        0,        0,  0.005505,        0,
         1L,         0L,  0L,  15.969, 266L, 0.00500521376433785,    4e-06,  1.3e-05,  -0.00016, 0.001875,
         2L,         0L,  0L,  15.968, 270L,  0.0100104275286757,        0,   -8e-06,  -6.4e-05, 0.001802,
         3L,         0L,  0L,  15.965, 272L,  0.0150156412930136, -1.3e-05, -2.2e-05,  -6.4e-05, 0.003387,
         4L,         0L,  0L,  15.969, 272L,  0.0200208550573514,   -6e-06, -1.5e-05,  -9.5e-05, 0.003912,
         5L,         0L,  0L,  15.967, 276L,  0.0250260688216893,   -6e-06, -1.8e-05, -0.000128, 0.004128
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
      ~time, ~percent_b, ~au, ~m_s_cm, ~psi, ~volume,    ~a280,    ~a260,     ~a550,    ~a650,
         0L,         0L,  0L,  15.967, 265L,      0L,        0,        0,  0.005505,        0,
         1L,         0L,  0L,  15.969, 266L,      0L,    4e-06,  1.3e-05,  -0.00016, 0.001875,
         2L,         0L,  0L,  15.968, 270L,      0L,        0,   -8e-06,  -6.4e-05, 0.001802,
         3L,         0L,  0L,  15.965, 272L,      0L, -1.3e-05, -2.2e-05,  -6.4e-05, 0.003387,
         4L,         0L,  0L,  15.969, 272L,      0L,   -6e-06, -1.5e-05,  -9.5e-05, 0.003912,
         5L,         0L,  0L,  15.967, 276L,      0L,   -6e-06, -1.8e-05, -0.000128, 0.004128
      )

  )
})
