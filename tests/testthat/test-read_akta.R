test_that("chrom_read_akta_csv() reads UNICORN curves", {
  dat <- chrom_read_akta_csv(example_file("example_akta_3.csv"))

  expect_named(dat, c("volume", "uv"))
  expect_equal(dat$volume[1:2], quantity(c(0.224, 0.237), "mL"))
  expect_equal(absorbance_unit(dat$uv), "mAU")
  expect_equal(chrom_meta(dat)$instrument, "ÄKTA")
})

test_that("chrom_read_akta_csv() reads fractions and drops the run log", {
  path <- withr::local_tempfile(fileext = ".csv")
  writeLines(
    c(
      "Chrom.1,,Chrom.1,,Chrom.1,,Chrom.1,,",
      "UV,,Gradient Concentration,,Fraction,,Run Log,,",
      "ml,mAU,ml,%B,ml,Fraction,ml,Logbook,",
      '0,1,0,0,0.5,"T1",0,"Method Run",',
      '1,2,1,10,1.5,"T2",,,',
      '2,3,2,20,2.5,"Waste",,,',
      "3,4,,,,,,,"
    ),
    path
  )

  dat <- chrom_read_akta_csv(path)
  expect_named(dat, c("volume", "fraction", "uv", "percent_b"))
  expect_equal(dat$fraction, fraction(c(NA, 1, 2, NA)))
  expect_equal(dat$percent_b[1:3], quantity(c(0, 10, 20), "%"))
})

test_that("chrom_read_akta_csv() errors for other files", {
  path <- example_file("ngc_sec.csv")
  expect_snapshot(chrom_read_akta_csv(path), error = TRUE, transform = \(x) {
    gsub(path, "<file>", x, fixed = TRUE)
  })
})

# A UNICORN result with a UV and a conductivity curve and two fractions. Like
# some real results, the curve archives are padded with zeros after their end.
local_akta_result <- function(env = parent.frame()) {
  skip_if_not_installed("xml2")
  skip_if(Sys.which("zip") == "", "Needs zip to build the test result.")
  dir <- withr::local_tempdir(.local_envir = env)
  net_array <- function(x) {
    c(
      as.raw(c(0, 1, 0, 0, 0, 0xff, 0xff, 0xff, 0xff, 1, 0, 0, 0, 0, 0, 0, 0)),
      as.raw(0x0f),
      writeBin(1L, raw(), size = 4, endian = "little"),
      writeBin(length(x), raw(), size = 4, endian = "little"),
      as.raw(11),
      writeBin(x, raw(), size = 4, endian = "little"),
      as.raw(0x0b)
    )
  }
  curve <- function(name, volume, value) {
    curve_dir <- file.path(dir, paste0(name, "_points"))
    dir.create(curve_dir)
    writeBin(net_array(volume), file.path(curve_dir, "CoordinateData.Volumes"))
    writeBin(
      net_array(value),
      file.path(curve_dir, "CoordinateData.Amplitudes")
    )
    archive <- file.path(dir, paste0(name, ".zip"))
    withr::with_dir(curve_dir, {
      utils::zip(
        archive,
        c("CoordinateData.Volumes", "CoordinateData.Amplitudes"),
        flags = "-q"
      )
    })
    bytes <- readBin(archive, "raw", file.size(archive))
    writeBin(c(bytes, raw(70000)), file.path(dir, name))
  }
  curve("Chrom.1_1_True", c(0, 1, 2, 3), c(1, 2, 3, 4))
  curve("Chrom.1_2_True", c(0, 2), c(10, 30))

  writeLines(
    paste0(
      "<Result><Name>Test run</Name><Created>2017-09-07T12:52:14.17</Created>",
      "<Chromatograms><Chromatogram><ChromatogramFileName>Chrom.1",
      "</ChromatogramFileName></Chromatogram></Chromatograms></Result>"
    ),
    file.path(dir, "Result.xml")
  )
  curve_xml <- function(name, unit, file) {
    paste0(
      "<Curve><Name>",
      name,
      "</Name><AmplitudeUnit>",
      unit,
      "</AmplitudeUnit>",
      "<CurvePoints><CurvePoint><BinaryCurvePointsFileName>",
      file,
      "</BinaryCurvePointsFileName></CurvePoint></CurvePoints></Curve>"
    )
  }
  event <- function(volume, text) {
    paste0(
      "<Event><EventVolume>",
      volume,
      "</EventVolume><EventText>",
      text,
      "</EventText></Event>"
    )
  }
  writeLines(
    paste0(
      "<Chromatogram><Curves>",
      curve_xml("UV", "mAU", "Chrom.1_1_True"),
      curve_xml("Conductivity", "mS/cm", "Chrom.1_2_True"),
      "</Curves><EventCurves>",
      "<EventCurve EventCurveType=\"Fraction\"><Events>",
      event(0.5, "T1"),
      event(1.5, "T2"),
      event(2.5, "Waste"),
      "</Events></EventCurve>",
      "<EventCurve EventCurveType=\"Logbook\"><Events>",
      event(0, "Method Run"),
      "</Events></EventCurve></EventCurves></Chromatogram>"
    ),
    file.path(dir, "Chrom.1.Xml")
  )

  path <- file.path(dir, "result.zip")
  withr::with_dir(dir, {
    utils::zip(
      path,
      c("Result.xml", "Chrom.1.Xml", "Chrom.1_1_True", "Chrom.1_2_True"),
      flags = "-q"
    )
  })
  path
}

test_that("chrom_read_akta_result() reads UNICORN results", {
  dat <- chrom_read_akta_result(local_akta_result())

  expect_named(dat, c("volume", "fraction", "uv", "conductivity"))
  expect_equal(dat$volume, quantity(c(0, 1, 2, 3), "mL"))
  expect_equal(dat$fraction, fraction(c(NA, 1, 2, NA)))
  expect_equal(dat$uv, absorbance(c(1, 2, 3, 4), unit = "mAU"))
  expect_equal(dat$conductivity, quantity(c(10, 20, 30, NA), "mS/cm"))

  meta <- chrom_meta(dat)
  expect_equal(meta$run, "Test run")
  expect_equal(meta$date, "2017-09-07T12:52:14.17")
})

test_that("chrom_read_akta_result() errors for other files", {
  path <- example_file("ngc_sec.csv")
  expect_snapshot(chrom_read_akta_result(path), error = TRUE, transform = \(x) {
    gsub(path, "<file>", x, fixed = TRUE)
  })
})

test_that("chrom_read_ngc_fractions() reads the fraction table", {
  fractions <- chrom_read_ngc_fractions(example_file("ngc_sec_fractions.txt"))

  expect_named(
    fractions,
    c("fraction", "rack", "tube", "start", "end", "collected")
  )
  expect_equal(fractions$fraction, fraction(1:21))
  expect_equal(fractions$rack[1], "A")
  expect_equal(fractions$start[1:2], quantity(c(25.3, 26.81), "mL"))
  expect_equal(fractions$collected[1], quantity(1.51, "mL"))

  path <- example_file("ngc_sec.csv")
  expect_snapshot(
    chrom_read_ngc_fractions(path),
    error = TRUE,
    transform = \(x) {
      gsub(path, "<file>", x, fixed = TRUE)
    }
  )
})
