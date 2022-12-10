test_that("expect output", {
  df <- data.frame(
    sample = c("m1", "m1"),
    CHROM = c("PB1", "PB1"),
    POS = c(234, 266),
    major = c("G", "G"),
    minor = c("A", "A"),
    ALT_TYPE = c("minor", "minor"),
    minorfreq = c(0.010, 0.022),
    majorfreq = c(0.990, 0.978)
  )

  expect_no_error(snv_location(df))
})
