test_that("expected output", {
  df <- data.frame(
    sample = c( "sample1"),
    minorfreq = c(0.1)
  )

  expect_no_error(af_distribution(df))
})
