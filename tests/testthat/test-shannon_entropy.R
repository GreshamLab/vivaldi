test_that("multiplication works", {
  # Sample dataframe
  df <- data.frame(sample = c("m1", "m2", "m1", "m2", "m1"),
                   CHROM = c("PB1", "PB1", "PB2", "PB2", "NP"),
                   minorfreq = c(0.010, 0.022, 0.043, 0.055, 0.011),
                   majorfreq = c(0.990, 0.978, 0.957, 0.945, 0.989),
                   SegmentSize = c(2280, 2280, 2274, 2274, 1809)
  )

  genome_size = 13133

  # Modifies the dataframe to add five new columns
  expect_equal(ncol(shannon_entropy(df, genome_size)),ncol(df)+5)
})
