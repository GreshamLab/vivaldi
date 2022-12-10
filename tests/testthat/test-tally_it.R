test_that("multiplication works", {
  # Sample dataframe of 7 variants across 2 samples
  df <- data.frame(
    sample = c( "sample1", "sample1", "sample1", "sample2",
                "sample2", "sample2", "sample2"),
    CHROM = c("PB1", "PB2", "PB2", "LEO", "LEO", "LEO", "ALE"),
    SegmentSize = c(2280, 2274, 2274, 1701, 1701, 1701, 1888 ),
    minorfreq = c(0.04422785, 0.03738175, 0.01390202, 0.02927786,
                  0.03071955, 0.02626025, 0.02875321)
  )

  # Example 1: to get the sum of variants on every segment:
  groupit = c('sample','CHROM', "SegmentSize")
  test = tally_it(df, groupit, "snv_count")

  expect_equal(ncol(test), length(groupit)+1)
  expect_equal(nrow(test), 4)

  # Example 2: to get the count across genomes:
  groupit = c('sample')
  test = tally_it(df, groupit, "snv_count")

  expect_equal(ncol(test), length(groupit)+1)
  expect_equal(nrow(test), 2 )

})
