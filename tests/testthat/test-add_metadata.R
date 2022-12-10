test_that("expected output", {
  df <- data.frame(
    CHROM = c("A", "B"),
    POS = c(234, 240),
    REF = c("G", "A"),
    ALT = c("A", "G")
  )

  metadata <- data.frame(segment = c("A", "B"),
                         SegmentSize = c(2280, 2274))

  # Add a new column of metadata of the segments
  test = add_metadata(df, metadata, c('CHROM'), c('segment'))

  expect_equal(ncol(test), ncol(df) + ncol(metadata) - 1)

})
