test_that("expected ouptut", {
  df <- data.frame(
    CHROM = c("A", "B", "C"),
    POS = c(234, 240, 255),
    ALT_FREQ = c(0.016, 0.049, 0.031),
    gt_DP = c(716, 600, 187)
  )

  # Default: filter by 3% frequency threshold and 200 coverage cutoff
  expect_message(expect_equal(nrow(filter_variants(df)), 1),
                 "Total number of SNP filtered out: 2")

  # Example 1: A 1% allele frequency threshold and 200 coverage cutoff
  expect_message(expect_equal(nrow(filter_variants(df, 200, 0.01)), 2),
                 "Total number of SNP filtered out: 1")

  # Example 2: A 2% allele frequency threshold and 100 coverage cutoff
  expect_message(expect_equal(nrow(filter_variants(df, 100, 0.02)), 2),
                 "Total number of SNP filtered out: 1")

  # Example 3: A 100% allele frequency threshold and 100 coverage cutoff
  expect_message(expect_equal(nrow(filter_variants(df, 100, 1)), 0),
                 "Total number of SNP filtered out: 3")

})
