test_that("merge_replicates", {
  df <- data.frame(
    sample = c("m1", "m2", "m1", "m2", "m1"),
    CHROM = c("PB1", "PB1", "PB2", "PB2", "NP"),
    POS = c(234, 234, 240, 240, 254),
    REF = c("G", "G", "A", "A", "C"),
    ALT = c("A", "A", "G", "G", "T"),
    minorfreq = c(0.010, 0.022, 0.043, 0.055, 0.011),
    majorfreq = c(0.990, 0.978, 0.957, 0.945, 0.989),
    minorcount = c(7, 15, 26, 32, 7),
    majorcount = c(709, 661, 574, 547, 610),
    gt_DP = c(716, 676, 600, 579, 617)
  )

  replicates <- data.frame(
    filename = c("m1", "m2"),
    replicate = c("rep1", "rep2"),
    sample = c("a_2_iv", "a_2_iv")
  )

  cols = c("sample", "CHROM", "POS", "REF", "ALT")

  test =  merge_replicates(df, replicates, "rep1", "rep2", cols)

  expect_equal(nrow(test), 2)
  expect_equal(ncol(test), 23)
})
