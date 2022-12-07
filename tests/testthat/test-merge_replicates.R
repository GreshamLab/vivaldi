test_that("merge_replicates", {
  expect_equal(nrow(VCF_DF), 2493)
  expect_equal(ncol(VCF_DF), 18)

  expect_equal(nrow(replicates), 24)
  expect_equal(ncol(replicates), 3)

  cols = c("sample","CHROM","POS","REF","ALT","ANN","ALT_TYPE","major","minor")
  DF_reps = vivaldi::merge_replicates(VCF_DF,replicates,"rep1","rep2",cols)

  expect_equal(nrow(DF_reps), 1117)
  expect_equal(ncol(DF_reps), 35)
})

test_that("")
