test_that("merge_replicates", {
  expect_equal(nrow(ex_VCF_DF), 5)
  expect_equal(ncol(ex_VCF_DF), 18)

  expect_equal(nrow(ex_replicates), 2)
  expect_equal(ncol(ex_replicates), 3)

  cols = c("sample","CHROM","POS","REF","ALT","ANN","ALT_TYPE","major","minor")
  DF_reps = vivaldi::merge_replicates(ex_VCF_DF,ex_replicates,"rep1","rep2",cols)

  expect_equal(nrow(DF_reps), 2)
  expect_equal(ncol(DF_reps), 35)
})
