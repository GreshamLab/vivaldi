test_that("plot is created without error", {
  expect_no_error(snv_genome(head(example_filtered_SNV_df)))
})
