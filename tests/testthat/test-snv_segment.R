test_that("expected output", {
  expect_no_error(snv_segment(head(example_filtered_SNV_df)))
})
