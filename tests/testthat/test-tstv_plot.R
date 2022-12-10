test_that("expected output", {
  df <- tstv_ratio(head(example_filtered_SNV_df),1300)
  expect_no_error(tstv_plot(df))
})
