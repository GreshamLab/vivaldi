test_that("expected output", {
  # Simplify sample dataframe
  df <- shared_snv_table(head(example_filtered_SNV_df))

  # Dataframe created has 15 columns
  expect_equal(ncol(df),15)
})
