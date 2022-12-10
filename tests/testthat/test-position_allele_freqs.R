test_that("expected output", {
  expect_no_error(position_allele_freq(head(example_filtered_SNV_df),"H1N1_HA", "1145"))
})
