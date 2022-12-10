test_that("expected output", {
  samples <- c("a_1_fb", "a_1_iv", "a_2_fb", "a_2_iv", "a_3_fb", "a_3_iv", "b_1_fb", "b_1_iv")
  expect_no_error(shared_snv_plot(head(example_filtered_SNV_df), samples))
})
