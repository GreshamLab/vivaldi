test_that("prepare_annotation", {
  # Given expected target "ANN" and number of pieces
  df <- data.frame( ANN = c("A|B|C|D|E|F|G|H|I|J|K|L|M|N|O|P"))
  expect_equal(ncol(prepare_annotations(df)), length(snpeff_info())+1)

  # Not given expected target "ANN"
  df <- data.frame( AN = c("A|B|C|D|E|F|G|H|I|J|K|L|M|N|O|P"))
  expect_message(prepare_annotations(df),"No annotations present in dataframe - to annotate use SNPeff")

  # Given target "ANN" but no the expected number of pieces
  df <- data.frame( ANN = c("A|B|C|D|E|F|G|H|I|J|K|L|M|N"))
  expect_warning(prepare_annotations(df))
})
