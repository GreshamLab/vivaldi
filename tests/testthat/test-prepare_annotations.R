test_that("prepare_annotation", {
  # Given expected target "ANN" and number of pieces
  df <- data.frame( ANN = c("A|B|C|D|E|F|G|H|I|J|K|L|M|N|O|P"))
  expect_equal(ncol(prepare_annotations(df)),  ncol(df)+length(snpeff_info()))

  # Not given expected target "ANN"
  df <- data.frame( AN = c("A|B|C|D|E|F|G|H|I|J|K|L|M|N|O|P"))
  expect_message(prepare_annotations(df),"No annotations present in dataframe - to annotate use SNPeff")

  # Given target "ANN" but no the expected number of pieces
  df <- data.frame( ANN = c("A|B|C|D|E|F|G|H|I|J|K|L|M|N"))
  expect_warning(prepare_annotations(df))

  # Real data
  df <- data.frame( sample = c("m1","m1","m1","m1"),
   CHROM = c("H1N1_HA","H1N1_HA","H1N1_HA","H1N1_HA"),
   POS = c(1007, 1145, 1293, 1319),
   REF = c("G", "T", "T", "C"),
   ALT = c("A", "A", "C", "T"),
   ANN = c("A|missense_variant|MODERATE|CDS_H1N1_HA_1_1701|H1N1_HA|transcript|H1N1_HA.1|protein_coding|1/1|c.1007G>A|p.Arg336Lys|1007/1701|1007/1701|336/566||",
   "A|missense_variant|MODERATE|CDS_H1N1_HA_1_1701|H1N1_HA|transcript|H1N1_HA.1|protein_coding|1/1|c.1145T>A|p.Leu382Gln|1145/1701|1145/1701|382/566||",
   "C|synonymous_variant|LOW|CDS_H1N1_HA_1_1701|H1N1_HA|transcript|H1N1_HA.1|protein_coding|1/1|c.1293T>C|p.Gly431Gly|1293/1701|1293/1701|431/566||",
   "T|missense_variant|MODERATE|CDS_H1N1_HA_1_1701|H1N1_HA|transcript|H1N1_HA.1|protein_coding|1/1|c.1319C>T|p.Ala440Val|1319/1701|1319/1701|440/566||")
  )
  expect_equal(ncol(prepare_annotations(df)), ncol(df)+length(snpeff_info()))



})
