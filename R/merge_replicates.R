#' merge_replicates
#'
#' Merges replicate VCF files into a single dataframe
#'
#' @name merge_replicates
#' @param vardf Data frame of variants
#' @param repdata Data frame of replicate information
#' @param nameofrep1 Name of variable representing the first replicate, must be written with quotes
#' @param nameofrep2 Name of variable representing the second replicate
#' @param commoncols List of columns to merge the replicates by
#' @return a data frame containing replicate information
#' @export
#' @examples
#' df <- data.frame(sample = c("m1", "m2", "m1", "m2", "m1"),
#'                  CHROM = c("PB1", "PB1", "PB2", "PB2", "NP"),
#'                  POS = c(234, 234, 240, 240, 254),
#'                  REF = c("G", "G", "A", "A", "C"),
#'                  ALT = c("A", "A", "G", "G", "T"),
#'                  minorfreq = c(0.010, 0.022, 0.043, 0.055, 0.011),
#'                  majorfreq = c(0.990, 0.978, 0.957, 0.945, 0.989),
#'                  minorcount = c(7, 15, 26, 32, 7),
#'                  majorcount = c(709, 661, 574, 547, 610),
#'                  gt_DP = c(716, 676, 600, 579, 617)
#' )
#'
#' # Dataframe shows a pair of replicates and their variants at 3 positions.
#' df
#'
#' replicates <- data.frame(filename = c("m1","m2"),
#'                          replicate = c("rep1", "rep2"),
#'                          sample = c("a_2_iv", "a_2_iv")
#' )
#'
#' # Dataframe showing relationship between filename, replicate, and sample name
#' replicates
#'
#' # Merge `df` and `replicates` dataframes by the following columns
#' cols = c("sample","CHROM","POS","REF","ALT")
#'
#' merge_replicates(df, replicates, "rep1", "rep2", cols)
#' # The dataframe now contains the 2 variants at positions 234 & 240 that were
#' # detected in both sequencing replicates whereas the variant at position 254
#' # was only in a single replicate so it was removed during the merge.
#'
merge_replicates = function(vardf, repdata, nameofrep1, nameofrep2,commoncols){

  df = merge(repdata,vardf, by.x = c("filename"), by.y = c("sample"))

  df_rep1 = dplyr::filter(df, replicate == nameofrep1)
  # nameofrep1 must be given in ""
  df_rep2 = dplyr::filter(df, replicate == nameofrep2)
  # nameofrep2 must be given in ""

  df_merged = merge(df_rep1, df_rep2, by = commoncols)
  df_merged = df_merged[!duplicated(df_merged), ]

  df_merged$minorfreq = (df_merged$minorfreq.x + df_merged$minorfreq.y) / 2
  df_merged$majorfreq = (df_merged$majorfreq.x + df_merged$majorfreq.y) / 2

  # add weighted average using read counts

  df_merged$weighted_minorfreq = (df_merged$minorcount.x + df_merged$minorcount.y) / (df_merged$gt_DP.x + df_merged$gt_DP.y)
  df_merged$weighted_majorfreq = (df_merged$majorcount.x + df_merged$majorcount.y) / (df_merged$gt_DP.x + df_merged$gt_DP.y)

  return(df_merged)

}
