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
#'
#' # The `merge_replicates()` functions generates a dataframe with all variants
#' # that are found in both sequencing replicates. It excludes variants that are
#' # only found in one replicate. In addition, average frequencies are computed
#' # from the two replicates.
#'
#' # `VCF_DF` dataframe shows a pair of replicates and their variants at 3 positions.
#' VCF_DF[ , c("sample", "CHROM", "POS")]
#'
#' # Merge `VCF_DF` and `replicates` dataframes by the following columns
#' cols = c("sample","CHROM","POS","REF","ALT","ALT_TYPE","major","minor")
#'
#' DF_reps = merge_replicates(VCF_DF,replicates,"rep1","rep2",cols)
#'
#' # The dataframe now contains the 2 variants at positions 234 & 240 that were
#' # detected in both sequencing replicates whereas variant at  position 254
#' # was only in a single replicate so it was removed during the merge.
#'
#' DF_reps[, c("sample","CHROM","POS","minorfreq","majorfreq","weighted_minorfreq","weighted_majorfreq")]
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
