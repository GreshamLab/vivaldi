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
#' cols = c("sample","CHROM","POS","REF","ALT","ANN","ALT_TYPE","major","minor")
#' DF_reps = merge_replicates(VCF_DF,replicates,"rep1","rep2",cols)
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
