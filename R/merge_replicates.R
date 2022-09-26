#' merge_replicates
#'
#' Merges replicate VCF files into a single dataframe
#'
#' @name merge_replicates
#' @param vardir Data frame of variants
#' @param vardir Data frame of replicate information
#' @param nameofrep1 Name of variable representing the first replicate, must be written with quotes
#' @param nameofrep2 Name of variable representing the second replicate
#' @param commoncols List of columns to merge the replicates by
#' @return a data frame containing replicate information
#' @export
#' @examples
#' merge_replicates(vardir, column, nameofrep1, nameofrep2, commoncols)
merge_replicates = function(vardir, repdata, nameofrep1, nameofrep2,commoncols){

  df = merge(vardir, repdata, by = c("sample"))

  vardir_rep1 = filter(df, replicate == nameofrep1)
  # nameofrep1 must be given in ""
  vardir_rep2 = filter(df, replicate == nameofrep2)
  # nameofrep2 must be given in ""

  vardir_merged = merge(vardir_rep1, vardir_rep2, by = commoncols)
  vardir_merged = vardir_merged[!duplicated(vardir_merged), ]

  vardir_merged$avg_freq = (vardir_merged$minorfreq.x + vardir_merged$minorfreq.y) / 2

  return(vardir_merged)

}
