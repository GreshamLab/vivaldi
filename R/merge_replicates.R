#' merge_replicates
#'
#' Merges replicate VCF files into a single dataframe
#'
#' @name merge_replicates
#' @param vardir Directory path containing vcf files
#' @param column Directory path containing vcf files
#' @param nameofrep1 Directory path containing vcf files
#' @param nameofrep2 Directory path containing vcf files
#' @param commoncols Directory path containing vcf files
#' @return
#' @export
#' @examples
#' merge_replicates(vardir, column, nameofrep1, nameofrep2, commoncols)
merge_replicates = function(vardir, column, nameofrep1, nameofrep2, commoncols){

  vardir_rep1 = filter(vardir, column == nameofrep1)
  # nameofrep1 must be given in ""
  vardir_rep2 = filter(vardir, column == nameofrep2)
  # nameofrep2 must be given in ""

  vardir_merged = merge(vardir_rep1, vardir_rep2, by = commoncols)
  vardir_merged = vardir_merged[!duplicated(vardir_merged), ]

}
