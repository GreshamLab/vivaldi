#' add_metadata
#'
#' Adds metadata information to the vcf dataframe
#'
#' @name add_metadata
#' @param df A rearranged vcf dataframe (arrange_gt_data)
#' @param metadf A metadata dataframe
#' @param by_vcf A vector of column names in the vcf dataframe that should be used to merge the vcf data with the metadata
#' @param by_meta A vector of column names in the metadata dataframe that should be used to merge the metadata with the vcf data
#' @return A vcf dataframe with metadata included
#' @export
#' @examples
#' add_metadata(vcf_df, metadf, c("CHROM"), c("segment"))
add_metadata = function(vcf_df, metadf, by_vcf, by_meta){

    return(merge(vcf_df, metadf, by.x=all_of(by_vcf), by.y=all_of(by_meta), all.x = TRUE))

}
