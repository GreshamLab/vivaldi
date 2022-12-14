#' add_metadata
#'
#' Adds metadata information to the vcf dataframe
#'
#' @name add_metadata
#' @param df A rearranged vcf dataframe (arrange_data)
#' @param metadf A metadata dataframe
#' @param by_vcf A vector of column names in the vcf dataframe that should be used to merge the vcf data with the metadata
#' @param by_meta A vector of column names in the metadata dataframe that should be used to merge the metadata with the vcf data
#' @return A vcf dataframe with metadata included
#' @export
#' @examples
#' df <- data.frame(CHROM = c("A", "B"),
#'                  POS = c(234, 240),
#'                  REF = c("G", "A"),
#'                  ALT = c("A", "G")
#' )
#'
#' sizes <- data.frame(segment = c("A", "B"),
#'                     SegmentSize = c(2280, 2274)
#' )
#'
#' df
#'
#' sizes
#'
#' # Add a new column of sizes of the segments which are necessary for
#' # downstream calculations such as transition:transversion (tstv) and dNdS.
#' add_metadata(df, sizes, c('CHROM'), c('segment'))
#'
add_metadata = function(df, metadf, by_vcf, by_meta){

    tmp = merge(df, metadf, by.x=tidyselect::all_of(by_vcf), by.y=tidyselect::all_of(by_meta), all.x = TRUE)

    tmp = tmp[!duplicated(tmp), ] %>% droplevels()

    return(tmp)

}
