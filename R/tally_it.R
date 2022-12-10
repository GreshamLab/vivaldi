#' tally_it
#'
#' Groups the input vcf data frame using a list of variables and tallies the number of occurrences
#'
#' @name tally_it
#' @param df A rearranged vcf dataframe (arrange_gt_data)
#' @param groupit A vector containing column names that data should be grouped by
#' @param new_colname The name of the count column
#' @return A dataframe with columns from the 'groupit' vector and the number of times each unique grouping occurs in the data
#' @export
#' @examples
#' # Sample dataframe of 7 variants across 2 samples
#' df <- data.frame(
#'   sample = c( "sample1", "sample1", "sample1", "sample2",
#'               "sample2", "sample2", "sample2"),
#'   CHROM = c("PB1", "PB2", "PB2", "LEO", "LEO", "LEO", "ALE"),
#'   SegmentSize = c(2280, 2274, 2274, 1701, 1701, 1701, 1888 ),
#'   minorfreq = c(0.04422785, 0.03738175, 0.01390202, 0.02927786,
#'                 0.03071955, 0.02626025, 0.02875321)
#' )
#'
#' # Example 1: to get the sum of variants on every segment:
#' groupit = c('sample','CHROM', "SegmentSize")
#' tally_it(df, groupit, "snv_count")
#'
#' # Example 2: to get the count across genomes:
#' groupit = c('sample')
#' tally_it(df, groupit, "snv_count")
#'
tally_it = function(df, groupit, new_colname){

    snpeff = snpeff_info()

    if (length(intersect(colnames(df), snpeff)) > 0){

      df = df %>% dplyr::select(!tidyselect::all_of(c(snpeff)))

      df = df[!duplicated(df), ] %>% droplevels()

    } else{

      df = df[!duplicated(df), ] %>% droplevels()

    }

    count_df = df %>% dplyr::group_by_at(dplyr::vars(tidyselect::all_of(groupit))) %>% dplyr::tally()

    colnames(count_df)[colnames(count_df) == 'n'] = new_colname

    return(count_df)

}
