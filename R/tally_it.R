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
#' \dontrun{
#' tally_it(df, c("sample","CHROM"), "snv_count")
#' }
tally_it = function(df, groupit, new_colname){

    snpeff = snpeff_info()

    if (length(intersect(colnames(df), snpeff)) > 0){

      df = df %>% dplyr::select(!all_of(c(snpeff)))

      df = df[!duplicated(df), ] %>% droplevels()

    } else{

      df = df[!duplicated(df), ] %>% droplevels()

    }

    count_df = df %>% group_by_at(vars(all_of(groupit))) %>% tally()

    colnames(count_df)[colnames(count_df) == 'n'] = new_colname

    return(count_df)

}
