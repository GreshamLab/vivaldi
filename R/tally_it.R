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
#' tally_it(df, c("sample","CHROM"), "snv_count")
tally_it = function(df, groupit, new_colname){
    #INPUT: dataframe and vector of variables want to group by to count
    #OUTPUT: count dataframe using the group variables

    df = df[!duplicated(df), ] %>% droplevels

    count_df = df %>% group_by_at(vars(all_of(groupit))) %>% tally()

    colnames(count_df)[colnames(count_df) == 'n'] = new_colname

    return(count_df)
}
