#' shared_snv_table
#'
#' Reads in a dataframe that has been arranged (arrange_data) and filtered
#' (filter_variants) and outputs a table
#'
#' The `shared_snv_table()` function takes the variant dataframe and creates a
#' new table, listing the variants in descending order of frequency how many
#' samples they are found in. This function is meant to simplify further
#' investigation of visual patterns in the previous plot.
#'
#' @name shared_snv_table
#' @param vardf A rearranged (arrange_data) and filtered (filtered_variants) vcf dataframe
#' @return A table listing variants in order by how many samples they are found in
#' @export
#' @examples
#' # Sample dataframe has 57 columns
#' dim(example_filtered_SNV_df)
#'
#' # Simplify sample dataframe
#' df <- shared_snv_table(example_filtered_SNV_df)
#'
#' # Dataframe created has 15 columns
#' df
#' dim(df)
#'
shared_snv_table = function(vardf){

  vardf$variant = paste0(vardf$CHROM,"_",vardf$major, vardf$POS, vardf$minor)
  df = dplyr::group_by(vardf, variant) %>% dplyr::mutate(count = 1, totalsamp = sum(count))

  df = df[!duplicated(df), ] %>% droplevels()

  ordered_df = dplyr::select(df, CHROM,POS,REF,ALT,major,minor,annotation,feature_type,feature_id,protein_position,
                      HGVS.p,majorfreq,minorfreq,variant,totalsamp) %>%
    dplyr::arrange(dplyr::desc(totalsamp))

  return(ordered_df)
}
