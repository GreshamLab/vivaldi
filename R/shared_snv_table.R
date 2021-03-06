#' shared_snv_table
#'
#' Reads in a dataframe that has been arranged (arrange_gt_data) and filtered (filter_variants) and outputs a table
#'
#' @name shared_snv_table
#' @param vardf A rearranged (arrange_gt_data) and filtered (filtered_variants) vcf dataframe 
#' @return A table listing variants in order by how many samples they are found in 
#' @export
#' @examples
#' shared_snv_plot(vardf)

shared_snv_table = function(vardf){
  
  vardf$variant = paste0(vardf$CHROM,"_",vardf$major, vardf$POS, vardf$minor)
  df = group_by(vardf, variant) %>% mutate(count = 1, totalsamp = sum(count)) 
  
  df = df[!duplicated(df), ] %>% droplevels()
  
  ordered_df = select(df, CHROM,POS,REF,ALT,allele,annotation,feature_type,feature_id,protein_position,
                      HGVS.p,majorfreq,minorfreq,variant,totalsamp) %>%
    arrange(desc(totalsamp))
  
  return(ordered_df)
}
