shared_snv_table = function(vardf){
  
  vardf$variant = paste0(vardf$CHROM,"_",vardf$major, vardf$POS, vardf$minor)
  df = group_by(vardf, variant) %>% mutate(count = 1, totalsamp = sum(count)) 
  
  ordered_df = select(df, CHROM,POS,REF,ALT,allele,annotation,feature_type,feature_id,protein_position,
                      HGVS.p,majorfreq,minorfreq,variant,totalsamp) %>%
    arrange(desc(totalsamp))
  
  return(ordered_df)
}