# vivaldi package
# Kate Johnson

FiltVar = function(arranged_df, coverage_cutoff=300, frequency_cutoff=0.02){

    # using ALT_FREQ in case there isn't a 'minor variant' present, it still keeps consensus changes from ref
    arranged_df = arranged_df %>% filter(ALT_FREQ >= frequency_cutoff & gt_DP >= coverage_cutoff)

    arranged_df = arranged_df[!duplicated(arranged_df), ] %>% droplevels()

    return(arranged_df)

}
