# vivaldi package
# Kate Johnson

# using SNPeff annotations
PrepAnn = function(vcf_dataframe){

    # names of elements selected from snpeff website
    snpeff = c('allele','annotation','putative_impact','gene_name','gene_id',
        'feature_type','feature_id','transcript_biotype','rank_total','HGVS.c','HGVS.p',
        'cDNA_position','CDS_position','protein_position','distance_to_feature')

    snpeff2 = c('allele2','annotation2','putative_impact2','gene_name2','gene_id2',
        'feature_type2','feature_id2','transcript_biotype2','rank_total2','HGVS.c2','HGVS.p2',
        'cDNA_position2','CDS_position2','protein_position2','distance_to_feature2')

    snpeff_multi = c('allele','annotation','putative_impact','gene_name','gene_id',
          'feature_type','feature_id','transcript_biotype','rank_total','HGVS.c','HGVS.p',
          'cDNA_position','CDS_position','protein_position','distance_to_feature',
          'allele2','annotation2','putative_impact2','gene_name2','gene_id2',
          'feature_type2','feature_id2','transcript_biotype2','rank_total2','HGVS.c2','HGVS.p2',
          'cDNA_position2','CDS_position2','protein_position2','distance_to_feature2','errors')

    # full df will contain all information - can be used for shannon etc
    # >45 would indicate that there are more than 2 annotations (based off of number of snpeff features)
    full_df = vcf_dataframe %>%
                    filter(lengths(gregexpr("[|]", vcf_dataframe$ANN)) < 45) %>%
                  droplevels()

    # Labeling for future filtering/figures
    full_df = full_df %>% mutate(ann_number = ifelse(lengths(gregexpr("[|]", vcf_dataframe$ANN)) > 16, "two", "one"))

    full_df = full_df %>% separate(ANN, snpeff_multi, "[|]") %>% droplevels()

    too_many_ann = vcf_dataframe %>%
                    filter(lengths(gregexpr("[|]", vcf_dataframe$ANN)) > 30) %>%
                  droplevels()

    message(">2 annotations: ", list(levels(factor(too_many_ann$sample))))

    # building just an annotation df
    # 16 different features provided by snpeff see website for more info
    single_anno = vcf_dataframe %>%
                    filter(lengths(gregexpr("[|]", vcf_dataframe$ANN)) <= 16) %>%
                  droplevels()

    single_anno = single_anno %>% separate(ANN, c(snpeff, 'errors'), "[|]") %>% droplevels()

    # two annotations will be more than 16 elements but less than 45 (which would indicate 3). In total it should be 30 elements
    multi_anno = vcf_dataframe %>%
                    filter(lengths(gregexpr("[|]", vcf_dataframe$ANN)) > 16 &
                          lengths(gregexpr("[|]", vcf_dataframe$ANN)) < 45) %>%
                  droplevels()

    multi_anno = multi_anno %>% separate(ANN, snpeff_multi, "[|]") %>% droplevels()

    multi_anno1 = multi_anno %>% select(!all_of(snpeff2)) # separate by first annotation

    multi_anno2 = multi_anno %>% select(!all_of(snpeff)) # separate by second annotation

    colnames(multi_anno2) = c(colnames(multi_anno1)) # change col names to match

    multi_anno = rbind(multi_anno1, multi_anno2) # concat annotation dfs to make long df

    multi_anno = rbind(single_anno, multi_anno) # concat with single annotations to have all

    # deduplicate just in case
    multi_anno = multi_anno[!duplicated(multi_anno), ] %>% droplevels()

    full_df = full_df[!duplicated(full_df), ] %>% droplevels()

    df_list = list("full_df"=full_df, "ann_df"=multi_anno) # form into list to return

    return(df_list)
}
