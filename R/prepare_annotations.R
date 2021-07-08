# vivaldi package
# Kate Johnson

# using SNPeff annotations
prepare_annotations = function(vcf_dataframe){

    # names of elements selected from snpeff website
    snpeff = c('allele','annotation','putative_impact','gene_name','gene_id',
        'feature_type','feature_id','transcript_biotype','rank_total','HGVS.c','HGVS.p',
        'cDNA_position','CDS_position','protein_position','distance_to_feature')

    snpeff2 = c()

    for (i in snpeff){

      snpeff2 = c(snpeff2, glue("{i}2"))

    }

    snpeff_multi = c(snpeff, snpeff2, 'errors')

    snpeff_length = length(snpeff)

    full_df = vcf_dataframe %>%
                    filter(lengths(gregexpr("[|]", vcf_dataframe$ANN)) < snpeff_length*3) %>%
                  droplevels()

    # Labeling for future filtering/figures
    full_df = full_df %>% mutate(ann_number = ifelse(lengths(gregexpr("[|]", vcf_dataframe$ANN)) > snpeff_length + 1, "two", "one"))

    full_df = full_df %>% separate(ANN, snpeff_multi, "[|]") %>% droplevels()

    too_many_ann = vcf_dataframe %>%
                    filter(lengths(gregexpr("[|]", vcf_dataframe$ANN)) > snpeff_length*2) %>%
                  droplevels()

    message(">2 annotations: ", list(levels(factor(too_many_ann$sample))))

    # building just an annotation df
    # 16 different features provided by snpeff see website for more info
    single_anno = vcf_dataframe %>%
                    filter(lengths(gregexpr("[|]", vcf_dataframe$ANN)) <= snpeff_length + 1) %>%
                  droplevels()

    single_anno = single_anno %>% separate(ANN, c(snpeff, 'errors'), "[|]") %>% droplevels()

    # two annotations will be more than 16 elements but less than 45 (which would indicate 3). In total it should be 30 elements
    multi_anno = vcf_dataframe %>%
                    filter(lengths(gregexpr("[|]", vcf_dataframe$ANN)) > snpeff_length + 1 &
                          lengths(gregexpr("[|]", vcf_dataframe$ANN)) < snpeff_length*3) %>%
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
