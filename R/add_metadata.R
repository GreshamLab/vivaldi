# vivaldi package
# Kate Johnson

# add meta information including size of segment/chromosomes
add_metadata = function(vcf_df, metadf, by_vcf, by_meta){

    return(merge(vcf_df, metadf, by.x=all_of(by_vcf), by.y=all_of(by_meta), all.x = TRUE))

}
