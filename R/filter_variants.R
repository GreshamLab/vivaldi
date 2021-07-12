#' filter_variants
#'
#' Filters single-nucleotide variants using a coverage and frequency cutoff
#'
#' @name filter_variants
#' @param arranged_df A rearranged vcf dataframe (rearranged using the arrange_gt_data function)
#' @param coverage_cutoff The coverage cutoff for calling a SNV (default: 300x)
#' @param frequency_cutoff Frequency cutoff for calling a SNV (default: 2%)
#' @return A filtered vcf dataframe
#' @export
#' @examples
#' filter_variants(arranged_df, 200, 0.02)
filter_variants = function(arranged_df, coverage_cutoff=300, frequency_cutoff=0.02){

    input_dim = nrow(arranged_df)
    # using ALT_FREQ in case there isn't a 'minor variant' present, it still keeps consensus changes from ref
    arranged_df = arranged_df %>% filter(ALT_FREQ >= frequency_cutoff & gt_DP >= coverage_cutoff)

    arranged_df = arranged_df[!duplicated(arranged_df), ] %>% droplevels()

    message("Total number of SNP filtered out: ", input_dim - nrow(arranged_df))
    
    return(arranged_df)

}
