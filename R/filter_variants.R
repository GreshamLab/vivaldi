#' filter_variants
#'
#' Filters single-nucleotide variants using a coverage and frequency cutoff
#'
#' @name filter_variants
#' @param df A rearranged VCF dataframe (rearranged using the arrange_gt_data function)
#' @param coverage_cutoff The coverage cutoff for calling a SNV (default: 300x)
#' @param frequency_cutoff Frequency cutoff for calling a SNV (default: 2%)
#' @return A filtered VCF dataframe
#' @export
#' @examples
#' filter_variants(df, coverage_cutoff = 200, frequency_cutoff = 0.02)
filter_variants = function(df, coverage_cutoff=300, frequency_cutoff=0.02){

    input_dim = nrow(df)
    # using ALT_FREQ in case there isn't a 'minor variant' present, it still keeps consensus changes from ref
    df = df %>% filter(ALT_FREQ >= frequency_cutoff & gt_DP >= coverage_cutoff)

    df = df[!duplicated(df), ] %>% droplevels()

    message("Total number of SNP filtered out: ", input_dim - nrow(df))

    return(df)

}
