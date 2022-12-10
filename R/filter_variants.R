#' filter_variants
#'
#' Filters single-nucleotide variants using a coverage and frequency cutoff
#'
#' @name filter_variants
#' @param df A rearranged VCF dataframe (rearranged using the arrange_gt_data function)
#' @param coverage_cutoff The coverage cutoff for calling a SNV (default: 200x)
#' @param frequency_cutoff Frequency cutoff for calling a SNV (default: 3\%)
#' @return A filtered VCF dataframe
#' @export
#' @examples
#' df <- data.frame(CHROM = c("A", "B", "C"),
#'                 POS = c(234, 240, 255),
#'                 ALT_FREQ = c(0.016, 0.049, 0.031),
#'                 gt_DP = c(716, 600, 187)
#' )
#'
#' df
#'
#' # Default: filter by 3% frequency threshold and 200 coverage cutoff
#' filter_variants(df)
#'
#' # Example 1: A 1% allele frequency threshold and 200 coverage cutoff
#' filter_variants(df, coverage_cutoff = 200, frequency_cutoff = 0.01)
#'
#' # Example 2: A 2% allele frequency threshold and 100 coverage cutoff
#' filter_variants(df, coverage_cutoff = 100, frequency_cutoff = 0.02)
#'
#'
filter_variants = function(df, coverage_cutoff=200, frequency_cutoff=0.03){

    input_dim = nrow(df)
    # using ALT_FREQ in case there isn't a 'minor variant' present, it still keeps consensus changes from ref
    # loop assures this function works on data with and without replicates
    if ("ALT_FREQ" %in% names(df)){

      df = df %>% dplyr::filter(ALT_FREQ >= frequency_cutoff &
                                  gt_DP >= coverage_cutoff)

    }else{

      df = df %>% dplyr::filter(ALT_FREQ.x >= frequency_cutoff & ALT_FREQ.y >= frequency_cutoff &
                                  gt_DP.x >= coverage_cutoff & gt_DP.y >= coverage_cutoff)

    }


    df = df[!duplicated(df), ] %>% droplevels()

    message("Total number of SNP filtered out: ", input_dim - nrow(df))

    return(df)

}
