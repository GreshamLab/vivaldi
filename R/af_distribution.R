#' af_distribution
#'
#' Plots distribution of all minor variants
#'
#' @name af_distribution
#' @param df A dataframe that has been arranged (arrange_data) and filtered (filter_variants)
#' @return plots with the distribution of all minor variants
#' @export
#' @examples
#' # Example 1:
#' df <- data.frame(sample = c("m1", "m2", "m1", "m2", "m1"),
#'                  CHROM = c("PB1", "PB1", "PB2", "PB2", "NP"),
#'                  POS = c(234, 234, 240, 240, 254),
#'                  REF = c("G", "G", "A", "A", "C"),
#'                  ALT = c("A", "A", "G", "G", "T"),
#'                  minorfreq = c(0.010, 0.022, 0.043, 0.055, 0.011),
#'                  majorfreq = c(0.990, 0.978, 0.957, 0.945, 0.989),
#'                  minorcount = c(7, 15, 26, 32, 7),
#'                  majorcount = c(709, 661, 574, 547, 610),
#'                  gt_DP = c(716, 676, 600, 579, 617)
#' )
#'
#' af_distribution(df)
#'
#' # Example 2:
#' af_distribution(example_filtered_SNV_df)
#'
af_distribution = function(df){

  p1 = ggplot2::ggplot(df, ggplot2::aes(x = minorfreq)) +
    ggplot2::geom_histogram(binwidth = 0.01) +
    ggplot2::ggtitle("Distrubution of minor allele frequencies") +
    ggplot2::theme(legend.key = ggplot2::element_blank(),
          strip.background = ggplot2::element_rect(colour="black", fill="white"),
          axis.text.x = ggplot2::element_text(angle = 90, vjust = 0.5, hjust=1))
  print(p1)

  p2 = ggplot2::ggplot(df, ggplot2::aes(x = minorfreq)) +
    ggplot2::geom_histogram(binwidth = 0.01) +
    ggplot2::facet_grid(~sample) +
    ggplot2::ggtitle("Distrubution of minor allele frequencies") +
    ggplot2::theme(legend.key = ggplot2::element_blank(),
                   strip.background = ggplot2::element_rect(colour="black", fill="white"),
                   axis.text.x = ggplot2::element_text(angle = 90, vjust = 0.5, hjust=1))
  print(p2)
}
