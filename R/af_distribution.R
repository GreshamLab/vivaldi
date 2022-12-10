#' af_distribution
#'
#' Plots a distribution of all minor variants
#'
#' @name af_distribution
#' @param df A dataframe that has been arranged (arrange_gt_data) and filtered (filter_variants)
#' @return A plot with the distribution of all minor variants
#' @export
#' @examples
#' df <- data.frame(
#'   sample = c( "sample1", "sample1", "sample1", "sample1", "sample1",
#'               "sample1", "sample1", "sample1", "sample2", "sample2",
#'               "sample2", "sample2", "sample2", "sample2"),
#'   minorfreq = c(0.04422785, 0.03738175, 0.01390202, 0.02927786, 0.02199074,
#'                 0.03071955, 0.02626025, 0.02875321, 0.02249272, 0.01674657,
#'                 0.02915796, 0.07306280, 0.04143398, 0.02571210)
#' )
#'
#' af_distribution(df)
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
