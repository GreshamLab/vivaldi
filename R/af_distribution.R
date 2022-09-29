#' af_distribution
#'
#' Plots a distribution of all minor variants
#'
#' @name af_distribution
#' @param df A dataframe that has been arranged (arrange_gt_data) and filtered (filter_variants)
#' @return A plot with the distribution of all minor variants
#' @export
#' @examples
#' \dontrun{
#' af_distribution(df)
#' }
af_distribution = function(df){

  plot = ggplot2::ggplot(df, ggplot2::aes(x = minorfreq)) +
    ggplot2::geom_histogram(binwidth = 0.01) +
    ggplot2::ggtitle("Distrubution of minor allele frequencies") +
    ggplot2::theme(legend.key = ggplot2::element_blank(),
          strip.background = ggplot2::element_rect(colour="black", fill="white"),
          axis.text.x = ggplot2::element_text(angle = 90, vjust = 0.5, hjust=1))
  print(plot)
}
