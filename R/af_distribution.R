#' af_distribution
#'
#' Plots a distribution of all minor variants
#'
#' @name af_distribution
#' @param vardir Directory path containing vcf files
#' @return A plot with the distribution of all minor variants
#' @export
#' @examples
#' af_distribution(vardir)
af_distribution = function(vardir){

  plot = ggplot2::ggplot(vardir, ggplot2::aes(x = minorfreq)) +
    ggplot2::geom_histogram(binwidth = 0.01) +
    ggplot2::ggtitle("Distrubution of minor allele frequencies") +
    ggplot2::theme(legend.key = ggplot2::element_blank(),
          strip.background = ggplot2::element_rect(colour="black", fill="white"),
          axis.text.x = ggplot2::element_text(angle = 90, vjust = 0.5, hjust=1))
  print(plot)
}
