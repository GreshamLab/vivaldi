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

  plot = ggplot(vardir, aes(x = minorfreq)) +
    geom_histogram(binwidth = 0.01) +
    ggtitle("Distrubution of minor allele frequencies") +
    theme(legend.key = element_blank(),
          strip.background = element_rect(colour="black", fill="white"),
          axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))
  print(plot)
}
