#' position_allele_freq
#'
#' Reads in a dataframe that has been arranged (arrange_gt_data) and filtered (filter_variants) and outputs plots
#'
#' @name position_allele_freq
#' @param vardf A rearranged (arrange_gt_data) and filtered (filtered_variants) vcf dataframe 
#' @param segment Name of segment (must be in quotes)
#' @param nt Position on segment (must be in quotes)
#' @return A plot showing the the frequencies of the major and minor allele at the given position across all samples
#' @export
#' @examples
#' position_allele_freqs(vardf,"H1N1_HA","439")

position_allele_freq = function(vardf,segment,nt){
  
  vardf = vardf %>% filter(CHROM == segment, POS == nt) %>% droplevels()
  
  plot = ggplot(vardf, aes(x = sample)) +
    geom_point(aes(y = majorfreq, color = major)) +
    geom_point(aes(y = minorfreq, color = minor)) +
    ggtitle(paste0("Allele Frequencies at ",segment," Position ",nt)) +
    ylab("Allele Frequency") +
    scale_color_discrete(name = "Allele") +
    theme(legend.key = element_blank(),
        strip.background = element_rect(colour="black", fill="white"),
        axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))
  
  print(plot)
}
