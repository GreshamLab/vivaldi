
#' snv_genome
#'
#' Reads in a dataframe that has been arranged (arrange_gt_data) and filtered (filter_variants) and outputs plots
#'
#' @name snv_genome
#' @param vardf A rearranged (arrange_gt_data) and filtered (filtered_variants) vcf dataframe 
#' @return A bar plot showing the number of variants per sample colored by their SNPEff annotation
#' @export
#' @examples
#' snv_genome(vardf)

snv_genome = function(vardf){
  
  sum_df = group_by(vardf, sample,annotation) %>% tally()
  
  plot = ggplot(sum_df, aes(x = sample , y = n, fill = annotation)) +
    geom_col() +
    ggtitle("Number of Variants per Genome") +
    theme(legend.key = element_blank(),
          strip.background = element_rect(colour="black", fill="white"),
          axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))
  
  print(plot)

}
