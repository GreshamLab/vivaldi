
#' shared_snv_plot
#'
#' Reads in a dataframe that has been arranged (arrange_gt_data) and filtered (filter_variants) and outputs plots
#'
#' @name snv_segment
#' @param vardf A rearranged (arrange_gt_data) and filtered (filtered_variants) vcf dataframe 
#' @return A plot showing the location of variants and the number of samples that conatain each variant
#' @export
#' @examples
#' shared_snv_plot(vardf)

shared_snv_plot = function(vardf){
  
  vardf$variant = paste0(vardf$CHROM,"_",vardf$major, vardf$POS, vardf$minor)
  
  df = group_by(vardf, variant) %>% mutate(count = 1, totalsamp = sum(count))
  
  plot = ggplot(df, aes(x = POS, y = CHROM)) +
    geom_point(aes(size = totalsamp)) +
    ggtitle("Number of Samples Containing Each Variant") +
    scale_y_discrete(limits=rev) +
    theme(legend.key = element_blank(),
          strip.background = element_rect(colour="black", fill="white"),
          axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))
  
  print(plot)
}
