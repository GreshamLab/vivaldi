# Plot of SNVs per sample across segments
# User inputs variant dataframe and save directory

SegmentSNV = function(vardf,savedir){
  
  sum_df = group_by(vardf, sample, CHROM, annotation) %>% tally()
  
  plot = ggplot(sum_df, aes(x = sample , y = n, fill = annotation)) +
    geom_col() +
    facet_grid(~CHROM) +
    ggtitle("Number of Variants per Segment") +
    theme(legend.key = element_blank(),
          strip.background = element_rect(colour="black", fill="white"),
          axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))
  
  print(plot)
  ggsave(plot, filename = glue("{savedir}/SegmentSNVPlot.pdf"),
         width = 12, height = 13.2, limitsize=FALSE)
  
}