# Plot to determine how many samples contain a variant
# User inputs variant dataframe, and save directory

SharedSNVs = function(vardf,savedir){
  
  vardf$variant = paste0(vardf$CHROM,"_",vardf$minor, vardf$POS, vardf$REF)
  
  df = group_by(vardf, variant) %>% mutate(count = 1, totalsamp = sum(count))
  
  plot = ggplot(df, aes(x = POS, y = CHROM)) +
    geom_point(aes(size = totalsamp)) +
    ggtitle("Number of Samples Containing Each Variant") +
    theme(legend.key = element_blank(),
          strip.background = element_rect(colour="black", fill="white"),
          axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))
  
  print(plot)
  ggsave(plot, filename = glue("{savedir}/SegmentSNVPlot.pdf"),
                width = 12, height = 13.2, limitsize=FALSE)
}