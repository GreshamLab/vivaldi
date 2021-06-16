# Plot of SNVs per sample across genome
# User inputs variant dataframe and save directory

GenomeSNV = function(vardf, savedir){
  
  sum_df = group_by(vardf, sample,annotation) %>% tally()
  
  plot = ggplot(sum_df, aes(x = sample , y = n, fill = annotation)) +
    geom_col() +
    ggtitle("Number of Variants per Genome") +
    theme(legend.key = element_blank(),
          strip.background = element_rect(colour="black", fill="white"),
          axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))
  
  print(plot)
  ggsave(plot, filename = glue("{savedir}/GenomeSNVPlot.pdf"),
         width = 12, height = 13.2, limitsize=FALSE)
}