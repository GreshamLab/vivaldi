# Calculating dN/dS ratio and plotting by genome

dNdSGenome = function(vardf, savedir){
  
  df_dN = filter(vardf, annotation == "missense_variant" | annotation == "nonsense_variant") %>% droplevels() %>% tally()
  df_dS = filter(vardf, annotation == "synonymous_variant") %>% droplevels() %>% tally()
  
  df = merge(df_dN, df_dS,  by = c("sample")) %>% select(sample,n.x,n.y)
  df$dNdS = (df$n.x) / (df$n.y)
  
  plot = ggplot(df, aes(x = sample , y = dNdS)) +
    geom_point() +
    ggtitle("dNdS Ratio Per Sample") +
    theme(legend.key = element_blank(),
          strip.background = element_rect(colour="black", fill="white"),
          axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))
  
  print(plot)
  ggsave(plot, filename = glue("{savedir}/GenomeSNVPlot.pdf"),
         width = 12, height = 13.2, limitsize=FALSE)
}