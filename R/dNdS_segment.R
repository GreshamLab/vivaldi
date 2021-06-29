# Calculating dN/dS ratio and plotting by segment

dNdS_segment = function(annotation_df){
  
  annotation_df$feature_id = factor(annotation_df$feature_id, levels = SPLICEFORMS)
  
  df_dN = filter(annotation_df, annotation == "missense_variant" | annotation == "nonsense_variant") %>% 
    group_by(sample, feature_id) %>% droplevels() %>% tally()
  df_dS = filter(annotation_df, annotation == "synonymous_variant") %>% 
    group_by(sample, feature_id) %>% droplevels() %>% tally()
  
  df = merge(df_dN, df_dS,  by = c("sample","feature_id")) %>% select(sample,feature_id,n.x,n.y)
  df$dNdS = (df$n.x) / (df$n.y)
  
  plot = ggplot(df, aes(x = sample , y = dNdS)) +
    geom_point() +
    facet_grid(~feature_id) +
    ggtitle("dNdS Ratio Per Splice Form") +
    theme(legend.key = element_blank(),
          strip.background = element_rect(colour="black", fill="white"),
          axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))
  
  print(plot)
}
