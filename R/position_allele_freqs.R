# Plot to determine AF of variant of interest across all samples
# User inputs variant dataframe, chromosome, nt position and save directory
# segment and nt need to be in "quotes" when called in the function

variant_allele_freq = function(vardf,segment,nt){
  
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
