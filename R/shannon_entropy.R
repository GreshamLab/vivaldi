# vivaldi package
# Kate Johnson

shannon_entropy = function(df, genome_size){
  ## only uses positions with minorvariants

  df$shannon_ntpos = (-(df$majorfreq)*(log2(df$majorfreq))) + (-(df$minorfreq)*(log2(df$minorfreq)))

  df = df %>% group_by(sample, CHROM) %>% mutate(chrom_shannon = sum(shannon_ntpos))

  df = df %>% group_by(sample) %>% mutate(genome_shannon = sum(chrom_shannon))

  df$shannon_chrom_perkb = (df$chrom_shannon/(df$chrom_size/1000))

  df$genome_shannon_perkb = (df$genome_shannon/(genome_size/1000))

  return(df)
}
