#' shannon_entropy
#'
#' Takes a rearranged vcf dataframe (arrange_gt_data) and calculates the Shannon entropy
#'
#' @name shannon_entropy
#' @param df A rearranged vcf dataframe (arrange_gt_data)
#' @param genome_size The full length of the genome
#' @return A dataframe with Shannon entropy/kb calculations for the chroms and entire genome
#' @export
#' @examples
#' shannon_entropy(df, 13000)
shannon_entropy = function(df, genome_size){
  ## only uses positions with minorvariants

  df$shannon_ntpos = (-(df$majorfreq)*(log2(df$majorfreq))) + (-(df$minorfreq)*(log2(df$minorfreq)))

  df = df %>% group_by(sample, CHROM) %>% mutate(chrom_shannon = sum(shannon_ntpos))

  df = df %>% group_by(sample) %>% mutate(genome_shannon = sum(chrom_shannon))

  df$shannon_chrom_perkb = (df$chrom_shannon/(df$chrom_size/1000))

  df$genome_shannon_perkb = (df$genome_shannon/(genome_size/1000))

  return(df)
}
