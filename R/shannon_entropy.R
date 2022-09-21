#' shannon_entropy
#'
#' Takes a rearranged vcf dataframe (arrange_gt_data) and calculates the Shannon entropy
#'
#' @name shannon_entropy
#' @param df A rearranged vcf dataframe (arrange_gt_data)
#' @return A dataframe with Shannon entropy/kb calculations for the chroms and entire genome
#' @export
#' @examples
#' shannon_entropy(df)
shannon_entropy = function(df){
  ## only uses positions with minorvariants
  snpeff = snpeff_info()

  if (length(intersect(colnames(df), snpeff)) > 0){

    df = df %>% select(!all_of(c(snpeff)))

    df = df[!duplicated(df), ] %>% droplevels()

  } else{

    df = df[!duplicated(df), ] %>% droplevels()

  }

  df$shannon_ntpos = (-(df$majorfreq)*(log2(df$majorfreq))) + (-(df$minorfreq)*(log2(df$minorfreq)))

  df = df %>% group_by(sample, CHROM, CHROM_SIZE) %>% mutate(chrom_shannon = sum(shannon_ntpos)) %>% ungroup()

  df = df %>% group_by(sample) %>% mutate(genome_shannon = sum(chrom_shannon)) %>% ungroup()

  df$shannon_chrom_perkb = (df$chrom_shannon/(df$CHROM_SIZE/1000))

  df$genome_shannon_perkb = (df$genome_shannon/(df$GENOME_SIZE/1000))

  return(df)

}
