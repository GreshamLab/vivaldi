#' shannon_entropy
#'
#' Takes a rearranged vcf dataframe (arrange_gt_data) and calculates the Shannon entropy
#'
#' @name shannon_entropy
#' @param df A rearranged vcf dataframe (arrange_gt_data)
#' @param genome_size Size of whole genome being used
#' @return A dataframe with Shannon entropy/kb calculations for the chroms and entire genome
#' @export
shannon_entropy = function(df,genome_size){
  ## only uses positions with minorvariants

  df$shannon_ntpos = (-(df$majorfreq)*(log2(df$majorfreq))) + (-(df$minorfreq)*(log2(df$minorfreq)))

  df = df %>% dplyr::group_by(sample, CHROM, SegmentSize) %>% dplyr::mutate(chrom_shannon = sum(shannon_ntpos)) %>% dplyr::ungroup()

  df = df %>% dplyr::group_by(sample) %>% dplyr::mutate(genome_shannon = sum(chrom_shannon)) %>% dplyr::ungroup()

  df$shannon_chrom_perkb = (df$chrom_shannon/(df$SegmentSize/1000))

  df$genome_shannon_perkb = (df$genome_shannon/genome_size/1000)

  return(df)

}
