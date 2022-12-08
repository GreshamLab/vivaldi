#' shannon_entropy
#'
#' Takes a rearranged vcf dataframe and calculates the Shannon entropy
#'
#' Shannon entropy is a commonly used metric to describe the amount of genetic
#' diversity in sequencing data. It is calculated by considering the frequency
#' of the ALT and REF allele at every position and then summing those values
#' over 1) a segment or 2) the entire genome. These values can then be
#' normalized by sequence length (kb) in order to compare across different
#' segments or samples.
#'
#' @name shannon_entropy
#' @param df A rearranged vcf dataframe (arrange_gt_data)
#' @param genome_size Size of whole genome being used
#' @return A dataframe with Shannon entropy/kb calculations for the chroms and entire genome
#' @export
#' @examples
#' # Sample dataframe
#' df <- data.frame(sample = c("m1", "m2", "m1", "m2", "m1"),
#'                  CHROM = c("PB1", "PB1", "PB2", "PB2", "NP"),
#'                  minorfreq = c(0.010, 0.022, 0.043, 0.055, 0.011),
#'                  majorfreq = c(0.990, 0.978, 0.957, 0.945, 0.989),
#'                  SegmentSize = c(2280, 2280, 2274, 2274, 1809)
#' )
#'
#' df
#'
#' genome_size = 13133
#'
#' # Modifies the dataframe to add five new columns
#' shannon_entropy(df, genome_size)
#'
shannon_entropy = function(df,genome_size){
  ## only uses positions with minorvariants

  df$shannon_ntpos = (-(df$majorfreq)*(log2(df$majorfreq))) + (-(df$minorfreq)*(log2(df$minorfreq)))

  df = df %>% dplyr::group_by(sample, CHROM, SegmentSize) %>% dplyr::mutate(chrom_shannon = sum(shannon_ntpos)) %>% dplyr::ungroup()

  df = df %>% dplyr::group_by(sample) %>% dplyr::mutate(genome_shannon = sum(chrom_shannon)) %>% dplyr::ungroup()

  df$shannon_chrom_perkb = (df$chrom_shannon/(df$SegmentSize/1000))

  df$genome_shannon_perkb = (df$genome_shannon/genome_size/1000)

  return(df)

}
