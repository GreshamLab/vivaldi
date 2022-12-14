#' plot_shannon
#'
#' Reads in a dataframe that has been arranged (arrange_data), filtered (filter_variants), and piped through the Shannon calculations (shannon_entropy) and outputs plots
#'
#' The `plot_shannon()` function takes the variant dataframe and generates three plots.
#' 1. The Shannon entropy, or amount of diversity, at each position in the genome at which a variant was found.
#' 2. The Shannon entropy summed over each segment
#' 3. The Shannon entropy summed over each genome
#' A higher value indicates more diversity.
#'
#' @name plot_shannon
#' @param shannon_df A dataframe that has been arranged (arrange_data), filtered (filter_variants), and piped through the Shannon calculations (shannon_entropy)
#' @return Three plots showing the nt Shannon, chrom Shannon, and full genome Shannon calculations
#' @export
#' @examples
#' # Sample dataframe
#' df <- data.frame(sample = c("m1", "m2", "m1", "m2", "m1"),
#'                  CHROM = c("PB1", "PB1", "PB2", "PB2", "NP"),
#'                  POS = c(234, 234, 240, 240, 254),
#'                  minorfreq = c(0.010, 0.022, 0.043, 0.055, 0.011),
#'                  majorfreq = c(0.990, 0.978, 0.957, 0.945, 0.989),
#'                  SegmentSize = c(2280, 2280, 2274, 2274, 1809)
#' )
#'
#' df
#'
#' genome_size = 13133
#'
#' # Modify the dataframe to add 5 new columns of shannon entropy data:
#' # 1. shannon_ntpos
#' # 2. chrom_shannon
#' # 3. genome_shannon
#' # 4. shannon_chrom_perkb
#' # 5. genome_shannon_perkb
#' shannon_df = shannon_entropy(df, genome_size)
#'
#' # Plot
#' plot_shannon(shannon_df)
#'
plot_shannon = function(shannon_df){

    p1 = ggplot2::ggplot(shannon_df, ggplot2::aes(x=POS, y=shannon_ntpos)) +
      ggplot2::geom_point(alpha=0.6) +
      ggplot2::theme_bw() +
      ggplot2::theme(legend.key = ggplot2::element_blank(),
                        strip.background = ggplot2::element_rect(colour="black", fill="white"),
                        axis.text.x = ggplot2::element_text(angle = 90, vjust = 0.5, hjust=1)) +
      ggplot2::facet_grid(.~CHROM,  scales = 'free_x')

    print(p1)

    temp = shannon_df %>% dplyr::select(CHROM, sample, shannon_chrom_perkb, genome_shannon_perkb)

    temp = temp[!duplicated(temp), ] %>% droplevels()

    p2 = ggplot2::ggplot(temp, ggplot2::aes(x=sample, y=shannon_chrom_perkb)) +
      ggplot2::geom_point() +
      ggplot2::theme_bw() +
      ggplot2::theme(legend.key = ggplot2::element_blank(),
                        strip.background = ggplot2::element_rect(colour="black", fill="white"),
                        axis.text.x = ggplot2::element_text(angle = 90, vjust = 0.5, hjust=1)) +
      ggplot2::facet_grid(.~CHROM)

    print(p2)

    temp = temp %>% dplyr::select(-shannon_chrom_perkb, -CHROM)

    temp = temp[!duplicated(temp), ] %>% droplevels()

    p3 = ggplot2::ggplot(temp, ggplot2::aes(x=sample, y=genome_shannon_perkb)) +
      ggplot2::geom_point() +
      ggplot2::theme_bw() +
      ggplot2::theme(legend.key = ggplot2::element_blank(),
                        strip.background = ggplot2::element_rect(colour="black", fill="white"),
                        axis.text.x = ggplot2::element_text(angle = 90, vjust = 0.5, hjust=1))

    print(p3)

}
