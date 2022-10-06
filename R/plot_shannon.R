#' plot_shannon
#'
#' Reads in a dataframe that has been arranged (arrange_gt_data), filtered (filter_variants), and piped through the Shannon calculations (shannon_entropy) and outputs plots
#'
#' @name plot_shannon
#' @param shannon_df A dataframe that has been arranged (arrange_gt_data), filtered (filter_variants), and piped through the Shannon calculations (shannon_entropy)
#' @return Three plots showing the nt Shannon, chrom Shannon, and full genome Shannon calculations
#' @export
#' @examples
#' \dontrun{
#' plot_shannon(shannon_df)
#' }
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
