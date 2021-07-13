#' plot_shannon
#'
#' Reads in a dataframe that has been arranged (arrange_gt_data), filtered (filter_variants), and piped through the Shannon calculations (shannon_entropy) and outputs plots
#'
#' @name plot_shannon
#' @param shannon_df A dataframe that has been arranged (arrange_gt_data), filtered (filter_variants), and piped through the Shannon calculations (shannon_entropy)
#' @return Three plots showing the nt Shannon, chrom Shannon, and full genome Shannon calculations
#' @export
#' @examples
#' plot_shannon(shannon_df)
plot_shannon = function(shannon_df){

    p1 = ggplot(shannon_df, aes(x=POS, y=shannon_ntpos)) +
            geom_point(alpha=0.6) +
            theme_bw() +
            theme(legend.key = element_blank(),
                        strip.background = element_rect(colour="black", fill="white"),
                        axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1)) +
        facet_grid(.~CHROM,  scales = 'free_x')

    print(p1)

    temp = shannon_df %>% select(CHROM, sample, shannon_chrom_perkb, genome_shannon_perkb)

    temp = temp[!duplicated(temp), ] %>% droplevels()

    p2 = ggplot(temp, aes(x=sample, y=shannon_chrom_perkb)) +
            geom_point() +
            theme_bw() +
            theme(legend.key = element_blank(),
                        strip.background = element_rect(colour="black", fill="white"),
                        axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1)) +
        facet_grid(.~CHROM)

    print(p2)

    temp = temp %>% select(-shannon_chrom_perkb, -CHROM)

    temp = temp[!duplicated(temp), ] %>% droplevels()

    p3 = ggplot(temp, aes(x=sample, y=genome_shannon_perkb)) +
            geom_point() +
            theme_bw() +
            theme(legend.key = element_blank(),
                        strip.background = element_rect(colour="black", fill="white"),
                        axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))

    print(p3)

}
