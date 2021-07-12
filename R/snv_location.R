#' snv_location
#'
#' Reads in the vcf dataframe and generates a plot showing the frequency and location of SNVs
#'
#' @name snv_location
#' @param vcf_df A rearranged (arrange_gt_data)
#' @return A plot showing the location and frequency of SNVs found across samples
#' @export
#' @examples
#' snv_location(vcf_df)
snv_location = function(vcf_df){

    xlim = length(levels(factor(vcf_df$CHROM)))

    ylim = length(levels(factor(vcf_df$sample)))

    plot4 = ggplot() +
        geom_point(data = vcf_df %>% filter(ALT_TYPE == 'minor') %>% droplevels(),
                      aes(x=POS, y=minorfreq, color=minor)) +

        geom_point(data = vcf_df %>% filter(ALT_TYPE == 'major') %>% droplevels(),
                      aes(x=POS, y=majorfreq, color=major)) +

        geom_rug(data = vcf_df %>% filter(ALT_TYPE == 'minor') %>% droplevels(),
                      aes(x=POS, y=minorfreq, color=minor)) +

        geom_rug(data = vcf_df %>% filter(ALT_TYPE == 'minor') %>% droplevels(),
                      aes(x=POS, y=minorfreq, color=minor)) +

        theme_bw() +

        theme(legend.key = element_blank(),
                        strip.background = element_rect(colour="black", fill="white"),
                        axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1)) +
        facet_grid(sample~CHROM,  scales = 'free_x')

    print(plot4)

}
