#' snv_location
#'
#' Reads in the vcf dataframe and generates a plot showing the frequency and location of SNVs
#'
#' @name snv_location
#' @param df A rearranged (arrange_gt_data)
#' @return A plot showing the location and frequency of SNVs found across samples
#' @export
#' @examples
#' \dontrun{
#' snv_location(df)
#' }
snv_location = function(df){


    plot4 = ggplot2::ggplot() +
      ggplot2::geom_point(data = df %>% dplyr::filter(ALT_TYPE == 'minor') %>% droplevels(),
                          ggplot2::aes(x=POS, y=minorfreq, color=minor)) +

      ggplot2::geom_point(data = df %>% dplyr::filter(ALT_TYPE == 'major') %>% droplevels(),
                          ggplot2::aes(x=POS, y=majorfreq, color=major)) +

      ggplot2::geom_rug(data = df %>% dplyr::filter(ALT_TYPE == 'minor') %>% droplevels(),
                        ggplot2::aes(x=POS, y=minorfreq, color=minor)) +

      ggplot2::geom_rug(data = df %>% dplyr::filter(ALT_TYPE == 'minor') %>% droplevels(),
                        ggplot2::aes(x=POS, y=minorfreq, color=minor)) +

      ggplot2::theme_bw() +

      ggplot2::theme(legend.key = ggplot2::element_blank(),
                        strip.background = ggplot2::element_rect(colour="black", fill="white"),
                        axis.text.x = ggplot2::element_text(angle = 90, vjust = 0.5, hjust=1)) +
      ggplot2::facet_grid(sample~CHROM,  scales = 'free_x') +

      ggplot2::xlab("Nucleotide Position")

    return(plotly::ggplotly(plot4))

}
