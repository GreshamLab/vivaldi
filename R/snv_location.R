#' snv_location
#'
#' Reads in the vcf dataframe and generates a plot showing the frequency and location of SNVs
#'
#' @name snv_location
#' @param df A rearranged dataframe
#' @return A plot showing the location and frequency of SNVs found across samples
#' @export
#' @examples
#' # Example 1:
#' df <- data.frame(sample = c("m1", "m1", "m1", "m1", "m1",
#'                             "m2", "m2", "m2", "m2", "m2"),
#'                  CHROM = c("PB1", "PB1", "PB2", "PB2", "PB2",
#' 				                    "PB1", "PB1", "PB2", "PB2", "PB2"),
#'                  POS = c(234, 266, 117, 134, 180,
#' 				                  234, 266, 199, 88, 180),
#'                  major = c("G", "G", "A", "A", "C",
#' 				                    "G", "G", "A", "G", "C"),
#' 			            minor = c("A", "A", "G", "G", "T",
#' 				                    "A", "A", "G", "A", "T"),
#' 				          ALT_TYPE = c("minor", "minor", "minor", "minor", "minor",
#' 				                       "minor", "minor", "minor", "major", "minor"),
#' 				          minorfreq = c(0.010, 0.022, 0.043, 0.055, 0.011,
#' 				                        0.010, 0.022, 0.043, 0.055, 0.011),
#' 				          majorfreq = c(0.990, 0.978, 0.957, 0.945, 0.989,
#' 				                        0.990, 0.978, 0.957, 0.945, 0.989)
#' )
#'
#' df
#'
#' snv_location(df)
#'
#' # Example 2:
#' snv_location(example_filtered_SNV_df)
#'
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
