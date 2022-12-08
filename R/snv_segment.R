#' snv_segment
#'
#' Reads in a dataframe that has been arranged (arrange_gt_data) and filtered (filter_variants) and outputs plots
#'
#' @name snv_segment
#' @param vardf A rearranged (arrange_gt_data) and filtered (filtered_variants) vcf dataframe
#' @return A bar plot showing the number of variants colored by their SNPEff annotation
#' @export
#' @examples
#'
#' df <- data.frame(sample = c("m1", "m1", "m1", "m1", "m1",
#'                             "m2", "m2", "m2", "m2", "m2"),
#'   CHROM = c("PB1", "PB1", "PB2", "PB2", "PB2",
#' 	           "PB1", "PB1", "PB2", "PB2", "PB2"),
#'   annotation = c("downstrean_gene_variant", "synonymous_variant",
#' 		              "synonymous_variant", "stop_gained", "missense_variant",
#' 		          	  "downstrean_gene_variant", "downstrean_gene_variant",
#' 				          "synonymous_variant", "stop_gained", "missense_variant")
#' )
#'
#' df
#'
#' snv_segment(df)
#'
snv_segment = function(vardf){

  sum_df = dplyr::group_by(vardf, sample, CHROM, annotation) %>% dplyr::tally()

  sum_df = sum_df[!duplicated(sum_df), ] %>% droplevels()

  plot = ggplot2::ggplot(sum_df, ggplot2::aes(x = sample , y = n, fill = annotation)) +
    ggplot2::geom_col() +
    ggplot2::facet_grid(~CHROM) +
    ggplot2::ggtitle("Number of Variants per Segment") +
    ggplot2::theme(legend.key = ggplot2::element_blank(),
          strip.background = ggplot2::element_rect(colour="black", fill="white"),
          axis.text.x = ggplot2::element_text(angle = 90, vjust = 0.5, hjust=1)) +
    ggplot2::ylab("Number of variants")

  print(plot)
}
