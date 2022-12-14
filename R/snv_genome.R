#' snv_genome
#'
#' Reads in a dataframe that has been arranged (arrange_data) and filtered (filter_variants) and outputs plots
#'
#' @name snv_genome
#' @param vardf A rearranged (arrange_data) and filtered (filtered_variants) vcf dataframe
#' @return A bar plot showing the number of variants per sample colored by their SNPEff annotation
#' @export
#' @examples
#' # Example 1: Simple dataframe
#' df <- data.frame(sample = c("m1", "m1", "m1", "m1", "m1",
#'                             "m2", "m2", "m2", "m2", "m2"),
#' 			    annotation = c("downstrean_gene_variant", "synonymous_variant",
#' 				                 "synonymous_variant", "stop_gained",
#' 				                 "missense_variant", "downstrean_gene_variant",
#' 				                 "downstrean_gene_variant", "synonymous_variant",
#' 				                 "stop_gained", "missense_variant")
#' )
#'
#' df
#'
#' snv_genome(df)
#'
#' # Example 2: Sample dataframe
#' snv_genome(example_filtered_SNV_df)
#'
snv_genome = function(vardf){

  sum_df = dplyr::group_by(vardf, sample,annotation) %>% dplyr::tally()

  sum_df = sum_df[!duplicated(sum_df), ] %>% droplevels()

  plot = ggplot2::ggplot(sum_df, ggplot2::aes(x = sample , y = n, fill = annotation)) +
    ggplot2::geom_col() +
    ggplot2::ggtitle("Number of Variants per Genome") +
    ggplot2::theme(legend.key = ggplot2::element_blank(),
          strip.background = ggplot2::element_rect(colour="black", fill="white"),
          axis.text.x = ggplot2::element_text(angle = 90, vjust = 0.5, hjust=1)) +
    ggplot2::ylab("Number of variants")

  print(plot)

}
