#' snv_segment
#'
#' Reads in a dataframe that has been arranged (arrange_gt_data) and filtered (filter_variants) and outputs plots
#'
#' @name snv_segment
#' @param vardf A rearranged (arrange_gt_data) and filtered (filtered_variants) vcf dataframe
#' @return A bar plot showing the number of variants colored by their SNPEff annotation
#' @export
#' @examples
#' \dontrun{
#' snv_segment(vardf)
#' }

snv_segment = function(vardf){

  sum_df = group_by(vardf, sample, CHROM, annotation) %>% tally()

  sum_df = sum_df[!duplicated(sum_df), ] %>% droplevels()

  plot = ggplot2::ggplot(sum_df, ggplot2::aes(x = sample , y = n, fill = annotation)) +
    ggplot2::geom_col() +
    ggplot2::facet_grid(~CHROM) +
    ggplot2::ggtitle("Number of Variants per Segment") +
    ggplot2::theme(legend.key = ggplot2::element_blank(),
          strip.background = ggplot2::element_rect(colour="black", fill="white"),
          axis.text.x = ggplot2::element_text(angle = 90, vjust = 0.5, hjust=1))

  print(plot)
}
