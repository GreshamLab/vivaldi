#' dNdS_segment
#'
#' Reads in a dataframe that has been arranged (arrange_gt_data), filtered (filter_variants), and annotated (prepare_annotations), calculates dNdS, and outputs plots
#'
#' @name dNdS_segment
#' @param annotation_df A rearranged, filtered, and annotated vcf dataframe - must be for amino-acid specific calculations, cannot be the same as the dataframe used for SNP calculations
#' @return A plot showing the dN/dS ratio for each splice form (rather than segment) for each sample
#' @export
#' @examples
#' \dontrun{
#' dNdS_segment(annotation_df)
#' }

dNdS_segment = function(annotation_df){

  annotation_df$feature_id = factor(annotation_df$feature_id, levels = SPLICEFORMS)

  df_dN = dplyr::filter(annotation_df, annotation == "missense_variant" | annotation == "nonsense_variant") %>%
    dplyr::group_by(sample, feature_id) %>% droplevels() %>% dplyr::tally()
  df_dS = dplyr::filter(annotation_df, annotation == "synonymous_variant") %>%
    dplyr::group_by(sample, feature_id) %>% droplevels() %>% dplyr::tally()

  df = dplyr::merge(df_dN, df_dS,  by = c("sample","feature_id")) %>% dplyr::select(sample,feature_id,n.x,n.y)
  df$dNdS = (df$n.x) / (df$n.y)

  plot = dplyr::ggplot(df,dplyr:: aes(x = sample , y = dNdS)) +
    dplyr::geom_point() +
    dplyr::facet_grid(~feature_id) +
    dplyr::ggtitle("dNdS Ratio Per Splice Form") +
    dplyr::theme(legend.key = element_blank(),
          strip.background = element_rect(colour="black", fill="white"),
          axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))

  dplyr::print(plot)
}
