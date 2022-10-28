#' shared_snv_plot
#'
#' Reads in a dataframe that has been arranged (arrange_gt_data) and filtered (filter_variants) and outputs plots
#'
#' @name shared_snv_plot
#' @param vardf A rearranged (arrange_gt_data) and filtered (filtered_variants) vcf dataframe
#' @param samples A vector of samples to be compared (default:all samples in DF_filt)
#' @return A plot showing the location of variants and the number of samples that contain each variant
#' @export
#' @examples
#' \dontrun{
#' shared_snv_plot(vardf)
#' }

shared_snv_plot = function(vardf, samples = unique(DF_filt$sample)){

    vardf = dplyr::filter(vardf, sample %in% samples)

    vardf$variant = paste0(vardf$CHROM,"_",vardf$major, vardf$POS, vardf$minor)

    df = dplyr::group_by(vardf, variant) %>% dplyr::mutate(count = 1, totalsamp = sum(count))

    df = df[!duplicated(df), ] %>% droplevels()

    plot = ggplot2::ggplot(df, ggplot2::aes(x = POS, y = CHROM)) +
      ggplot2::geom_point(ggplot2::aes(color = totalsamp)) +
      ggplot2::ggtitle("Number of Samples Containing Each Variant") +
      ggplot2::scale_y_discrete(limits=rev) +
      ggplot2::theme(legend.key = ggplot2::element_blank(),
                     strip.background = ggplot2::element_rect(colour="black", fill="white"),
                     axis.text.x = ggplot2::element_text(angle = 90, vjust = 0.5, hjust=1))

    print(plot)

}

