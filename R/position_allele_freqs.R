#' position_allele_freq
#'
#' Reads in a dataframe that has been arranged (arrange_gt_data) and filtered (filter_variants) and outputs plots
#'
#' @name position_allele_freq
#' @param vardf A rearranged (arrange_gt_data) and filtered (filtered_variants) vcf dataframe
#' @param segment Name of segment (must be in quotes)
#' @param nt Position on segment (must be in quotes)
#' @return A plot showing the the frequencies of the major and minor allele at the given position across all samples
#' @export
position_allele_freq = function(vardf,segment,nt){

  vardf = vardf %>% dplyr::filter(CHROM == segment, POS == nt) %>% droplevels()

  vardf = vardf[!duplicated(vardf), ] %>% droplevels()

  plot = ggplot2::ggplot(vardf, ggplot2::aes(x = sample)) +
    ggplot2::geom_point(ggplot2::aes(y = majorfreq, color = major)) +
    ggplot2::geom_point(ggplot2::aes(y = minorfreq, color = minor)) +
    ggplot2::geom_line(ggplot2::aes(group = "allele")) +
    ggplot2::ggtitle(paste0("Allele Frequencies at ",segment," Position ",nt)) +
    ggplot2::ylab("Allele Frequency") +
    ggplot2::scale_color_discrete(name = "Allele") +
    ggplot2::theme(legend.key = ggplot2::element_blank(),
        strip.background = ggplot2::element_rect(colour="black", fill="white"),
        axis.text.x = ggplot2::element_text(angle = 90, vjust = 0.5, hjust=1))

  print(plot)
}
