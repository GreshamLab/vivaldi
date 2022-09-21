#' tstv_plot
#'
#' Plots Ts/Tv ratios
#'
#' @name tstv_plot
#' @param df TsTv dataframe generated using the tstv_ratio function
#' @return two plots showing the K2P and simple Ts/Tv ratios
#' @export
#' @examples
#' tstv_plot(tstv_df)
tstv_plot = function(df){

    p1 = df %>% filter(chrom_or_genome == 'tstv_genome_count') %>% droplevels() %>%
          ggplot2::ggplot(., ggplot2::aes(x=sample, y=tstv_ratio)) +
      ggplot2::geom_point() +
      ggplot2::theme_bw() +
      ggplot2::ggtitle("Genome Ts/Tv") +
      ggplot2::ylab("Genome Transition/Transversion ratio") +
      ggplot2::theme(legend.key = ggplot2::element_blank(),
            strip.background = ggplot2::element_rect(colour="black", fill="white"),
            axis.text.x = ggplot2::element_text(angle = 90, vjust = 0.5, hjust=1))

    print(p1)

    p2 = df %>% filter(chrom_or_genome == 'tstv_genome_count') %>% droplevels() %>%
      ggplot2::ggplot(., ggplot2::aes(x=sample, y=tstv_ratio_perkb)) +
      ggplot2::geom_point() +
      ggplot2::theme_bw() +
      ggplot2::ggtitle("Genome Ts/Tv per kb") +
      ggplot2::ylab("Transition/Transversion ratio per genome kb") +
      ggplot2::theme(legend.key = ggplot2::element_blank(),
            strip.background = ggplot2::element_rect(colour="black", fill="white"),
            axis.text.x = ggplot2::element_text(angle = 90, vjust = 0.5, hjust=1))

    print(p2)

    p3 = df %>% filter(chrom_or_genome == 'tstv_chrom_count') %>% droplevels() %>%
      ggplot2::ggplot(., ggplot2::aes(x=sample, y=tstv_ratio)) +
      ggplot2::geom_point() +
      ggplot2::theme_bw() +
      ggplot2::ggtitle("Chrom Ts/Tv") +
      ggplot2::ylab("Chrom Transition/Transversion ratio") +
      ggplot2::theme(legend.key = ggplot2::element_blank(),
            strip.background = ggplot2::element_rect(colour="black", fill="white"),
            axis.text.x = ggplot2::element_text(angle = 90, vjust = 0.5, hjust=1)) +
      ggplot2::facet_grid(.~CHROM)

    print(p3)

    p4 = df %>% filter(chrom_or_genome == 'tstv_chrom_count') %>% droplevels() %>%
      ggplot2::ggplot(., ggplot2::aes(x=sample, y=tstv_ratio_perkb)) +
      ggplot2::geom_point() +
      ggplot2::theme_bw() +
      ggplot2::ggtitle("Chrom Ts/Tv per kb") +
      ggplot2::ylab("Transition/Transversion ratio per chrom kb") +
      ggplot2::theme(legend.key = ggplot2::element_blank(),
            strip.background = ggplot2::element_rect(colour="black", fill="white"),
            axis.text.x = ggplot2::element_text(angle = 90, vjust = 0.5, hjust=1)) +
      ggplot2::facet_grid(.~CHROM)


    print(p4)

}
