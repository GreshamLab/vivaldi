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
          ggplot(., aes(x=sample, y=tstv_ratio)) +
            geom_point() +
            theme_bw() +
            ggtitle("Genome Ts/Tv") +
            ylab("Genome Transition/Transversion ratio") +
            theme(legend.key = element_blank(),
            strip.background = element_rect(colour="black", fill="white"),
            axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))

    print(p1)

    p2 = df %>% filter(chrom_or_genome == 'tstv_genome_count') %>% droplevels() %>%
          ggplot(., aes(x=sample, y=tstv_ratio_perkb)) +
            geom_point() +
            theme_bw() +
            ggtitle("Genome Ts/Tv per kb") +
            ylab("Transition/Transversion ratio per genome kb") +
            theme(legend.key = element_blank(),
            strip.background = element_rect(colour="black", fill="white"),
            axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))

    print(p2)

    p3 = df %>% filter(chrom_or_genome == 'tstv_chrom_count') %>% droplevels() %>%
          ggplot(., aes(x=sample, y=tstv_ratio)) +
            geom_point() +
            theme_bw() +
            ggtitle("Chrom Ts/Tv") +
            ylab("Chrom Transition/Transversion ratio") +
            theme(legend.key = element_blank(),
            strip.background = element_rect(colour="black", fill="white"),
            axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1)) +
          facet_grid(.~CHROM)

    print(p3)

    p4 = df %>% filter(chrom_or_genome == 'tstv_chrom_count') %>% droplevels() %>%
          ggplot(., aes(x=sample, y=tstv_ratio_perkb)) +
            geom_point() +
            theme_bw() +
            ggtitle("Chrom Ts/Tv per kb") +
            ylab("Transition/Transversion ratio per chrom kb") +
            theme(legend.key = element_blank(),
            strip.background = element_rect(colour="black", fill="white"),
            axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1)) +
            facet_grid(.~CHROM)


    print(p4)

}
