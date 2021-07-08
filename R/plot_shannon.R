# vivaldi package
# Kate Johnson

plot_shannon = function(vcf_df){

    xlim = length(levels(factor(vcf_df$CHROM)))

    xlim2 = length(levels(factor(vcf_df$sample)))

    p1 = ggplot(vcf_df, aes(x=POS, y=shannon_ntpos)) +
            geom_point(alpha=0.6) +
            theme_bw() +
            theme(legend.key = element_blank(),
                        strip.background = element_rect(colour="black", fill="white"),
                        axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1)) +
        facet_grid(.~CHROM,  scales = 'free_x')

    print(p1)

    temp = vcf_df %>% select(CHROM, sample, shannon_chrom_perkb, genome_shannon_perkb)

    temp = temp[!duplicated(temp), ] %>% droplevels()

    p2 = ggplot(temp, aes(x=sample, y=shannon_chrom_perkb)) +
            geom_point() +
            theme_bw() +
            theme(legend.key = element_blank(),
                        strip.background = element_rect(colour="black", fill="white"),
                        axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1)) +
        facet_grid(.~CHROM)

    print(p2)

    temp = temp %>% select(-shannon_chrom_perkb, -CHROM)

    temp = temp[!duplicated(temp), ] %>% droplevels()

    p3 = ggplot(temp, aes(x=sample, y=genome_shannon_perkb)) +
            geom_point() +
            theme_bw() +
            theme(legend.key = element_blank(),
                        strip.background = element_rect(colour="black", fill="white"),
                        axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))

    print(p3)

}
