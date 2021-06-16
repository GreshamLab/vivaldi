# vivaldi package
# Kate Johnson

plotShannon = function(vcf_df, wkdir, freq_cutoff=0.02, coverage_cutoff=300){

  if (!dir.exists(glue("{wkdir}/SNV_figures"))) {
      dir.create(glue("{wkdir}/SNV_figures"))
    }

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

    ggsave(p1,
       filename = glue("{wkdir}SNV_figures/Shannon.ntpos.{freq_cutoff}.{coverage_cutoff}.pdf"),
       width = 1.8*xlim,
       height = 6, limitsize=FALSE, useDingbats = FALSE)

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

    ggsave(p2,
       filename = glue("{wkdir}SNV_figures/Shannon.chrom.perkb.{freq_cutoff}.{coverage_cutoff}.pdf"),
       width = 1.8*xlim,
       height = 6, limitsize=FALSE, useDingbats = FALSE)

    temp = temp %>% select(-shannon_chrom_perkb, -CHROM)

    temp = temp[!duplicated(temp), ] %>% droplevels()

    p3 = ggplot(temp, aes(x=sample, y=genome_shannon_perkb)) +
            geom_point() +
            theme_bw() +
            theme(legend.key = element_blank(),
                        strip.background = element_rect(colour="black", fill="white"),
                        axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))

    print(p3)

    ggsave(p3,
       filename = glue("{wkdir}SNV_figures/Shannon.perkb.{freq_cutoff}.{coverage_cutoff}.pdf"),
       width = 0.6*xlim,
       height = 6, limitsize=FALSE, useDingbats = FALSE)

}
