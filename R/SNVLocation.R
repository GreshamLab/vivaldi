# vivaldi package
# Kate Johnson

SNVLocation = function(vcf_df, wkdir, freq_cutoff=0.02, coverage_cutoff=300){

  if (!dir.exists(glue("{wkdir}/SNV_figures"))) {
        dir.create(glue("{wkdir}/SNV_figures"))
      }

    xlim = length(levels(factor(vcf_df$CHROM)))

    ylim = length(levels(factor(vcf_df$sample)))

    plot4 = ggplot() +
        geom_point(data = vcf_df %>% filter(ALT_TYPE == 'minor') %>% droplevels(),
                      aes(x=POS, y=minorfreq, color=minor)) +

        geom_point(data = vcf_df %>% filter(ALT_TYPE == 'major') %>% droplevels(),
                      aes(x=POS, y=majorfreq, color=major)) +

        geom_rug(data = vcf_df %>% filter(ALT_TYPE == 'minor') %>% droplevels(),
                      aes(x=POS, y=minorfreq, color=minor)) +

        geom_rug(data = vcf_df %>% filter(ALT_TYPE == 'minor') %>% droplevels(),
                      aes(x=POS, y=minorfreq, color=minor)) +

        theme_bw() +

        theme(legend.key = element_blank(),
                        strip.background = element_rect(colour="black", fill="white"),
                        axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1)) +
        facet_grid(sample~CHROM,  scales = 'free_x')

    print(plot4)

    ggsave(plot4,
       filename = glue("{wkdir}SNV_figures/SNV.position.{freq_cutoff}.{coverage_cutoff}.pdf"),
       width = 1.8*xlim,
       height = 1.1*ylim, limitsize=FALSE, useDingbats = FALSE)
}
